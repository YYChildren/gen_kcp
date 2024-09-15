/* port_driver.c */

#include <stdio.h>
#include <string.h>
#include <errno.h>
#include "kcp.h"

// driver interface
static int kcp_drv_init(void);
static ErlDrvData kcp_drv_start(ErlDrvPort port, char *buff);
static void kcp_drv_stop(ErlDrvData handle);
static void kcp_drv_output(ErlDrvData handle, char *buff, ErlDrvSizeT bufflen);
static ErlDrvSSizeT kcp_drv_ctl(ErlDrvData e, unsigned int cmd, char* buf, ErlDrvSizeT len, char** rbuf, ErlDrvSizeT rlen);
static void kcp_drv_timeout(ErlDrvData e);

// base flow
static ikcpcb* kcp_create(kcp_descriptor* kd, unsigned int conv);
static void maybe_recv_data(kcp_descriptor * kd);
static int kcp_recv(kcp_descriptor * kd);
static int maybe_extend_buffer(char **p_buf, size_t *p_buf_len, size_t need_size);
static void maybe_set_timer(kcp_descriptor * kd);

// control flow
typedef ErlDrvSSizeT (*KCP_CTL_HOOK)(kcp_descriptor* kd, char* buf, ErlDrvSizeT len, char** rbuf, ErlDrvSizeT rlen);
static KCP_CTL_HOOK kcp_ctl_list[KCP_CTL_MAX_SIZE];
static void kcp_ctl_init(void);
static ErlDrvSSizeT kcp_ctl_create(kcp_descriptor* kd, char* buf, ErlDrvSizeT len, char** rbuf, ErlDrvSizeT rlen);
static ErlDrvSSizeT kcp_ctl_wndsize(kcp_descriptor* kd, char* buf, ErlDrvSizeT len, char** rbuf, ErlDrvSizeT rlen);
static ErlDrvSSizeT kcp_ctl_nodelay(kcp_descriptor* kd, char* buf, ErlDrvSizeT len, char** rbuf, ErlDrvSizeT rlen);
static ErlDrvSSizeT kcp_ctl_setmtu(kcp_descriptor* kd, char* buf, ErlDrvSizeT len, char** rbuf, ErlDrvSizeT rlen);
static ErlDrvSSizeT kcp_ctl_reqrecv(kcp_descriptor* kd, char* buf, ErlDrvSizeT len, char** rbuf, ErlDrvSizeT rlen);
static ErlDrvSSizeT kcp_ctl_waitsnd(kcp_descriptor* kd, char* buf, ErlDrvSizeT len, char** rbuf, ErlDrvSizeT rlen);
static ErlDrvSSizeT kcp_ctl_waitrcv(kcp_descriptor* kd, char* buf, ErlDrvSizeT len, char** rbuf, ErlDrvSizeT rlen);
static ErlDrvSSizeT kcp_ctl_getwndsize(kcp_descriptor* kd, char* buf, ErlDrvSizeT len, char** rbuf, ErlDrvSizeT rlen);
static ErlDrvSSizeT kcp_ctl_getnodelay(kcp_descriptor* kd, char* buf, ErlDrvSizeT len, char** rbuf, ErlDrvSizeT rlen);
static ErlDrvSSizeT kcp_ctl_getmtu(kcp_descriptor* kd, char* buf, ErlDrvSizeT len, char** rbuf, ErlDrvSizeT rlen);
static ErlDrvSSizeT kcp_ctl_peeksize(kcp_descriptor* kd, char* buf, ErlDrvSizeT len, char** rbuf, ErlDrvSizeT rlen);

static ErlDrvEntry kcp_driver_entry = {
    kcp_drv_init,
    kcp_drv_start,
    kcp_drv_stop,
    kcp_drv_output,
    NULL,
    NULL,
    "gen_kcp",
    NULL,
    NULL,
    kcp_drv_ctl,
    kcp_drv_timeout,
    NULL,
    NULL,
    NULL,
    NULL,
    NULL,
    ERL_DRV_EXTENDED_MARKER,
    ERL_DRV_EXTENDED_MAJOR_VERSION,
    ERL_DRV_EXTENDED_MINOR_VERSION,
    0,
    NULL,
    NULL,
    NULL
};

static int kcp_drv_init(void) {
    ikcp_allocator(driver_alloc, driver_free);
    kcp_atom_init();
    kcp_ctl_init();
    return 0;
}

static ErlDrvData kcp_drv_start(ErlDrvPort port, char *buff) {
    kcp_descriptor* kd = (kcp_descriptor*)ALLOC(sizeof(kcp_descriptor));
    if(!kd) return ERL_DRV_ERROR_ERRNO;
    kd->port = port;
    kd->dport = driver_mk_port(port);
    kd->caller = 0;
    kd->state = KCP_F_OPEN;
    kd->in_timeout = 0;
    kd->in_reqrecv = 0;
    kd->kcp = NULL;
    kd->buf_len = 0;
    kd->buffer = NULL;
    set_port_control_flags(port, PORT_CONTROL_FLAG_BINARY);
    return (ErlDrvData)kd;
}

static void kcp_drv_stop(ErlDrvData e) {
    kcp_descriptor* kd = (kcp_descriptor*)e;
    if(kd) {
        kd->state = KCP_F_CLOSE;
        if(kd->kcp) {
            kd->kcp->user = NULL;
            ikcp_release(kd->kcp);
        }
        if(kd->buffer) {
            FREE(kd->buffer);
        }
        FREE((char*)e);
    }
}

static void kcp_drv_output(ErlDrvData e, char *buf, ErlDrvSizeT len) {
    kcp_descriptor* kd = (kcp_descriptor*)e;
    IUINT8 cmd;
    buf = btohc(buf, cmd);
    if(!IS_ACTIVE(kd)) {
        kcp_reply_error(kd, (IUINT32)cmd, EINVAL);
        return;
    }
    int res;
    switch (cmd) {
        case KCP_MSG_SEND:
            res = ikcp_send(kd->kcp, (const char *)buf, (int)(len - 1));
            break;
        case KCP_MSG_INPUT:
            res = ikcp_input(kd->kcp, (const char *)buf, (long)(len - 1));
            break;
        default:
            res = 1;
            return;
    }
    if(res > 0) {
        kcp_reply_xerror(kd, (IUINT32)cmd, EXBADARG);
    } else if(res < 0) {
        kcp_reply_error(kd, (IUINT32)cmd, EAGAIN);
    } else {
        if(KCP_MSG_INPUT == cmd) maybe_recv_data(kd);
        maybe_set_timer(kd);
        kcp_reply_ok(kd, (IUINT32)cmd);
    }
}

static void kcp_drv_timeout(ErlDrvData e) {
    kcp_descriptor *kd = (kcp_descriptor*) e;
    kd->in_timeout = 0;
    IUINT32 now = CLOCK32();
    ikcp_update(kd->kcp, now);
    if(kd->kcp->probe != 0 || ikcp_waitsnd(kd->kcp) > 0 || ikcp_waitrcv(kd->kcp) > 0) {
        maybe_set_timer(kd);
    }
}

static ErlDrvSSizeT kcp_drv_ctl(ErlDrvData e, unsigned int cmd, char* buf, ErlDrvSizeT len, char** rbuf, ErlDrvSizeT rlen) {
    kcp_descriptor* kd = (kcp_descriptor*)e;
    KCP_CTL_HOOK kcp_ctl = cmd < KCP_CTL_MAX_SIZE ? kcp_ctl_list[cmd] : NULL;
    if(kcp_ctl) {
        return kcp_ctl(kd, buf, len, rbuf, rlen);
    } else {
        return ctl_xerror(EXBADARG, rbuf, rlen);
    }
}

static ikcpcb* kcp_create(kcp_descriptor* kd, unsigned int conv) {
    ikcpcb* kcp = NULL;
    kcp = ikcp_create(conv, (void*)kd);
    if(!kcp){
        return NULL;
    } else{
        kcp->output = kcp_output;
        return kcp;
    }
}

static void maybe_set_timer(kcp_descriptor * kd) {
    IUINT32 now = CLOCK32();
    IUINT32 next = ikcp_check(kd->kcp, now);
    unsigned long nleft = (unsigned long)TIMEDIFF(next, now);
    if(kd->in_timeout) {
        unsigned long time_left = 0;
        driver_read_timer(kd->port, &time_left);
        if(TIMEDIFF(time_left, nleft) > 0) {
            driver_cancel_timer(kd->port);
            driver_set_timer(kd->port, nleft);
        }
    } else {
        kd->in_timeout = 1;
        driver_set_timer(kd->port, nleft);
    }
}

static void maybe_recv_data(kcp_descriptor * kd) {
    if(kd->in_reqrecv) {
        int hr = kcp_recv(kd);
        if(hr >= 0) {
            kcp_async_binary_data(kd, (size_t)hr);
            kd->in_reqrecv = 0;
        } else if(hr == -4) {
            // no data
        } else if(hr == -5 || hr == -6) {
            send_async_errorno(kd, ENOMEM);
            kd->in_reqrecv = 0;
        }
    }
}

static int kcp_recv(kcp_descriptor * kd) {
    int peeksize = 0;
    int times;
    for(times = 2; times > 0; times--){
        int er = maybe_extend_buffer(&kd->buffer, &kd->buf_len, peeksize);
        // OOM 的情况下应该关闭连接，具体操作由上层处理
        if(er < 0) {
            return -5;
        }
        int hr = ikcp_recv(kd->kcp, kd->buffer, kd->buf_len);
        if(hr == -1 || hr == -2) {
            return -4;
        } else if(hr == -3) {
            peeksize = ikcp_peeksize(kd->kcp);
            continue;
        } else if(hr ==0) {
            return 0;
        } else {
            return hr;
        }
    }
    // 理论上不应该会跑到这里，跑到这里意味着出现了意料之外的代码，这时应该关闭连接
    return -6;
}

// 可能需要增大buffer
static int maybe_extend_buffer(char **p_buf, size_t *p_buf_len, size_t need_size) {
    *p_buf_len = *p_buf ? *p_buf_len : 0;
    int nb_len = RECV_BUF_LEN_MIN > *p_buf_len ? RECV_BUF_LEN_MIN : *p_buf_len;
    for (; nb_len < need_size; nb_len <<= 1);
    if(nb_len > RECV_BUF_LEN_MAX) {
        return -1;
    }
    if(nb_len > *p_buf_len) {
        char *t_buf = ALLOC(nb_len);
        if(!t_buf) {
            return -2;
        }
        if(*p_buf) {
            FREE(*p_buf);
        }
        *p_buf = t_buf;
        *p_buf_len = nb_len;
        return 0;
    } else {
        return 0;
    }
}

static void kcp_ctl_init(void) {
    for(int i = 0;i < KCP_CTL_MAX_SIZE;i++) {
        kcp_ctl_list[i] = NULL;
    }
    kcp_ctl_list[KCP_CTL_CREATE] = &kcp_ctl_create;
    kcp_ctl_list[KCP_CTL_WNDSIZE] = &kcp_ctl_wndsize;
    kcp_ctl_list[KCP_CTL_NODELAY] = &kcp_ctl_nodelay;
    kcp_ctl_list[KCP_CTL_SETMTU] = &kcp_ctl_setmtu;
    kcp_ctl_list[KCP_CTL_REQRECV] = &kcp_ctl_reqrecv;
    kcp_ctl_list[KCP_CTL_WAITSND] = &kcp_ctl_waitsnd;
    kcp_ctl_list[KCP_CTL_WAITRCV] = &kcp_ctl_waitrcv;
    kcp_ctl_list[KCP_CTL_GETWNDSIZE] = &kcp_ctl_getwndsize;
    kcp_ctl_list[KCP_CTL_GETNODELAY] = &kcp_ctl_getnodelay;
    kcp_ctl_list[KCP_CTL_GETMTU] = &kcp_ctl_getmtu;
    kcp_ctl_list[KCP_CTL_PEEKSIZE] = &kcp_ctl_peeksize;
}

static ErlDrvSSizeT kcp_ctl_create(kcp_descriptor* kd, char* buf, ErlDrvSizeT len, char** rbuf, ErlDrvSizeT rlen) {
    if(!IS_OPEN(kd)) {
        return ctl_error(EINVAL, rbuf, rlen);
    }
    IUINT32 conv;
    buf = btohl(buf, conv);
    kd->kcp = kcp_create(kd, conv);
    kd->state |= KCP_F_ACTIVE;
    kd->in_reqrecv = 0;
    kd->ref = 0;
    return ctl_reply(KCP_REP_OK, NULL, 0, rbuf, rlen);
}

static ErlDrvSSizeT kcp_ctl_wndsize(kcp_descriptor* kd, char* buf, ErlDrvSizeT len, char** rbuf, ErlDrvSizeT rlen) {
    if(!IS_ACTIVE(kd)) {
        return ctl_error(EINVAL, rbuf, rlen);
    }
    IUINT32 sndwnd, rcvwnd;
    buf = btohl(buf, sndwnd);
    buf = btohl(buf, rcvwnd);
    ikcp_wndsize(kd->kcp, (int)sndwnd, (int)rcvwnd);
    return ctl_reply(KCP_REP_OK, NULL, 0, rbuf, rlen);
}

static ErlDrvSSizeT kcp_ctl_nodelay(kcp_descriptor* kd, char* buf, ErlDrvSizeT len, char** rbuf, ErlDrvSizeT rlen) {
    if(!IS_ACTIVE(kd)) {
        return ctl_error(EINVAL, rbuf, rlen);
    }
    IUINT32 nodelay,interval,resend,nc;
    buf = btohl(buf, nodelay);
    buf = btohl(buf, interval);
    buf = btohl(buf, resend);
    buf = btohl(buf, nc);
    ikcp_nodelay(kd->kcp, (int)nodelay, (int)interval, (int)resend, (int)nc);
    return ctl_reply(KCP_REP_OK, NULL, 0, rbuf, rlen);
}

static ErlDrvSSizeT kcp_ctl_setmtu(kcp_descriptor* kd, char* buf, ErlDrvSizeT len, char** rbuf, ErlDrvSizeT rlen) {
    if(!IS_ACTIVE(kd)) {
        return ctl_error(EINVAL, rbuf, rlen);
    }
    IUINT32 mtu;
    buf = btohl(buf, mtu);
    ikcp_setmtu(kd->kcp, mtu);
    return ctl_reply(KCP_REP_OK, NULL, 0, rbuf, rlen);
}

static ErlDrvSSizeT kcp_ctl_reqrecv(kcp_descriptor* kd, char* buf, ErlDrvSizeT len, char** rbuf, ErlDrvSizeT rlen) {
    if(!IS_ACTIVE(kd)) {
        return ctl_error(EINVAL, rbuf, rlen);
    }
    int tbuf_len = 4;
    char tbuf[tbuf_len];
    char *ptr = tbuf;
    kd->caller = driver_caller(kd->port);
    if (kd->in_reqrecv) {
        ptr = hltob(kd->ref, ptr);
        return ctl_reply(KCP_REP_OK, tbuf, tbuf_len, rbuf, rlen);
    }
    kd->ref++;
    kd->in_reqrecv = 1;
    maybe_recv_data(kd);
    ptr = hltob(kd->ref, ptr);
    return ctl_reply(KCP_REP_OK, tbuf, tbuf_len, rbuf, rlen);
}

static ErlDrvSSizeT kcp_ctl_waitsnd(kcp_descriptor* kd, char* buf, ErlDrvSizeT len, char** rbuf, ErlDrvSizeT rlen) {
    if(!IS_ACTIVE(kd)) {
        return ctl_error(EINVAL, rbuf, rlen);
    }
    int tbuf_len = 4;
    char tbuf[tbuf_len];
    char *ptr = tbuf;
    IUINT32 waitsnd = (IUINT32)ikcp_waitsnd(kd->kcp);
    ptr = hltob(waitsnd, ptr);
    return ctl_reply(KCP_REP_OK, tbuf, tbuf_len, rbuf, rlen);
}

static ErlDrvSSizeT kcp_ctl_waitrcv(kcp_descriptor* kd, char* buf, ErlDrvSizeT len, char** rbuf, ErlDrvSizeT rlen) {
    if(!IS_ACTIVE(kd)) {
        return ctl_error(EINVAL, rbuf, rlen);
    }
    int tbuf_len = 4;
    char tbuf[tbuf_len];
    char *ptr = tbuf;
    IUINT32 waitrcv = (IUINT32)ikcp_waitrcv(kd->kcp);
    ptr = hltob(waitrcv, ptr);
    return ctl_reply(KCP_REP_OK, tbuf, tbuf_len, rbuf, rlen);
}

static ErlDrvSSizeT kcp_ctl_getwndsize(kcp_descriptor* kd, char* buf, ErlDrvSizeT len, char** rbuf, ErlDrvSizeT rlen) {
    if(!IS_ACTIVE(kd)) {
        return ctl_error(EINVAL, rbuf, rlen);
    }
    int tbuf_len = 8;
    char tbuf[tbuf_len];
    char *ptr = tbuf;
    IUINT32 snd_wnd = (IUINT32)(kd->kcp->snd_wnd);
    IUINT32 rcv_wnd = (IUINT32)(kd->kcp->rcv_wnd);
    ptr = hltob(snd_wnd, ptr);
    ptr = hltob(rcv_wnd, ptr);
    return ctl_reply(KCP_REP_OK, tbuf, tbuf_len, rbuf, rlen);
}

static ErlDrvSSizeT kcp_ctl_getnodelay(kcp_descriptor* kd, char* buf, ErlDrvSizeT len, char** rbuf, ErlDrvSizeT rlen) {
    if(!IS_ACTIVE(kd)) {
        return ctl_error(EINVAL, rbuf, rlen);
    }
    int tbuf_len = 16;
    char tbuf[tbuf_len];
    char *ptr = tbuf;
    IUINT32 nodelay = (IUINT32)(kd->kcp->nodelay);
    IUINT32 interval = (IUINT32)(kd->kcp->interval);
    IUINT32 resend = (IUINT32)(kd->kcp->fastresend);
    IUINT32 nc = (IUINT32)(kd->kcp->nocwnd);
    ptr = hltob(nodelay, ptr);
    ptr = hltob(interval, ptr);
    ptr = hltob(resend, ptr);
    ptr = hltob(nc, ptr);
    return ctl_reply(KCP_REP_OK, tbuf, tbuf_len, rbuf, rlen);
}

static ErlDrvSSizeT kcp_ctl_getmtu(kcp_descriptor* kd, char* buf, ErlDrvSizeT len, char** rbuf, ErlDrvSizeT rlen) {
    if(!IS_ACTIVE(kd)) {
        return ctl_error(EINVAL, rbuf, rlen);
    }
    int tbuf_len = 4;
    char tbuf[tbuf_len];
    char *ptr = tbuf;
    IUINT32 mtu = (IUINT32)(kd->kcp->mtu);
    ptr = hltob(mtu, ptr);
    return ctl_reply(KCP_REP_OK, tbuf, tbuf_len, rbuf, rlen);
}

static ErlDrvSSizeT kcp_ctl_peeksize(kcp_descriptor* kd, char* buf, ErlDrvSizeT len, char** rbuf, ErlDrvSizeT rlen) {
    if(!IS_ACTIVE(kd)) {
        return ctl_error(EINVAL, rbuf, rlen);
    }
    int tbuf_len = 4;
    char tbuf[tbuf_len];
    char *ptr = tbuf;
    IINT32 peeksize = ikcp_peeksize(kd->kcp);
    ptr = hltob(peeksize, ptr);
    return ctl_reply(KCP_REP_OK, tbuf, tbuf_len, rbuf, rlen);
}

DRIVER_INIT(kcp_drv) /* must match name in driver_entry */
{
    return &kcp_driver_entry;
}