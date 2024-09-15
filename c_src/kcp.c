/* kcp.c */
#include "kcp.h"
#include <string.h>

static ErlDrvTermData am_kcp_output;
static ErlDrvTermData am_kcp_async;
static ErlDrvTermData am_kcp_reply;
static ErlDrvTermData am_ok;
static ErlDrvTermData am_error;

void kcp_atom_init(void) {
    INIT_ATOM(kcp_output);
    INIT_ATOM(kcp_async);
    INIT_ATOM(kcp_reply);
    INIT_ATOM(ok);
    INIT_ATOM(error);
}

int kcp_async_binary_data(kcp_descriptor* kd, size_t bsize) {
    ErlDrvTermData spec [2 * LOAD_ATOM_CNT + LOAD_PORT_CNT + LOAD_UINT_CNT + LOAD_BUF2BINARY_CNT + LOAD_TUPLE_CNT * 2];
    ErlDrvTermData caller = kd->caller;
    int i = 0;
    i = LOAD_ATOM(spec, i, am_kcp_async);                /* 'kcp_async' */
    i = LOAD_PORT(spec, i, kd->dport);                /* S        */
    i = LOAD_UINT (spec, i, kd->ref);                /* Ref        */
    i = LOAD_ATOM(spec, i, am_ok);
    i = LOAD_BUF2BINARY(spec, i, kd->buffer, bsize);
    /* Close up the {ok, ...} or {error, ...} tuple: */
    i = LOAD_TUPLE(spec, i, 2);
    /* Close up the outer {kcp_async, S, Ref, {ok|error, ...}} tuple: */
    i = LOAD_TUPLE(spec, i, 4);
    kd->caller = 0;
    return erl_drv_send_term(kd->dport, caller, spec, i);
}

/* send message:
**      {kcp_async, Port, Ref, {error,Reason}}
*/
int send_async_errorno(kcp_descriptor* kd, int err) {
    return send_async_error(kd, driver_mk_atom(errno_str(err)));
}

/* send message:
**      {kcp_async, Port, Ref, {error,Reason}}
*/
int send_async_error(kcp_descriptor* kd, ErlDrvTermData reason) {
    ErlDrvTermData spec[3*LOAD_ATOM_CNT + LOAD_PORT_CNT + LOAD_UINT_CNT + 2*LOAD_TUPLE_CNT];
    ErlDrvTermData caller = kd->caller;
    int i = 0;
    i = LOAD_ATOM(spec, i, am_kcp_async);
    i = LOAD_PORT(spec, i, kd->dport);
    i = LOAD_UINT(spec, i, kd->ref);
    i = LOAD_ATOM(spec, i, am_error);
    i = LOAD_ATOM(spec, i, reason);
    i = LOAD_TUPLE(spec, i, 2);
    i = LOAD_TUPLE(spec, i, 4);
    kd->caller = 0;
    return erl_drv_send_term(kd->dport, caller, spec, i);
}

/* send:
**   {kcp_reply, S, ok}
*/
int kcp_reply_ok(kcp_descriptor* kd, IUINT32 cmd)
{
    ErlDrvTermData spec[2*LOAD_ATOM_CNT + LOAD_PORT_CNT + LOAD_UINT_CNT + LOAD_TUPLE_CNT];
    ErlDrvTermData caller = driver_caller(kd->port);
    int i = 0;
    i = LOAD_ATOM(spec, i, am_kcp_reply);
    i = LOAD_PORT(spec, i, kd->dport);
    i = LOAD_UINT(spec, i, cmd);
    i = LOAD_ATOM(spec, i, am_ok);
    i = LOAD_TUPLE(spec, i, 4);
    return erl_drv_send_term(kd->dport, caller, spec, i);
}

/* send:
**   {kcp_reply, S, {error, Reason}}
*/
int kcp_reply_error(kcp_descriptor* kd, IUINT32 cmd, int err) {
    return kcp_reply_xerror(kd, cmd, errno_str(err));
}

int kcp_reply_xerror(kcp_descriptor* kd, IUINT32 cmd, char* xerr) {
    return kcp_reply_error_am(kd, cmd, driver_mk_atom(xerr));
}

int kcp_reply_error_am(kcp_descriptor* kd, IUINT32 cmd, ErlDrvTermData reason) {
    ErlDrvTermData spec[3*LOAD_ATOM_CNT + LOAD_PORT_CNT + LOAD_UINT_CNT + 2*LOAD_TUPLE_CNT];
    ErlDrvTermData caller = driver_caller(kd->port);
    int i = 0;
    i = LOAD_ATOM(spec, i, am_kcp_reply);
    i = LOAD_PORT(spec, i, kd->dport);
    i = LOAD_UINT(spec, i, cmd);
    i = LOAD_ATOM(spec, i, am_error);
    i = LOAD_ATOM(spec, i, reason);
    i = LOAD_TUPLE(spec, i, 2);
    i = LOAD_TUPLE(spec, i, 4);
    return erl_drv_send_term(kd->dport, caller, spec, i);
}

/* general control reply function */
ErlDrvSSizeT ctl_reply(int rep, char* buf, ErlDrvSizeT len, char** rbuf, ErlDrvSizeT rsize) {
    char* ptr;
    if ((len+1) > rsize) {
        ptr = ALLOC(len+1);
        *rbuf = ptr;
    }
    else
    ptr = *rbuf;
    *ptr++ = rep;
    memcpy(ptr, buf, len);
    return len+1;
}

/* general control error reply function */
ErlDrvSSizeT ctl_error(int err, char** rbuf, ErlDrvSizeT rsize) {
    char* s = errno_str(err);
    return ctl_reply(KCP_REP_ERROR, s, strlen(s), rbuf, rsize);
}

ErlDrvSSizeT ctl_xerror(char* xerr, char** rbuf, ErlDrvSizeT rsize) {
    int n = strlen(xerr);
    return ctl_reply(KCP_REP_ERROR, xerr, n, rbuf, rsize);
}

/* return lowercase string form of errno value */
char *errno_str(int err) {
    switch(err) {
        case KCP_ERRNO_SYSTEM_LIMIT:
            return "system_limit";
        default:
            return erl_errno_id(err);
    }
}

int kcp_output(const char *buf, int len, ikcpcb *kcp, void *e) {
    kcp_descriptor* kd = (kcp_descriptor*)e;
    // req to send buffer
    ErlDrvTermData spec[LOAD_ATOM_CNT + LOAD_PORT_CNT + LOAD_BUF2BINARY_CNT + LOAD_TUPLE_CNT];
    int i = 0;
    i = LOAD_ATOM(spec, i, am_kcp_output);                /* 'kcp_async' */
    i = LOAD_PORT(spec, i, kd->dport);                /* S        */
    i = LOAD_BUF2BINARY(spec, i, buf, len);
    /* Close up the outer {kcp_output, S, Data} tuple: */
    i = LOAD_TUPLE(spec, i, 3);
    return erl_drv_output_term(kd->dport, spec, i);
}