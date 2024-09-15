#ifndef __KCP_H__
#define __KCP_H__
#include <stdlib.h>
#include "ikcp.h"

#include "erl_driver.h"

#if IWORDS_BIG_ENDIAN || IWORDS_MUST_ALIGN
#define btohl(s, r)   (memcpy((IUINT8*)&r, (IUINT8*)s, 4),s+4)
#define btohs(s, r)   (memcpy((IUINT8*)&r, (IUINT8*)s, 2),s+2)
#else
#define btohl(s, r)   (((IUINT8*)&r)[0] = ((IUINT8*)s)[3], \
                       ((IUINT8*)&r)[1] = ((IUINT8*)s)[2], \
                       ((IUINT8*)&r)[2] = ((IUINT8*)s)[1], \
                       ((IUINT8*)&r)[3] = ((IUINT8*)s)[0], \
                       s+4)
#define btohs(s, r)   (((IUINT8*)&r)[0] = ((IUINT8*)s)[1], \
                       ((IUINT8*)&r)[1] = ((IUINT8*)s)[0], \
                       s+2)
#endif
#define btohc(s, r)   (r = (((IUINT8*)s)[0]), s+1)

#if IWORDS_BIG_ENDIAN || IWORDS_MUST_ALIGN
#define hltob(r, s)   (memcpy((IUINT8*)s, (IUINT8*)&r, 4),s+4)
#else
#define hltob(r, s)   (((IUINT8*)s)[0] = ((IUINT8*)&r)[3],\
                       ((IUINT8*)s)[1] = ((IUINT8*)&r)[2],\
                       ((IUINT8*)s)[2] = ((IUINT8*)&r)[1],\
                       ((IUINT8*)s)[3] = ((IUINT8*)&r)[0],\
                       s+4)
#endif


#define ALLOC(X) driver_alloc((X))
#define REALLOC(X,Y) driver_realloc((X), (Y))
#define FREE(P) driver_free((P))

#define INIT_ATOM(NAME) am_ ## NAME = driver_mk_atom(#NAME)

#define LOAD_ATOM_CNT 2
#define LOAD_ATOM(vec, i, atom) \
  (((vec)[(i)] = ERL_DRV_ATOM), \
  ((vec)[(i)+1] = (atom)), \
  ((i)+LOAD_ATOM_CNT))

#define LOAD_INT_CNT 2
#define LOAD_INT(vec, i, val) \
  (((vec)[(i)] = ERL_DRV_INT), \
  ((vec)[(i)+1] = (ErlDrvTermData)(val)), \
  ((i)+LOAD_INT_CNT))

#define LOAD_UINT_CNT 2
#define LOAD_UINT(vec, i, val) \
  (((vec)[(i)] = ERL_DRV_UINT), \
  ((vec)[(i)+1] = (ErlDrvTermData)(val)), \
  ((i)+LOAD_UINT_CNT))

#define LOAD_PORT_CNT 2
#define LOAD_PORT(vec, i, port) \
  (((vec)[(i)] = ERL_DRV_PORT), \
  ((vec)[(i)+1] = (port)), \
  ((i)+LOAD_PORT_CNT))

#define LOAD_PID_CNT 2
#define LOAD_PID(vec, i, pid) \
  (((vec)[(i)] = ERL_DRV_PID), \
  ((vec)[(i)+1] = (pid)), \
  ((i)+LOAD_PID_CNT))

#define LOAD_BINARY_CNT 4
#define LOAD_BINARY(vec, i, bin, offs, len) \
  (((vec)[(i)] = ERL_DRV_BINARY), \
  ((vec)[(i)+1] = (ErlDrvTermData)(bin)), \
  ((vec)[(i)+2] = (len)), \
  ((vec)[(i)+3] = (offs)), \
  ((i)+LOAD_BINARY_CNT))

#define LOAD_BUF2BINARY_CNT 3
#define LOAD_BUF2BINARY(vec, i, buf, len) \
  (((vec)[(i)] = ERL_DRV_BUF2BINARY), \
  ((vec)[(i)+1] = (ErlDrvTermData)(buf)), \
  ((vec)[(i)+2] = (len)), \
  ((i)+LOAD_BUF2BINARY_CNT))

#define LOAD_STRING_CNT 3
#define LOAD_STRING(vec, i, str, len) \
  (((vec)[(i)] = ERL_DRV_STRING), \
  ((vec)[(i)+1] = (ErlDrvTermData)(str)), \
  ((vec)[(i)+2] = (len)), \
  ((i)+LOAD_STRING_CNT))

#define LOAD_STRING_CONS_CNT 3
#define LOAD_STRING_CONS(vec, i, str, len) \
  (((vec)[(i)] = ERL_DRV_STRING_CONS), \
  ((vec)[(i)+1] = (ErlDrvTermData)(str)), \
  ((vec)[(i)+2] = (len)), \
  ((i)+LOAD_STRING_CONS_CNT))

#define LOAD_TUPLE_CNT 2
#define LOAD_TUPLE(vec, i, size) \
  (((vec)[(i)] = ERL_DRV_TUPLE), \
  ((vec)[(i)+1] = (size)), \
  ((i)+LOAD_TUPLE_CNT))

#define LOAD_NIL_CNT 1
#define LOAD_NIL(vec, i) \
  (((vec)[(i)] = ERL_DRV_NIL), \
  ((i)+LOAD_NIL_CNT))

#define LOAD_LIST_CNT 2
#define LOAD_LIST(vec, i, size) \
  (((vec)[(i)] = ERL_DRV_LIST), \
  ((vec)[(i)+1] = (size)), \
  ((i)+LOAD_LIST_CNT))

#define CLOCK()              erl_drv_monotonic_time(ERL_DRV_MSEC)
#define CLOCK32()          ((IUINT32)(CLOCK() & 0xfffffffful))
#define TIMEDIFF(later, earlier)  ((IINT32)((later) - (earlier)))

// state
#define KCP_F_OPEN           0x0001
#define KCP_F_ACTIVE         0x0002
#define KCP_F_CLOSE          0x0004

#define KCP_CTL_MAX_SIZE     256
// control indicator
#define KCP_CTL_CREATE       1
#define KCP_CTL_WNDSIZE      2
#define KCP_CTL_NODELAY      3
#define KCP_CTL_SETMTU       4
#define KCP_CTL_REQRECV      5
#define KCP_CTL_WAITSND      6
#define KCP_CTL_WAITRCV      7
#define KCP_CTL_GETWNDSIZE   8
#define KCP_CTL_GETNODELAY   9
#define KCP_CTL_GETMTU       10
#define KCP_CTL_PEEKSIZE     11

// message indicator
#define KCP_MSG_SEND         1
#define KCP_MSG_INPUT        2

// reply code
#define KCP_REP_ERROR       0
#define KCP_REP_OK          1

// error string
#define EXBADARG             "exbadarg"

#define KCP_ERRNO_SYSTEM_LIMIT  (15 << 8)

#define IS_OPEN(kd) (((kd)->state & KCP_F_OPEN) == KCP_F_OPEN)
#define IS_ACTIVE(kd) (((kd)->state & KCP_F_ACTIVE) == KCP_F_ACTIVE)

#define RECV_BUF_LEN_MIN (1 * 1024)
#define RECV_BUF_LEN_MAX (4 * 1024)

typedef struct {
    ErlDrvPort port;
    ErlDrvTermData dport;
    ErlDrvTermData caller;
    int in_timeout;
    int in_reqrecv;
    IUINT32 ref;
    IINT32 state;
    ikcpcb *kcp;
    char *buffer;
    size_t buf_len;
} kcp_descriptor;

#ifdef __cplusplus
extern "C" {
#endif

void kcp_atom_init(void);
int kcp_async_binary_data(kcp_descriptor* kd, size_t bsize);
int send_async_errorno(kcp_descriptor* kd, int err);
int send_async_error(kcp_descriptor* kd, ErlDrvTermData reason);
int kcp_reply_ok(kcp_descriptor* desc, IUINT32 cmd);
int kcp_reply_error(kcp_descriptor* kd, IUINT32 cmd, int err);
int kcp_reply_xerror(kcp_descriptor* kd, IUINT32 cmd, char* xerr);
int kcp_reply_error_am(kcp_descriptor* kd, IUINT32 cmd, ErlDrvTermData reason);
ErlDrvSSizeT ctl_reply(int rep, char* buf, ErlDrvSizeT len, char** rbuf, ErlDrvSizeT rsize);
ErlDrvSSizeT ctl_error(int err, char** rbuf, ErlDrvSizeT rsize);
ErlDrvSSizeT ctl_xerror(char* xerr, char** rbuf, ErlDrvSizeT rsize);
char *errno_str(int err);
int kcp_output(const char *buf, int len, ikcpcb *kcp, void *e);

#ifdef __cplusplus
}
#endif
#endif