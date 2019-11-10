#include <stdio.h>
#include <stddef.h>
#include <signal.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/ptrace.h>
#include <sys/user.h>
#include <sys/prctl.h>
#include <sys/types.h>
#include <stdint.h>
#include <errno.h>
#include <stdbool.h>
#include <assert.h>
#include <sys/signal.h>
#include <syscall.h>
#include <string.h>
#include <stdlib.h>

#include "../include/hwwp.h"

long   hwval[4]  = {0, 0, 0, 0};
long*  hwaddr[4] = {NULL, NULL, NULL, NULL};
long   hwset[4]  = {0, 0, 0, 0};
size_t hwlen[4]  = {0, 0, 0, 0};
long   hwmode[4] = {0, 0, 0, 0};

typedef void (*watchpoint_call)(long, long);
watchpoint_call hw_callback[4] = {NULL, NULL, NULL, NULL};

extern int errno;

#define SET_DR7(dr7, num) {\
    dr7 |= (0x1 << num * 2); \
}
#define SET_LEN_DR7(dr7, num, len) {\
    long n = 0; \
    switch (len) { \
        case 1: n = 0x0; break; \
        case 2: n = 0x1; break; \
        case 4: n = 0x3; break; \
        case 8: n = 0x2; break; \
        default: assert(0); \
    } \
    n <<= (18 + num * 4); \
    dr7 |= n; \
}
#define SET_MODE_DR7(dr7, num, mode) {\
    long n = mode;\
    n <<= (16 + num * 4); \
    dr7 |= n; \
}

#define CLEAR_DR7_INFO(dr7, num) {\
    switch (num) { \
        case 0: dr7 &= 0xfff000fc; break;\
        case 1: dr7 &= 0xff0f00f3; break;\
        case 2: dr7 &= 0xf0ff00cf; break;\
        case 3: dr7 &= 0x0fff003f; break;\
        default: assert(0); \
    }\
}

static void trival_handle(int sig){(void)sig;}

static void sigtrap_dispatch(int sig) //{
{
    assert(sig == SIGTRAP); (void)sig;
    void* old_handle = signal(SIGCONT, trival_handle);
    pid_t parent = getpid();
#ifdef SYS_gettid
    pid_t tid = syscall(SYS_gettid);
#else
#error "SYS_gettid unavailable on this system"
#endif
    pid_t child;
    int parent_status = 0;
    int  child_status = 0;
    long dr7_backup = 0;
    if(!(child = fork())) {
		if (ptrace(PTRACE_ATTACH, parent, NULL, NULL)) goto __error_exit;

		while (!WIFSTOPPED(parent_status))
			waitpid(parent, &parent_status, 0);

        int errno_backup = errno;
        errno = 0;
		long int dr7_ori = ptrace(PTRACE_PEEKUSER, parent, offsetof(struct user, u_debugreg[7]), (void*)0);
        if(errno != 0) goto __error_exit;
        errno = errno_backup;

		if (ptrace(PTRACE_POKEUSER, parent, offsetof(struct user, u_debugreg[7]), 0)) goto __error_exit;
        if (ptrace(PTRACE_POKEDATA, parent, &dr7_backup, dr7_ori)) goto __error_exit;
        if (ptrace(PTRACE_DETACH, parent, (void*)0, (void*)0)) goto __error_exit;
        syscall(SYS_tkill, tid, SIGCONT);
        _exit(0);
    }
    pause();
    int  i;
    long mask;
    char changed = 0;
    for( i = 0; i<=3; ++i) {
        if(hwaddr[i] == NULL) continue;
        if(hwset[i] && *hwaddr[i] != hwval[i]) {
            mask = ~0;
            mask <<= hwlen[i] * 8;
            if(hwlen[i] == 8) mask = 0;
            if((*hwaddr[i] | mask) != (hwval[i] | mask)) {
                changed = 1;
                break;
            }
        }
    }

    if(changed) {
        if(hw_callback[i] != NULL) {
            hw_callback[i](hwval[i], *hwaddr[i]);
            hwval[i] = *hwaddr[i];
        } 
        goto __success_return;
    }

    for(i = 0; i<=3; ++i) {
        if((hwmode[i] == 0x0) && hwset[i] == 1)
            printf("maybe execute watchpoint hit at 0x%lx\n", (long)hwaddr[i]);
        if((hwmode[i] == 0x3) && hwset[i] == 1)
            printf("maybe read watchpoint hit at 0x%lx\n", (long)hwaddr[i]);
    }

__success_return:
    if(!(child = fork())) {
		if (ptrace(PTRACE_ATTACH, parent, NULL, NULL)) goto __error_exit;
		while (!WIFSTOPPED(parent_status))
			waitpid(parent, &parent_status, 0);
		if (ptrace(PTRACE_POKEUSER, parent, offsetof(struct user, u_debugreg[7]), dr7_backup)) goto __error_exit;
        if (ptrace(PTRACE_DETACH, parent, (void*)0, (void*)0)) goto __error_exit;
        _exit(0);
    }
    waitpid(child, &child_status, 0);
    signal(SIGCONT, old_handle);
    return;

__error_exit:
        fprintf(stderr, "%s\n", strerror(errno));
        _exit(1);
} //}

/// <summary> watchpoint </summary>
/// <param name="bpno"> from 0 to 3, dr0 dr1 dr2 dr3 </param>
/// <param name="mode"> "w", "rw", "e" </param>
bool install_watchpoint(void *addr, int bpno, watchpoint_call func, char* mode, size_t len) //{
{
    if(bpno >= 4 || bpno < 0) return false;
    if(len != 1 && len != 2 && len != 4 && len != 8)  return false;
    if(strlen(mode) > 2)      return false;
    long watch_mode = 0;
    switch(mode[0]) {
        case 'e': watch_mode = 0x0; len = 1; break;
        case 'w': watch_mode = 0x1; break;
        case 'r': assert(mode[1] == 'w'); watch_mode = 0x3; break;
    }

	pid_t child        = 0;
	pid_t parent       = getpid();
	int   child_status = 0;

    struct   sigaction act;
    sigset_t sigmask;
    sigfillset(&sigmask);
    sigdelset(&sigmask, SIGCONT);
    act.sa_mask    = sigmask;
    act.sa_handler = NULL;
    sigaction(SIGTRAP, &act, NULL);

	if (!(child = fork()))
	{
		int parent_status = 0;
		if (ptrace(PTRACE_ATTACH, parent, NULL, NULL)) goto __error_exit;

		while (!WIFSTOPPED(parent_status))
			waitpid(parent, &parent_status, 0);

        if (ptrace(PTRACE_POKEUSER, parent, offsetof(struct user, u_debugreg[bpno]), addr)) goto __error_exit;

        int errno_backup = errno;
        errno = 0;
		long int dr7_ori = ptrace(PTRACE_PEEKUSER, parent, offsetof(struct user, u_debugreg[7]), (void*)0);
//        printf("origin 0x%lx\n", dr7_ori);
        if(errno != 0) goto __error_exit;
        errno   = errno_backup;
        CLEAR_DR7_INFO(dr7_ori, bpno);
        SET_DR7(dr7_ori, bpno);
        SET_LEN_DR7(dr7_ori, bpno, len);
        SET_MODE_DR7(dr7_ori, bpno, watch_mode);

 //       printf("mmm 0x%lx\n", dr7_ori);
		if (ptrace(PTRACE_POKEUSER, parent, offsetof(struct user, u_debugreg[7]), dr7_ori)) goto __error_exit;

        if (ptrace(PTRACE_POKEDATA, parent, &hwset[bpno], 0x1))          goto __error_exit;
        if (ptrace(PTRACE_POKEDATA, parent, &hwaddr[bpno], addr))        goto __error_exit;
        if (ptrace(PTRACE_POKEDATA, parent, &hwval[bpno], *(long*)addr)) goto __error_exit;
        if (ptrace(PTRACE_POKEDATA, parent, &hw_callback[bpno], func))   goto __error_exit;
        if (ptrace(PTRACE_POKEDATA, parent, &hwlen[bpno], len))          goto __error_exit;
        if (ptrace(PTRACE_POKEDATA, parent, &hwmode[bpno], watch_mode))  goto __error_exit;

		if (ptrace(PTRACE_DETACH, parent, NULL, NULL)) goto __error_exit;
		_exit(0);

__error_exit:
        fprintf(stderr, "%s\n", strerror(errno));
        _exit(1);
	}

	waitpid(child, &child_status, 0);

    act.sa_handler = sigtrap_dispatch;
    sigaction(SIGTRAP, &act, NULL);

	if (WIFEXITED(child_status) && !WEXITSTATUS(child_status))
		return true;
	return false;
} //}

bool disable_watchpoint(int bpno) //{
{
    if(bpno < 0 || bpno > 3) return false;
    bool remove  = install_watchpoint(0x0, bpno, NULL, "w", 8);
    hwlen[bpno]  = 0; hwset[bpno]       = 0;
    hwval[bpno]  = 0; hwaddr[bpno]      = NULL;
    hwmode[bpno] = 0; hw_callback[bpno] = NULL;
    return remove;
} //}
