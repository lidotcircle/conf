#ifndef HWWP_H_
#define HWWP_H_

#if defined(WIN64) || defined(WIN32)
#error "don't support windows"
#endif // WIN64 || WIN32

#ifdef __cplusplus
extern "C" {
#endif // __cplusplus

#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>

typedef void (*watchpoint_call)(long, long);

/// <summary> watchpoint </summary>
/// <param name="bpno"> from 0 to 3, dr0 dr1 dr2 dr3 </param>
/// <param name="mode"> "w", "rw", "e" </param>
bool install_watchpoint(void *addr, int bpno, watchpoint_call func, char* mode, size_t len);

/// <param name="bpno"> from 0 to 3, dr0 dr1 dr2 dr3 </param>
bool disable_watchpoint(int bpno);

inline void trival_call_back(long o, long n){printf("old value: 0x%lx, new value: 0x%lx\n", o, n);}

#ifdef __cplusplus
}
#endif // __cplusplus

#endif // HWWP_H_
