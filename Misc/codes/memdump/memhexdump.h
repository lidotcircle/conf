#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include<errno.h>

#include<sys/ioctl.h>

#if !defined(MEMHEXDUMP_H)
#define MEMHEXDUMP_H 1

// main algrothim
void __memtobase__(const void* mem, void* memstr, size_t n,
        char base, unsigned short win_cols);

void __M_memtobase__(const void* mem, void* memstr, size_t n,
        char base, unsigned short win_cols);

// add error check for main algrothim
int memtobase(const void* mem, void* memstr, size_t n, char base);

int M_memtobase(const void* mem, void* memstr, size_t n, char base);

// alloc_memtobase function, base on memtobase()
char* alloc_memtobase(const void*, size_t, char);
// macro defined by alloc_memtobase()
#define alloc_memtobin(mem, length)\
    alloc_memtobase(mem, length, 2)

#define alloc_memtooct(mem, length)\
    alloc_memtobase(mem, length, 8)

#define alloc_memtohex(mem, length)\
    alloc_memtobase(mem, length, 16)

// alloc_M_memtobase function, base on M_memtobase()
char* alloc_M_memtobase(const void*, size_t, char);
// macro defined by alloc_M_memtobase()
#define alloc_M_memtobin(mem, length)\
    alloc_M_memtobase(mem, length, 2)

#define alloc_M_memtooct(mem, length)\
    alloc_M_memtobase(mem, length, 8)

#define alloc_M_memtohex(mem, length)\
    alloc_M_memtobase(mem, length, 16)

#endif
