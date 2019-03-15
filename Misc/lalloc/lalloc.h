#include<stdlib.h>

typedef unsigned msize_t;
typedef long MMIN_ALIGN;

union __lheader{
    struct {
        union __lheader* nptr;
        msize_t size;
    } s;
    MMIN_ALIGN __only_align;
};
typedef union __lheader LHeader;

void* lalloc(size_t nbytes);
void* lcalloc(size_t nobj, size_t nbytes);
void* lralloc(void* addr, size_t newbytes);
void  lfree(void* addr);
