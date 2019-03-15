#include "lalloc.h"

#define MIN_ALLOC 1024

// sbrk system call
char* sbrk(size_t);

/* 
 * system hold a circular linked list to stored unused memory block
 * that allocated by sbrk system call (a. sbrk() allocate block greater
 * than need so, dividing it; b. calling the lfree() function).
 * At initialization, the linked list is NULL, a global pointer is used to 
 * indicate this list.
 */

// implemente this relation cirList->s.nptr > cirList, 
// except the tail element;
LHeader* cirList = NULL;

static void add_list(LHeader* elem)
{
    if(cirList == NULL){ // when cirList isn't initialized.
        cirList = elem;
        cirList->s.nptr = elem;
        return;
    }
    LHeader *find_ptr;
    for(find_ptr = cirList; // find the appropriated insert position
            !(find_ptr < elem && find_ptr->s.nptr > elem);
            find_ptr = find_ptr->s.nptr){
        // add to tail or head
        if(find_ptr->s.nptr <= find_ptr &&
                (elem > find_ptr || elem < find_ptr->s.nptr)) 
                break;
    }
    if(elem + elem->s.size == find_ptr->s.nptr){
        elem->s.size += find_ptr->s.nptr->s.size;
        elem->s.nptr = find_ptr->s.nptr->s.nptr;
        find_ptr->s.nptr = elem;
    } else
        elem->s.nptr = find_ptr->s.nptr;
    if(find_ptr + find_ptr->s.size == elem){
        find_ptr->s.size += elem->s.size;
        find_ptr->s.nptr = elem->s.nptr;
    } else 
        find_ptr->s.nptr = elem;
    cirList = elem;
}

/* if the required size can't be satisfy in unused list return NULL,
 * else return the address
 */
static LHeader* get_apt_block(msize_t reqs)
{
    LHeader* ret_addr;
    LHeader* find_ptr;
    if(cirList == NULL)
        return NULL;
    for(find_ptr = cirList; find_ptr->s.nptr->s.size < reqs;
            find_ptr = find_ptr->s.nptr)
        if(find_ptr->s.nptr == cirList->s.nptr)
            break;
    cirList = find_ptr;
    ret_addr = cirList->s.nptr;
    if(ret_addr->s.size < reqs)
        return NULL;
    if(ret_addr->s.size == reqs ||
            ret_addr->s.size == reqs + 1){
        if(ret_addr == cirList)
            cirList = NULL;
        else
            cirList->s.nptr = ret_addr->s.nptr;
    } else {
        find_ptr = ret_addr + reqs;
        find_ptr->s.size = ret_addr->s.size - reqs;
        find_ptr->s.nptr = ret_addr->s.nptr;
        cirList->s.nptr = find_ptr;
        ret_addr->s.size = reqs;
    }
    return ret_addr;
}

static void* morealloc(msize_t nunits)
{
    LHeader *ret_addr, *over_part;
    msize_t alloc_units = nunits > MIN_ALLOC ? nunits : MIN_ALLOC;
    if((ret_addr = (LHeader*)sbrk(alloc_units * sizeof(LHeader))) ==\
            (LHeader*) -1)
        return NULL;
    if(nunits + 1 >= alloc_units){
        ret_addr->s.size = alloc_units;
        return ret_addr;
    }
    over_part = ret_addr + nunits;
    ret_addr->s.size = nunits;
    over_part->s.size = alloc_units - nunits;
    add_list(over_part);
    return ret_addr;
}

void* lalloc(size_t nbytes)
{
    LHeader *ret_addr;
    msize_t a_units = (nbytes + sizeof(LHeader) - 1)/sizeof(LHeader) + 1;
    if((ret_addr = get_apt_block(a_units)) == NULL)
        if((ret_addr = morealloc(a_units)) == NULL)
            return NULL;
    return (ret_addr + 1);
}

void* lcalloc(size_t nobj, size_t nbytes)
{
    return lalloc(nobj * nbytes);
}

void* lralloc(void* addr, size_t newbytes)
{
    msize_t reqs_unit = 
        (sizeof(LHeader) + newbytes - 1)/sizeof(LHeader) + 1;
    LHeader* old_bl = (LHeader*)addr - 1;
    // condition 1. request is equal with origin block, just return
    if(reqs_unit == old_bl->s.size || reqs_unit + 1 == old_bl->s.size)
        return addr;
    // condition 2. request is less than origin block, trim the 
    // block and return
    else if(reqs_unit < old_bl->s.size){
        LHeader* depre_bl = old_bl + reqs_unit;
        depre_bl->s.size = old_bl->s.size - reqs_unit;
        old_bl->s.size = reqs_unit;
        add_list(depre_bl);
        return addr;
    }
    //condition 3. need much more space.
    LHeader* expect_addr = old_bl + old_bl->s.size;
    LHeader* find_ptr;
    for(find_ptr=cirList;expect_addr!=find_ptr->s.nptr;
            find_ptr=find_ptr->s.nptr){
        if(find_ptr->s.nptr == cirList)
            break;
    }
    if(find_ptr->s.nptr != expect_addr || 
            find_ptr->s.nptr->s.size + old_bl->s.size < reqs_unit){
        lfree(addr);
        return lalloc(newbytes);
    }
    (old_bl + reqs_unit)->s.nptr = expect_addr->s.nptr;
    (old_bl + reqs_unit)->s.size = 
        old_bl->s.size + expect_addr->s.size - reqs_unit;
    find_ptr->s.nptr = old_bl + reqs_unit;
    old_bl->s.size = reqs_unit;
    return addr;
}

void lfree(void* addr)
{
    LHeader* lh_addr = (LHeader*)addr - 1;
    add_list(lh_addr);
}
