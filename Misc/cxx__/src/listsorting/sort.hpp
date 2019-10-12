#include<cstdlib>
#include<cstring>
#include<exception>

#include<algorithm>
#include<functional>

namespace ListSort {

template<typename ET>
void swap(ET* x, ET* y)
{
    if(x == y)
        return;
    ET t;
    memcpy(&t,  x, sizeof(ET));
    memcpy( x,  y, sizeof(ET));
    memcpy( y, &t, sizeof(ET));
    return;
}

typedef bool (*sortCompare)(void*, void*);

template<typename ET>
ET* bubblesort(ET* ef, size_t len, sortCompare comp_func)
{
    if(len <= 1) return ef;
    size_t i = 0;
    while(i<len-1){
        if(comp_func(ef + i, ef + i + 1)){
            i++;
            continue;
        }
        swap<ET>(ef + i, ef + i + 1);
        if(i>0)
            i--;
    }
    return ef;
}

template<typename ET>
ET* selectionsort__(ET* ef, size_t len, sortCompare comp_func)
{
    if(len <= 1) return ef;
    size_t i, holder = 0;
    for(i=1;i<len;i++)
    {
        if(comp_func(ef + i, ef + holder))
            holder = i;
    }
    swap<ET>(ef, ef + holder);
    return ef;
}

template<typename ET>
ET* selectionsort(ET* ef, size_t len, sortCompare comp_func)
{
    size_t i;
    for(i=0;i<len;i++)
        selectionsort__(ef + i, len - i, comp_func);
    return ef;
}

template<typename ET>
void insert(ET* ef, size_t len, size_t ti, ET* x)
{
    if(ti > len)
        throw new std::exception();
    if(ti == len){
        ef[len] = *x;
        return;
    }
    memmove(ef + ti + 1, ef + ti, (len - ti) * sizeof(ET)); 
    ef[ti] = *x;
    return;
}

template<typename ET>
ET* insertionsort(ET* ef, size_t len, sortCompare comp_func)
{
    ET* cc = (ET*) malloc(len * sizeof(ET));
    std::memcpy(cc, ef, len * sizeof(ET));
    size_t i, j, k;
    for(i = 1;i<len;i++){
        k = 0;
        for(j = 0; j<i;j++){
            if(comp_func(ef + j, cc + i))
                k++;
        }
        insert<ET>(ef, i, k, cc + i);
    }
    return ef;
}

template<typename ET>
size_t quicksort_partioin(ET* ef, size_t len, sortCompare comp_func){
    size_t l, r;
    for(l=0, r=1;r<len;r++){
        if(comp_func(ef+r, ef))
            swap<ET>(++l + ef, ef + r);
    }
    swap<ET>(ef, ef + l);
    return l;
}


template<typename ET>
ET* quicksort(ET* ef, size_t len, sortCompare comp_func)
{
    if(len <= 1) return ef;
    size_t pivot = quicksort_partioin(ef, len, comp_func);
    quicksort(ef, pivot, comp_func);
    quicksort(ef + pivot + 1, len - (pivot + 1), comp_func);
    return ef;
}


template<typename ET>
ET* mergesort(ET* ef, size_t len, sortCompare comp_func)
{
    if(len <= 1) return ef;
    size_t pivot = len / 2;
    mergesort(ef, pivot, comp_func);
    mergesort(ef + pivot, len - pivot, comp_func);
    ET* rt = (ET*)malloc(len * sizeof(ET));
    size_t top, i=0, j=0;
    for(top = 0; top<len && i < pivot && j < len - pivot; top++){
        if(comp_func(ef + i, ef + pivot + j))
            memcpy(rt + top, ef + i++, sizeof(ET));
        else
            memcpy(rt + top, ef + pivot + j++, sizeof(ET));
    }
    for(;i<pivot;)
            memcpy(rt + top++, ef + i++, sizeof(ET));
    for(;j<len - pivot;)
            memcpy(rt + top++, ef + pivot + j++, sizeof(ET));
    memcpy(ef, rt, sizeof(ET) * len);
    free(rt);
    return ef;
}

template<typename ET>
ET* mergesort__(ET* ef, ET* ex_space, size_t len, sortCompare comp_func, bool first)
{
    if(len <= 1){
        return first ? ef : (ET*)memcpy(ex_space, ef, len);
    };
    ET* target = first ? ef : ex_space;
    size_t pivot = len / 2;
    ET* f = mergesort__(ef, ex_space, pivot, comp_func, first ? false : true);
    ET* s = mergesort__(ef + pivot, ex_space+ pivot, len - pivot, comp_func, first ? false : true);
    size_t top, i=0, j=0;
    for(top = 0; top<len && i < pivot && j < len - pivot; top++){
        if(comp_func(f + i, s + j))
            memcpy(target + top, f + i++, sizeof(ET));
        else
            memcpy(target + top, s + j++, sizeof(ET));
    }
    for(;i<pivot;)
            memcpy(target + top++, f + i++, sizeof(ET));
    for(;j<len - pivot;)
            memcpy(target + top++, s + j++, sizeof(ET));
    return target;
}

template<typename ET>
ET* mergesort_ip(ET* ef, size_t len, sortCompare comp_func)
{
    if(len == 1) return ef; 
    ET* ex_space = (ET*)malloc(sizeof(ET) * len);
    mergesort__(ef, ex_space, len, comp_func, true);
    free(ex_space);
    return ef;
}

template<typename ET>
ET* mergesortv2(ET* ef, size_t len, sortCompare comp_func)
{
    if(len <= 20) return selectionsort(ef, len, comp_func);
    size_t pivot = len / 2;
    mergesortv2(ef, pivot, comp_func);
    mergesortv2(ef + pivot, len - pivot, comp_func);
    ET* rt = (ET*)malloc(len * sizeof(ET));
    size_t top, i=0, j=0;
    for(top = 0; top<len && i < pivot && j < len - pivot; top++){
        if(comp_func(ef + i, ef + pivot + j))
            memcpy(rt + top, ef + i++, sizeof(ET));
        else
            memcpy(rt + top, ef + pivot + j++, sizeof(ET));
    }
    for(;i<pivot;)
            memcpy(rt + top++, ef + i++, sizeof(ET));
    for(;j<len - pivot;)
            memcpy(rt + top++, ef + pivot + j++, sizeof(ET));
    memcpy(ef, rt, sizeof(ET) * len);
    free(rt);
    return ef;
}

template int* bubblesort<int>(int*, size_t, sortCompare);

template<typename T_Iterator_, typename _Compare>
void __fix_down__heaplify(T_Iterator_ _begin, T_Iterator_ _end, _Compare compare){
    typedef typename std::iterator_traits<T_Iterator_>::difference_type diff_t;
    while(_begin < (_end - 1)){
        diff_t len = _end -_begin;
        diff_t parent = len / 2 - 1;
        if(compare(*--_end, *(_begin + parent))){
            std::swap(*_end, *(_begin + parent));
            _end = _begin + parent + 1;
        } else return;
    }
    return;
}

template<typename T_Iterator_, typename _Compare>
void __fix_up__heaplify(T_Iterator_ _begin, T_Iterator_ _end, _Compare compare){
    typedef typename std::iterator_traits<T_Iterator_>::difference_type diff_t;
    if(_begin >= (_end - 1)) return;
    diff_t len     = _end - _begin;
    diff_t current = 1;
    diff_t max     = current;
    for(;;){
        diff_t left  = current * 2;
        diff_t right = current * 2 + 1;
        if(left  <= len && compare(*(_begin + left - 1), *(_begin + max - 1)))
            max = left;
        if(right <= len && compare(*(_begin + right - 1), *(_begin + max - 1)))
            max = right;
        if(max != current){
            std::swap(*(_begin + max - 1), *(_begin + current - 1));
            current = max;
        } else return;
    }
}

template<typename T_Iterator_, typename _Compare>
void __heaplify(T_Iterator_ _begin, T_Iterator_ _end, _Compare compare){
    if(_begin >= _end - 1) return;
    for(T_Iterator_ param = _begin + 2; param <= _end; param++){
        __fix_down__heaplify(_begin, param, compare);
    }
    return;
}

template<typename T_Iterator_, typename _Compare>
void __heapsort(T_Iterator_ _begin, T_Iterator_ _end, _Compare compare){
    __heaplify(_begin, _end, compare);
    _end--;
    for(;_begin < _end;_end--){
        std::swap(*_begin, *_end);
        __fix_up__heaplify(_begin, _end, compare);
    }
    return;
}

template<typename T_Iterator_, typename _Compare>
void heapsort(T_Iterator_ _begin, T_Iterator_ _end, _Compare compare){
    __heapsort(_begin, _end, compare);
    return;
}

template<typename T_Iterator_>
void heapsort(T_Iterator_ _begin, T_Iterator_ _end){
    __heapsort(_begin, _end, 
            std::less<typename std::iterator_traits<T_Iterator_>::value_type>());
    return;
}

}
