#ifndef LINKED_LIST_MERGE_SORT_HPP_
#define LINKED_LIST_MERGE_SORT_HPP_

#include <cassert>

#include "merge_sorted_linked_list.hpp"

template<typename LLT, typename _Compare>
LLT __linked_list_merge_sort(LLT list, size_t list_len, _Compare less_func)
{
    assert(list != nullptr && list_len > 0);
    if(list_len == 1) return list;
    if(list_len == 2) {
        if(less_func(list, list->next)) return list;
        list->next->next = list;
        list->next = nullptr;
        return list;
    }
    size_t half_floor = list_len / 2;
    LLT sorted_second_part = list;
    for(size_t i=1;i<=half_floor;++i)
        sorted_second_part = sorted_second_part->next;
    list = __linked_list_merge_sort(list, half_floor, less_func);
    sorted_second_part = __linked_list_merge_sort(sorted_second_part, list_len - half_floor, less_func);
    list = __merge_sorted_linked_list(list, sorted_second_part, less_func);
    return list;
}

/**
 * @pre this assertion <code>assert(len(list) == list_len)</code> must be true
 */
template<typename LLT>
LLT linked_list_merge_sort(LLT list, size_t list_len)
{
    return __linked_list_merge_sort(list, list_len, data_less<LLT>());
}

template<typename LLT>
LLT linked_list_merge_sort(LLT list)
{
    assert(list != nullptr);
    LLT len_ccc = list;
    size_t list_len = 0;
    for(;len_ccc != nullptr; len_ccc = len_ccc->next, ++list_len);
    return linked_list_merge_sort(list, list_len);
}

#endif // LINKED_LIST_MERGE_SORT_HPP_
