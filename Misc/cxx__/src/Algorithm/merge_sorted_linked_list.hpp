#ifndef MERGE_SORTED_LINKED_LIST_HPP_
#define MERGE_SORTED_LINKED_LIST_HPP_

#include <algorithm>
#include <functional>

/*
 * merge two sorted linked list, and keep the original sorted order
 */

/**
 * @brief used to implement @fn merge_sorted_linked_list(), complexity is O(max(len(list_a), len(list_b)))
 * @pre  this algorithm assume <code>less_func(list[i], list[j])</code> always return <code>true</code> 
 *       when <code> i < j</code>
 * @post the original two linked list become invalidated
 * @tparam LLT type of the linked list
 * @pre type of the linked list must contain field @var next,
 *      and the type of functor must be <code>bool (*)(LLT*, LLT*)</code>
 * @param list_a the first  linked list
 * @param list_b the second linked list
 * @param less_func functor of comparison of elements
 */
template<typename LLT, typename _Compare>
LLT __merge_sorted_linked_list(LLT list_a, LLT list_b, _Compare less_func)
{
    if (list_a == nullptr) return list_b;
    if (list_b == nullptr) return list_a;
    LLT ins_f, ins_r;
    ins_f = list_a;
    ins_r = nullptr;
    if(less_func(list_b, ins_f)) {
        LLT the_next = list_b;
        while(the_next->next && less_func(the_next->next, ins_f))
            the_next = the_next->next;
        list_a = list_b;
        list_b = the_next->next;
        the_next->next = ins_f;
        if(list_b == nullptr) return list_a;
    }
    ins_r = ins_f;
    ins_f = ins_r->next;
    while(list_b->next) {
        if(ins_f == nullptr) {
            ins_r->next = list_b;
            return list_a;
        }
        if(less_func(list_b, ins_f)) {
            LLT the_next = list_b;
            while(the_next->next && less_func(the_next->next, ins_f))
                the_next = the_next->next;
            ins_r->next = list_b;
            list_b = the_next->next;
            the_next->next = ins_f;
        }
        ins_r = ins_f;
        ins_f = ins_r->next;
    }
    return list_a;
}


/**
 * @struct data_less is default functor for linked list element compare
 * @tparam LLT the linked list type should contains field @var data
 */
template<typename LLT>
struct data_less {
    bool operator()(const LLT& la, const LLT& lb) const {
        return la->data < lb->data;
    }
};


/**
 * @tparam LLT the linked list type should contains field @var data
 */
template<typename LLT>
LLT merge_sorted_linked_list(LLT list_a, LLT list_b)
{
    return __merge_sorted_linked_list(list_a, list_b, data_less<LLT>());
}

#endif // MERGE_SORTED_LINKED_LIST_HPP_
