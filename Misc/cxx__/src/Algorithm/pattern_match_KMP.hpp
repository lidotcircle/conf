#ifndef PATTERN_MATCH_KMP_HPP_
#define PATTERN_MATCH_KMP_HPP_

#include <iterator>
#include <vector>

#include <cassert>

#include "utils.hpp"
ANNA_BEGIN

/**
 * @brief generate kmp match table
 * @param[in]  start begin of iterator
 * @param[in]  end   end of iterator
 * @param[out] table output table, which should be allocated by caller, 
 *                   <code>table[0]</code> always equal <code>-1</code>, 
 *                   because we start from 0 to index string
 * @except if the memory of \p table isn't allocated validated, exception will raise
 */
template<typename _Iterator_P>
void kmp_match_table(_Iterator_P start, _Iterator_P end, int table[])
{
    size_t length = end - start;
    table[0] = -1;
    for(size_t i = 1; i<length; ++i) {
        int prev = i - 1;
        while(true) {
            int nn = table[prev] + 1;
            if(start[nn] == start[i]) {
                table[i] = nn;
                break;
            }
            if (nn == 0) {
                table[i] = -1;
                break;
            }
            prev = nn - 1;
        }
    }
}

/**
 * @brief return first matched position of pattern in string
 */
template<typename _Iterator_P, typename _Iterator_S>
std::pair<bool, typename std::iterator_traits<_Iterator_S>::difference_type>
__kmp_first_match(_Iterator_P ptn_start, _Iterator_P ptn_end, 
                _Iterator_S str_start, _Iterator_S str_end, 
                const int kmp_match_table[])
{
    static_assert(std::is_same<typename std::iterator_traits<_Iterator_S>::iterator_category, 
                  std::random_access_iterator_tag>::value,
                  "only support random access iterator");
    static_assert(std::is_same<typename std::iterator_traits<_Iterator_P>::iterator_category, 
                  std::random_access_iterator_tag>::value,
                  "only support random access iterator");
    assert(kmp_match_table[0] == -1);

    typedef typename std::iterator_traits<_Iterator_S>::difference_type string_diff;
    typedef typename if_else<
        std::is_signed<typename std::iterator_traits<_Iterator_P>::difference_type>::value, 
        typename std::iterator_traits<_Iterator_P>::difference_type,
        int>::type pattern_diff;

    string_diff  string_length  = str_end - str_start;
    pattern_diff pattern_length = ptn_end - ptn_start;
    if (string_length < pattern_length) return std::make_pair(false, 0);
    string_diff  string_counter = 0;
    pattern_diff pattern_loop   = 0;

    for(;string_counter < string_length; ++string_counter) {
        ++pattern_loop;
        if(pattern_loop == pattern_length)
            return std::make_pair(true, string_counter - pattern_length);
        while(true) {
            if (str_start[string_counter] == ptn_start[pattern_loop]) break;
            if(pattern_loop == 0 ) {pattern_loop = -1; break;}
            pattern_loop = kmp_match_table[pattern_loop - 1] + 1;
        }
    }

    return std::make_pair(false, 0);
}

template<typename _Iterator_P, typename _Iterator_S>
std::pair<bool, typename std::iterator_traits<_Iterator_S>::difference_type>
kmp_first_match(_Iterator_P ptn_start, _Iterator_P ptn_end, 
                _Iterator_S str_start, _Iterator_S str_end)
{
    typedef typename if_else<
        std::is_signed<typename std::iterator_traits<_Iterator_P>::difference_type>::value, 
        typename std::iterator_traits<_Iterator_P>::difference_type,
        int>::type pattern_diff;

    pattern_diff ptn_length = ptn_end - ptn_start;

    int* table = (int*)malloc(sizeof(int) * ptn_length);
    kmp_match_table(ptn_start, ptn_end, table);
    auto ret   = __kmp_first_match(ptn_start, ptn_end, str_start, str_end, table);
    free(table);
    return ret;
}

template<typename _Iterator_P, typename _Iterator_S>
std::pair<bool, typename std::iterator_traits<_Iterator_S>::difference_type>
kmp_first_match(_Iterator_P ptn_start, size_t ptn_len, 
                _Iterator_S str_start, size_t str_len)
{
    return kmp_first_match(ptn_start, ptn_start + ptn_len,
                          str_start, str_start + str_len);
}


template<typename _Iterator_P, typename _Iterator_S>
std::vector<typename std::iterator_traits<_Iterator_S>::difference_type>
kmp_search_all(_Iterator_P ptn_start, _Iterator_P ptn_end, 
               _Iterator_S str_start, _Iterator_S str_end)
{
    typedef typename if_else<
        std::is_signed<typename std::iterator_traits<_Iterator_P>::difference_type>::value, 
        typename std::iterator_traits<_Iterator_P>::difference_type,
        int>::type pattern_diff;

    pattern_diff ptn_length = ptn_end - ptn_start;

    int* table = (int*)malloc(sizeof(int) * ptn_length);
    std::vector<typename std::iterator_traits<_Iterator_S>::difference_type> ret;
    kmp_match_table(ptn_start, ptn_end, table);
    int start_pos = 0;
    auto search_result   = __kmp_first_match(ptn_start, ptn_end, str_start + start_pos, str_end, table);
    while(search_result.first) {
        ret.push_back(search_result.second + start_pos);
        start_pos = ptn_length + ret.back();
        search_result = __kmp_first_match(ptn_start, ptn_end, str_start + start_pos, str_end, table);
    }
    free(table);
    return ret;
}

template<typename _Iterator_P, typename _Iterator_S>
std::vector<typename std::iterator_traits<_Iterator_S>::difference_type>
kmp_search_all(_Iterator_P ptn_start, size_t ptn_len, 
               _Iterator_S str_start, size_t str_len)
{
    return kmp_search_all(ptn_start, ptn_start + ptn_len,
                          str_start, str_start + str_len);
}

ANNA_END
#endif // PATTERN_MATCH_KMP_HPP_
