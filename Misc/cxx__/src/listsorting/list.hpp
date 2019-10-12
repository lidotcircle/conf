#include<cstdlib>
#include<iostream>
#include<iterator>

/*
 * length of array A is len(A) = n + 1, the left rotate is p, so we have
 * A = [A_{0}:A_{p-1}, A_{p}:A_{n}]
 * if n - p >= p -1    <=>     n+1 >= 2*p    <=> len >= 2p
 * A can be decomposite into A = [A_{0}:A_{p-1}, A_{p}:A_{n-p}, A_{n-p+1}:A_{n}]
 * so the p left rotate will carry A to [A', A_{0}:A_{p-1}]
 * here A' = [A_{n-p+1}:A_{n}, A_{p}:A_{n-p}], finally we can recursively
 * solve rotate_left(A, p) = [rotate_left(A', p), A_{0}:A_{p-1}]
 *
 * when n - p < p -1    <=>     n+1 < 2*p    <=>  len < 2p
 * A can be decomposite into A = [A_{0}:A_{n-p}, A_{n-p+1}:A_{p-1}, A_{p}:A_{n}]
 * the left rotate will carry A to [A_{p}:A_{n}, A''] 
 * here A'' = [A_{n-p+1}:A_{p-1}, A_{0}:A_{n-p}]
 * so rotate_left(A, p) = [A_{p}:A_{n}, rotate_left(A'', 2*p - n - 2)]
 *
 * With induction the algorithm can be proved is correct
 *
 * the time complexity of this algorithm is O(n), because T(n) = T(n - k) + O(k), T(1) = O(1)
 * And by reduce the tail recursion the space comlexity will be O(1).
 */
template<typename T>
void __left_rotate(T begin, T end, size_t p, std::random_access_iterator_tag i_tag)
{
    if(end <= begin + 1) return;
    long int array_len  = end - begin;
    long int real_shift = p % array_len;
    if(real_shift == 0) return;
    if(array_len >= real_shift * 2){
        for(long int j = array_len - 1, i = real_shift - 1; 
                i >= 0; 
                i--, j--)
        {
            std::swap(*(begin + i), *(begin + j));
        }
        __left_rotate(begin, end - real_shift, real_shift, i_tag);
    } else {
        for(long int i = 0, j = real_shift;
                i <= array_len - real_shift - 1;
                i++, j++){
            std::swap(*(begin + i), *(begin + j));
        }
        __left_rotate(begin + array_len - real_shift,
                end, 
                2 * real_shift - array_len,
                i_tag);
    }
    return;
}

// base on the above idea, reduce the tail recursion, we can get bottom
// algorithm
template<typename T>
void __left_rotate_iter(T begin, T end, size_t p, std::random_access_iterator_tag)
{
    long int real_shift = p;
    for(;;){
        if(end <= begin + 1) return;
        long int array_len  = end - begin;
        real_shift = real_shift % array_len;
        if(real_shift == 0) return;
        if(array_len >= real_shift * 2){
            for(long int j = array_len - 1, i = real_shift - 1; 
                    i >= 0; 
                    i--, j--)
            {
                std::swap(*(begin + i), *(begin + j));
            }
            end -= real_shift;
        } else {
            for(long int i = 0, j = real_shift;
                    i <= array_len - real_shift - 1;
                    i++, j++){
                std::swap(*(begin + i), *(begin + j));
            }
            begin += array_len - real_shift;
            real_shift = 2 * real_shift - array_len;
        }
    }
    return;
}

template<typename T>
void left_rotate(T begin, T end, size_t p)
{
    __left_rotate_iter<T>(begin, end, p, typename std::iterator_traits<T>::iterator_category());
    return;
}

template<typename T>
void right_rotate(T begin, T end, size_t p)
{
    if(end <= begin + 1) return;
    size_t array_len  = end - begin;
    size_t real_shift = p % array_len;
    if(real_shift == 0) return;
    __left_rotate_iter<T>(begin, end, array_len - real_shift, typename std::iterator_traits<T>::iterator_category());
}
