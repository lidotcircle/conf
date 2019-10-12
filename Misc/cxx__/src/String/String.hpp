#if !defined(_STRING_HPP_)
#define _STRING_HPP_

#include<cstdlib>
#include<vector>

template<typename T>
size_t KMP_map(T* src_s, size_t * map, size_t n)
{
    if(n == 1) return 0;
    map--;
    T*     s     = src_s - 1;
    size_t t = map[n-1];
    if(s[t + 1] == s[n]) return (t + 1);
    while(t>0){
        t = map[t];
        if(s[t + 1] == s[n]) return (t + 1);
    };
    return t;
}

template<typename T>
size_t* get_KMP_map(T* src_s, T* src_e)
{
    size_t s_len = src_e - src_s;
    size_t *ret = (size_t*)malloc(s_len * sizeof(size_t));
    size_t t;
    for(size_t i = 0; i<s_len; i++){
        t = KMP_map(src_s, ret, i + 1);
        ret[i] = t;
    }
    return ret;
}

template<typename T>
size_t KMP_search_A(T* target_s, T* target_e, T* source_s, T* source_e, size_t* kmp_map)
{
    T* t = target_s - 1;
    T* s = source_s - 1;
    kmp_map--;
    size_t t_len = target_e - target_s;
    size_t s_len = source_e - source_s;
    size_t p, sp;
    for(p = 1, sp=1; p<=t_len; p++){
        if(t[p] == s[sp]);
        else{
            while(sp>1){
                sp = kmp_map[sp - 1] + 1;
                if(t[p] == s[sp]) break;
            }
        }

        if(t[p] == s[sp]){
            if(sp == s_len) break;
            else sp++;
        }
    }
    if(sp == s_len) return (p - sp);
    else return -1;
}

template<typename T>
size_t KMP_search(T* target_s, T* target_e, T* source_s, T* source_e)
{
    size_t *kmp_map = get_KMP_map<T>(source_s, source_e);
    return KMP_search_A(target_s, target_e, source_s, source_e, kmp_map);
    free(kmp_map);
}

template<typename T>
std::vector<size_t> KMP_search_all(T* target_s, T* target_e, T* source_s, T* source_e)
{
    size_t s_len = source_e - source_s;
    std::vector<size_t> ret;
    size_t *kmp_map = get_KMP_map<T>(source_s, source_e);
    size_t tr = KMP_search_A(target_s, target_e, source_s, source_e, kmp_map);
    size_t cc = 0;
    while(1){
        if((int)tr != -1){
            cc += tr;
            ret.push_back(cc);
        } else  break;
        cc += s_len;
        tr = KMP_search_A(target_s + cc, target_e, source_s, source_e, kmp_map);
    }
    free(kmp_map);
    return ret;
}

template size_t KMP_search<char>(char*, char*, char*, char*);

#endif // _STRING_HPP_
