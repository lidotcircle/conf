#ifndef TUPLEX_HPP
#define TUPLEX_HPP

#include<cstdlib>
#include<type_traits>

/*
template<size_t> class xget;

template<typename T, typename ...ARGS>
class tuplex{
    public:
        typedef T _typex;
        T _a;
        tuplex<ARGS...> _b;
    public:
        tuplex(const T& t1, const ARGS&... args): _a(t1), _b(tuplex<ARGS...>(args...)){}
};

template<typename T>
class tuplex<T>{
    public:
        typedef T _typex;
        T _a;
    public:
        tuplex(const T& t1): _a(t1){}
};


template class tuplex<int>;
template class tuplex<int, int, int>;
*/

#endif // TUPLEX_HPP
