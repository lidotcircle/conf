#include<ctime>
#include<ios>
#include<iostream>
#include<iomanip>

#include<cstdlib>

#include<unistd.h>
#include "../src/utils/type.hpp"
#include "../src/utils/logger.h"

class A {
    public:
        int a;
        int b;
};

void testA()
{
    A hh;
    int A::* ptw;
    hh.a = 100;
    hh.b = 200;
    if(hh.a > hh.b){
        ptw = &A::a;
    } else {
        ptw = &A::b;
    }

    hh.*ptw = 20000;
    std::cout << "Class Point:" << ptw << ", Maximum:" << hh.*ptw << std::endl;
}


template<typename T>
class __has_member_type_name {
    template<typename U>
        static char test(typename U::name*);
    template<typename U>
        static int test(...);
    public:
    static constexpr bool value = sizeof(test<T>(nullptr)) == sizeof(char);
};

HAS_MEMBER(x);

HAS_MEMBER_TYPE(jjyy);

class B {public: 
    typedef int name; 
    typedef int jjyy; 
    public: 
    void operator()(B);
    void operator()();
    void operator()(A);
};
void testB()
{
    constexpr bool problem = has_member_x<B>::value;
    constexpr bool problem2 = is_callable<B&>::value;
    constexpr bool problem3 = has_member_type_jjyy<B>::value;
    std::cout << std::boolalpha << problem << std::endl;
    std::cout << std::boolalpha << problem2 << std::endl;
    std::cout << std::boolalpha << problem3 << std::endl;
}

#include <iostream>

void tprintf(const char* format) // base function
{
    std::cout << format;
}

template<typename T, typename... Targs>
void tprintf(const char* format, T value, Targs... Fargs) // recursive variadic function
{
    for ( ; *format != '\0'; format++ ) {
        if ( *format == '%' ) {
            std::cout << value;
            tprintf(format+1, Fargs...); // recursive call
            return;
        }
        std::cout << *format;
    }
}

int main()
{
//    tprintf("% world% %\n","Hello",'!',123);
    logger log_a("HelloD2D");
    log_a.begin_log();
    log_a.ostream() << "hello logger";
    log_a.new_line();
    log_a.ostream() << "hello logger next line.";
    return 0;
}
