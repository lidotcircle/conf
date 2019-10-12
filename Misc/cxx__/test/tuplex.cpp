#include<ctime>
#include<ios>
#include<iostream>
#include<iomanip>

#include<cstdlib>

#include<unistd.h>
#include "../src/utils/type.hpp"
#include "../src/tuplex/tuplex.hpp"

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
    auto tx  = tuplex<int, int, int, int>(1, 2, 3, 4);
    auto txa  __attribute__((unused))= tuplex<int, int, int>(1, 2, 3);
//    tprintf("% world% %\n","Hello",'!',123);
    std::cout << xget<1>(tx);
    return 0;
}
