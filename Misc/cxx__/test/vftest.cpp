#include<ctime>
#include<ios>
#include<iostream>
#include<iomanip>

#include<cstdlib>

#include<unistd.h>
#include "../src/utils/type.hpp"

class A {
    public:
        void foo() {
            std::cout << "A::foo() is called." << std::endl;
            return;
        }
};

class D: public A {
    public:
        virtual void foo() {
            std::cout << "D::foo() is called." << std::endl;
            return;
        }
};

int main()
{
    D d;
    A* a = &d;
    a->foo(); // this will call A::foo()
    return 0;
}
