#include<ctime>
#include<ios>
#include<iostream>
#include<iomanip>

#include<memory>

#include<cstdlib>

// #define MATRIX_DEBUG 1

#include<unistd.h>
#include "../src/utils/type.hpp"
#include "../src/utils/logger.h"
#include "../src/Graph/Graph.hpp"
#include "../src/Matrix/matrix.hpp"

#define TEST_SIZE 8
#define MAX_TEST_NUM 50

void testA()
{
    SquareMatrix<double> A(4);
    A.SetValue([](size_t i, size_t j) -> double {if(i == j) return 1; return 0;});
    A.get(2,4) = 5;
    A.get(4,4) = 0;
    CVector<double> v(4);
    v[1] = 4; v[2] = 3; v[3] = 1; v[4] = 5;
    std::cout << A.Rank() << std::endl;
    std::cout << A << std::endl;
    std::cout << v << std::endl;
    auto X = A.SolveLinearEquationEX(v);
    CVector<double> nn;
    for(auto bi = X.begin(); bi != X.end(); ++bi) {
        std::cout << *(*bi) << std::endl;
    }
}

void XsolverTest()
{
    SquareMatrix<double> A(TEST_SIZE);
    std::srand(time(nullptr));
    A.SetValue([](size_t i, size_t j) -> double{
            if(i >= TEST_SIZE - 5) return 0; if(i == j) return double(1.0); else 
                if(std::rand() % 100 < 90){
                    return double(0.1 * (std::rand() % MAX_TEST_NUM - MAX_TEST_NUM / 2));
                    }
                return double(0.0);
            });

    CVector<double> b(TEST_SIZE);
    b.SetValue([](size_t) -> double {return double(0.0);});

    std::cout << A << std::endl;
    std::cout << b << std::endl;

    auto x = A.Kernel();

    if(x.size() > 0) {
        for(auto bi = x.begin(); bi != x.end(); ++bi) {
            std::cout << *(*bi) << std::endl;
            std::cout << "A * y = :" << A * *(*bi) << std::endl;
            delete *bi;
        }
    } else {
        std::cout << "unsolvable Ax = b" << std::endl;
    }
}

int main()
{
    XsolverTest();
//    testA();
    return 0;
}
