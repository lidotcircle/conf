#include<ctime>
#include<ios>
#include<iostream>
#include<iomanip>

#include<memory>
#include<random>

#include<cstdlib>

#include <unistd.h>

//#define FIBONACCIHEAP_DEBUG 1

#include "../src/utils/type.hpp"
#include "../src/utils/logger.h"
#include "../src/Matrix/matrix.hpp"
#include "../src/PriorityQueue/BinaryHeap.hpp"
#include "../src/PriorityQueue/FibonacciHeap.hpp"
#include "../src/utils/range.hpp"

using namespace ANNA;

/*
void testA()
{
    BinaryHeap<double> A, B;
    auto a = range(20.0, 30.0);
    A.Add(a.begin(), a.end());
    auto b = range(10.0, 18.0);
    B.Add(b.begin(), b.end());
    std::cout << A << std::endl;
}
*/

void testB()
{
    FibonacciHeap<double> A, B;
    auto a = range(20.0, 30.0);
    A.Add(a.begin(), a.end());
    auto b = range(10.0, 30.0);
    B.Add(b.begin(), b.end());
    auto f = A.Add(300.0);
    auto n = A.Add(400.0);
    A.UnionWith(B);
    A.DecreaseKey(f, 7);
    (void)n;
    (void)f;
    A.DeleteKey(n);
    std::cout << A << std::endl;
}

void testC()
{
    FibonacciHeap<int> A;
    std::random_device rd;
    std::mt19937 gen(rd());
    std::uniform_int_distribution<int> dis(99, 100);
    for(int i = 1; i<=50; i++){
        A.Add(dis(gen));
    }
    std::cout << A << std::endl;
}

int main()
{
    testC();
    return 0;
}
