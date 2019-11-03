#include<ctime>
#include<ios>
#include<iostream>
#include<iomanip>

#include<memory>

#include<cstdlib>

#include <unistd.h>

#define FIBONACCIHEAP_DEBUG 1

#include "../src/utils/type.hpp"
#include "../src/utils/logger.h"
#include "../src/Graph/Graph.hpp"
#include "../src/Matrix/matrix.hpp"
#include "../src/PriorityQueue/BinaryHeap.hpp"
#include "../src/PriorityQueue/FibonacciHeap.hpp"
#include "../src/utils/range.hpp"

using namespace ANNA;

void testA()
{
    BinaryHeap<double> A, B;
    auto a = range(20.0, 30.0);
    A.Add(a.begin(), a.end());
    auto b = range(10.0, 18.0);
    B.Add(b.begin(), b.end());
    std::cout << A << std::endl;
}

void testB()
{
    FibonacciHeap<double> A, B;
    auto a = range(20.0, 30.0);
    A.Add(a.begin(), a.end());
    auto xx = FibonacciHeap<double>::JointHolder[0];
    std::cout << xx->m_kv << std::endl;
    auto b = range(10.0, 30.0);
    B.Add(b.begin(), b.end());
    A.UnionWith(B);
    A.DeleteKey(xx);
    std::cout << A << std::endl;
}

int main()
{
    testB();
    return 0;
}
