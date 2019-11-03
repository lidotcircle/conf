#include<ctime>
#include<ios>
#include<iostream>
#include<iomanip>

#include<memory>

#include<cstdlib>

#include <unistd.h>
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
    BinaryHeap<double> A;
    auto a = range(20.0, 30.0);
    A.Add(a.begin(), a.end());
    auto b = range(10.0, 38.0);
    A.Add(b.begin(), b.end());
    std::cout << A << std::endl;
}

void testB()
{
    FibonacciHeap<double> A;
    auto a = range(20.0, 30.0);
    A.Add(a.begin(), a.end());
    auto b = range(10.0, 38.0);
    A.Add(b.begin(), b.end());
    std::cout << A << std::endl;
}

int main()
{
    testB();
    return 0;
}
