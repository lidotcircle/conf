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
#include "../src/Set/DisjointSet.hpp"

void testA()
{
    xxx a, b;
    a.m_val = 200; b.m_val = 300;
    DisjointSetForestXXX ds;
    ds.MakeSet(&a);
    ds.MakeSet(&b);
    ds.UnionWith(&a, &b);
    for(auto xx = ds.begin(&a); xx != ds.end(&a); ++xx)
        std::cout << (*xx)->m_val << std::endl;
}

int main()
{
    testA();
    return 0;
}
