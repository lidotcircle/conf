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
    for(int i = 1; i<=500; ++i)
    {
        xxx* new_bb = new xxx();
        new_bb->m_val = i;
        ds.MakeSet(new_bb);
        if(i % 2 == 0)
            ds.UnionWith(&a, new_bb);
        else 
            ds.UnionWith(&b, new_bb);
    }
    ds.UnionWith(&a, &b);
    std::cout << std::boolalpha << ds.empty() << std::endl;
    for(auto xx = ds.begin(&a); xx != ds.end(&a); ++xx)
        std::cout << (*xx)->m_val << std::endl;
}

int main()
{
    testA();
    return 0;
}
