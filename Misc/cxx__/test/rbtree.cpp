#include<utility>
#include<iostream>
#include<vector>

#include "../src/Tree/RBTree.hpp"

using namespace ANNA;

void testA()
{
    RBTree<int, double> TT;
    for(int i = 1; i<=400; ++i)
        TT.Insert(100 * i, 50 * i);
    std::ostream_iterator<double> ooo(std::cout, ", ");
    std::copy(TT.begin(), TT.end(), ooo);
    return;
}

int main()
{
    testA();
    return 0;
}
