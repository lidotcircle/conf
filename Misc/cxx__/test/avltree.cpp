#include<utility>
#include<iostream>
#include<vector>

#include "../src/Tree/AVLTree.hpp"

using namespace ANNA;

void testA()
{
    AVLTree<int, double> TT;
    for(int i = 1; i<=400; ++i)
        TT.Insert(100 * i, 50 * i);
    std::ostream_iterator<double> ooo(std::cout, ", ");
    std::copy(TT.begin(), TT.end(), ooo);
    std::cout << std::endl;
    std::cout << TT.Depth() << std::endl;
    return;
}

int main()
{
    testA();
    return 0;
}
