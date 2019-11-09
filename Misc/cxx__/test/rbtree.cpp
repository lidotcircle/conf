#include<utility>
#include<iostream>
#include<vector>
#include <random>

#include <cstdlib>
#include <cstdio>

#include "../src/Tree/RBTree.hpp"

using namespace ANNA;

void testA()
{
    FILE *OpenFile = ::fopen("./data_out", "w");
    if(OpenFile == nullptr){
        ::fprintf(stderr, "FILE error");
        ::exit(1);
    }
    RBTree<int, double> TT;
    std::random_device rd;
    std::mt19937 gen(rd());
    std::uniform_int_distribution<int> dis(1, 10000);
    for(int i = 1; i<=800; ++i) {
        int ss = dis(gen);
        ::fprintf(OpenFile, "%d\n", ss);
        ::fflush_unlocked(OpenFile);
        TT.Insert(ss, ss);
    }
    std::ostream_iterator<double> ooo(std::cout, ", ");
    std::copy(TT.begin(), TT.end(), ooo);
    std::cout << std::endl;
    std::cout << TT.Depth() << std::endl;
    return;
}

void testB()
{
    FILE *OpenFile = ::fopen("./data_out", "r");
    if(OpenFile == nullptr){
        ::fprintf(stderr, "FILE error");
        ::exit(1);
    }
    RBTree<int, double> TT;
    size_t buf_size = 500;
    char* buf = (char*)malloc(buf_size);
    for(int i = 1; i<=800; ++i) {
        ::getline(&buf, &buf_size, OpenFile);
        int ss = ::atof(buf);
        TT.Insert(ss, ss);
    }
    ::free(buf);
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
