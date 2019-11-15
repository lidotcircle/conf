#include<utility>
#include<iostream>
#include<vector>
#include <random>
#include <iomanip>
#include <iterator>

#include <cstring>

#define __BTREE_DEBUG 1
#include "../src/Tree/BTree.hpp"

// using namespace ANNA;

void testA()
{
    FILE *OpenFile = ::fopen("./data_out", "w");
    if(OpenFile == nullptr){
        ::fprintf(stderr, "FILE error");
        ::exit(1);
    }
    BTree<int, double, 8> TT;
    std::random_device rd;
    std::mt19937 gen(rd());
    std::uniform_int_distribution<int> dis(1, 500000);
    for(int i = 1; i<=66600; ++i) {
        int ss = dis(gen);
        ::fprintf(OpenFile, "%d\n", ss);
        ::fflush(OpenFile);
        TT.Insert(std::make_pair(ss, ss));
    }
    if(::fclose(OpenFile) == -1) {
        ::fprintf(stderr, "close file failed");
        ::exit(1);
    }
    std::cout << TT.Depth() << std::endl;
//    std::ostream_iterator<double> ooo(std::cout, ",");
//    std::copy(TT.begin(), TT.end(), ooo);
    return;
}

void testB()
{
    FILE *OpenFile = ::fopen("./data_out", "r");
    if(OpenFile == nullptr){
        ::fprintf(stderr, "FILE error");
        ::exit(1);
    }
    BTree<int, double, 8> TT;

    size_t insert_s = 0, delete_s = 0;

    size_t buf_size = 500;
    char* buf = (char*)malloc(buf_size);
    for(int i = 1; i<=66600; ++i) {
        ::getline(&buf, &buf_size, OpenFile);
        int ss = ::atof(buf);
        if(TT.Insert(std::make_pair(ss, ss)))
            ++insert_s;
    }
    ::fseek(OpenFile, 0, SEEK_SET);
    for(int i = 1; i<=66600; ++i) {
        ::getline(&buf, &buf_size, OpenFile);
        int ss = ::atof(buf);
        if(TT.Delete(ss))
            ++delete_s;
    }
    ::free(buf);
    for(auto bi = TT.begin(); bi != TT.end(); ++bi)
        std::cout << (*bi).first << "\n";
    std::cout << "count: " << TT.size() << ", depth: " << TT.Depth() << std::endl;
    std::cout << "insert success: " << insert_s << ", delete success: " << delete_s << std::endl;
    print_statistic();
    return;
}

// 982, 949

int main()
{
    printf("testA() / testB(): ");
    size_t buf_size = 500;
    char* buf = (char*)malloc(buf_size);
    for(;;) {
        ::getline(&buf, &buf_size, stdin);
        if(::memcmp(buf, "testA()", sizeof("testA()") - 1) == 0) {
            testA(); return 0;
        } else if(::memcmp(buf, "testB()", sizeof("testB()") - 1) == 0) {
            testB(); return 0;
        }
        printf("\nIncorrect input! testA() / testB(): ");
    }
    return 0;
}
