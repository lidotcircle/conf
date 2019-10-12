#include "../src/listsorting/sort.hpp"
#include<ctime>
#include<ios>
#include<iostream>
#include<iomanip>

#include<cstdlib>

#include<unistd.h>

using namespace ListSort;

#define TEST_LEN 100000
#define TEST_MAX 1000000
#define PRINT_PER_LINE 15

void print_int_list(int* list, size_t len)
{
    size_t i = 0;
    std::cout << "[";
    for(;i<len - 1;i++){
        std::cout << std::left <<std::setw(5) << list[i];
        if(i % PRINT_PER_LINE == (PRINT_PER_LINE- 1))
            std::cout << ",\n ";
        else
            std::cout << ", ";
    }
    std::cout << list[len - 1] << "]\n";
    std::cout << std::flush;
    return;
}

void sortTest(int* sort_func(int*, size_t, bool comp_f(void*, void*)), std::string sorttype)
{
    std::cout << std::right << std::setw(16) << sorttype << ":";
    clock_t begin = clock();
    int* intlist = (int*)malloc(TEST_LEN * sizeof(int));
    std::srand(std::time(nullptr));
    for(int i = 0;i<TEST_LEN;i++)
    {
        std::srand(1);
        intlist[i] =  std::rand() % TEST_MAX;
    }
    sort_func(intlist, TEST_LEN, 
            [](void* x, void* y){return *((int*)x) <= *((int*)y);});
    clock_t gap = clock() - begin;
    std::cout << std::right << std::setw(15) << std::dec << std::setprecision(8) << std::fixed
        << (gap / (1.0)) << " Clock" << std::endl;
//        << (gap / (CLOCKS_PER_SEC / 10e3)) << " ms" << std::endl;
//    print_int_list(intlist, TEST_LEN);
    return;
}

void sortTestv2(size_t len, int* sort_func(int*, size_t, bool comp_f(void*, void*)), std::string sorttype)
{
    clock_t begin = clock();
    int* intlist = (int*)malloc(len * sizeof(int));
    std::srand(std::time(nullptr));
    for(size_t i = 0;i<len;i++)
    {
        intlist[i] =  std::rand() % TEST_MAX;
    }
    sort_func(intlist, len, 
            [](void* x, void* y){return *((int*)x) <= *((int*)y);});
    clock_t gap = clock() - begin;
    std::cout << std::right << std::setw(16) << sorttype << " : {Length : " 
        << std::right << std::setw(8) << len << ", Time :";
    std::cout << std::right << std::setw(8) << std::setprecision(5) << std::fixed 
        << (gap / (CLOCKS_PER_SEC / 10e3)) << " ms}" << std::endl;
    return;
}

void sortTestv3(size_t len, void sort_func(int*, int*), std::string sorttype)
{
    clock_t begin = clock();
    int* intlist = (int*)malloc(len * sizeof(int));
    std::srand(std::time(nullptr));
    for(size_t i = 0;i<len;i++)
    {
        intlist[i] =  std::rand() % TEST_MAX;
    }
    sort_func(intlist, intlist + len);
    clock_t gap = clock() - begin;
    std::cout << std::right << std::setw(16) << sorttype << " : {Length : " 
        << std::right << std::setw(8) << len << ", Time :";
    std::cout << std::right << std::setw(8) << std::setprecision(5) << std::fixed 
        << (gap / (CLOCKS_PER_SEC / 10e3)) << " ms}" << std::endl;
    print_int_list(intlist, TEST_LEN);
    return;
}



void compare_test()
{
    sortTest(ListSort::mergesort_ip<int>, "mergesort_ip");
    sortTest(ListSort::mergesort<int>, "mergesort");
    sortTest(ListSort::quicksort<int>, "quicksort");
    sortTest(ListSort::bubblesort<int>, "bubblesort");
    sortTest(ListSort::selectionsort<int>, "selectionsort");
    sortTest(ListSort::insertionsort<int>, "insertionsort");
}


void single_test()
{
    /*
    for(int i = 1; i < 10; i++)
    {
        sortTestv2(i * TEST_LEN, ListSort::mergesort<int>, "mergesort");
    }

    for(int i = 1; i < 10; i++)
    {
        sortTestv2(i * TEST_LEN, ListSort::mergesort_ip<int>, "mergesort_ip");
    }
    */
    sortTestv3(TEST_LEN, ListSort::heapsort<int*>, "heapsort");
    /*
    for(int i = 1; i < 10; i++)
    {
        sortTestv2(i * TEST_LEN, ListSort::mergesortv2<int>, "mergesortv2");
    }

    for(int i = 1; i < 10; i++)
    {
        sortTestv2(i * TEST_LEN, ListSort::quicksort<int>, "quicksort");
    }
    */
}

int main()
{
    single_test();
//    compare_test();
    return 0;
}
