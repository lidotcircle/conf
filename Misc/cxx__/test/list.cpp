#include "../src/listsorting/list.hpp"
#include<ctime>
#include<ios>
#include<iostream>
#include<iomanip>

#include<cstdlib>

#include<unistd.h>


#define TEST_LEN 5000
#define TEST_MAX 5000
#define PRINT_PER_LINE 15

int main()
{
    int* array;
    std::srand(time(nullptr));
    size_t x = std::rand() % TEST_LEN;
    size_t y = std::rand() % TEST_LEN;

    array = (int*)malloc(sizeof(int) * x);
    for(size_t i=0;i<x; i++)
        array[i] = (int)i;

    for(size_t i = 0;i<x- 1; i++){
        std::cout << array[i] << " ";
        if((i + 1) % PRINT_PER_LINE == 0)
            std::cout << std::endl;
    }
    std::cout << array[x-1] << std::endl;
    std::cout << "x = " << x << ", y = " << y << std::endl;

    left_rotate (array, array + x, y);
    right_rotate(array, array + x, y);
    for(size_t i = 0;i<x- 1; i++){
        std::cout << array[i] << " ";
        if((i + 1) % PRINT_PER_LINE == 0)
            std::cout << std::endl;
    }
    std::cout << array[x-1] << std::endl;
    return 0;
}
