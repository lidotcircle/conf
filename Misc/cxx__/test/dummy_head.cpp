#include "../src/File/dummy_head.hpp"
#include <iostream>
#include <iomanip>
#include <type_traits>
#include <iterator>

void testA()
{
    file_head<size_t, 4, 20> fff("hell");
    fff.first_id() = 334455;
    FILE* ff = ::fopen("file_head_test.hel", "w+");
    if(ff == nullptr) {std::cerr << "open file fail." << std::endl; return;}
    fff.write_to_file(ff);
    ::fclose(ff);
}

int main()
{
    testA();
    return 0;
}
