#include "../src/String/String.hpp"

#include<vector>

#include<iostream>
#include<iomanip>

#include<cstdlib>
#include<ctime>

#include<cstring>

#define STRING_MATCH_SIZE    10000
#define STRING_MATCH_SIZE_M  4
#define STRING_MATCH_RATIO   20000
#define CHAR_TYPE            10

void kmp_test()
{
    char hello[STRING_MATCH_SIZE];
    hello[STRING_MATCH_SIZE - 1] = '\0';
    char w[STRING_MATCH_SIZE_M];
    char holder[STRING_MATCH_SIZE_M];
    w[STRING_MATCH_SIZE_M - 1] = '\0';
    std::srand(std::time(nullptr));
    for(size_t i = 0;i<STRING_MATCH_SIZE - 1;i++){
        hello[i] = rand() % CHAR_TYPE + 'a';
    }
    for(size_t i = 0;i<STRING_MATCH_SIZE_M- 1;i++){
        w[i] = rand() % CHAR_TYPE + 'a';
    }
    /*
    char hello[] = "iiie";
    char w[]     = "iie";
    */

    std::cout << "*" << hello << "*" << std::endl << std::endl; 
    std::cout << "*" << w     << "*" << std::endl; 

    size_t *x = get_KMP_map<char>(w, w + ::strlen(w));
//    int s = KMP_search<char>(hello, hello + strlen(hello), w, w + strlen(w));
    auto aa = KMP_search_all<char>(hello, hello + strlen(hello), w, w + strlen(w));
    for(size_t i = 0; w[i] != '\0';i++)
        std::cout << std::setw(5) << x[i] << std::endl << std::flush;
    for(auto i = aa.begin(); i!=aa.end(); i++){
        ::memcpy(holder, hello + *i, strlen(w));
        std::cout << *i << "     " << holder << std::endl;
    }
    free(x);
}

int main()
{
    kmp_test();
}
