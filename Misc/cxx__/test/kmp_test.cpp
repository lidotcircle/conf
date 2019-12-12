#include "../src/Algorithm//algorithm.hpp"

#include <cstring>
#include <iostream>

int main() {
    char hello[] = "hello world, hello you, hello me, hello she, hello he, hello, hello, hello!";
    char ppp[] = "hello";

    auto x = ANNA::kmp_search_all(ppp, strlen(ppp), hello, strlen(hello));

    std::cout << x.size() << std::endl;

    for(auto& bi: x)
        std::cout << bi << " ";
}
