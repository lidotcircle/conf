#include<type_traits>

#define HAS_MEMBER(name) \
template<typename T> \
class __has_member_##name { \
    private: \
        struct fallback {int name;}; \
        struct derived: fallback, T{}; \
        template<typename U> \
            static char test(decltype(U::name)*); \
        template<typename U> \
            static int  test(...); \
    public: \
        static constexpr bool value = sizeof(test<derived>(nullptr)) == sizeof(int); \
}; \
template<typename T> \
class has_member_##name: public std::integral_constant<bool, __has_member_##name<T>::value>{}

#define HAS_MEMBER_TYPE(name) \
namespace __has_member_type_##name##_imp { \
template<typename U> \
    static char test(typename U::name*); \
template<typename U> \
    static int test(...); \
}; \
template<typename T> \
class has_member_type_##name: public std::integral_constant<bool, \
sizeof(__has_member_type_##name##_imp::test<T>(nullptr)) == 1> {};

template<typename T, typename ARG>
class __is_callable {
    private:
        template<typename U, void (U::*)(ARG)>
        struct check {};
        template<typename U>
            static char test(check<U, &U::operator()>*);
        template<typename U>
            static int test(...);
    public:
        static constexpr bool value = sizeof(test<T>(nullptr)) == sizeof(char);
};
template<typename T>
class __is_callable<T, void> {
    private:
        template<typename U, void (U::*)()>
        struct check {};
        template<typename U>
            static char test(check<U, &U::operator()>*);
        template<typename U>
            static int test(...);
    public:
        static constexpr bool value = sizeof(test<T>(nullptr)) == sizeof(char);
};
template<typename T, typename ARG = void>
class is_callable: public std::integral_constant<bool, __is_callable<T, ARG>::value> {};
