#ifndef ANNA_UTILS_HPP_
#define ANNA_UTILS_HPP_

#define ANNA_BEGIN namespace ANNA {
#define ANNA_END   }
ANNA_BEGIN

template<bool, typename T1, typename T2>
struct if_else {};
template<typename T1, typename T2>
struct if_else<true , T1, T2> {typedef T1 type;};
template<typename T1, typename T2>
struct if_else<false, T1, T2> {typedef T2 type;};

ANNA_END
#endif // ANNA_UTILS_HPP_
