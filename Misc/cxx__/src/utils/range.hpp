#ifndef ANNA_RANGE_HPP_

#include <iterator>

namespace ANNA {

template<typename T>
class Range_IMP {
    public:
        typedef T value_type_t;
        class iterator //{
        {
            public:
                friend class Range_IMP;
                using value_type = value_type_t;
                typedef value_type&             reference;
                typedef value_type*             pointer;
                typedef int                     difference_type;
                typedef std::input_iterator_tag iterator_category;
            public:
                value_type operator *() const { return i_; }
                const iterator &operator ++() { ++i_; return *this; }
                iterator operator ++(int) { iterator copy(*this); ++i_; return copy; }

                bool operator ==(const iterator &other) const { return i_ == other.i_; }
                bool operator !=(const iterator &other) const { return i_ != other.i_; }

            protected:
                iterator(value_type start) : i_(start) { }

            private:
                value_type i_;
        }; //}

    public:
        iterator begin() const { return begin_; }
        iterator end() const { return end_; }
        Range_IMP(value_type_t begin, value_type_t end) : begin_(begin), end_(end) {}

    private:
        iterator begin_;
        iterator end_;
};

template<typename T>
Range_IMP<T> range(T start, T end) //{
{
    return Range_IMP<T>(start, end);
} //}

}
#endif // ANNA_RANGE_HPP_
