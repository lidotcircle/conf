#ifndef BINARY_HEAP_HPP_
#define BINARY_HEAP_HPP_

#include <vector>
#include <stdexcept>
#include <tuple>
#include <iterator>
#include <iostream>

#include <cstdlib>
#include <cassert>

template<typename KV>
class BinaryHeap_IMP //{
{
    public:
        typedef KV                               KeyValueType;
        typedef std::vector<KV>                  ContainerType;
        typedef size_t                           ItemSize;
        typedef typename ContainerType::iterator ReturnIter;

    protected:
        ContainerType m_data;
        ItemSize      m_size;

        inline KeyValueType& __get(const ItemSize& pos) //{
        {
            assert(pos >= 1 && pos <= m_size);
            return this->m_data[pos - 1];
        } //}
        void __fix_bottom_up_heaplify(ItemSize _end) //{ range[1, n]
        {
            assert(m_size >= 1 && _end <= m_size);
            while(_end >= 2) {
                ItemSize parent = _end / 2;
                if(this->kv_less(this->__get(_end), this->__get(parent))){
                    std::swap(__get(_end), __get(parent));
                    _end = parent;
                } else return;
            }
            return;
        } //}
        void __fix_top_down__heaplify(ItemSize _begin) //{ range[1, n]
        {
            assert(m_size >= 1 && _begin <= m_size);
            while(2 * _begin <= m_size) {
                ItemSize left_child  = _begin * 2;
                ItemSize right_child = left_child + 1;
                ItemSize min_item    = _begin;
                if(this->kv_less(this->__get(left_child), this->__get(min_item)))
                    min_item = left_child;
                if(right_child <= m_size && this->kv_less(this->__get(right_child), this->__get(min_item)))
                    min_item = right_child;
                if(min_item != _begin) {
                    std::swap(this->__get(min_item), this->__get(_begin));
                    _begin = min_item;
                } else return;
            }
        } //}
        /// <summary> compare of key value </summary>
        virtual bool kv_less(const KeyValueType& a, const KeyValueType& b) //{
        {
            return a < b;
        } //}
        virtual bool kv_equal(const KeyValueType& a, const KeyValueType& b) //{
        {
            return a == b;
        } //}
        /// <summary> Add new elem </summary>
        template<typename Iterator_t>
        void __Add(Iterator_t _begin, Iterator_t _end, std::input_iterator_tag) //{
        {
            static_assert(std::is_convertible<decltype(*_begin), KeyValueType>::value, "Not compatible type.");
            for(; _begin != _end; ++_begin){
                this->Add(*_begin);
            }
            return;
        } //}

    public:
        /// <summary> constructor </summary>
        BinaryHeap_IMP(): m_data(0), m_size(0){}
        /// <summary> Extract the minimum element, and return it. </summary>
        KeyValueType ExtractMin() //{
        {
            assert(m_size >= 1);
            KeyValueType ret = this->Min();
            std::swap(this->__get(1), this->__get(m_size));
            --this->m_size;
            if(m_size >= 1)
                this->__fix_top_down__heaplify(1);
            this->m_data.erase(m_data.begin() + m_size);
            return ret;
        } //}
        /// <summary> return a const reference of top </summary>
        const KeyValueType& Min() //{
        {
            assert(m_size >= 1);
            return this->m_data[0];
        } //}
        /// <summary> Add new element </summary>
        void Add(const KeyValueType& kv) //{
        {
            this->m_data.push_back(kv);
            ++this->m_size;
            this->__fix_bottom_up_heaplify(this->m_size);
            return;
        } //}
        template<typename Iterator_t>
        void Add(Iterator_t _begin, Iterator_t _end) //{
        {
            this->__Add(_begin, _end, typename std::iterator_traits<Iterator_t>::iterator_category());
        } //}
        /// <summary> Decrease the key </summary>
        void UpdateKey(const ItemSize& where, const KeyValueType& new_elem) //{
        {
            assert(where >= 1 && where <= m_size);
            if(this->kv_less(this->__get(where), new_elem)) {
                this->__get(where) = new_elem;
                this->__fix_top_down__heaplify(where);
            } else {
                this->__get(where) = new_elem;
                this->__fix_top_down__heaplify(where);
            }
            return;
        } //}
        /// <summary> Change by iterator </summary>
        void UpdateKey(ReturnIter iter_where, const KeyValueType& new_elem) //{
        {
            ItemSize where = (iter_where - m_data.begin()) + 1;
            UpdateKey(where, new_elem);
        } //}
        /// <summary> access, (*this)[1] is the min </summary>
        inline ReturnIter operator[](const ItemSize& pos) //{
        {
            assert(pos >= 1 && pos <= m_size);
            ReturnIter begin_iter = m_data.begin();
            return begin_iter + (pos - 1);
        } //}
        /// <summary> empty() </summary>
        inline bool empty() {return this->m_size == 0;}
}; //}

template<typename K, typename V>
class BinaryHeapKV: public BinaryHeap_IMP<std::pair<K, V>> //{
{
    using KeyValueType = typename BinaryHeap_IMP<std::pair<K, V>>::KeyValueType;
    using ItemSize     = typename BinaryHeap_IMP<std::pair<K, V>>::ItemSize;
    protected:
    bool kv_less(const KeyValueType& a, const KeyValueType& b) //{
    {
        return a.first < b.first;
    } //}
    bool kv_equal(const KeyValueType& a, const KeyValueType& b) //{
    {
        return a.first == b.first;
    } //}
}; //}

template<typename KV>
class BinaryHeap: public BinaryHeap_IMP<KV>{};

template<typename KV>
std::ostream& operator<<(std::ostream& os, BinaryHeap_IMP<KV>& bh) //{
{
    os << "[";
    while(!bh.empty())
        os << bh.ExtractMin() << "  ";
    os << "]";
    return os;
} //}

template class BinaryHeap_IMP<double>;
template class BinaryHeapKV<double, int>;

#endif // BINARY_HEAP_HPP_
