#ifndef FIBONACCIHEAP_HPP_
#define FIBONACCIHEAP_HPP_

#include <vector>
#include <stdexcept>
#include <tuple>
#include <iterator>
#include <limits>

#include <cstdlib>
#include <cassert>
#include <cmath>
#include <cstring>

namespace FibonacciHeap_IMP_HELP {
template<typename KV>
KV min_elem_only()
{
    return KV(0);
}
template<> double min_elem_only(){return -std::numeric_limits<double>::infinity();}
}

/// <summary> Amortized Function: h(FibonacciHeap_IMP dd) = dd.min.length + blacked.length * 2
///           h(x) >= 0 for any situation, so it's validate amortized function.
///           function extractMin() : O(log(n)), worst case: O(n)
///           function DecreaseKey(): O(1),      worst case: O(log(n))
///           function UnionWith()  : O(1),      worst case: O(1)
///           function Delete()     : O(1),      worst case: O(n)
/// </summary>
template<typename KV>
class FibonacciHeap_IMP //{
{
    public:
        typedef KV                               KeyValueType;
        typedef std::vector<KV>                  ContainerType;
        typedef size_t                           ItemSize;
        typedef typename ContainerType::iterator ReturnIter;
        struct __elem //{
        {
            __elem*      m_parent;
            __elem*      m_next; // circular list
            __elem*      m_prev; // circular list
            __elem*      m_child;
            KeyValueType m_kv;
            bool         m_blacked;
            ItemSize     m_degree;
            int          m_checked_num; // for debug
            __elem() = delete;
            __elem(const KeyValueType& kv): 
                m_parent(nullptr),
                m_next(nullptr),
                m_prev(nullptr),
                m_child(nullptr),
                m_kv(kv),
                m_blacked(false),
                m_degree(0), 
                m_checked_num(0){}
            ~__elem() {
                if(this->m_prev != nullptr)
                    this->m_prev->m_next = nullptr;
                if(this->m_next  != nullptr) delete m_next;
                if(this->m_child != nullptr) delete m_child;
            }
        }; //}

    protected:
        __elem*  m_min_pointer;
        ItemSize m_size;
        /// <summary> compare of key value </summary>
        virtual bool kv_less(const KeyValueType& a, const KeyValueType& b) //{
        {
            return a < b;
        } //}
        virtual bool kv_equal(const KeyValueType& a, const KeyValueType& b) //{
        {
            return a == b;
        } //}
        void apppend_to_child_list(__elem* child, __elem* pointer) //{
        {
            assert(pointer != nullptr);
            assert(child   != nullptr);
            pointer->m_prev       = child->m_prev;
            child->m_prev->m_next = pointer;
            pointer->m_next       = child;
            child->m_prev         = pointer;
            pointer->m_parent     = child->m_parent;
        } //}
        inline void apppend_to_top_list(__elem* pointer) //{
        {
            apppend_to_child_list(m_min_pointer, pointer);
        } //}

        void __merge_list(__elem* degree_list[], __elem* xxp) //{
        {
            assert(degree_list[xxp->m_degree] != nullptr);
            assert(degree_list[xxp->m_degree]->m_degree == xxp->m_degree);
            __elem* holder = degree_list[xxp->m_degree];
            __elem *lesser, *bigger;
            if(this->kv_less(xxp->m_kv, holder->m_kv)){
                lesser = xxp;
                bigger = holder;
            } else {
                lesser = holder;
                bigger = xxp;
            }
            if(bigger->m_prev != nullptr)
                bigger->m_prev->m_next = bigger->m_next;
            if(bigger->m_next != nullptr)
                bigger->m_next->m_prev = bigger->m_prev;
            if(lesser->m_child == nullptr) {
                lesser->m_child  = bigger;
                bigger->m_parent = lesser;
                bigger->m_prev   = bigger;
                bigger->m_next   = bigger;
            } else apppend_to_child_list(lesser->m_child, bigger);
            degree_list[lesser->m_degree] = nullptr; // FUCK
            ++lesser->m_degree;
            if(degree_list[lesser->m_degree] == nullptr)
                degree_list[lesser->m_degree] = lesser;
            else
                __merge_list(degree_list, lesser);
            return;
        } //}
        void consolidate_list(__elem* list) //{
        {
            assert(list != nullptr);
            ItemSize degree_list_size = std::log(this->m_size) / std::log((1 + std::sqrt(5)) / 2);
            __elem** degree_list      = (__elem**)std::malloc(sizeof(__elem*) * degree_list_size);
            std::memset(degree_list, '\0', sizeof(__elem*) * degree_list_size);
            __elem* xyz__ = list;
            __elem* min__ = list;
            while(xyz__ != nullptr) {
                list = list->m_next;
                if(list != nullptr && this->kv_less(list->m_kv, min__->m_kv))
                    min__ = list;
                if(degree_list[xyz__->m_degree] != nullptr) // merge
                    __merge_list(degree_list, xyz__);
                else degree_list[xyz__->m_degree] = xyz__;
                xyz__ = list;
            }
            this->m_min_pointer = min__;
            __elem *last = min__;
            for(;last->m_next  != nullptr; last  = last->m_next);
            for(;min__->m_prev != nullptr; min__ = min__->m_prev);
            last->m_next  = min__;
            min__->m_prev = last;
            std::free(degree_list);
            return;
        } //}

        void move_to_top(__elem* elem) //{
        {
            if(elem->m_next == elem) // last child
                elem->m_parent->m_child = nullptr;
            else {
                elem->m_next->m_prev = elem->m_prev;
                elem->m_prev->m_next = elem->m_next;
            }
            apppend_to_top_list(elem);
        } //}
        void decrease_check(__elem* parent) //{
        {
            --parent->m_degree;
            if(parent->m_parent == nullptr)// top
                return;
            if(parent->m_blacked == false) { parent->m_blacked = true; return;}
            __elem* pp = parent->m_parent;
            parent->m_blacked = false;
            move_to_top(parent);
            decrease_check(pp);
            return;
        } //}
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
         FibonacciHeap_IMP(): m_min_pointer(nullptr), m_size(0){}
        ~FibonacciHeap_IMP(){if(this->m_min_pointer != nullptr) delete this->m_min_pointer;}

        /// <summary> new element. Amortized Time: O(1) </summary>
        void Add(const KeyValueType& kv) //{
        {
            ++this->m_size;
            __elem* x_new = new __elem(kv);
#ifdef FIBONACCIHEAP_DEBUG
            JointHolder.push_back(x_new);
#endif // FIBONACCIHEAP_DEBUG
            if(this->m_min_pointer == nullptr) {
                m_min_pointer   = x_new;
                x_new->m_next   = x_new;
                x_new->m_prev   = x_new;
                x_new->m_parent = nullptr;
                x_new->m_child  = nullptr;
            } else apppend_to_top_list(x_new);
            if(this->kv_less(kv, m_min_pointer->m_kv))
                m_min_pointer = x_new;
            return;
        } //}
        template<typename Iterator_t>
        void Add(Iterator_t _begin, Iterator_t _end) //{
        {
            this->__Add(_begin, _end, typename std::iterator_traits<Iterator_t>::iterator_category());
        } //}
        /// <summary> Reference to minimum element </summary>
        const KeyValueType& Min(){assert(this->m_min_pointer != nullptr); return this->m_min_pointer->m_kv;}
        /// <summary> extract minimum element. Amotized Time: O(log(n)) </summary>
        KeyValueType ExtractMin() //{
        {
#ifdef FIBONACCIHEAP_DEBUG
            JointHolder.push_back(x_new);
#endif // FIBONACCIHEAP_DEBUG
            KeyValueType ret = std::move(m_min_pointer->m_kv);
            __elem* children = this->m_min_pointer->m_child;
            __elem* prev_p   = this->m_min_pointer->m_prev;
            __elem* next_p   = this->m_min_pointer->m_next;
            prev_p->m_next   = nullptr;
            next_p->m_prev   = nullptr;
            __elem* deleted_ = this->m_min_pointer;
            m_min_pointer->m_next  = nullptr;
            m_min_pointer->m_prev  = nullptr;
            m_min_pointer->m_child = nullptr;
            bool top_list_is_empty = prev_p->m_prev == nullptr;
            bool children_is_empty = children == nullptr;
            if(top_list_is_empty && children_is_empty) { // true, true
                assert(this->m_size == 1);
                this->m_min_pointer = nullptr;
                this->m_size = 0;
                delete deleted_;
                return ret;
            } else this->m_min_pointer = prev_p;
            if(top_list_is_empty) { // true, false
                children->m_prev->m_next = nullptr;
                children->m_next->m_prev = nullptr;
                next_p = children;
            }
            if(!children_is_empty && !top_list_is_empty) { // false, false
                children->m_prev->m_next = nullptr;
                prev_p->m_next           = children;
                children->m_prev         = prev_p;
            }  // false, true is trival
            consolidate_list(next_p);
            --this->m_size;
            delete deleted_;
#ifdef FIBONACCIHEAP_DEBUG
            int num = check_fibonacci_heap(this->m_min_pointer);
            assert(num == this->m_size);
#endif // FIBONACCIHEAP_DEBUG
            return ret;
        } //}
        /// <summary> decrease key </summary>
        void DecreaseKey(__elem* elem, const KeyValueType& new_v) //{
        {
            if(elem->m_parent == nullptr || !kv_less(new_v, elem->m_parent->m_kv)) {
                elem->m_kv = new_v;
                if(elem->m_parent == nullptr && kv_less(elem->m_kv, m_min_pointer->m_kv))
                    m_min_pointer = elem;
                return;
            }
            elem->m_kv = new_v;
            __elem* parent = elem->m_parent;
            move_to_top(elem);
            decrease_check(parent);
            return;
        } //}
        void UnionWith(FibonacciHeap_IMP& fh) //{
        {
            if(&fh == this) return;
            if(fh.m_min_pointer == nullptr) return;
            if(this->m_min_pointer == nullptr) {
                this->m_min_pointer = fh.m_min_pointer;
                fh.m_min_pointer    = nullptr;
                this->m_size = fh.m_size;
                fh.m_size    = 0;
                return;
            }
            __elem* possible_2 = fh.m_min_pointer;
            m_min_pointer->m_next->m_prev = possible_2->m_prev;
            possible_2->m_prev->m_next = m_min_pointer->m_next;
            m_min_pointer->m_next = possible_2;
            possible_2->m_prev = m_min_pointer;
            if(kv_less(possible_2->m_kv, m_min_pointer->m_kv))
                m_min_pointer = possible_2;
            this->m_size += fh.m_size;
            fh.m_size     = 0;
            return;
        } //}
        void DeleteKey(__elem* elem) //{
        {
            KeyValueType min_elem = FibonacciHeap_IMP_HELP::min_elem_only<KeyValueType>();
            DecreaseKey(elem, min_elem);
            this->ExtractMin();
            return;
        } //}
        inline bool empty() {return this->m_size == 0;}

        /// <summary> debug function </summary>
        static std::vector<__elem*> JointHolder;
        static int current_checked;
        static ItemSize check_fibonacci_heap_helper(__elem* cl, int num)
        {
            int result = 0;
            for(;cl->m_checked_num != num; cl = cl->m_next) {
                ++result;
                assert(cl->m_checked_num < num);
                cl->m_checked_num = num;
                if(cl->m_child != nullptr) result += check_fibonacci_heap_helper(cl->m_child, num);
            }
            return result;
        }
        static ItemSize check_fibonacci_heap(__elem* circular_list_elem)
        {
            ++current_checked;
            return check_fibonacci_heap_helper(circular_list_elem, current_checked);
        }
}; //}

template<typename KV>
int FibonacciHeap_IMP<KV>::current_checked = 1;
template<typename KV>
std::vector<typename FibonacciHeap_IMP<KV>::__elem*> FibonacciHeap_IMP<KV>::JointHolder;

template<typename KV>
std::ostream& operator<<(std::ostream& os, FibonacciHeap_IMP<KV>& bh) //{
{
    os << "[";
    while(!bh.empty())
        os << bh.ExtractMin() << "  " << std::endl;
    os << "]";
    return os;
} //}

template<typename K, typename V>
class FibonacciHeapKV: public FibonacciHeap_IMP<std::pair<K, V>> //{
{
    using KeyValueType = typename FibonacciHeap_IMP<std::pair<K, V>>::KeyValueType;
    using ItemSize     = typename FibonacciHeap_IMP<std::pair<K, V>>::ItemSize;
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
class FibonacciHeap: public FibonacciHeap_IMP<KV>{};

template class FibonacciHeap<double>;
template class FibonacciHeapKV<double, int>;

template class FibonacciHeap_IMP<double>;

#endif // FIBONACCIHEAP_HPP_
