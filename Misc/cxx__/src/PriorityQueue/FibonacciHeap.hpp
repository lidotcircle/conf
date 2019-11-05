#ifndef FIBONACCIHEAP_HPP_
#define FIBONACCIHEAP_HPP_

#include <vector>
#include <stdexcept>
#include <tuple>
#include <iterator>
#include <limits>
#include <algorithm>
#include <iostream>
#include <stack>

#include <cstdlib>
#include <cassert>
#include <cmath>
#include <cstring>

namespace FibonacciHeap_IMP_HELP {
template<typename KV>
KV min_elem_only()
{
    return KV(std::numeric_limits<KV>::min());
}
//template<> double min_elem_only(){return -std::numeric_limits<double>::infinity();}
}

/// <summary> Amortized Function: h(FibonacciHeap_IMP dd) = dd.min.length + blacked.length * 2
///           h(x) >= 0 for any situation, so it's validate amortized function.
///           function extractMin() : O(log(n)), worst case: O(n)
///           function DecreaseKey(): O(1),      worst case: O(log(n))
///           function UnionWith()  : O(1),      worst case: O(1)
///           function Delete()     : O(1),      worst case: O(n)
/// </summary>
template<typename KV, typename VT>
class FibonacciHeap_IMP //{
{

    static_assert(std::is_pointer<KV>::value, "T should be pointer type");
    static_assert(std::is_class<std::remove_pointer_t<KV>>::value, "T should be pointer to a non-atom object");
    public:
        typedef KV                               KeyValueType;
        typedef VT                               NewValueType;
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
        virtual bool         kv_less(const KeyValueType& a, const KeyValueType& b) = 0;
        virtual bool         kv_equal(const KeyValueType& a, const KeyValueType& b) = 0;
        virtual __elem*      GetPointer(const KeyValueType&)    = 0;
        virtual void         SetPointer(KeyValueType&, __elem*) = 0;
        virtual void         SetNewValue(KeyValueType&, const NewValueType&) = 0;

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
            assert(xxp->m_parent == nullptr);
            assert(degree_list[xxp->m_degree] != nullptr);
            assert(degree_list[xxp->m_degree]->m_degree == xxp->m_degree);
#ifdef FIBONACCIHEAP_DEBUG
            assert(is_bidirection_linked_list(xxp, "__merge_list()"));
#endif // FIBONACCIHEAP_DEBUG
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
#ifdef FIBONACCIHEAP_DEBUG
            assert(is_bidirection_linked_list(lesser, "__merge_list()2"));
#endif // FIBONACCIHEAP_DEBUG
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
#ifdef FIBONACCIHEAP_DEBUG
            assert(is_bidirection_linked_list(list, "consolidate_list()"));
#endif // FIBONACCIHEAP_DEBUG
            ItemSize degree_list_size = std::log(this->m_size) / std::log((1 + std::sqrt(5)) / 2);
            __elem** degree_list      = (__elem**)std::malloc(sizeof(__elem*) * degree_list_size);
            std::memset(degree_list, '\0', sizeof(__elem*) * degree_list_size);
            for(;list->m_prev != nullptr; list = list->m_prev);
            __elem* xyz__ = list;
            while(xyz__ != nullptr) {
                list = list->m_next;
                if(degree_list[xyz__->m_degree] != nullptr) // merge
                    // the minimum precedence of kv_less in __merge_list() not only relate with order in <list>, 
                    // that also be influenced by degree. Such higher degree may have higher precedence when they merge.
                    // so the minimum element should be chose in degree_list...
                    __merge_list(degree_list, xyz__); 
                else degree_list[xyz__->m_degree] = xyz__;
                xyz__ = list;
            }
            __elem* min__ = nullptr;
            __elem* loop__ = nullptr;
            __elem *start = nullptr, *last = nullptr;
            for(size_t i=0;i<degree_list_size && min__ == nullptr;++i)
                if(degree_list[i] != nullptr)
                    min__ = degree_list[i];
            for(;min__->m_prev != nullptr; min__ = min__->m_prev);
            loop__ = min__;
            start = min__;
            for(;loop__->m_next != nullptr; loop__ = loop__->m_next)
                if(kv_less(loop__->m_next->m_kv, min__->m_kv))
                    min__ = loop__->m_next;
            this->m_min_pointer = min__;
            last = loop__;
            assert(m_min_pointer->m_parent == nullptr);
#ifdef FIBONACCIHEAP_DEBUG
            assert(is_bidirection_linked_list(last, "consolidate_list()2"));
#endif // FIBONACCIHEAP_DEBUG
            last->m_next  = start;
            start->m_prev = last;
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
        template<typename Iterator_t, std::enable_if_t<std::is_convertible<typename std::iterator_traits<Iterator_t>::value_type, KeyValueType>::value, int> = 0>
        void __Add(Iterator_t _begin, Iterator_t _end, std::input_iterator_tag) //{
        {
            for(; _begin != _end; ++_begin){
                this->Add(*_begin);
            }
            return;
        } //}

    public:
         FibonacciHeap_IMP(): m_min_pointer(nullptr), m_size(0){}
        ~FibonacciHeap_IMP(){if(this->m_min_pointer != nullptr) delete this->m_min_pointer;}
        FibonacciHeap_IMP(const FibonacciHeap_IMP&) = delete;
        FibonacciHeap_IMP& operator=(const FibonacciHeap_IMP&) = delete;
        FibonacciHeap_IMP(FibonacciHeap_IMP&& fh): FibonacciHeap_IMP(){*this = std::move(fh);}
        FibonacciHeap_IMP& operator=(FibonacciHeap_IMP&& fh) //{
        {
            if(this == &fh) return *this;
            if(this->m_min_pointer != nullptr) delete this->m_min_pointer;
            this->m_size        = fh.m_size;
            this->m_min_pointer = fh.m_min_pointer;
            fh.m_size           = 0;
            fh.m_min_pointer    = nullptr;
#ifdef FIBONACCIHEAP_DEBUG
            this->JointHolder     = std::move(fh.JointHolder);
            this->CheckedHolder   = std::move(fh.CheckedHolder);
            this->current_checked = fh.current_checked;
            fh.current_checked    = 0;
#endif // FIBONACCIHEAP_DEBUG
            return *this;
        } //}

        virtual NewValueType GetValueType(KeyValueType) = 0;

        inline void check_point() //{
        {
#ifdef FIBONACCIHEAP_DEBUG
            int num = check_fibonacci_heap(this->m_min_pointer);
            if((int)this->m_size != num) {
                element_fault_check();
                throw *new std::runtime_error("DON't match element with size, memory leaked.");
            }
#endif // FIBONACCIHEAP_DEBUG
            return;
        } //}

        /// <summary> new element. Amortized Time: O(1) </summary>
        void Add(KeyValueType kv) //{
        {
            ++this->m_size;
            __elem* x_new = new __elem(kv);
            this->SetPointer(kv, x_new);
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
            check_point();
            return;
        } //}
        template<typename Iterator_t, std::enable_if_t<std::is_convertible<typename std::iterator_traits<Iterator_t>::value_type, KeyValueType>::value, int> = 0>
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
            for(auto bi = JointHolder.begin(); bi != JointHolder.end(); ++bi){
                if(*bi == this->m_min_pointer){
                    JointHolder.erase(bi);
                    break;
                }
            }
            assert(is_bidirection_circular_linked_list(this->m_min_pointer));
#endif // FIBONACCIHEAP_DEBUG
            KeyValueType ret = std::move(m_min_pointer->m_kv);
            this->SetPointer(ret, nullptr); // clear reference in object
            __elem* children = this->m_min_pointer->m_child;
            __elem* prev_p   = this->m_min_pointer->m_prev;
            __elem* next_p   = this->m_min_pointer->m_next;
            bool top_list_is_empty = m_min_pointer == m_min_pointer->m_next;
            prev_p->m_next   = nullptr;
            next_p->m_prev   = nullptr;
            __elem* deleted_ = this->m_min_pointer;
            m_min_pointer->m_next  = nullptr;
            m_min_pointer->m_prev  = nullptr;
            m_min_pointer->m_child = nullptr;
            bool children_is_empty = children == nullptr;
            if(top_list_is_empty && children_is_empty) { // true, true
                assert(this->m_size == 1);
                this->m_min_pointer = nullptr;
                this->m_size = 0;
                delete deleted_;
                return ret;
            } else this->m_min_pointer = prev_p;
            if(!children_is_empty){
                children->m_parent = nullptr;
                for(auto x = children->m_next; x != children; x = x->m_next)
                    x->m_parent = nullptr;
            }
            if(top_list_is_empty) { // true, false
                children->m_prev->m_next = nullptr;
                children->m_prev         = nullptr;
                next_p = children;
            }
            if(!children_is_empty && !top_list_is_empty) { // false, false
                children->m_prev->m_next = nullptr;
                prev_p->m_next           = children;
                children->m_prev         = prev_p;
            }  // false, true is trival
            consolidate_list(next_p);
#ifdef FIBONACCIHEAP_DEBUG
            assert(is_bidirection_circular_linked_list(this->m_min_pointer));
#endif // FIBONACCIHEAP_DEBUG
            --this->m_size;
            check_point();
            delete deleted_;
            return ret;
        } //}
        /// <summary> decrease key </summary>
        void __DecreaseKey(__elem* elem, const NewValueType& new_v) //{
        {
            SetNewValue(elem->m_kv, new_v);
            if(elem->m_parent == nullptr || !kv_less(elem->m_kv, elem->m_parent->m_kv)){
                if(elem->m_parent == nullptr){
                    if(kv_less(elem->m_kv, m_min_pointer->m_kv))
                        m_min_pointer = elem;
                }
                return;
            }
            __elem* parent = elem->m_parent;
            move_to_top(elem);
            decrease_check(parent);
            return;
        } //}
        void DecreaseKey(KeyValueType kv, const NewValueType& nv) //{
        {
            __elem* pp = this->GetPointer(kv);
            this->__DecreaseKey(pp, nv);
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
            __elem* possible_2            = fh.m_min_pointer;
            m_min_pointer->m_next->m_prev = possible_2->m_prev;
            possible_2->m_prev->m_next    = m_min_pointer->m_next;
            m_min_pointer->m_next         = possible_2;
            possible_2->m_prev            = m_min_pointer;
            if(kv_less(possible_2->m_kv, m_min_pointer->m_kv))
                m_min_pointer = possible_2;
            this->m_size     += fh.m_size;
            fh.m_size         = 0;
            fh.m_min_pointer  = nullptr;
#ifdef FIBONACCIHEAP_DEBUG
            for(auto bi : fh.JointHolder)
                this->JointHolder.push_back(bi);
            fh.CheckedHolder.resize(0);
            fh.JointHolder.resize(0);
            this->current_checked += fh.current_checked;
#endif // FIBONACCIHEAP_DEBUG
            return;
        } //}
        void DeleteKey(__elem* elem) //{
        {
            NewValueType min_elem = FibonacciHeap_IMP_HELP::min_elem_only<NewValueType>();
            __DecreaseKey(elem, min_elem);
            this->ExtractMin();
            return;
        } //}
        void DeleteKey(KeyValueType kv) //{
        {
            __elem* elem = this->GetPointer(kv);
            this->DeleteKey(elem);
            return;
        } //}
        inline bool empty() const {return this->m_size == 0;}

        /// <summary> debug function </summary>
#ifdef FIBONACCIHEAP_DEBUG //{
        static std::vector<__elem*> JointHolder;
        static std::vector<__elem*> CheckedHolder;
        static int current_checked;
        static ItemSize check_fibonacci_heap_helper(__elem* cl, int num)
        {
            int result = 0;
            for(;cl->m_checked_num != num; cl = cl->m_next) {
                ++result;
                CheckedHolder.push_back(cl);
                assert(cl->m_checked_num < num);
                cl->m_checked_num = num;
                if(cl->m_child != nullptr) result += check_fibonacci_heap_helper(cl->m_child, num);
            }
            return result;
        }
        static ItemSize check_fibonacci_heap(__elem* circular_list_elem)
        {
            ++current_checked;
            CheckedHolder.resize(0);
            return check_fibonacci_heap_helper(circular_list_elem, current_checked);
        }
        static void element_fault_check()
        {
            assert(JointHolder.size() >= CheckedHolder.size());
            std::sort(JointHolder.begin(), JointHolder.end());
            std::sort(CheckedHolder.begin(), CheckedHolder.end());
            std::cout << "Number of allocated element is: " << JointHolder.size() << std::endl;
            std::cout << "Number of accessed  element is: " << CheckedHolder.size() << std::endl;
            std::cout << "Number of lossed element is   : " << JointHolder.size() - CheckedHolder.size() << std::endl;
            std::cout << "------------------------------- LOSS" << std::endl;
            auto ck = CheckedHolder.begin();
            for(auto hd = JointHolder.begin(); hd != JointHolder.end(); ++hd) {
                if(ck == CheckedHolder.end()){
                    while(hd != JointHolder.end()){
                        std::cout << "LOSS - Address: 0x" << std::hex << *hd << ", Data: " << (*hd)->m_kv << ", Degree: " << (*hd)->m_degree
                            << std::endl;
                        ++hd;
                    }
                    break;
                };
                if(*ck != *hd){ // loss
                    std::cout << "LOSS - Address: 0x" << std::hex << *hd << ", Data: " << (*hd)->m_kv << ", Degree: " << (*hd)->m_degree
                        << std::endl;
                    continue;
                }
                std::cout << "HAS  - Address: 0x" << std::hex << *hd << ", Data: " << (*ck)->m_kv << ", Degree: " << (*hd)->m_degree
                    << std::endl;
                ++ck;
            }
        }
        static bool is_bidirection_circular_linked_list(__elem* begin, std::string where = "NOWHERE") //{
        {
            assert(begin != nullptr);
            std::stack<__elem*> p_holder;
            __elem* loop = begin;
            for(;loop->m_next != begin; loop = loop->m_next) p_holder.push(loop);
            p_holder.push(loop);
            for(loop = begin->m_prev;;loop = loop->m_prev) {
                if(loop != p_holder.top()) {
                    throw *new std::runtime_error("break here");
                    return false;
                }
                p_holder.pop();
                if(p_holder.empty()) break;
            }
            if(loop != begin) return false;
            return true;
        } //}
        static bool is_bidirection_linked_list(__elem* begin, std::string where = "NOWHERE") //{
        {
            std::cout << std::endl;
            std::cout << where << ": " << std::endl;
            assert(begin != nullptr);
            std::stack<__elem*> p_holder;
            __elem* loop = begin;
            for(;loop->m_next != nullptr; loop = loop->m_next) {
                std::cout << "XXX s -> l: " << std::hex << loop << " -> " << loop->m_next << std::endl;
                if(loop->m_next == begin) throw *new std::runtime_error("maybe circular linked list.");
            } 
            begin = loop; // end
            for(;loop->m_prev != nullptr; loop = loop->m_prev){
                if(loop->m_prev == begin) throw *new std::runtime_error("maybe circular linked list.");
                p_holder.push(loop);
                std::cout << "l -> s: " << std::hex << loop << " -> " << loop->m_prev << std::endl;
            }
            std::cout << "l -> s: " << std::hex << loop << " -> " << loop->m_prev << std::endl;
            p_holder.push(loop); // loop is start point
            begin = loop;
            for(;loop->m_next != nullptr;loop = loop->m_next) {
                if(loop != p_holder.top()) {
                    throw *new std::runtime_error("break here");
                    return false;
                }
                p_holder.pop();
                std::cout << "s -> l: " << std::hex << loop << " -> " << loop->m_next << std::endl;
                if(p_holder.empty()) break;
            }
            std::cout << "s -> l: " << std::hex << loop << " -> " << loop->m_next << std::endl;
            return true;
        } //}
#endif // FIBONACCIHEAP_DEBUG //}
}; //}

#ifdef FIBONACCIHEAP_DEBUG
template<typename KV, typename VT>
int FibonacciHeap_IMP<KV, VT>::current_checked = 1;
template<typename KV, typename VT>
std::vector<typename FibonacciHeap_IMP<KV, VT>::__elem*> FibonacciHeap_IMP<KV, VT>::JointHolder;
template<typename KV, typename VT>
std::vector<typename FibonacciHeap_IMP<KV, VT>::__elem*> FibonacciHeap_IMP<KV, VT>::CheckedHolder;
#endif // FIBONACCIHEAP_DEBUG

template<typename KV, typename VT>
std::ostream& operator<<(std::ostream& os, FibonacciHeap_IMP<KV, VT>& bh) //{
{
    os << "[";
    while(!bh.empty())
        os << bh.GetValueType(bh.ExtractMin()) << " ";
    os << '\b';
    os << "]";
    return os;
} //}

template<typename K, typename V>
class FibonacciHeapKV: public FibonacciHeap_IMP<std::tuple<K, V, void*>*, K> //{
{
    public:
        using KeyValueType = typename FibonacciHeap_IMP<std::tuple<K, V, void*>*, K>::KeyValueType;
        using NewValueType = typename FibonacciHeap_IMP<std::tuple<K, V, void*>*, K>::NewValueType;
        using ItemSize     = typename FibonacciHeap_IMP<std::tuple<K, V, void*>*, K>::ItemSize;
        using __elem       = typename FibonacciHeap_IMP<std::tuple<K, V, void*>*, K>::__elem;

    protected:
    bool kv_less(const KeyValueType& a, const KeyValueType& b) //{
    {
        return std::get<0>(*a) < std::get<0>(*b);
    } //}
    bool kv_equal(const KeyValueType& a, const KeyValueType& b) //{
    {
        return std::get<0>(*a) == std::get<0>(*b);
    } //}
    __elem*      GetPointer(const KeyValueType& e){return (__elem*)std::get<2>(*e);}
    void         SetPointer(KeyValueType& e, __elem* p){std::get<2>(*e) = p;}
    void         SetNewValue(KeyValueType& e, const NewValueType& v){std::get<0>(*e) = v;}

    std::vector<KeyValueType> m_extra_container;
    using FibonacciHeap_IMP<KeyValueType, NewValueType>::Add;

    public:
    FibonacciHeapKV(): FibonacciHeap_IMP<KeyValueType, NewValueType>(), m_extra_container(){}
    FibonacciHeapKV(FibonacciHeapKV&& fh): FibonacciHeapKV(){*this = std::move(fh);}
    FibonacciHeapKV& operator=(FibonacciHeapKV&& fh) //{
    {
        if(this == &fh) return *this;
        FibonacciHeap_IMP<KeyValueType, NewValueType>::operator=(std::move(fh));
        this->m_extra_container = std::move(fh.m_extra_container);
            return *this;
    } //}

    NewValueType GetValueType(KeyValueType e) {return std::get<0>(*e);}

    KeyValueType Add(const K& k, const V& v) //{
    {
        KeyValueType new_x = new std::tuple<K, V, void*>(k, v, nullptr);
        m_extra_container.push_back(new_x);
        this->Add(new_x);
        return new_x;
    } //}

    template<typename IteratorK_t, typename IteratorV_t, 
        std::enable_if_t<std::is_convertible<typename std::iterator_traits<IteratorK_t>::value_type, KeyValueType>::value, int> = 0,
        std::enable_if_t<std::is_convertible<typename std::iterator_traits<IteratorV_t>::value_type, NewValueType>::value, int> = 0>
    void Add(IteratorK_t _begin, IteratorK_t _end, IteratorV_t _Vbegin, IteratorV_t _Vend) //{
    {
        for(; _begin != _end && _Vbegin != _Vend; ++_begin){
            this->Add(*_begin, *_Vbegin);
        }
        return;
    } //}

    void UnionWith(FibonacciHeapKV& fh) //{
    {
        FibonacciHeap_IMP<KeyValueType, NewValueType>::UnionWith(fh);
        this->m_extra_container.insert(m_extra_container.end(), 
                std::make_move_iterator(fh.m_extra_container.begin()), 
                std::make_move_iterator(fh.m_extra_container.end()));
        fh.m_extra_container.resize(0);
        return;
    } //}

    ~FibonacciHeapKV(){for(auto bi : this->m_extra_container) delete bi;}
}; //}

template<typename K>
class FibonacciHeap: public FibonacciHeap_IMP<std::pair<K, void*>*, K> //{
{
    public:
        using KeyValueType = typename FibonacciHeap_IMP<std::pair<K, void*>*, K>::KeyValueType;
        using NewValueType = typename FibonacciHeap_IMP<std::pair<K, void*>*, K>::NewValueType;
        using ItemSize     = typename FibonacciHeap_IMP<std::pair<K, void*>*, K>::ItemSize;
        using __elem       = typename FibonacciHeap_IMP<std::pair<K, void*>*, K>::__elem;

    protected:
    bool kv_less(const KeyValueType& a, const KeyValueType& b) //{
    {
        return a->first < b->first;
    } //}
    bool kv_equal(const KeyValueType& a, const KeyValueType& b) //{
    {
        return a->first == b->first;
    } //}
    __elem*      GetPointer(const KeyValueType& e){return (__elem*)e->second;}
    void         SetPointer(KeyValueType& e, __elem* p){e->second = p;}
    void         SetNewValue(KeyValueType& e, const NewValueType& v){e->first = v;}

    std::vector<KeyValueType> m_extra_container;
    using FibonacciHeap_IMP<KeyValueType, NewValueType>::Add;

    public:
    NewValueType GetValueType(KeyValueType e) {return e->first;}

    FibonacciHeap(): FibonacciHeap_IMP<KeyValueType, NewValueType>(), m_extra_container(){}
    FibonacciHeap(FibonacciHeap&& fh): FibonacciHeap(){*this = std::move(fh);}
    FibonacciHeap& operator=(FibonacciHeap&& fh) //{
    {
        if(this == &fh) return *this;
        FibonacciHeap_IMP<KeyValueType, NewValueType>::operator=(*this, std::move(fh));
        this->m_extra_container = std::move(fh.m_extra_container);
        return *this;
    } //}

    KeyValueType Add(const K& kv)//{
    {
        KeyValueType new_x = new std::pair<K, void*>(kv, nullptr);
        m_extra_container.push_back(new_x);
        this->Add(new_x);
        return new_x;
    } //}
    template<typename Iterator_t, std::enable_if_t<std::is_convertible<typename std::iterator_traits<Iterator_t>::value_type, K>::value, int> = 0>
    void Add(Iterator_t _begin, Iterator_t _end) //{
    {
        for(; _begin != _end; ++_begin){
            this->Add(*_begin);
        }
        return;
    } //}

    void UnionWith(FibonacciHeap& fh) //{
    {
        FibonacciHeap_IMP<KeyValueType, NewValueType>::UnionWith(fh);
        this->m_extra_container.insert(m_extra_container.end(), 
                std::make_move_iterator(fh.m_extra_container.begin()), 
                std::make_move_iterator(fh.m_extra_container.end()));
        fh.m_extra_container.resize(0);
        return;
    } //}

    ~FibonacciHeap(){for(auto bi : this->m_extra_container) delete bi;}
}; //}

template class FibonacciHeapKV<double, int>;

#endif // FIBONACCIHEAP_HPP_
