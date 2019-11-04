#ifndef DISJOINTSET_HPP_
#define DISJOINTSET_HPP_
#include <type_traits>
#include <vector>
#include <random>
#include <limits>
#include <iostream>

#include <cstdlib>
#include <cassert>

struct xxx
{
    void* m_ref;
    int   m_val;
};

template<typename T>
class DisjointSetForest_IMP //{
{
    static_assert(std::is_pointer<T>::value, "T should be pointer type");
    static_assert(std::is_class<std::remove_pointer_t<T>>::value, "T should be pointer to a non-atom object");
    public:
        typedef T      ElemType;
        typedef size_t ItemSize;

    protected:
        struct __elem;

        ItemSize             m_elem_counter;
        long int             m_guid;
        std::vector<__elem*> m_allocated;
        long int m_vtable;

        struct __elem //{
        {
            private:
            __elem*  m_next;
            __elem*  m_parent;
            __elem*  m_last;
            ElemType m_obj;
            ItemSize m_rank;
            long int m_guid;
            friend class DisjointSetForest_IMP;
            friend class iterator;
            __elem(const ElemType& e): m_next(nullptr), m_parent(this), m_last(this), m_obj(e), m_rank(1){}
            ~__elem(){}
            public:
            ElemType GetObj()   {return this->m_obj;}
            ElemType operator*(){return this->m_obj;}
        }; //}
        class iterator //{
        {
            public:
                typedef ElemType                  value_type;
                typedef value_type&               reference;
                typedef value_type*               pointer;
                typedef size_t                    difference_type;
                typedef std::forward_iterator_tag iterator_category;
            private:
                __elem* m_current;
            public:
                iterator(__elem* pp): m_current(pp){}
                bool operator==(const iterator& _oth){return this->m_current == _oth.m_current;}
                bool operator!=(const iterator& _oth){return !((*this) == _oth);}
                ElemType  operator*(){return m_current->m_obj;}
                iterator& operator++(){assert(m_current != nullptr); m_current = m_current->m_next; return *this;}
                iterator  operator++(int){iterator ret = *this; ++ret; return ret;}
                iterator  operator+(int dis){assert(dis >= 0); iterator ret = *this; for(; dis>0; --dis, ++ret){} return ret;}
        }; //}

        virtual __elem* GetPointer(const ElemType&)    = 0;
        virtual void    SetPointer(ElemType&, __elem*) = 0;

        DisjointSetForest_IMP(): m_elem_counter(0) //{
        {
            std::random_device rd;
            std::mt19937 gen(rd());
            std::uniform_int_distribution<long int> dis(0, std::numeric_limits<long int>::max());
            this->m_guid = dis(gen);
            return;
        } //}
        __elem* __MakeSet(ElemType& e) //{
        {
            __elem* ret = new __elem(e);
            ++this->m_elem_counter;
            this->SetPointer(e, ret);
            ret->m_guid = this->m_guid;
            this->m_allocated.push_back(ret);
            return ret;
        } //}
        void    __UnionWith(__elem* s1, __elem* s2) //{ union with rank
        {
            assert(s1 != nullptr);
            assert(s2 != nullptr);
            assert(s1->m_guid == this->m_guid);
            assert(s2->m_guid == this->m_guid);
            __elem *big, *small;
            for(;s1->m_parent != s1; s1=s1->m_parent);
            for(;s2->m_parent != s2; s2=s2->m_parent);
            if(s1 == s2) return;
            if(s1->m_rank >= s2->m_rank) {
                big = s1; small = s2;
            } else {
                big = s2; small = s1;
            }
            small->m_parent = big;
            assert(big->m_last->m_next == nullptr);
            big->m_last->m_next = small;
            big->m_last = small->m_last;
            small->m_last = nullptr; // ???
            big->m_rank += small->m_rank;
            return;
        } //}
        __elem* __FindSet(__elem* s1) //{ path compression
        {
            assert(s1 != nullptr);
            assert(s1->m_guid == this->m_guid);
            std::vector<__elem*> _m_e;
            for(;s1 != s1->m_parent; s1=s1->m_parent)
                _m_e.push_back(s1);
            for(auto bi : _m_e)
                bi->m_parent = s1;
            return s1;
        } //}
        iterator __begin(__elem* ss){ss = __FindSet(ss); return iterator(ss);}
        iterator __end  (__elem*)  {return iterator(nullptr);}


    public:
        void MakeSet(ElemType  e){this->__MakeSet(e); return;}
        void UnionWith(ElemType  e1, ElemType  e2){this->__UnionWith(this->GetPointer(e1), this->GetPointer(e2));}
        long int FindSet(const ElemType& e){return (long int)__FindSet(this->GetPointer(e));}

        iterator begin(const ElemType& e){return __begin(this->GetPointer(e));}
        iterator end  (const ElemType& e){return __end  (this->GetPointer(e));}

        void FreeElems() //{
        {
            for(auto bi = m_allocated.begin(); bi != m_allocated.end(); ++bi){
                SetPointer((*bi)->m_obj, nullptr);
                delete *bi;
            }
            this->m_elem_counter = 0;
        } //}

        virtual ~DisjointSetForest_IMP(){}
        bool empty(){return this->m_elem_counter == 0;}
}; //}

class DisjointSetForestXXX: public DisjointSetForest_IMP<xxx*>
{
    using elem = DisjointSetForest_IMP<xxx*>::__elem;
    using ElemType = DisjointSetForest_IMP<xxx*>::ElemType;
    protected:
        elem* GetPointer(const ElemType& e){return (elem*)e->m_ref;}
        void  SetPointer(ElemType& e, elem* p){e->m_ref = p;}
    public:
        ~DisjointSetForestXXX(){this->FreeElems();}
};

#endif // DISJOINTSET_HPP_
