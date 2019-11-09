#ifndef _AVLTREE_HPP_
#define _AVLTREE_HPP_

#include <vector>
#include <tuple>
#include <iterator>
#include <stdexcept>

#include <cassert>

namespace ANNA {

template<typename KV, typename K, typename V>
class AVLTree_IMP //{
{
    public:
        typedef KV     KeyValuePair;
        typedef K      KeyType;
        typedef V      ValueType;
        typedef size_t ItemSize;

        virtual bool           k_less  (const KeyValuePair&, const KeyType&) = 0;
        virtual bool           k_equal (const KeyValuePair&, const KeyType&) = 0;
        virtual ValueType&     GetValueRef(KeyValuePair&) = 0;
        virtual const KeyType& GetKeyRef(const KeyValuePair&) = 0;
        virtual bool           kv_less (const KeyValuePair& a1, const KeyValuePair& a2) //{
        {
            auto b2 = this->GetKeyRef(a2);
            return this->k_less(a1, b2);
        } //}
        virtual bool           kv_equal(const KeyValuePair& a1, const KeyValuePair& a2) //{
        {
            auto b2 = this->GetKeyRef(a2);
            return this->k_equal(a1, b2);
        } //}

    protected:
        struct avltree_node;
        static size_t avltree_height(avltree_node* t){if(t == nullptr) return 0; return t->m_height;}
        static void fix_height(avltree_node* t)
        {
            t->m_height = (avltree_height(t->m_left_child) > avltree_height(t->m_right_child)) ? 
                avltree_height(t->m_left_child) + 1 : 
                avltree_height(t->m_right_child) + 1;
        }
        struct avltree_node //{
        {
            avltree_node* m_left_child;
            avltree_node* m_right_child;
            avltree_node* m_parent;
            size_t        m_height;
            bool          m_is_deleted;
            KeyValuePair  m_keyval;
            size_t        m_iter_generation;
            avltree_node(): m_left_child(nullptr), m_right_child(nullptr), m_parent(nullptr),
                           m_height(1), m_is_deleted(false), m_keyval(), m_iter_generation(0){}
            avltree_node(const KeyValuePair& kv): 
                           m_left_child(nullptr), m_right_child(nullptr), m_parent(nullptr),
                           m_height(1), m_is_deleted(false), m_keyval(kv), m_iter_generation(0){}
            ~avltree_node() {
                if(this->m_left_child  != nullptr) delete this->m_left_child;
                if(this->m_right_child != nullptr) delete this->m_right_child;
            }

            size_t depth() //{
            {
                size_t left_depth  = 0;
                size_t right_depth = 0;
                if(this->m_left_child  != nullptr) left_depth  = this->m_left_child->depth();
                if(this->m_right_child != nullptr) right_depth = this->m_right_child->depth();
                if(left_depth == 0 && right_depth == 0)
                    return 1;
                if(left_depth < right_depth) return right_depth + 1;
                return left_depth + 1;
            } //}

            bool match_HLHL() //{
            {
                size_t lh = avltree_height(this->m_left_child);
                size_t rh = avltree_height(this->m_right_child);
                if(rh >= lh) return false; if(lh - rh == 1) return false;
                size_t llh = avltree_height(this->m_left_child->m_left_child);
                size_t lrh = avltree_height(this->m_left_child->m_right_child);
                if(lrh <= llh) return true;
                return false;
            } //}
        avltree_node* rotate_hlhl() //{
        {
            avltree_node* parent = this->m_parent;
            avltree_node /* *A = this, */          *B = this->m_left_child,
                         *C __attribute__((unused)) = this->m_right_child, *D __attribute__((unused)) = B->m_left_child,
                         *E = B->m_right_child;
            assert(avltree_height(B) - avltree_height(C) == 2);
            if(parent != nullptr) {
                if(parent->m_left_child  == this) parent->m_left_child  = B;
                if(parent->m_right_child == this) parent->m_right_child = B;
                B->m_parent = parent;
            } else B->m_parent = nullptr;
            B->m_right_child = this; this->m_parent = B;
            this->m_left_child = E; if(E != nullptr) E->m_parent = this;
            this->m_height = avltree_height(E) + 1;
            B->m_height = avltree_height(this) + 1;
            return B;
        } //}
            bool match_HLHR() //{
            {
                size_t lh = avltree_height(this->m_left_child);
                size_t rh = avltree_height(this->m_right_child);
                if(rh >= lh) return false; if(lh - rh == 1) return false;
                size_t llh = avltree_height(this->m_left_child->m_left_child);
                size_t lrh = avltree_height(this->m_left_child->m_right_child);
                if(lrh <= llh) return false;
                return true;
            } //}
        avltree_node* rotate_hlhr() //{
        {
            avltree_node* parent = this->m_parent;
            avltree_node /* *A = this, */          *B = this->m_left_child,
                         *C __attribute__((unused)) = this->m_right_child, *D __attribute__((unused)) = B->m_left_child,
                         *E = B->m_right_child,
                         *F = E->m_left_child, *G = E->m_right_child;
            assert(avltree_height(B) - avltree_height(C) == 2);
            assert(avltree_height(E) - avltree_height(D) == 1);
            if(parent != nullptr) {
                if(parent->m_left_child  == this) parent->m_left_child  = E;
                if(parent->m_right_child == this) parent->m_right_child = E;
                E->m_parent = parent;
            } else E->m_parent = nullptr;
            E->m_left_child  = B;    B->m_parent    = E;
            E->m_right_child = this; this->m_parent = E;
            B->m_right_child = F;    if(F != nullptr) F->m_parent    = B;
            this->m_left_child = G;  if(G != nullptr) G->m_parent    = this;
            // inner node A(this) B E
            this->m_height = avltree_height(this->m_right_child) > avltree_height(this->m_left_child) ?
                                avltree_height(this->m_right_child) + 1 : avltree_height(this->m_left_child) + 1;
            B->m_height    = avltree_height(B->m_right_child) > avltree_height(B->m_left_child) ?
                                avltree_height(B->m_right_child) + 1 : avltree_height(B->m_left_child) + 1;
            E->m_height    = avltree_height(E->m_right_child) > avltree_height(E->m_left_child) ?
                                avltree_height(E->m_right_child) + 1 : avltree_height(E->m_left_child) + 1;
            return E;
        } //}
            bool match_HRHR() //{
            {
                size_t rh = avltree_height(this->m_right_child);
                size_t lh = avltree_height(this->m_left_child);
                if(lh >= rh) return false; if(rh - lh == 1) return false;
                size_t rrh = avltree_height(this->m_right_child->m_right_child);
                size_t rlh = avltree_height(this->m_right_child->m_left_child);
                if(rlh <= rrh) return true;
                return false;
            } //}
        avltree_node* rotate_hrhr() //{
        {
            avltree_node* parent = this->m_parent;
            avltree_node /* *A = this, */         *B = this->m_right_child,
                         *C __attribute__((unused)) = this->m_left_child, *D __attribute__((unused)) = B->m_right_child,
                         *E = B->m_left_child;
            assert(avltree_height(B) - avltree_height(C) == 2);
            if(parent != nullptr) {
                if(parent->m_right_child  == this) parent->m_right_child  = B;
                if(parent->m_left_child == this) parent->m_left_child = B;
                B->m_parent = parent;
            } else B->m_parent = nullptr;
            B->m_left_child = this; this->m_parent = B;
            this->m_right_child = E;if(E != nullptr) E->m_parent = this;
            this->m_height = avltree_height(E) + 1;
            B->m_height    = avltree_height(this) + 1;
            return B;
        } //}
            bool match_HRHL() //{
            {
                size_t rh = avltree_height(this->m_right_child);
                size_t lh = avltree_height(this->m_left_child);
                if(lh >= rh) return false; if(rh - lh == 1) return false;
                size_t rrh = avltree_height(this->m_right_child->m_right_child);
                size_t rlh = avltree_height(this->m_right_child->m_left_child);
                if(rlh <= rrh) return false;
                return true;
            } //}
        avltree_node* rotate_hrhl() //{
        {
            avltree_node* parent = this->m_parent;
            avltree_node /* *A = this, */          *B = this->m_right_child,
                         *C __attribute__((unused)) = this->m_left_child, *D __attribute__((unused)) = B->m_right_child,
                         *E = B->m_left_child,
                         *F = E->m_right_child, *G = E->m_left_child;
            assert(avltree_height(B) - avltree_height(C) == 2);
            assert(avltree_height(E) - avltree_height(D) == 1);
            if(parent != nullptr) {
                if(parent->m_right_child  == this) parent->m_right_child  = E;
                if(parent->m_left_child == this) parent->m_left_child = E;
                E->m_parent = parent;
            } else E->m_parent = nullptr;
            E->m_right_child    = B;       B->m_parent = E;
            E->m_left_child     = this; this->m_parent = E;
            B->m_left_child     = F;    if(F != nullptr) F->m_parent = B;
            this->m_right_child = G;    if(G != nullptr) G->m_parent = this;
            // inner node A(this) B E
            this->m_height = avltree_height(this->m_right_child) > avltree_height(this->m_left_child) ?
                                avltree_height(this->m_right_child) + 1 : avltree_height(this->m_left_child) + 1;
            B->m_height    = avltree_height(B->m_right_child) > avltree_height(B->m_left_child) ?
                                avltree_height(B->m_right_child) + 1 : avltree_height(B->m_left_child) + 1;
            E->m_height    = avltree_height(E->m_right_child) > avltree_height(E->m_left_child) ?
                                avltree_height(E->m_right_child) + 1 : avltree_height(E->m_left_child) + 1;
            return E;
        } //}

        avltree_node* min_elem() //{
        {
            avltree_node* ret = this;
            while(ret->m_left_child != nullptr) ret = ret->m_left_child;
            return ret;
        } //}
        }; //}

    class Iterator //{
    {
        public:
            typedef size_t                  difference_type;
            typedef ValueType               value_type;
            typedef value_type*             pointer;
            typedef value_type&             reference;
            typedef std::input_iterator_tag iterator_category;

        private:
            size_t       m_iter_check;
            avltree_node* m_current_node;
            AVLTree_IMP*  m_delegate_obj;
        public:
            Iterator(avltree_node* bn, AVLTree_IMP* dobj, size_t check): 
                m_iter_check(check), 
                m_current_node(bn), 
                m_delegate_obj(dobj){}
            reference operator*(){return m_delegate_obj->GetValueRef(m_current_node->m_keyval);}
            Iterator&  operator++() //{
            {
                if(this->m_iter_check != m_delegate_obj->m_iter_validate)
                    throw *new std::logic_error("iterator is out of date");
                if(m_current_node == nullptr) throw *new std::runtime_error("iterator is in end");
                m_current_node->m_iter_generation = m_iter_check;
                if(m_current_node->m_right_child != nullptr) {
                    m_current_node = m_current_node->m_right_child->min_elem(); 
                    return *this;
                }
                while(m_current_node != nullptr && m_current_node->m_iter_generation == m_iter_check)
                    m_current_node = m_current_node->m_parent;
                return *this;
            } //}
            Iterator  operator++(int) //{
            {
                Iterator ret = *this;
                this->operator++();
                return ret;
            } //}
            bool operator==(const Iterator& _oth){return this->m_current_node == _oth.m_current_node;}
            bool operator!=(const Iterator& _oth){return !this->operator==(_oth);}
    }; //}

    avltree_node* m_root;
    ItemSize     m_size;
    ItemSize     m_deleted;
    size_t       m_iter_validate;

    std::pair<bool, avltree_node*> __find(const KeyType& k) //{
    {
        avltree_node* find__ = this->m_root;
        bool has_find       = false;
        while(find__ != nullptr) {
            if(this->k_equal(find__->m_keyval,k)) {
                if(find__->m_is_deleted == false)
                    has_find = true; 
                break;
            }
            if(!this->k_less(find__->m_keyval, k))
                find__ = find__->m_left_child;
            else 
                find__ = find__->m_right_child;
        }
        if(has_find == false) return std::make_pair(false, nullptr);
        return std::make_pair(true, find__);
    } //}
    void __fix_unbalance_bottom_up(avltree_node* bottom) //{
    {
        assert(bottom != nullptr);
        avltree_node* result = nullptr;
        while(bottom != nullptr) {
            result = nullptr;
            avltree_node* next = bottom->m_parent;
                 if(bottom->match_HLHL()) result = bottom->rotate_hlhl();
            else if(bottom->match_HRHR()) result = bottom->rotate_hrhr();
            else if(bottom->match_HRHL()) result = bottom->rotate_hrhl();
            else if(bottom->match_HLHR()) result = bottom->rotate_hlhr();
            fix_height(bottom);
            bottom = next;
        }
        if(result != nullptr) this->m_root = result;
    } //}

    public:
    std::pair<bool, ValueType*> Find(const KeyType& k) //{
    {
        auto fff = this->__find(k);
        if(fff.first == false) return std::make_pair(false, nullptr);
        return std::make_pair(true, &this->GetValueRef(fff.second->m_keyval));
    } //}
    std::pair<bool, ValueType*> Find(const KeyValuePair& kv) //{
    {
        const KeyType& kkk = this->GetKeyRef(kv);
        return this->Find(kkk);
    } //}

    bool Delete(const KeyType& k) //{
    {
        auto kkk = this->__find(k);
        if(kkk.first == false) return false;
        kkk.second->m_is_deleted = true;
        ++m_deleted;
        --m_size;
        ++this->m_iter_validate;
        return true;
    } //}
    bool Delete(const KeyValuePair& k) //{
    {
        return this->Delete(this->GetKeyRef(k));
    } //}
    bool empty() const {return m_size == 0;}
    size_t Depth() //{
    {
        if(this->m_root == nullptr) return 0;
        return this->m_root->m_height;
    } //}

    AVLTree_IMP(): m_root(nullptr), m_size(0), m_deleted(0), m_iter_validate(0){}
    ~AVLTree_IMP(){if(m_root != nullptr) delete m_root;}

    bool Insert(const KeyValuePair& kv) //{
    {
        avltree_node* current_node = this->m_root;
        avltree_node* current_node_parent = nullptr;
        bool is_left_insert = false;
        while(current_node != nullptr) {
            if(this->kv_equal(current_node->m_keyval, kv)) {
                if(!current_node->m_is_deleted) return false;
                ++this->m_size;
                ++this->m_iter_validate;
                current_node->m_keyval = kv; return true;
            }
            current_node_parent = current_node;
            if(this->kv_less(kv, current_node->m_keyval)) {
                current_node   = current_node->m_left_child;
                is_left_insert = true;
            } else {
                current_node   = current_node->m_right_child;
                is_left_insert = false;
            }
        }
        if(current_node_parent == nullptr) {
            this->m_root = new avltree_node(kv);
            ++this->m_size;
            return true;
        }
        avltree_node* new_node = new avltree_node(kv);
        if(is_left_insert)
            current_node_parent->m_left_child  = new_node;
        else
            current_node_parent->m_right_child = new_node;
        new_node->m_parent = current_node_parent;
        this->__fix_unbalance_bottom_up(new_node);
        ++this->m_size;
        ++this->m_iter_validate;
        return true;
    } //}

    Iterator begin(){
        ++m_iter_validate; 
        if(m_root != nullptr) 
            return Iterator(m_root->min_elem(), this, m_iter_validate); 
        return Iterator(nullptr, this, m_iter_validate);
    }
    Iterator end(){return Iterator(nullptr, nullptr, 0);}
}; //}

template<typename KT, typename VT>
class AVLTree: public AVLTree_IMP<std::pair<KT, VT>, KT, VT> //{
{
    public:
         using KeyValuePair = typename AVLTree_IMP<std::pair<KT, VT>, KT, VT>::KeyValuePair;
         using KeyType      = typename AVLTree_IMP<std::pair<KT, VT>, KT, VT>::KeyType;
         using ValueType    = typename AVLTree_IMP<std::pair<KT, VT>, KT, VT>::ValueType;
         using ItemSize     = typename AVLTree_IMP<std::pair<KT, VT>, KT, VT>::ItemSize;

    protected:
         bool           k_less  (const KeyValuePair& a1, const KeyType& a2) //{
         {
             return a1.first < a2;
         } //}
         bool           k_equal (const KeyValuePair& a1, const KeyType& a2) //{
         {
             return a1.first == a2;
         } //}
         ValueType&     GetValueRef(KeyValuePair& a1) //{
         {
             return a1.second;
         } //}
         const KeyType& GetKeyRef(const KeyValuePair& a1) //{
         {
             return a1.first;
         } //}

    public:
        bool Insert(const KeyType& k, const ValueType& v) //{
        {
            return AVLTree_IMP<std::pair<KeyType, ValueType>, KeyType, ValueType>::Insert(std::make_pair(k, v));
        } //}
}; //}

template class AVLTree<int, double>;
}

#endif // _AVLTREE_HPP_
