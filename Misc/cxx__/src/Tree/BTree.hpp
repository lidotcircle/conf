#ifndef BTREE_HPP_
#define BTREE_HPP_

#include <vector>
#include <tuple>
#include <iostream>
#include <iomanip>
#include <memory>
#include <type_traits>

#include <cstdlib>
#include <cassert>
#include <cstring>

#ifndef __BTREE_DEBUG
#define __BTREE_DEBUG 1
#endif // __BTREE_DEBUG

#ifdef __BTREE_DEBUG
size_t dbg_search_fail    = 0;
size_t dbg_search_success = 0;
size_t dbg_search_try     = 0;
size_t dbg_insert_fail    = 0;
size_t dbg_insert_try     = 0;
size_t dbg_insert_success = 0;
size_t dbg_delete_fail    = 0;
size_t dbg_delete_try     = 0;
size_t dbg_delete_success = 0;
size_t dbg_free_call      = 0;
size_t dbg_new_node_call  = 0;

inline void print_statistic() //{
{
    std::cout << "Search try:     " << dbg_search_try     << std::endl;
    std::cout << "Search success: " << dbg_search_success << std::endl;
    std::cout << "Search fail:    " << dbg_search_fail    << std::endl;
    std::cout << "Insert try:     " << dbg_insert_try     << std::endl;
    std::cout << "Insert success: " << dbg_insert_success << std::endl;
    std::cout << "Insert fail:    " << dbg_insert_fail    << std::endl;
    std::cout << "Delete try:     " << dbg_delete_try     << std::endl;
    std::cout << "Delete success: " << dbg_delete_success << std::endl;
    std::cout << "Delete fail:    " << dbg_delete_fail    << std::endl;
    std::cout << "new node call:  " << dbg_new_node_call  << std::endl;
    std::cout << "FreeMM call:    " << dbg_free_call      << std::endl;
} //}
#endif // __BTREE_DEBUG

template<typename K, typename V, typename ID, typename KV, size_t Vt, typename BTN> class BTree_IMP; 

template<typename K, typename V, typename ID, typename KV, size_t Vt>
class BTreeNode_IMP //{
{
    static_assert(Vt >= 2, "t should greater than or equal 2 in BTree");
    public:
        typedef K              key_type;
        typedef V              val_type;
        typedef ID             id_type;
        typedef KV             kv_type;
        typedef size_t         size_type;
        typedef BTreeNode_IMP* btn_pointer;

    protected:
        virtual val_type&   GetValue(kv_type&) const = 0;
        virtual key_type&   GetKey(kv_type&) const = 0;
        virtual btn_pointer GetNode(id_type) const = 0;
        virtual bool        k_less (const key_type&, const key_type&) const = 0;
        virtual bool        k_equal(const key_type&, const key_type&) const = 0;
        virtual bool        parent_null(id_type) const = 0;
        virtual id_type     null_parent() const = 0;
        virtual void        setup_node() = 0; // building information about id, write node to disk, balabala
        virtual void        free_node() = 0; // release this node
        virtual btn_pointer v_new_node() const = 0;
        virtual bool        need_write_back(id_type) = 0;
        virtual void        write_back() = 0;

        const val_type& GetValue(const kv_type& a) const         {return const_cast<btn_pointer>(this)->GetValue(const_cast<kv_type&>(a));}
        const key_type& GetKey(const kv_type& a) const           {return const_cast<btn_pointer>(this)->GetKey  (const_cast<kv_type&>(a));}
        bool kv_less (const kv_type& a, const key_type& b) const {return this->k_less  (this->GetKey(a), b);}
        bool kv_equal(const kv_type& a, const key_type& b) const {return this->k_equal (this->GetKey(a), b);}
        bool kv_less (const key_type& a, const kv_type& b) const {return this->k_less  (a, this->GetKey(b));}
        bool kv_equal(const key_type& a, const kv_type& b) const {return this->k_equal (a, this->GetKey(b));}
        bool kv_less (const kv_type& a, const kv_type& b) const  {return this->k_less  (this->GetKey(a), this->GetKey(b));}
        bool kv_equal(const kv_type& a, const kv_type& b) const  {return this->k_equal (this->GetKey(a), this->GetKey(b));}

        btn_pointer new_node(){++dbg_new_node_call; return this->v_new_node();}

    protected:
        class btree_iterator //{
        {
            public:
                typedef size_t                  difference_type;
                typedef kv_type                 value_type;
                typedef value_type*             pointer;
                typedef value_type&             reference;
                typedef std::input_iterator_tag iterator_category;

            private:
                btn_pointer   m_btree_node;
                btn_pointer   m_top_node;
                size_type m_where; // from 0 to ...

            public:
                reference       operator*(){return this->m_btree_node->m_keyvals[m_where];}
                btree_iterator& operator++() //{
                {
                    if(this->m_btree_node == nullptr) throw *new std::runtime_error("unexcepted iterator");
                    if(this->m_btree_node->IsLeaf()) {
                        if(m_where < this->m_btree_node->m_num_of_children - 2) {
                            ++m_where; return *this;
                        } else {
                            ++m_where;
                            while(this->m_btree_node->m_num_of_children- 1 == this->m_where) {
                                if(this->m_btree_node == this->m_top_node) {
                                    this->m_btree_node = nullptr; 
                                    this->m_where = 0; 
                                    return *this;
                                }
                                btn_pointer parent = m_btree_node->GetNode(m_btree_node->m_parent);
                                size_type i;
                                for(i = 0; i<=parent->m_num_of_children - 2; ++i)
                                    if(parent->m_children[i] == m_btree_node->m_this_id) break;
                                assert(i <= parent->m_num_of_children);
                                m_where = i;
                                m_btree_node = parent;
                            }
                            return *this;
                        }
                    }
                    ++m_where;
                    while(!m_btree_node->IsLeaf()) {
                        m_btree_node = m_btree_node->GetNode(m_btree_node->m_children[m_where]);
                        m_where = 0;
                    }
                    return *this;
                } //}
                btree_iterator  operator++(int){btree_iterator ret = *this; this->operator++(); return ret;}

                bool operator==(const btree_iterator& oth) {return (this->m_btree_node == oth.m_btree_node && this->m_where == this->m_where);}
                bool operator!=(const btree_iterator& oth) {return !this->operator==(oth);}

                btree_iterator(btn_pointer node): m_btree_node(node), m_top_node(node), m_where(0)
                {
                    if(m_btree_node == nullptr) return;
                    while(!m_btree_node->IsLeaf())
                        m_btree_node = m_btree_node->GetNode(m_btree_node->m_children[0]);
                }
        }; //}
        friend class btree_iterator;

    protected:
        template<typename Kf, typename Vf, typename IDf, typename KVf, size_t Vtf, typename BTNf>
        friend class BTree_IMP;
        id_type   m_children[2 * Vt];
        kv_type   m_keyvals[2 * Vt - 1];
        size_type m_num_of_children;
        bool      m_is_leaf;
        id_type   m_parent;
        id_type   m_this_id;

        std::pair<BTreeNode_IMP*, size_type> __search(const key_type& key) //{
        {
#ifdef __BTREE_DEBUG
            ++dbg_search_try;
#endif // __BTREE_DEBUG
            BTreeNode_IMP* search_node = this;
            size_type  where = 0;
            for(;;) {
                if(kv_equal(search_node->m_keyvals[where], key)) {
#ifdef __BTREE_DEBUG
                    ++dbg_search_success;
#endif // __BTREE_DEBUG
                    return std::make_pair(search_node, where);
                }
                if(kv_less(key, search_node->m_keyvals[where])) {
                    if(search_node->IsLeaf()) {
#ifdef __BTREE_DEBUG
                        ++dbg_search_fail;
#endif // __BTREE_DEBUG
                        return std::make_pair(nullptr, size_type(0));
                    }
                    search_node = this->GetNode(search_node->m_children[where]);
                    where = 0;
                    continue;
                }
                if(where == search_node->m_num_of_children - 2) {
                    if(search_node->IsLeaf()) {
#ifdef __BTREE_DEBUG
                        ++dbg_search_fail;
#endif // __BTREE_DEBUG
                        return std::make_pair(nullptr, size_type(0));
                    }
                    search_node = this->GetNode(search_node->m_children[where + 1]);
                    where = 0;
                    continue;
                }
                ++where;
            }
        } //}
        void children_move_right_one(size_type start) // @start is in range of [0, Vt * 2 - 1] //{
        {
            assert(start < this->m_num_of_children);
            assert(this->m_num_of_children != Vt * 2 && this->m_num_of_children >= 2);
            for(size_type i = this->m_num_of_children - 1; i >= start && i < this->m_num_of_children; --i)
                this->m_children[i + 1] = std::move(this->m_children[i]);
        } //}
        void keyval_move_right_one(size_type start) // above //{
        {
            assert(start < this->m_num_of_children);
            assert(this->m_num_of_children != Vt * 2 && this->m_num_of_children >= 2);
            for(size_type i = this->m_num_of_children - 2; i >= start && i < this->m_num_of_children; --i)
                this->m_keyvals[i + 1] = std::move(this->m_keyvals[i]);
        } //}
        void keyval_move_left_one (size_type start) // above //{
        {
            assert(start < this->m_num_of_children && start > 0 && this->m_num_of_children >= 3);
            assert(this->m_num_of_children >= 2);
//          assert(this->m_num_of_children != Vt * 2); ???
            for(size_type i = start; i <= this->m_num_of_children - 2; ++i)
                this->m_keyvals[i - 1] = std::move(this->m_keyvals[i]);
        } //}
        void children_move_left_one (size_type start) // above //{
        {
            assert(start < this->m_num_of_children && start > 0 && this->m_num_of_children >= 3);
            assert(this->m_num_of_children >= 2);
//          assert(this->m_num_of_children != Vt * 2); ???
            for(size_type i = start; i <= this->m_num_of_children - 1; ++i)
                this->m_children[i - 1] = std::move(this->m_children[i]);
        } //}
        void __split_full_node(btn_pointer* root) //{
        {
            assert(this->m_num_of_children == 2 * Vt);
            BTreeNode_IMP* parent;
            if(this->parent_null(this->m_parent)) { // root node HERE
                parent = this->new_node();
                parent->setup_node();
                parent->m_is_leaf = false;
                parent->m_parent = this->null_parent();
                *root = parent;
            } else parent = this->GetNode(this->m_parent);

            BTreeNode_IMP* new_sibling = this->new_node();
            new_sibling->setup_node();
            this->m_parent        = parent->m_this_id;
            new_sibling->m_parent = parent->m_this_id;
            if(this->IsLeaf())
                new_sibling->m_is_leaf = true;
            else 
                new_sibling->m_is_leaf = false;

            new_sibling->m_num_of_children = Vt;
            this->m_num_of_children        = Vt;
            for(size_type i = 0, j = Vt; i < Vt - 1; ++i, ++j) {
                new_sibling->m_keyvals[i] = std::move(this->m_keyvals[j]);
                if(!this->IsLeaf()) {
                    new_sibling->m_children[i] = std::move(this->m_children[j]);
                }
            }
            if(!this->IsLeaf()) {
                new_sibling->m_children[Vt - 1] = std::move(this->m_children[2 * Vt - 1]);
                for(size_type i = 0; i <= Vt - 1; ++i) {
                    btn_pointer ah = this->GetNode(new_sibling->m_children[i]);
                    ah->m_parent = new_sibling->m_this_id;
                }
            }
            if(parent->m_num_of_children == 0) {
                parent->m_num_of_children = 2;
                parent->m_keyvals[0]      = std::move(this->m_keyvals[Vt - 1]);
                parent->m_children[0]     = this->m_this_id;
                parent->m_children[1]     = new_sibling->m_this_id;
                return;
            }
            assert(parent->m_num_of_children != 2 * Vt);
            size_type where_this = 0;
            for(;where_this < parent->m_num_of_children; ++where_this)
                if(parent->m_children[where_this] == this->m_this_id) break;
            assert(parent->m_children[where_this] == this->m_this_id);
            if(where_this == parent->m_num_of_children - 1) {
                parent->m_children[parent->m_num_of_children] = new_sibling->m_this_id;
                parent->m_keyvals[parent->m_num_of_children - 1] = std::move(this->m_keyvals[Vt - 1]);
                ++parent->m_num_of_children;
                return;
            }
            parent->keyval_move_right_one(where_this);
            parent->m_keyvals[where_this] = std::move(this->m_keyvals[Vt - 1]);
            ++where_this;
            parent->children_move_right_one(where_this);
            parent->m_children[where_this] = new_sibling->m_this_id;
            ++parent->m_num_of_children;
            return;
        } //}
        bool __insert(const kv_type& what, const key_type& key, btn_pointer* root) //{
        {
            size_type insert_place = size_type(0);
            if(this->m_num_of_children == 0){++this->m_num_of_children; goto insert_first;}
            for(; insert_place < this->m_num_of_children - 1; ++insert_place) {
                if(this->kv_equal(this->m_keyvals[insert_place], key)) return false; // duplicated key
                if(this->kv_less(key, this->m_keyvals[insert_place])) break;
            }
            if(insert_place > this->m_num_of_children - 2) {
                assert(this->m_num_of_children == insert_place + 1);
                if(!this->IsLeaf()) {
                    BTreeNode_IMP* next_insert = this->GetNode(this->m_children[insert_place]);
                    if(next_insert->IsFull()) {
                        next_insert->__split_full_node(root);
                        if(this->kv_equal(this->m_keyvals[insert_place], key)) return false;
                        if(this->kv_less(this->m_keyvals[insert_place], key))
                            next_insert = this->GetNode(this->m_children[insert_place + 1]);
                    }
                    return next_insert->__insert(what, key, root);
                }
                this->m_keyvals[this->m_num_of_children - 1] = what;
                ++this->m_num_of_children;
                return true;
            }
            if(!this->IsLeaf()) {
                BTreeNode_IMP* next_insert = this->GetNode(this->m_children[insert_place]);
                if(next_insert->IsFull()) {
                    next_insert->__split_full_node(root);
                    if(this->kv_equal(what, this->m_keyvals[insert_place])) return false;
                    if(!this->kv_less(what, this->m_keyvals[insert_place]))
                        next_insert = this->GetNode(this->m_children[insert_place + 1]);
                }
                return next_insert->__insert(what, key, root);
            }
            this->keyval_move_right_one(insert_place);
insert_first:
            this->m_keyvals[insert_place] = what;
            ++this->m_num_of_children;
            return true;
        } //}
        static btn_pointer __merge(BTreeNode_IMP* parent, BTreeNode_IMP* left, BTreeNode_IMP* right, size_type where, btn_pointer* root) //{
        {
            assert(parent->GetNode(parent->m_children[where]) == left);
            assert(parent->GetNode(parent->m_children[where + 1]) == right);
            assert(left->m_num_of_children == Vt && right->m_num_of_children == Vt);
            assert(parent->m_num_of_children > Vt || parent->parent_null(parent->m_parent));
            for(size_type i = Vt; i <= 2 * Vt - 2; ++i) {
                left->m_keyvals[i] = std::move(right->m_keyvals[i - Vt]);
                if(!left->IsLeaf())left->m_children[i] = std::move(right->m_children[i - Vt]);
            }
            if(!left->IsLeaf()) left->m_children[2 * Vt - 1] = std::move(right->m_children[Vt - 1]);
            if(!left->IsLeaf()) {
                for(size_type i = Vt; i<=(2 * Vt - 1); ++i) {
                    btn_pointer ahp = parent->GetNode(left->m_children[i]);
                    ahp->m_parent   = left->m_this_id;
                }
            }
            left ->m_num_of_children = 2 * Vt;
            right->m_num_of_children = 0;
            right->free_node();
            delete right;
            left->m_keyvals[Vt - 1] = std::move(parent->m_keyvals[where]);
            if(parent->m_num_of_children == 2) { // new root HERE
                parent->m_num_of_children = 0;
                parent->free_node();
                delete parent;
                left->m_parent = left->null_parent();
                *root = left;
                return left;
            }
            if(where != parent->m_num_of_children - 2) {
                parent->keyval_move_left_one(where + 1);
                parent->children_move_left_one(where + 2);
            }
            --parent->m_num_of_children;
            return left;
        } //}
        btn_pointer __ensure_at_t_keyval(btn_pointer* root) //{
        {
            if(this->m_num_of_children >= Vt + 1) return this;
            assert(!this->parent_null(this->m_parent));
            BTreeNode_IMP* parent = this->GetNode(this->m_parent);
            size_type where = size_type(0);
            for(;where < parent->m_num_of_children; ++where)
                if(parent->m_children[where] == this->m_this_id) break;
            assert(parent->m_children[where] == this->m_this_id);
            BTreeNode_IMP *left_sibling = nullptr, *right_sibling=nullptr;
            if(where != 0) left_sibling  = this->GetNode(parent->m_children[where - 1]);
            if(left_sibling != nullptr) {
                if(left_sibling->m_num_of_children == Vt) // merge left and this
                    return __merge(parent, left_sibling, this, where - 1, root);
                /* move max keyval from left to this*/
                this->keyval_move_right_one(0);
                if(!this->IsLeaf()) this->children_move_right_one(0);
                this->m_keyvals[0] = std::move(parent->m_keyvals[where - 1]);
                if(!this->IsLeaf()) {
                    btn_pointer trans   = this->GetNode(left_sibling->m_children[left_sibling->m_num_of_children - 1]);
                    this->m_children[0] = std::move(left_sibling->m_children[left_sibling->m_num_of_children - 1]);
                    trans->m_parent     = this->m_this_id;
                }
                ++this->m_num_of_children;
                parent->m_keyvals[where - 1] = std::move(left_sibling->m_keyvals[left_sibling->m_num_of_children - 2]);
                --left_sibling->m_num_of_children;
                return this;
            }
            if(where <  parent->m_num_of_children - 1) right_sibling = this->GetNode(parent->m_children[where + 1]);
            assert(right_sibling != nullptr);
            if(right_sibling->m_num_of_children == Vt)
                return __merge(parent, this, right_sibling, where, root);
            /* move min keyval from right to this */
            if(!this->IsLeaf()) {
                btn_pointer trans   = this->GetNode(right_sibling->m_children[0]);
                this->m_children[this->m_num_of_children] = std::move(right_sibling->m_children[0]);
                trans->m_parent     = this->m_this_id;
            }
            this->m_keyvals[this->m_num_of_children - 1] = std::move(parent->m_keyvals[where]);
            ++this->m_num_of_children;
            parent->m_keyvals[where] = std::move(right_sibling->m_keyvals[0]);
            right_sibling->keyval_move_left_one(1);
            if(!right_sibling->IsLeaf()) right_sibling->children_move_left_one(1);
            --right_sibling->m_num_of_children;
            return this;
        } //}
         void __delete(size_type where, btn_pointer* root) //{
         {
             assert(where <= this->m_num_of_children - 2);
             BTreeNode_IMP* target = this;
             key_type test_key = this->GetKey(this->m_keyvals[0]);
             if(!this->parent_null(this->m_parent)) target = this->__ensure_at_t_keyval(root);
             if(!target->kv_equal(target->m_keyvals[0], test_key)) {
                 if(target->kv_equal(target->m_keyvals[1], test_key))
                     ++where;
                 else
                     where += Vt;
             }
             if(!target->IsLeaf()) {
                 BTreeNode_IMP *left_child = target->GetNode(target->m_children[where]); // find the max
                 while(!left_child->IsLeaf())
                     left_child = target->GetNode(left_child->m_children[left_child->m_num_of_children - 1]);
                 std::swap(target->m_keyvals[where], left_child->m_keyvals[left_child->m_num_of_children - 2]);
                 bool call_result = left_child->__delete_aux(std::make_pair(left_child, left_child->m_num_of_children - 2), root);
                 (void)call_result;
                 assert(call_result);
                 return;
             }
             /* Leaf node */
             if(where != target->m_num_of_children - 2)
                 target->keyval_move_left_one(where + 1);
             --target->m_num_of_children;
         } //}
        bool __delete_aux(const std::pair<btn_pointer, size_type>& result, btn_pointer* root) //{
        {
            // If @result.first has parent node, ensuring the parent node has at least k key-value-pair
            std::vector<btn_pointer> ensure_list;
            if(!this->parent_null(result.first->m_parent)) {
                btn_pointer parent = this->GetNode(result.first->m_parent);
                while(parent->m_num_of_children <= Vt) {
                    if(this->parent_null(parent->m_parent)) break;
                    ensure_list.push_back(parent);
                    parent = this->GetNode(parent->m_parent);
                }
                for(auto bi = ensure_list.rbegin(); bi != ensure_list.rend(); ++bi)
                    (*bi)->__ensure_at_t_keyval(root);
            }
            result.first->__delete(result.second, root);
            return true;
        } //}

    public:
        BTreeNode_IMP(): m_children(), m_keyvals(), m_num_of_children(0), m_is_leaf(true), m_parent(), m_this_id(){}
        ~BTreeNode_IMP(){}
        void recursive_free_node() //{
        {
#ifdef __BTREE_DEBUG
            ++dbg_free_call;
#endif // __BTREE_DEBUG
            if(!this->IsLeaf()) {
                for(size_type i = 0; i<=this->m_num_of_children - 1; ++i) {
                    if(this->need_write_back(this->m_children[i])) {
                        btn_pointer subnode = this->GetNode(this->m_children[i]);
                        subnode->recursive_free_node();
                    }
                }
            }
            this->write_back();
        } //}

        void    SetupNode(){this->setup_node();}
        id_type Parent(){return this->m_parent;}
        bool    IsLeaf(){return this->m_is_leaf;}
        bool    IsFull(){return this->m_num_of_children == Vt * 2;}

        val_type* Search(const key_type& key) //{
        {
            assert(this->parent_null(this->m_parent)); // ensure this node is root
            std::pair<BTreeNode_IMP*, size_type> result = this->__search(key);
            if(result.first == nullptr) return nullptr;
            return &this->GetValue(result.first->m_keyvals[result.second]);
        } //}
        bool Delete(const key_type& key, btn_pointer* root) //{
        {
#ifdef __BTREE_DEBUG
            ++dbg_delete_try;
#endif // __BTREE_DEBUG
            assert(this->parent_null(this->m_parent));
            *root = this;
            std::pair<btn_pointer, size_type> result = this->__search(key);
            if(result.first == nullptr) return false;
            bool s_or_f = this->__delete_aux(result, root);
            if((*root)->m_num_of_children < 2) {
                assert((*root)->IsLeaf() && (*root)->m_num_of_children == 1);
                (*root)->free_node() && (*root)->m_num_of_children == 1;
                delete (*root);
            }
#ifdef __BTREE_DEBUG
            if(s_or_f == true) ++dbg_delete_success;
            else               ++dbg_delete_fail;
#endif // __BTREE_DEBUG
            return s_or_f;
        } //}
        bool Insert(const kv_type& what, btn_pointer* root) //{
        {
            assert(this->parent_null(this->m_parent));
            const key_type& key = this->GetKey(what);
            *root = this;
            BTreeNode_IMP* insert_node = this;
            if(this->IsFull()) {
                this->__split_full_node(root);
                insert_node = this->GetNode(this->m_parent);
            }
            bool result = insert_node->__insert(what, key, root);
#ifdef __BTREE_DEBUG
            ++dbg_insert_try;
            if(result == true) ++dbg_insert_success;
            else               ++dbg_insert_fail;
#endif // __BTREE_DEBUG
            return result;
        } //}

    void keep_order(kv_type* left, kv_type* right) //{
    {
        if(left  != nullptr) assert(this->kv_less(*left, this->m_keyvals[0]));
        if(right != nullptr) assert(this->kv_less(this->m_keyvals[this->m_num_of_children - 2], *right));
        if(this->m_num_of_children > 2) {
            for(size_type i = 0; i<=this->m_num_of_children - 3; ++i)
            assert(this->kv_less(this->m_keyvals[i], this->m_keyvals[i + 1]));
        }
        if(!this->IsLeaf()) {
            btn_pointer node = this->GetNode(this->m_children[0]);
            bool child_is_leaf = node->IsLeaf();
            (void)child_is_leaf;
            for(size_type i = 1; i<=this->m_num_of_children - 1; ++i) {
                node = this->GetNode(this->m_children[i]);
                assert(node->IsLeaf() == child_is_leaf);
            }
        }
        if(this->IsLeaf()) return;
        for(size_type i = 0; i<=this->m_num_of_children - 1; ++i) {
            if(i == 0)
                this->GetNode(this->m_children[i])->keep_order(nullptr, &this->m_keyvals[i]);
            else if(i == this->m_num_of_children - 1)
                this->GetNode(this->m_children[i])->keep_order(&this->m_keyvals[i - 1], nullptr);
            else 
                this->GetNode(this->m_children[i])->keep_order(&this->m_keyvals[i - 1], &this->m_keyvals[i]);
        }
    } //}

        btree_iterator begin(){return btree_iterator(this);}
        btree_iterator end  (){return btree_iterator(nullptr);}
}; //}

template<typename K, typename V, typename ID, typename KV, size_t Vt, typename BTN>
class BTree_IMP //{
{
    static_assert(std::is_base_of<BTreeNode_IMP<K, V, ID, KV, Vt>, BTN>::value, "unexcepted btree node type BTN");
    public:
        using key_type       = typename BTN::key_type;
        using val_type       = typename BTN::val_type;
        using id_type        = typename BTN::id_type;
        using kv_type        = typename BTN::kv_type;
        using size_type      = typename BTN::size_type;
        using btnode_pointer = typename BTN::btn_pointer;
        using basic_iterator = typename BTN::btree_iterator;

    private:
        size_type      m_size;
        btnode_pointer m_root;

    public:
        BTree_IMP(): m_size(0), m_root(nullptr){}
        ~BTree_IMP(){if(this->m_root != nullptr) this->m_root->recursive_free_node();}

        val_type* Search(const key_type& key) //{
        {
            if(this->m_size == 0) return nullptr;
            return this->m_root->Search(key);
        } //}
        bool Delete(const key_type& key) //{
        {
            if(this->m_size == 0) return false;
            btnode_pointer root_h = this->m_root;
            bool result = this->m_root->Delete(key, &root_h);
            this->m_root = root_h;
            if(result == true) --this->m_size;
            if(this->m_size == 0) this->m_root = nullptr;
#ifdef __BTREE_DEBUG
            if(this->m_root != nullptr) this->m_root->keep_order(nullptr, nullptr);
#endif // __BTREE_DEBUG
            return result;
        } //}
        bool Insert(const kv_type& what) //{
        {
            if(this->m_root == nullptr) {
                this->m_root = new BTN();
                this->m_root->SetupNode();
            }
            btnode_pointer root_h = this->m_root;
            bool result = this->m_root->Insert(what, &root_h);
            if(result == true) ++this->m_size;
            this->m_root = root_h;
#ifdef __BTREE_DEBUG
            this->m_root->keep_order(nullptr, nullptr);
#endif // __BTREE_DEBUG
            return result;
        } //}
        size_type Depth() //{
        {
            if(this->m_root == nullptr) return 0;
            btnode_pointer probe_p = m_root;
            size_type result = 1;
            while(!probe_p->IsLeaf()) {
                probe_p = probe_p->GetNode(probe_p->m_children[0]);
                ++result;
            }
            return result;
        } //}
        inline size_type size(){return this->m_size;}

        basic_iterator begin(){if(this->m_root == nullptr) return basic_iterator(nullptr); return this->m_root->begin();}
        basic_iterator end  (){if(this->m_root == nullptr) return basic_iterator(nullptr); return this->m_root->end  ();}
}; //}

template<typename K, typename V, size_t Vt>
class BTreeNode: public BTreeNode_IMP<K, V, void*, std::pair<K, V>, Vt> //{
{
    public:
        using key_type    = typename BTreeNode_IMP<K, V, void*, std::pair<K, V>, Vt>::key_type;
        using val_type    = typename BTreeNode_IMP<K, V, void*, std::pair<K, V>, Vt>::val_type;
        using id_type     = typename BTreeNode_IMP<K, V, void*, std::pair<K, V>, Vt>::id_type;
        using kv_type     = typename BTreeNode_IMP<K, V, void*, std::pair<K, V>, Vt>::kv_type;
        using size_type   = typename BTreeNode_IMP<K, V, void*, std::pair<K, V>, Vt>::size_type;
        using btn_pointer = typename BTreeNode_IMP<K, V, void*, std::pair<K, V>, Vt>::btn_pointer;

    protected:
        virtual val_type&   GetValue(kv_type& kv) const { return kv.second;}
        virtual key_type&   GetKey(kv_type& kv) const {return kv.first;}
        virtual btn_pointer GetNode(id_type n_id) const {return reinterpret_cast<btn_pointer>(n_id);}
        virtual bool        k_less (const key_type& a, const key_type& b) const {return a < b;}
        virtual bool        k_equal(const key_type& a, const key_type& b) const {return a == b;}
        virtual bool        parent_null(id_type n_id) const {return n_id == nullptr;}
        virtual id_type     null_parent() const {return nullptr;}
        virtual void        setup_node() {this->m_this_id = this;}
        virtual btn_pointer v_new_node() const {return new BTreeNode();}
        virtual void        free_node() {::memset(static_cast<void*>(this), '\0', sizeof(*this));}
        virtual bool        need_write_back(id_type){return true;}
        virtual void        write_back(){delete this;}
    public:
        BTreeNode(): BTreeNode_IMP<K, V, void*, std::pair<K, V>, Vt>() {}
}; //}

template<typename K, typename V, size_t Vt>
class BTree: public BTree_IMP<K, V, void*, std::pair<K,V>, Vt, BTreeNode<K, V, Vt>>{};

template class BTree<size_t, double, 50>;

#endif // BTREE_HPP_
