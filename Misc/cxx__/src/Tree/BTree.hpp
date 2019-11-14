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

template<typename K, typename V, typename ID, typename KV, size_t Vt>
class BTree_IMP //{
{
    static_assert(Vt >= 2, "t should greater than or equal 2 in BTree");
    public:
        typedef K          key_type;
        typedef V          val_type;
        typedef ID         id_type;
        typedef KV         kv_type;
        typedef size_t     size_type;
        typedef BTree_IMP* pointer;

    protected:
        virtual val_type&  GetValue(kv_type&) const = 0;
        virtual key_type&  GetKey(kv_type&) const = 0;
        virtual BTree_IMP* GetNode(id_type) const = 0;
        virtual bool       k_less (const key_type&, const key_type&) const = 0;
        virtual bool       k_equal(const key_type&, const key_type&) const = 0;
        virtual bool       parent_null(id_type) const = 0;
        virtual id_type    null_parent() const = 0;
        virtual void       setup_node() = 0; // building information about id, write node to disk, balabala
        virtual void       free_node() = 0; // release this node
        virtual BTree_IMP* new_node() const = 0;

        const val_type& GetValue(const kv_type& a) const         {return this->GetValue(const_cast<kv_type&>(a));}
        const key_type& GetKey(const kv_type& a) const           {return this->GetKey  (const_cast<kv_type&>(a));}
        bool kv_less (const kv_type& a, const key_type& b) const {return this->k_less  (this->GetKey(a), b);}
        bool kv_equal(const kv_type& a, const key_type& b) const {return this->k_equal (this->GetKey(a), b);}
        bool kv_less (const key_type& a, const kv_type& b) const {return this->k_less  (a, this->GetKey(b));}
        bool kv_equal(const key_type& a, const kv_type& b) const {return this->k_equal (a, this->GetKey(b));}

    protected:
        id_type   m_children[2 * Vt];
        kv_type   m_keyvals[2 * Vt - 1];
        size_type m_num_of_children;
        bool      m_is_leaf;
        id_type   m_parent;
        id_type   m_this_id;

        id_type Parent(){return this->m_parent;}
        bool    IsLeaf(){return this->m_is_leaf;}
        bool    IsFull(){return this->m_num_of_children == Vt * 2;}

        std::pair<BTree_IMP*, size_type> __search(const key_type& key) //{
        {
            BTree_IMP* search_node = this;
            size_type  where = 0;
            for(;;++where) {
                if(kv_equal(search_node->m_keyvals[where], key))
                    return std::make_pair(search_node, where);
                if(kv_less(key, search_node->m_keyvals[where])) {
                    if(search_node->IsLeaf()) return std::make_pair(nullptr, size_type(0));
                    search_node = this->GetNode(search_node->m_children[where]);
                    where = 0;
                    continue;
                }
                if(where == search_node->m_num_of_children - 2) {
                    if(search_node->IsLeaf()) return std::make_pair(nullptr, size_type(0));
                    search_node = this->GetKey(search_node->m_children[where + 1]);
                    where = 0;
                    continue;
                }
            }
        } //}
        void children_move_right_one(size_type start) // @start is in range of [0, Vt * 2 - 1] //{
        {
            assert(start < this->m_num_of_children);
            assert(this->m_num_of_children != Vt * 2 && this->m_num_of_children >= 2);
            for(size_type i = this->m_num_of_children - 1; i >= start; --i)
                this->m_children[i + 1] = std::move(this->m_children[i]);
        } //}
        void keyval_move_right_one(size_type start) // above //{
        {
            assert(start < this->m_num_of_children);
            assert(this->m_num_of_children != Vt * 2 && this->m_num_of_children >= 2);
            for(size_type i = this->m_num_of_children - 2; i >= start; --i)
                this->m_keyvals[i + 1] = std::move(this->m_keyvals[i]);
        } //}
        void keyval_move_left_one (size_type start) // above //{
        {
            assert(start < this->m_num_of_children && start > 0);
            assert(this->m_num_of_children != Vt * 2 && this->m_num_of_children >= 2);
            for(size_type i = start; i <= this->m_num_of_children - 2; ++i)
                this->m_keyvals[i - 1] = std::move(this->m_keyvals[i]);
        } //}
        void children_move_left_one (size_type start) // above //{
        {
            assert(start < this->m_num_of_children && start > 0);
            assert(this->m_num_of_children != Vt * 2 && this->m_num_of_children >= 2);
            for(size_type i = start; i <= this->m_num_of_children - 1; ++i)
                this->m_children[i - 1] = std::move(this->m_children[i]);
        } //}
        void __split_full_node() //{
        {
            assert(this->m_num_of_children == 2 * Vt);
            BTree_IMP* parent;
            if(this->parent_null(this->m_parent)) { // root node
                BTree_IMP* parent = this->new_node();
                parent->setup_node();
                parent->m_is_leaf = false;
                parent->m_parent = this->null_parent();
            } else parent = this->GetNode(this->m_parent);

            BTree_IMP* new_sibling = this->new_node();
            new_sibling->setup_node();
            this->m_parent        = parent->m_this_id;
            new_sibling->m_parent = parent->m_this_id;
            if(this->IsLeaf())
                new_sibling->m_is_leaf = true;

            new_sibling->m_num_of_children = Vt - 1;
            this->m_num_of_children        = Vt - 1;
            for(size_type i = 0, j = Vt; i < Vt - 1; ++i, ++j) {
                new_sibling->m_keyvals[i] = std::move(this->m_keyvals[j]);
                if(!this->IsLeaf()) new_sibling->m_children[i] = std::move(this->m_children[j]);
            }
            if(!this->IsLeaf()) new_sibling->m_children[Vt - 1] = std::move(this->m_children[2 * Vt - 1]);
            if(parent->m_num_of_children == 0) {
                parent->m_num_of_children = 2;
                parent->m_keyvals[0]      = std::move(this->m_keyvals[Vt - 1]);
                parent->m_children[0]     = this->m_this_id;
                parent->m_children[1]     = this->m_this_id;
                return;
            }
            assert(parent->m_num_of_children != 2 * Vt);
            size_type where_this = 0;
            for(;where_this < parent->m_num_of_children; ++where_this)
                if(parent->m_children[where_this] == this->m_this_id) break;
            assert(parent->m_children[where_this] == this->m_this_id);
            if(where_this == parent->m_num_of_children - 1) {
                parent->m_children[m_num_of_children] = new_sibling->m_this_id;
                parent->m_keyvals[m_num_of_children - 1] = std::move(this->m_keyvals[Vt - 1]);
                ++parent->m_num_of_children;
                return;
            }
            ++where_this;
            parent->children_move_right_one(where_this);
            parent->keyval_move_right_one(where_this);
            parent->m_keyvals[where_this] = std::move(this->m_keyvals[Vt - 1]);
            parent->m_children[where_this] = new_sibling->m_this_id;
            ++parent->m_num_of_children;
            return;
        } //}
        bool __insert(const kv_type& what, const key_type& key) //{
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
                    BTree_IMP* next_insert = this->GetNode(this->m_children[insert_place]);
                    if(next_insert->IsFull()) {
                        next_insert->__split_full_node();
                        if(this->kv_equal(this->m_keyvals[insert_place], key)) return false;
                        if(this->kv_less(this->m_keyvals[insert_place], key))
                            next_insert = this->GetNode(this->m_children[insert_place + 1]);
                    }
                    return next_insert->__insert(what, key);
                }
                this->m_keyvals[this->m_num_of_children - 1] = what;
                ++this->m_num_of_children;
                return true;
            }
            if(!this->IsLeaf()) {
                BTree_IMP* next_insert = this->GetNode(this->m_children[insert_place]);
                if(next_insert->IsFull())
                    next_insert->__split_full_node();
                return next_insert->__insert(what, key);
            }
            this->keyval_move_right_one(insert_place);
insert_first:
            this->m_keyvals[insert_place] = what;
            ++this->m_num_of_children;
            return true;
        } //}
        BTree_IMP* __merge(BTree_IMP* parent, BTree_IMP* left, BTree_IMP* right, size_type where) //{
        {
            assert(this->GetNode(parent->m_children[where]) == left);
            assert(this->GetNode(parent->m_children[where + 1]) == right);
            assert(left->m_num_of_children == Vt && right->m_num_of_children == Vt);
            assert(parent->m_num_of_children > Vt || this->parent_null(parent->m_this_id));
            for(size_type i = Vt; i <= Vt - 2; ++i) {
                left->m_children[i] = std::move(right->m_children[i - Vt]);
                left->m_keyvals[i] = std::move(right->m_keyvals[i - Vt]);
            }
            left->m_children[2 * Vt - 1] = std::move(right->m_children[Vt - 1]);
            right->m_num_of_children = 0;
            right->free_node();
            delete right;
            left->m_keyvals[Vt - 1] = std::move(parent->m_keyvals[where]);
            if(parent->m_num_of_children == 2) { // new root
                parent->m_num_of_children = 0;
                parent->free_node();
                delete parent;
                this->m_parent = this->null_parent();
                return left;
            }
            if(where != parent->m_num_of_children - 2) {
                parent->keyval_move_left_one(where + 1);
                parent->children_move_left_one(where + 2);
            }
            --parent->m_num_of_children;
            return left;
        } //}
        BTree_IMP* __ensure_at_t_keyval() //{
        {
            if(this->m_num_of_children >= Vt + 1) return this;
//            if(this->parent_null(this->m_parent)) return this;
            assert(!this->parent_null(this->m_parent));
            BTree_IMP* parent = this->GetNode(this->m_parent);
            size_type where = size_type(0);
            for(;where < parent->m_num_of_children; ++where)
                if(parent->m_children[where] == this) break;
            assert(parent->m_children[where] == this);
            BTree_IMP *left_sibling = nullptr, *right_sibling=nullptr;
            if(where != 0) left_sibling  = this->GetNode(parent->m_children[where - 1]);
            if(left_sibling != nullptr) {
                if(left_sibling->m_num_of_children == Vt) // merge left and this
                    return __merge(parent, left_sibling, this, where - 1);
                /* move max keyval from left to this*/
                this->keyval_move_right_one(0);
                this->children_move_right_one(0);
                this->m_keyvals[0] = std::move(parent->m_keyvals[where - 1]);
                this->m_children[0] = std::move(left_sibling->m_children[left_sibling->m_num_of_children - 1]);
                ++this->m_num_of_children;
                parent->m_keyvals[where - 1] = std::move(left_sibling->m_keyvals[left_sibling->m_num_of_children - 2]);
                --left_sibling->m_num_of_children;
                return this;
            }
            if(where <  parent->m_num_of_children - 1) right_sibling = this->GetNode(parent->m_children[where + 1]);
            assert(right_sibling != nullptr);
            if(right_sibling->m_num_of_children == Vt)
                return __merge(parent, this, right_sibling, where);
            /* move min keyval from right to this */
            this->m_children[this->m_num_of_children] = std::move(right_sibling->m_children[0]);
            this->m_keyvals[this->m_num_of_children - 1] = std::move(parent->m_keyvals[where]);
            ++this->m_num_of_children;
            parent->m_keyvals[where] = std::move(right_sibling->m_keyvals[0]);
            right_sibling->keyval_move_left_one(1);
            right_sibling->children_move_left_one(1);
            --right_sibling->m_num_of_children;
            return this;
        } //}
         void __delete(size_type where) //{
         {
             assert(where <= this->m_num_of_children - 2);
             BTree_IMP* target = this;
             key_type& test_key = this->GetKey(this->m_keyvals[0]);
             if(!this->parent_null(this->m_parent)) target = this->__ensure_at_t_keyval();
             if(!this->kv_equal(target->m_keyvals[0], test_key)) where += Vt - 1;
             if(!target->IsLeaf()) {
                 BTree_IMP *left_child = target->GetNode(target->m_children[where]);
                 std::swap(target->m_keyvals[where], left_child->m_keyvals[left_child->m_num_of_children - 2]);
                 left_child->__delete(left_child->m_num_of_children - 2);
                 return;
             }
             /* Leaf node */
             if(where != target->m_num_of_children - 2)
                 target->keyval_move_left_one(where + 1);
             --target->m_num_of_children;
         } //}

    public:
        BTree_IMP(): m_children(), m_keyvals(), m_num_of_children(0), m_is_leaf(true), m_parent(), m_this_id(){}

        val_type* Search(const key_type& key) //{
        {
            assert(this->parent_null(this->m_parent)); // ensure this node is root
            std::pair<BTree_IMP*, size_type> result = this->__search(key);
            if(result.first == nullptr) return nullptr;
            return &this->GetValue(result.first->m_keyvals[result.second]);
        } //}
        bool Delete(const key_type& key) //{ TODO
        {
            assert(this->parent_null(this->m_parent));
            std::pair<BTree_IMP*, size_type> result = this->__search(key);
            if(result.first == nullptr) return false;
        } //}
        bool Insert(const kv_type& what) //{
        {
            assert(this->parent_null(this->m_parent));
            const key_type& key = this->GetKey(what);
            BTree_IMP* insert_node = this;
            if(this->IsFull()) {
                this->__split_full_node();
                insert_node = this->GetNode(this->m_parent);
            }
            return insert_node->__insert(what, key);
        } //}
}; //}

template<typename K, typename V, size_t Vt>
class BTree: public BTree_IMP<K, V, void*, std::pair<K, V>, Vt> //{
{
        using key_type  = typename BTree_IMP<K, V, void*, std::pair<K, V>, Vt>::key_type;
        using val_type  = typename BTree_IMP<K, V, void*, std::pair<K, V>, Vt>::val_type;
        using id_type   = typename BTree_IMP<K, V, void*, std::pair<K, V>, Vt>::id_type;
        using kv_type   = typename BTree_IMP<K, V, void*, std::pair<K, V>, Vt>::kv_type;
        using size_type = typename BTree_IMP<K, V, void*, std::pair<K, V>, Vt>::size_type;
        using pointer   = typename BTree_IMP<K, V, void*, std::pair<K, V>, Vt>::pointer;

    protected:
        virtual val_type&  GetValue(kv_type& kv) const { return kv.second;}
        virtual key_type&  GetKey(kv_type& kv) const {return kv.first;}
        virtual pointer    GetNode(id_type n_id) const {return reinterpret_cast<pointer>(n_id);}
        virtual bool       k_less (const key_type& a, const key_type& b) const {return a < b;}
        virtual bool       k_equal(const key_type& a, const key_type& b) const {return a == b;}
        virtual bool       parent_null(id_type n_id) const {return n_id == nullptr;}
        virtual id_type    null_parent() const {return nullptr;}
        virtual void       setup_node() {this->m_this_id = this;}
        virtual pointer    new_node() const {return new BTree();}
        virtual void       free_node() {}
    public:
        BTree(): BTree_IMP<K, V, void*, std::pair<K, V>, Vt>() {}
}; //}

template class BTree<size_t, double, 50>;

#endif // BTREE_HPP_
