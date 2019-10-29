#ifndef GTREE_HPP
#define GTREE_HPP

#include<vector>
#include<stack>
#include<queue>

template<typename K, typename V>
class GTree //{
{
    public:
        typedef K                   key_type;
        typedef V                   value_type;
        typedef std::vector<GTree*> forest_type;

    private:
        forest_type m_children;
        key_type    m_key;
        value_type  m_val;

        bool search_children(const key_type& _k, GTree** _out){
            for(auto x = m_children.begin(); x!=m_children.end(); x++){
                if((*x)->m_key == _k) {*_out = *x; return true;}
            }
            return false;
        }

    public:
        GTree(const key_type& _key, const value_type& _val):
            m_children(), m_key(_key), m_val(_val){}
        GTree() = delete;

        void new_child(GTree* const& x){this->m_children.push_back(x);}

        value_type& GetValue() {return this->m_val;}
        const value_type& GetValue() const {return this->m_val;}
        value_type& GetChildren() {return this->m_children;}
        const value_type& GetChildren() const {return this->m_children;}

        ~GTree() {
            for(auto x = m_children.begin(); x != m_children.end(); x++)
                delete *x;
        }

        bool dfs_search(const key_type& _k, GTree** _out) {
            std::stack<GTree*> traversal_stack;
            traversal_stack.push(this);
            while(!traversal_stack.empty()) {
                GTree* _pp = traversal_stack.top(); traversal_stack.pop();
                if(_pp->m_key == _k){*_out = _pp; return true;}
                for(auto x = _pp->m_children.begin(); x!=_pp->m_children.end(); x++)
                    traversal_stack.push(*x);
            }
            return false;
        }
        bool bfs_search(const key_type& _k, GTree** _out) {
            std::queue<GTree*> traversal_queue;
            traversal_queue.push(this);
            while(!traversal_queue.empty()) {
                GTree* _pp = traversal_queue.front(); traversal_queue.pop();
                if(_pp->m_key == _k){*_out = _pp; return true;}
                for(auto x = _pp->m_children.begin(); x!=_pp->m_children.end(); x++)
                    traversal_queue.push(*x);
            }
            return false;
        }

        void bfs_traverse(void (*func)(GTree*, void*), void* obj = nullptr) {
            std::queue<GTree*> traversal_queue;
            traversal_queue.push(this);
            while(!traversal_queue.empty()) {
                GTree* _pp = traversal_queue.front(); traversal_queue.pop();
                func(_pp, obj);
                for(auto x = _pp->m_children.begin(); x!=_pp->m_children.end(); x++)
                    traversal_queue.push(*x);
            }
        }
}; //}

#endif //GTREE_HPP
