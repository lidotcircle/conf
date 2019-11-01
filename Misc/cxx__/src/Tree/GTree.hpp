#ifndef GTREE_HPP
#define GTREE_HPP

#include<vector>
#include<stack>
#include<queue>

#include<iostream>
#include<iomanip>

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
        key_type& GetKey() {return this->m_key;}
        const key_type& GetKey() const {return this->m_key;}
        forest_type& GetChildren() {return this->m_children;}
        const forest_type& GetChildren() const {return this->m_children;}

        ~GTree() {
            for(auto x = m_children.begin(); x != m_children.end(); x++)
                delete *x;
        }
}; //}

template<typename K, typename V>
void operator_out_aux(std::ostream& os, const GTree<K, V>& t, int level) //{
{
    for(int i = level; i > 0; --i)
        os << "|" << std::string(3, ' ');
    os << "> " << t.GetKey() << std::endl;
    for(auto bi = t.GetChildren().begin(); bi != t.GetChildren().end(); ++bi)
        operator_out_aux(os, *(*bi), level + 1);
    return;
} //}

template<typename K, typename V>
std::ostream& operator<<(std::ostream& os, const GTree<K, V>& t) //{
{
    operator_out_aux<K, V>(os, t, 0);
    return os;
} //}

#endif //GTREE_HPP
