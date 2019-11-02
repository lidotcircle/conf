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

template<typename KV> class BGTree;
template<typename KV>
void operator_out_aux_BGTree(std::ostream& os, const BGTree<KV>& t, int level);
template<typename KV>
class BGTree //{
{
    public:
        typedef KV KeyValType;

    private:
        BGTree*    m_child;
        BGTree*    m_siblings;
        KeyValType m_keyval;

        friend void operator_out_aux_BGTree<KV>(std::ostream& os, const BGTree<KV>& t, int level);

    public:
        BGTree(const KeyValType& kv): m_child(nullptr), m_siblings(nullptr), m_keyval(kv){}
        BGTree(KeyValType&& kv): m_child(nullptr), m_siblings(nullptr), m_keyval(std::forward<KeyValType>(kv)){}
        BGTree(): m_child(nullptr), m_siblings(nullptr), m_keyval(){}
        BGTree(const BGTree& _oth): BGTree(){*this = _oth;}
        BGTree(BGTree&& _oth):      BGTree(){*this = std::move(_oth);}

        ~BGTree() //{
        {
            if(m_child != nullptr)
                delete this->m_child;
            if(m_siblings != nullptr)
                delete this->m_siblings;
        } //}

        BGTree& operator=(const BGTree<KeyValType>& _oth) //{
        {
            if(this == &_oth) return *this;
            this->m_keyval = _oth.m_keyval;
            if(this->m_child    != nullptr) delete this->m_child;
            if(this->m_siblings != nullptr) delete this->m_siblings;
            if(_oth.m_child != nullptr){
                this->m_child  = new BGTree();
                *this->m_child = *_oth.m_child;
            }
            if(_oth.m_siblings != nullptr) {
                this->m_siblings  = new BGTree();
                *this->m_siblings = *_oth.m_siblings;
            }
            return *this;
        } //}
        BGTree& operator=(BGTree<KeyValType>&& _oth) //{
        {
            if(this == &_oth) return *this;
            this->m_keyval = std::move(_oth.m_keyval);
            if(this->m_child    != nullptr){delete this->m_child; this->m_child = nullptr;}
            if(this->m_siblings != nullptr){delete this->m_siblings; this->m_siblings = nullptr;}
            std::swap(this->m_child, _oth.m_child);
            std::swap(this->m_siblings, _oth.m_siblings);
            return *this;
        } //}

        void new_child(const BGTree<KeyValType>& _child) //{
        {
            if(this->m_child == nullptr) {
                this->m_child = const_cast<BGTree<KeyValType>*>(&_child);
                return;
            }
            this->m_child->app_sibling(_child);
        } //}
        void new_child(const BGTree<KeyValType>*& _child){this->new_child(*_child);}
        void app_sibling(const BGTree<KeyValType>& _sibling) //{
        {
            if(this->m_siblings == nullptr) {
                this->m_siblings = const_cast<BGTree<KeyValType>*>(&_sibling);
                return;
            }
            BGTree<KeyValType>* l_var = nullptr;
            for(l_var = this->m_siblings; l_var->m_siblings != nullptr; l_var = l_var->m_siblings);
            l_var->m_siblings = const_cast<BGTree<KeyValType>*>(&_sibling);
            return;
        } //}
        void app_sibling(const BGTree<KeyValType>*& _sibling){this->app_sibling(*_sibling);}

        KeyValType& KeyValue(){return this->m_keyval;}
        const KeyValType& KeyValue() const {return this->m_keyval;}
}; //}

template class BGTree<double>;

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

template<typename F, typename S>
std::ostream& operator<<(std::ostream& os, const std::pair<F, S>& xx) //{
{
    os << "Key: " << xx.first << ", Value: " << xx.second;
} //}
template<typename KV>
void operator_out_aux_BGTree(std::ostream& os, const BGTree<KV>& t, int level) //{
{
    for(int i = level; i > 0; --i)
        os << "|" << std::string(3, ' ');
    os << "> " << t.KeyValue() << std::endl;
    for(BGTree<KV>* bi = t.m_child; bi != nullptr; bi = bi->m_siblings)
        operator_out_aux_BGTree(os, *bi, level + 1);
    return;
} //}
template<typename KV>
std::ostream& operator<<(std::ostream& os, const BGTree<KV>& t) //{
{
    operator_out_aux_BGTree(os, t, 0);
    return os;
} //}

template<typename K, typename V>
BGTree<std::pair<K,V>>& GTreeToBGTree(const GTree<K,V>& gt) //{
{
    using KVType = std::pair<K, V>;
    BGTree<KVType>* ret = new BGTree<KVType>();
    ret->KeyValue() = std::make_pair(gt.GetKey(), gt.GetValue());
    for(auto bi = gt.GetChildren().begin(); bi != gt.GetChildren().end(); ++bi) {
        BGTree<KVType>& xyz = GTreeToBGTree(*(*bi));
        ret->new_child(xyz);
    }
    return *ret;
} //}

#endif //GTREE_HPP
