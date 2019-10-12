#include<cstdint>
#include<tuple>
#include<utility>

#include<string>

#include<vector>

#include<new>

// Integer Trie

using namespace std;

template<class T> 

/*
 * T.key is integer, this will be hold by uint64
 * T.value should be a pointer
 * T::KeyType is value type of key field
 * T::ValueType is value type of value field
 */

class Trie
{
    public:
        typedef typename T::KeyType   KeyType;
        typedef typename T::ValueType ValueType;

    private:
        Trie<T> *left  = nullptr, 
                *right = nullptr;
        T       *value = nullptr;

        void insert_unbox(T* pair, KeyType key)
        {
            if(key == 0)
            {
                this->value = pair;
                return;
            }
            if((key & 0x01) == 0x01)
            {
                if(this->right == nullptr)
                    this->right = new Trie<T>();
                right->insert_unbox(pair, key >> 1);
                return;
            }
            if(this->left == nullptr)
                this->left = new Trie<T>();
            left->insert_unbox(pair, key >> 1);
            return;
        }

    public:
        void insert(T* pair)
        {
            this->insert_unbox(pair, pair->key);
            return;
        }
        void insert(T& pair)
        {
            this->insert(&pair);
            return;
        }
        void insert(T&& pair)
        {
            T* new_obj = new T(static_cast<T&&>(pair));
            this->insert(new_obj);
            return;
        }

        T* search(const KeyType key)
        {
            if(key == 0)
                return this->value;
            if((key & 0x01) == 0x01){
                if(right == nullptr)
                    return nullptr;
                return right->search(key >> 1);
            }
            if(left == nullptr)
                return nullptr;
            return left->search(key >> 1);
        }

        void delete_key(const KeyType key)
        {
            if(key == 0){
                this->value = nullptr;
                return;
            }
            if((key & 0x01) == 0x01){
                if(right != nullptr)
                    right->delete_key(key >> 1);
                return;
            }
            if(left != nullptr)
                left->delete_key(key >> 1);
            return;
        }

        Trie() = default;
        Trie(Trie& x) // deep copy
        {
            if(x.left != nullptr)
                this->left = new Trie(*x.left);
            if(x.right != nullptr)
                this->right = new Trie(*x.right);
            this->value = x.value;
        }
        Trie(Trie&& x)
        {
            this->left = x.left;
            this->right = x.right;
            this->value = x.value;
            x.left = x.right = nullptr;
        }

        Trie& operator=(Trie& x)
        {
            return *(new Trie(x));
        }
        Trie& operator=(Trie&& x)
        {
            return *(new Trie(static_cast<Trie<T>&&>(x)));
        }

        ~Trie()
        {
            delete this->left;
            delete this->right;
        }
};

template<typename KT, typename VT>
class KeyValuePair
{
    public:
        typedef KT KeyType;
        typedef VT ValueType;

        KeyType    key;
        ValueType* value = nullptr;

    KeyValuePair(KT key, VT* value)
    {
        this->key = key;
        this->value = value;
    }
};


template class Trie<KeyValuePair<uint64_t, int>>;
using TrieTest = Trie<KeyValuePair<uint64_t, int>>;

template<typename VT> class StringTrie;
template<typename VT>
struct __ternary__
{
    __ternary__() = delete;
    __ternary__(char _c, StringTrie<VT>* _t, __ternary__<VT>* _n):
        c(_c), t(_t), n(_n){}
    char c;
    StringTrie<VT>* t;
    __ternary__<VT>* n;
};

template struct __ternary__<int>;

template<typename VT>
class StringTrie
{
    public:
        typedef VT ValueType;

    private:
        ValueType* value;
        __ternary__<VT>* children = nullptr;
        __ternary__<VT>* last_child = nullptr;

    public:
        StringTrie(): value(nullptr){}
        StringTrie(StringTrie<VT>&)  = delete;
        StringTrie(StringTrie<VT>&&) = delete;
        StringTrie<VT>& operator=(StringTrie<VT>&)  = delete;
        StringTrie<VT>& operator=(StringTrie<VT>&&) = delete;
        ~StringTrie() = default;

        void insert(const string& str, ValueType* v)
        {
            if(str.empty()){
                this->value = v;
                return;
            }
            __ternary__<VT>* loop_ptr = children;
            char lookup_char = *str.begin();
            for(;loop_ptr != nullptr && loop_ptr->c != lookup_char;loop_ptr = loop_ptr->n){}
            if(nullptr != loop_ptr){
                loop_ptr->t->insert(str.substr(1, str.size() - 1), v);
                return;
            }
            if(last_child == nullptr){
                children = new __ternary__<VT>(lookup_char, new StringTrie(), nullptr);
                last_child = children;
            } else {
                last_child->n = new __ternary__<VT>(lookup_char, new StringTrie(), nullptr);
                last_child = last_child->n;
            }
            last_child->t->insert(str.substr(1, str.size() - 1), v);
            return;
        }

        ValueType* search(const string& str)
        {
            if(str.empty())
                return this->value;
            typeof(children) loop_ptr = children;
            char first_char = *str.begin();
            for(;loop_ptr != nullptr && loop_ptr->c != first_char;loop_ptr = loop_ptr->n){}
            if(loop_ptr != nullptr)
                return loop_ptr->t->search(str.substr(1, str.size() - 1));
            return nullptr;
        }
};

template class StringTrie<int>;
