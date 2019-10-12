// Binary Tree
/*
 * This implementation is base on linked list
 *         -------------------------------------------
 *         |           Root: BinaryTreeNode              |
 *         |-----------------------------------------|
 *         | left: BinaryTreeNode*  |  right: BinaryTreeNode*|
 *         -------------------------------------------
 *                  -                   - 
 *                 -                     -
 *                -                       -
 *           leftChild                   rightChild
 */

#include<cstdlib>
#include<string>
#include<type_traits>

#include "../utils/type.hpp"

#define CONV_SELF_OP(op, x) \
    using t_type = typename ::std::remove_cv<decltype(x)>::type;\
    typename ::std::decay<t_type>::type y(static_cast<t_type&>(*this)); \
    y.operator op(x); \
    return y;

#include<utility>
#include<type_traits>

template<typename KV>
class BinaryTreeNode //{
{
    public:
    using KeyValueType = KV;
    using KeyType      = typename KV::KeyType;
    using ValueType    = typename KV::ValueType;

    private:
    KeyValueType _kv_data;
    BinaryTreeNode<KeyValueType>* left;
    BinaryTreeNode<KeyValueType>* right;

    public:
    BinaryTreeNode(): left(nullptr), right(nullptr){}
    ~BinaryTreeNode(){
        if(left  != nullptr) delete left; 
        if(right != nullptr) delete right;
    }
    BinaryTreeNode(const BinaryTreeNode&  oth){*this = oth;};
    BinaryTreeNode(const BinaryTreeNode&& oth){*this = std::move(oth);};
    BinaryTreeNode<KeyValueType>& operator=(const BinaryTreeNode& oth){ //{
        if(&oth == this)
            return *this;
        this->_kv_data = (oth._kv_data);
        delete this->left;
        delete this->right;
        if(oth.left != nullptr){
            BinaryTreeNode* new_left = new BinaryTreeNode();
            *new_left = *oth.left;
            this->left = new_left;
        } else {
            this->left = nullptr;
        }
        if(oth.right != nullptr){
            BinaryTreeNode* new_right = new BinaryTreeNode();
            *new_right = *oth.left;
            this->left = new_right;
        } else {
            this->left = nullptr;
        }
        return *this;
    } //}
    BinaryTreeNode<KeyValueType>& operator=(BinaryTreeNode&& oth){ //{
        if(&oth == this)
            return *this;
        this->_kv_data = std::move(oth._kv_data);
        delete this->left;
        delete this->right;
        this->left     = oth.left;
        this->right    = oth.right;
        oth.left  = nullptr;
        oth.right = nullptr;
        return *this;
    } //}

    bool search(const KeyType& key, KeyValueType*& ret);
    void preTraversing(void (*)(BinaryTreeNode&));
    template<typename Functor, typename std::enable_if<is_callable<Functor, BinaryTreeNode<KeyValueType>&>::value>::type>
    void preTraversing(Functor&);

    void infTraversing(void (*)(BinaryTreeNode&));
    template<typename Functor, typename std::enable_if<is_callable<Functor, BinaryTreeNode<KeyValueType>&>::value>::type>
    void infTraversing(Functor&);

    void postTraversing(void (*)(BinaryTreeNode&));
    template<typename Functor, typename std::enable_if<is_callable<Functor, BinaryTreeNode<KeyValueType>&>::value>::type>
    void postTraversing(Functor&);
}; //}
// member function of class template BinaryTreeNode //{
template<typename KV>
bool BinaryTreeNode<KV>::search(const KeyType& key, KeyValueType*& ret){ //{
    if(this->_kv_data.getKey() == key){
        ret = &this->_kv_data;
        return true;
    }
    if(this->left != nullptr  && this->left->search(key, ret))
        return true;
    if(this->right != nullptr && this->right->search(key, ret))
        return true;
    return false;
} //}

template<typename KV>
void BinaryTreeNode<KV>::preTraversing(void (*func)(BinaryTreeNode&)){//{
    func(*this);
    if(this->left != nullptr)
        this->left->preTraversing(func);
    if(this->right != nullptr)
        this->right->preTraversing(func);
}//}
template<typename KV>
template<typename Functor, typename std::enable_if<is_callable<Functor, BinaryTreeNode<KV>&>::value>::type>
void BinaryTreeNode<KV>::preTraversing(Functor& functor){ //{
    functor(*this);
    if(this->left != nullptr)
        this->left->preTraversing(functor);
    if(this->right != nullptr)
        this->right->preTraversing(functor);
} //}

template<typename KV>
void BinaryTreeNode<KV>::infTraversing(void (*func)(BinaryTreeNode&)){//{
    if(this->left != nullptr)
        this->left->preTraversing(func);
    func(*this);
    if(this->right != nullptr)
        this->right->preTraversing(func);
}//}
template<typename KV>
template<typename Functor, typename std::enable_if<is_callable<Functor, BinaryTreeNode<KV>&>::value>::type>
void BinaryTreeNode<KV>::infTraversing(Functor& functor){ //{
    functor(*this);
    if(this->left != nullptr)
        this->left->preTraversing(functor);
    if(this->right != nullptr)
        this->right->preTraversing(functor);
} //}

template<typename KV>
void BinaryTreeNode<KV>::postTraversing(void (*func)(BinaryTreeNode&)){//{
    if(this->left != nullptr)
        this->left->preTraversing(func);
    if(this->right != nullptr)
        this->right->preTraversing(func);
    func(*this);
}//}
template<typename KV>
template<typename Functor, typename std::enable_if<is_callable<Functor, BinaryTreeNode<KV>&>::value>::type>
void BinaryTreeNode<KV>::postTraversing(Functor& functor){ //{
    if(this->left != nullptr)
        this->left->preTraversing(functor);
    if(this->right != nullptr)
        this->right->preTraversing(functor);
    functor(*this);
} //}
//} end member function of class template BinaryTreeNode

template<typename KV>
class BinaryTree  //{
{
    public:
        using KeyValueType = KV;
        using KeyType      = typename KV::KeyType;
        using ValueType    = typename KV::ValueType;
    private:
        BinaryTreeNode<KV> rootNode;
    public:
        BinaryTree(): rootNode(){}
        BinaryTree(const BinaryTree& o){*this = o;}
        BinaryTree(BinaryTree&& o)     {*this = std::move(o);}
        BinaryTree& operator=(const BinaryTree& o){
            if(this == &o)
                return *this;
            this->rootNode = o.rootNode;
            return *this;
        }
        BinaryTree& operator=(BinaryTree&& o){
            if(this == &o)
                return *this;
            this->rootNode = std::move(o.rootNode);
            return *this;
        }
}; //}

template<typename K, typename V>
class KeyValuePair //{
{
    public:
        using KeyType = K;
        using ValueType = V;
    private:
        KeyType __key;
        ValueType __value;
    public:
        inline KeyType&   getKey()  {return this->__key;}
        inline ValueType& getValue(){return this->__value;}
        KeyValuePair(): __key(), __value(){}
        ~KeyValuePair() = default;
        KeyValuePair(const KeyType& k, const ValueType& v): __key(k), __value(v){}
        KeyValuePair(KeyType&& k, ValueType&& v): __key(std::forward(k)), __value(std::forward(v)){}
        KeyValuePair(const KeyValuePair& kv): __key(kv.__key), __value(kv.__value){}
        KeyValuePair(KeyValuePair&& kv): __key(std::forward(kv.__key)), __value(std::forward(kv.__value)){}
        KeyValuePair& operator=(const KeyValuePair& kv){
            if(this == &kv)
                return *this;
            new(&__key) KeyType(kv.__key);
            new(&__value) ValueType(kv.__value);
            return *this;
        }
        KeyValuePair& operator=(KeyValuePair&& kv){
            if(this == &kv)
                return *this;
            new(&__key  ) KeyType(std::move(kv.__key));
            new(&__value) ValueType(std::move(kv.__value));
            return *this;
        }
}; //} end class template KeyValuePair

using kvTest = KeyValuePair<size_t, std::string>;
template class BinaryTree<kvTest>;
template class BinaryTreeNode<kvTest>;
