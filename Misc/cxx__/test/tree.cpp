#include "../src/BinaryTree/BinaryTree.hpp"
#include<utility>
#include<iostream>
#include<vector>

int main(int argc, char** argv)
{
    BTNode<int, int> n(1, 2);
    BTree<int, int> s(&n);
    s.insert(static_cast<const BTNode<int, int>*>(&n));
    s.insert(static_cast<const BTNode<int, int>*>(&n));
    s.insert(static_cast<const BTNode<int, int>*>(&n));
    s.insert(static_cast<const BTNode<int, int>*>(&n));
    s.insert(static_cast<const BTNode<int, int>*>(&n));
    s.insert(static_cast<const BTNode<int, int>*>(&n));
    s.insert(static_cast<const BTNode<int, int>*>(&n));
    s.insert(static_cast<const BTNode<int, int>*>(&n));
    s.insert(static_cast<const BTNode<int, int>*>(&n));
    s.insert(static_cast<const BTNode<int, int>*>(&n));
    s.insert(static_cast<const BTNode<int, int>*>(&n));
    s.insert(static_cast<const BTNode<int, int>*>(&n));
    s.insert(static_cast<const BTNode<int, int>*>(&n));
    return 0;
}
