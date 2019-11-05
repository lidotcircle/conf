#include<ctime>
#include<ios>
#include<iostream>
#include<iomanip>
#include<chrono>

#include<memory>

//#define FIBONACCIHEAP_DEBUG

#include<cstdlib>

#include<unistd.h>
#include "../src/utils/type.hpp"
#include "../src/utils/logger.h"
#include "../src/Graph/Graph.hpp"

void testA() //{
{
    DenseGraph<double, size_t> A(10, true);
    A.NewVertex(100.2);
    A.NewVertex(100.2);
    A.NewVertex(100.2);
    A.NewVertex(100.2);
    A.NewVertex(100.2);
    A.NewVertex(100.2);
    A.DeleteVertex(3);
    A.DeleteVertex(2);
    std::srand(time(nullptr));
    for(auto bi = A.VertexBegin(); bi != A.VertexEnd(); bi++) {
        for(auto xi = A.VertexBegin(); xi != A.VertexEnd(); xi++){
            if(std::rand() % 4 == 0)
                A.SetWeight(bi->GetId(), xi->GetId(), 1);
        }
    }
    for(auto bi = A.VertexBegin(); bi != A.VertexEnd(); bi++){
        A.update_vertex_msg(bi->GetId());
        std::cout << "vertex-" << bi->GetId() << ": [ ";
        for(auto ki = bi->AdjecentsBegin(); ki != bi->AdjecentsEnd(); ki++){
            std::cout << (*ki)->GetId() << ", ";
        }
        std::cout << "] " << std::endl;
    }
    for(auto ei = A.EdgeBegin(); ei != A.EdgeEnd(); ei++){
        typename decltype(A)::Vertex& v1 = (*ei).StartV();
        typename decltype(A)::Vertex& v2 = (*ei).EndV();
        std::cout << "<edge:" << v1.GetId() << ", " << v2.GetId() << ">" << std::endl;
    }
    using vertex_id = decltype(A)::vertex_id;
    std::vector<vertex_id> sss;
    A.DFS(1, &sss, [](decltype(A)::Vertex& vx, void* ss){
                std::cout << vx.GetId() << std::endl;
                std::vector<vertex_id>* p_v = reinterpret_cast<std::vector<vertex_id>*>(ss);
                if(p_v == nullptr) return;
                p_v->push_back(vx.GetId());
            });
    auto xxx = A.DFS_GTree(1);
    auto& yyy = GTreeToBGTree(*xxx);
    std::cout << yyy << std::endl;
    std::cout << *xxx << std::endl;
    std::cout << "Graph is connected ? " << std::boolalpha << A.IsConnected() << std::endl;
    delete xxx;
    return;
} //}

void testB() //{
{
    DenseGraph<double, size_t> A(400, false);
    std::srand(time(nullptr));
    for(auto bi = A.VertexBegin(); bi != A.VertexEnd(); bi++) {
        for(auto xi = A.VertexBegin(); xi != A.VertexEnd(); xi++){
            if(std::rand() % 4 == 0)
                A.SetWeight(bi->GetId(), xi->GetId(), std::rand() % 2000);
        }
    }
    size_t ee;
    auto zzz = A.MST_Kruskal(ee);
    for(auto bi = zzz.begin(); bi != zzz.end(); ++bi) {
        size_t ww; A.GetWeight(bi->first, bi->second, ww);
        std::cout << "f: " << bi->first << ", s: " << bi->second  << ", w: " << ww << std::endl;
    }
    std::cout << "kruskal total weight is: " << ee << std::endl;
    return;
} //}

void testD() //{
{
    DenseGraph<double, size_t> A(1000, false);
    std::srand(time(nullptr));
    for(auto bi = A.VertexBegin(); bi != A.VertexEnd(); bi++) {
        for(auto xi = A.VertexBegin(); xi != A.VertexEnd(); xi++){
            if(std::rand() % 4 == 0)
                A.SetWeight(bi->GetId(), xi->GetId(), std::rand() % 2000);
        }
    }
    size_t aa;
    auto qqq = A.MST_Prim(aa);
    std::cout << "prim    total weight is: " << aa << std::endl;
    return;
} //}

void testC() //{
{
    DenseGraph<double, size_t> A(20, true);
    A.NewVertex(100.2);
    A.NewVertex(100.2);
    A.NewVertex(100.2);
    A.NewVertex(100.2);
    A.NewVertex(100.2);
    A.NewVertex(100.2);
    A.DeleteVertex(3);
    A.DeleteVertex(2);
    std::srand(time(nullptr));
    for(auto bi = A.VertexBegin(); bi != A.VertexEnd(); bi++) {
        for(auto xi = A.VertexBegin(); xi != A.VertexEnd(); xi++){
            if(std::rand() % 4 == 0)
                A.SetWeight(bi->GetId(), xi->GetId(), 1);
        }
    }
    for(auto bi = A.VertexBegin(); bi != A.VertexEnd(); bi++){
        A.update_vertex_msg(bi->GetId());
        std::cout << "vertex-" << bi->GetId() << ": [ ";
        for(auto ki = bi->AdjecentsBegin(); ki != bi->AdjecentsEnd(); ki++){
            std::cout << (*ki)->GetId() << ", ";
        }
        std::cout << "] " << std::endl;
    }
    for(auto ei = A.EdgeBegin(); ei != A.EdgeEnd(); ei++){
        typename decltype(A)::Vertex& v1 = (*ei).StartV();
        typename decltype(A)::Vertex& v2 = (*ei).EndV();
        std::cout << "<edge:" << v1.GetId() << ", " << v2.GetId() << ">" << std::endl;
    }
    using vertex_id = decltype(A)::vertex_id;
    std::vector<vertex_id> sss;
    A.DFS(1, &sss, [](decltype(A)::Vertex& vx, void* ss){
                std::cout << vx.GetId() << std::endl;
                std::vector<vertex_id>* p_v = reinterpret_cast<std::vector<vertex_id>*>(ss);
                if(p_v == nullptr) return;
                p_v->push_back(vx.GetId());
            });
    auto xxx = A.DFS_GTree(1);
    auto& yyy = GTreeToBGTree(*xxx);
    std::cout << yyy << std::endl;
    std::cout << *xxx << std::endl;
    std::cout << "Graph is connected ? " << std::boolalpha << A.IsConnected() << std::endl;
    delete xxx;
    return;
} //}

int main()
{
    testD();
    return 0;
}
