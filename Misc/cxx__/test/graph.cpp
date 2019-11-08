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

void testE() //{
{
    DenseGraph<double, size_t> A(1000, false);
    std::srand(time(nullptr));
    for(auto bi = A.VertexBegin(); bi != A.VertexEnd(); bi++) {
        for(auto xi = A.VertexBegin(); xi != A.VertexEnd(); xi++){
            if(std::rand() % 4 == 0)
                A.SetWeight(bi->GetId(), xi->GetId(), std::rand() % 2000);
        }
    }
    bool as = A.__SingleSourceShortestPath_Dijkstra(1);
    std::cout << "find short path: " << (as ? "success" : "failed") << std::endl;
    return;
} //}

void testF() //{
{
    DenseGraph<double, size_t> A(1000, false);
    std::srand(time(nullptr));
    for(auto bi = A.VertexBegin(); bi != A.VertexEnd(); bi++) {
        for(auto xi = A.VertexBegin(); xi != A.VertexEnd(); xi++){
            if(std::rand() % 4 == 0)
                A.SetWeight(bi->GetId(), xi->GetId(), std::rand() % 2000);
        }
    }
    auto as = A.ShortPathFromTo(1, 8);
    std::cout << "find short path: " << (as.first.size() ? "success" : "failed") << std::endl;
    if(as.first.size() == 0) return;
    auto ci = std::ostream_iterator<size_t>(std::cout, ", ");
    std::copy(as.first.begin(), as.first.end(), ci);
    std::cout << std::endl << "Total weight: " << as.second << ", Vertex: " << as.first.size() << std::endl;
    return;
} //}

void testG() //{
{
    DenseGraph<double, size_t> A(10, false);
    std::srand(time(nullptr));
    for(auto bi = A.VertexBegin(); bi != A.VertexEnd(); bi++) {
        for(auto xi = A.VertexBegin(); xi != A.VertexEnd(); xi++){
            if(std::rand() % 2 == 0)
                A.SetWeight(bi->GetId(), xi->GetId(), std::rand() % 2000);
        }
    }
    auto as = A.AllPairShortPath_Dijkstra_Bellman_Ford();
    std::cout << as.first << std::endl;
    std::cout << as.second << std::endl;
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
    testG();
    return 0;
}
