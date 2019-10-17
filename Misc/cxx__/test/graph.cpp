#include<ctime>
#include<ios>
#include<iostream>
#include<iomanip>

#include<cstdlib>

#include<unistd.h>
#include "../src/utils/type.hpp"
#include "../src/utils/logger.h"
#include "../src/Graph/Graph.hpp"

void testA()
{
    DenseGraph<double, size_t> A(88, true);
    A.NewVertex(100.2);
    std::srand(time(nullptr));
    for(auto bi = A.VertexBegin(); bi != A.VertexEnd(); bi++){
        for(auto xi = A.VertexBegin(); xi != A.VertexEnd(); xi++){
            if(std::rand() % 2 == 0)
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
    A.BFS(1, &sss, [](decltype(A)::Vertex& vx, void* ss){
                std::cout << vx.GetId() << std::endl;
                std::vector<vertex_id>* p_v = reinterpret_cast<std::vector<vertex_id>*>(ss);
                if(p_v == nullptr) return;
                p_v->push_back(vx.GetId());
            });
    return;
}

int main()
{
    testA();
    return 0;
}
