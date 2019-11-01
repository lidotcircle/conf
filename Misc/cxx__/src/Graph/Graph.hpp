#ifndef _GRAPH_HPP
#define _GRAPH_HPP

#include <type_traits>
#include <vector>
#include <functional>
#include <algorithm>
#include <stdexcept>

#include <queue>
#include <stack>

#include <cmath>

#include "../utils/type.hpp"
#include "../utils/logger.h"
#include "../Tree/GTree.hpp"

template<typename VD, typename EW>
class BaseGraph  //{
{
    public:
        typedef VD     value_type;
        typedef EW     edge_weight;
        typedef size_t vertex_id;

    protected:
        class Vertex //{
        {
            private:
                value_type           m_msg;
                std::vector<Vertex*> m_adjecents;
                vertex_id            m_id;
                bool                 m_is_visited;

            public:
                typedef decltype(m_adjecents.begin()) IterType;
                auto AdjecentsBegin() {return m_adjecents.begin();}
                auto AdjecentsEnd  () {return m_adjecents.end  ();}
                value_type& GetData() {return this->m_msg;}
                vertex_id     GetId() const  {return this->m_id;}
                void          SetId(const vertex_id& id) {this->m_id = id;}
                std::vector<Vertex*>& GetAdj(){return this->m_adjecents;}

                Vertex(const value_type& data, const vertex_id& id): m_msg(data), m_id(id){}
                Vertex(value_type&& data, const vertex_id& id): m_msg(std::move(data)), m_id(id){}
                Vertex(): m_msg(), m_id(0), m_adjecents(){}
                bool& Visited() {return this->m_is_visited;}

                bool operator==(const Vertex& _o){return _o.m_id == this->m_id;}
                bool operator!=(const Vertex& _o){return !this->operator==(_o);}
        }; //}
        class Edge //{
        {
            private:
                Vertex *m_vertex_f, *m_vertex_s;
            public:
                Edge(): m_vertex_f(nullptr), m_vertex_s(nullptr){}
                Edge(Vertex* a, Vertex* b): m_vertex_f(a), m_vertex_s(b){}
                Vertex& StartV(){return *m_vertex_f;}
                Vertex& EndV  (){return *m_vertex_s;}
        }; //}

    public:
    virtual bool Directed () = 0;
    virtual bool GetWeight(const vertex_id& v1, const vertex_id& v2, edge_weight& _out) = 0;
    virtual bool GetWeight(const Vertex& v1, const Vertex& v2, edge_weight& _out){
        return GetWeight(v1.GetId(), v2.GetId(), _out);
    };
    virtual void NewVertex(const value_type&) = 0;
    virtual bool DeleteVertex(const vertex_id&) = 0;
    virtual bool DeleteVertex(const Vertex& v){
        return this->DeleteVertex(v.GetId());
    }
    virtual bool SetWeight(const vertex_id&, const vertex_id&, const edge_weight& w) = 0;
    virtual bool SetWeight(const Vertex& v1, const Vertex& v2, const edge_weight& w){
        return SetWeight(v1.GetId(), v2.GetId(), w);
    }
    virtual bool GetVertex(const vertex_id&, Vertex** _out) = 0;
    virtual bool GetVertex(const vertex_id&, Vertex&  _out) = 0;

    virtual bool BFS(const vertex_id& begin_vertex, void* obj, void (*func)(Vertex& c_p, void* _somethingHanppend)) = 0;
    virtual bool DFS(const vertex_id& begin_vertex, void* obj, void (*func)(Vertex& c_p, void* _somethingHanppend)) = 0;
    /*
    virtual VertexIterator VertexBegin() = 0;
    virtual VertexIterator VertexEnd  () = 0;
    virtual EdgeIterator   EdgeBegin  () = 0;
    virtual EdgeIterator   EdgeEnd    () = 0;
    */
}; //}

template<typename EW>
class DenseGraphMatrix //{
{
    public:
        typedef EW     weight_type;
        typedef size_t vertex_id;
    private:
        const bool   m_is_directed;
              size_t m_valid_counter;
              size_t m_vertex_counter;
        std::vector<std::pair<bool, weight_type>> m_data;
        std::vector<vertex_id> m_map;

        class edge_pair_iterator //{
        {
            public:
                typedef size_t                         difference_type;
                typedef std::pair<size_t, size_t> value_type;
                typedef value_type&                    reference;
                typedef value_type*                    pointer;
                typedef std::input_iterator_tag        iterator_catogory;
            private:
                DenseGraphMatrix* m_target;
                size_t            m_current_valid;
                size_t            m_current_row, m_current_column;
                bool              m_is_end;
                void calculate_next() //{
                {
                    if(m_is_end) throw *new std::out_of_range("iterator out of range");
                        for(;m_current_row<=m_target->m_vertex_counter; m_current_row++){
                            for(; m_current_column<=m_target->m_vertex_counter; m_current_column++){
                                if(m_target->get_m_data(m_current_row, m_current_column).first)
                                    goto iter__out_this__;
                            }
                            m_current_column = 1;
                        }
iter__out_this__:
                        if(m_current_row > m_target->m_vertex_counter || !m_target->get_m_data(m_current_row, m_current_column).first)
                            m_is_end = true;
                        return;
                } //}

            public:
                edge_pair_iterator(DenseGraphMatrix* target): //{
                    m_target(target),
                    m_current_valid(target->m_valid_counter), 
                    m_current_row(1), 
                    m_current_column(1),
                    m_is_end(false){
                        this->calculate_next();
                    } //}
                edge_pair_iterator(): m_is_end(true){}
                edge_pair_iterator& operator++() //{
                {
                    if(m_current_valid != m_target->m_valid_counter)
                        throw *new std::runtime_error("Access iterator after data is modified, which is invalid.");
                    if(m_current_column != m_target->m_vertex_counter){
                        m_current_column++;
                        this->calculate_next();
                        return *this;
                    }
                    m_current_column = 1;
                    if(m_current_row == m_target->m_vertex_counter){
                        this->m_is_end = true;
                        return *this;
                    }
                    ++m_current_row;
                    calculate_next();
                    return *this;
                } //}
                edge_pair_iterator  operator++(int) //{
                {
                    edge_pair_iterator ret(*this);
                    this->operator++();
                    return ret;
                } //}
                value_type operator*() //{
                {
                    if(m_is_end) throw *new std::out_of_range("end of iterator.");
                    /*
                    return std::make_pair(m_target->m_map[m_current_row - 1],
                            m_target->m_map[m_current_column - 1]);
                    */
                    return std::make_pair(m_current_row, m_current_column);
                } //}
        bool operator==(const edge_pair_iterator& _o){return _o.m_is_end == this->m_is_end;}
        bool operator!=(const edge_pair_iterator& _o){return !this->operator==(_o);}
        }; //}
//        friend class edge_pair_iterator;
        class adjecents_iterator //{
        {
            public:
                typedef size_t                  difference_type;
                typedef vertex_id               value_type;
                typedef value_type&             reference;
                typedef value_type*             pointer;
                typedef std::input_iterator_tag iterator_catogory;

            private:
                value_type        m_self_loc;
                value_type        m_current;
                DenseGraphMatrix* mp_dm;

                void update_next() //{
                {
                    m_current++;
                    for(;m_current<=mp_dm->m_vertex_counter;m_current++){
                        if(mp_dm->get_m_data(m_self_loc, m_current).first) break;
                    }
                    if(m_current > mp_dm->m_vertex_counter || !mp_dm->get_m_data(m_self_loc, m_current).first){
                        m_self_loc = 0;
                        m_current  = 0;
                        mp_dm      = nullptr;
                    }
                } //}

            public:
                adjecents_iterator(DenseGraphMatrix* mm, value_type m_id): m_current(0), mp_dm(mm){
                    if(mm != nullptr)
                        m_self_loc = mp_dm->reverse_map(m_id);
                    if(!m_self_loc) throw *new std::domain_error("adjecents_iterator: unknown vertex.");
                    update_next();
                }
                adjecents_iterator(): m_self_loc(0), m_current(0), mp_dm(nullptr){}

                bool operator==(const adjecents_iterator& _o){
                    return this->mp_dm      == _o.mp_dm
                        && this->m_self_loc == _o.m_self_loc
                        && this->m_current  == _o.m_current;}
                bool operator!=(const adjecents_iterator& _o){return !this->operator==(_o);}
                value_type operator*(){
                    if(mp_dm == nullptr) throw *new std::out_of_range("iterator out of range"); 
//                    return mp_dm->m_map[m_current - 1]; // or return m_current 
                    return m_current;
                }
                adjecents_iterator& operator++(){update_next();return *this;}
                adjecents_iterator  operator++(int){adjecents_iterator ret(*this); update_next();return ret;}
        }; //}

        std::pair<bool, weight_type>& get_m_data(size_t i, size_t j) //{
        { // TODO: out_of_range
            if(m_is_directed)
                return m_data[(i - 1) * m_vertex_counter + j - 1];
            else {
                if(i > j)
                    std::swap(i, j);
                return m_data[i * (m_vertex_counter + 1) + j - i * (i + 1) / 2 - m_vertex_counter - 1];
            }
        } //}
        size_t search_place(const vertex_id& id) //{
        {
            auto x = std::lower_bound(m_map.begin(), m_map.end(), id);
            if(*x == id) return std::distance(m_map.begin(), x) + 1;
            else return 0;
        } //}
        void m_data_delete_vertex(size_t _p) //{ delete is expensive ...
        {
            std::vector<std::pair<bool, weight_type>> new_data;
            if(m_is_directed){
                for(size_t i = 1; i<=m_vertex_counter; i++){
                    if(i == _p) continue;
                    for(size_t j = 1; i<=m_vertex_counter; i++){
                        if(j == _p) continue;
                        new_data.push_back(get_m_data(i, j));
                    }
                }
            } else {
                for(size_t i = 1; i<=m_vertex_counter; i++){
                    if(i == _p) continue;
                    for(size_t j = i; i<=m_vertex_counter; i++){
                        if(j == _p) continue;
                        new_data.push_back(get_m_data(i, j));
                    }
                }
            }
            this->m_data = new_data;
            return;
        } //}
        void m_data_add_vertex() //{ add is also expensive
        {
            std::vector<std::pair<bool, weight_type>> new_data;
            if(m_is_directed){
                for(size_t i = 1; i<=m_vertex_counter+1; i++){
                    if(i == m_vertex_counter + 1){
                        for(size_t k = 1; k<=m_vertex_counter+1; k++)
                            new_data.push_back(std::make_pair(false, weight_type(0)));
                        continue;
                    }
                    for(size_t j = 1; j<=m_vertex_counter+1; j++){
                        if(j == m_vertex_counter + 1) new_data.push_back(std::make_pair(false, weight_type(0)));
                        new_data.push_back(get_m_data(i, j));
                    }
                }
            } else {
                for(size_t i = 1; i<=m_vertex_counter+1; i++){
                    for(size_t j = i; j<=m_vertex_counter; j++){ // so stupid
                        new_data.push_back(get_m_data(i, j));
                    }
                    new_data.push_back(std::make_pair(false, weight_type(0)));
                }
            }
            this->m_data = new_data;
            return;
        } //}
        
    public:
        using edgeIter = edge_pair_iterator;
        DenseGraphMatrix(const bool x = false): m_is_directed(x), m_valid_counter(0){}
        DenseGraphMatrix(size_t num_vex, const bool x = false): //{
            m_is_directed(x), 
            m_valid_counter(0),
            m_vertex_counter(num_vex){
                if(m_is_directed)
                    this->m_data.resize(num_vex * num_vex);
                else 
                    this->m_data.resize((num_vex * num_vex + num_vex) / 2);
                for(size_t i = 1; i<=num_vex; i++) m_map.push_back(i);
            } //}

        // vertex <a> and <b> is connected reutrn true, else if then return false
        bool GetWeight(const vertex_id& a, const vertex_id& b, weight_type& _out) //{
        {
            size_t p1 = this->search_place(a);
            size_t p2 = this->search_place(b);

            if(p1 == 0 || p2 == 0){return false;}
            auto& xyz = this->get_m_data(p1, p2);
            if(!xyz.first) return false;
            _out = xyz.second;
            return true;
        } //} 
        bool SetWeight(const vertex_id& a, const vertex_id& b, const weight_type& _in) //{
        {
            size_t p1 = this->search_place(a);
            size_t p2 = this->search_place(b);
            if(p1 == 0 || p2 == 0){return false;}
            this->get_m_data(p1, p2).first = true;
            this->get_m_data(p1, p2).second = _in;
            this->m_valid_counter++;
            return true;
        } //} 
        bool DeleteVertex(const vertex_id& id) //{
        {
            size_t _p;
            if((_p = this->search_place(id)) == 0)
                return false;
            m_map.erase(m_map.begin() + _p - 1);
            this->m_data_delete_vertex(_p);
            this->m_vertex_counter--;
            this->m_valid_counter++;
            return true;
        } //}
        bool NewVertex(const vertex_id& id) //{
        {
            if(id <= m_map.back()) return false;
            m_map.push_back(id);
            this->m_data_add_vertex();
            this->m_vertex_counter++;
            this->m_valid_counter++;
            return true;
        } //}
        edge_pair_iterator EdgeBegin() {return edge_pair_iterator(this);}
        edge_pair_iterator EdgeEnd  () {return edge_pair_iterator();}

        inline size_t reverse_map(const vertex_id& id){return this->search_place(id);}

        adjecents_iterator AdjBegin(const vertex_id& id){return adjecents_iterator(this, id);}
        adjecents_iterator AdjEnd  (const vertex_id&)   {return adjecents_iterator();}
}; //}

template<typename EW>
class SparseGraphList //{
{
}; //}

template<typename VD, typename EW>
class DenseGraph: public BaseGraph<VD, EW> //{
{
    public:
        using value_type  = typename BaseGraph<VD, EW>::value_type;
        using edge_weight = typename BaseGraph<VD, EW>::edge_weight;
        using vertex_id   = typename BaseGraph<VD, EW>::vertex_id;
        using Vertex      = typename BaseGraph<VD, EW>::Vertex;
        using Edge        = typename BaseGraph<VD, EW>::Edge;

        typedef DenseGraphMatrix<edge_weight> EdgeHolder ;

    private:
        EdgeHolder                 m_edge;
        std::vector<Vertex>      m_vertex;
        vertex_id                m_max_id;
        const bool          m_is_directed;
        logger                   m_logger;

        class edge_iterator //{
        {
            public:
                typedef size_t                  difference_type;
                typedef Edge                    value_type;
                typedef value_type&             reference;
                typedef value_type*             pointer;
                typedef std::input_iterator_tag iterator_catogory;

                using edge_iter_inn = typename DenseGraph::EdgeHolder::edgeIter;

            private:
                Edge  m_current_edge;
                DenseGraph* mp_dg;
                edge_iter_inn m_inner_iter;

                void update_next() //{
                {
                    if(mp_dg == nullptr || mp_dg->m_edge.EdgeEnd() == m_inner_iter)
                        throw *new std::out_of_range("iterator out of range.");
                    m_inner_iter++;
                    if(m_inner_iter == mp_dg->m_edge.EdgeEnd())
                        m_current_edge = Edge();
                    else {
                        auto xyz = *m_inner_iter;
                        m_current_edge = Edge(&mp_dg->m_vertex[xyz.first - 1], &mp_dg->m_vertex[xyz.second - 1]);
                    }
                    return;
                } //}

            public:
                edge_iterator(): m_current_edge(), mp_dg(nullptr), m_inner_iter(){}
                edge_iterator(DenseGraph* p_dg): mp_dg(p_dg){
                    if(mp_dg != nullptr){
                        m_inner_iter = mp_dg->m_edge.EdgeBegin();
                    }
                    if(m_inner_iter != mp_dg->m_edge.EdgeEnd()){
                        auto xyz = *m_inner_iter;
                        m_current_edge = Edge(&mp_dg->m_vertex[xyz.first - 1], &mp_dg->m_vertex[xyz.second - 1]); // keep consistent with updata_next()
                    }
                }
                bool operator==(const edge_iterator& _o){
                        return this->m_inner_iter == _o.m_inner_iter;
                }
                bool operator!=(const edge_iterator& _o){return !this->operator==(_o);}
                edge_iterator& operator++(){this->update_next();return *this;}
                edge_iterator  operator++(int){edge_iterator ret(*this); this->operator++();return ret;}
                Edge& operator*(){return this->m_current_edge;}
        }; //}

    public:
        using VertexIterator = decltype(m_vertex.begin());
        using EdgeIterator   = edge_iterator;

        DenseGraph(size_t num_vex = 0, bool _directed = false): 
            m_edge(num_vex, _directed), 
            m_vertex(num_vex), 
            m_max_id(num_vex), 
            m_is_directed(_directed), 
            m_logger("DenseGraph-" + random_string()){
                m_logger.begin_log(); m_logger.ostream() << "Create an instance of DenseGraph" << std::endl;
                for(vertex_id i = 1; i<=m_max_id; i++){
                    m_vertex[i - 1].SetId(i);
                }
            }

#define LOGGER(str) m_logger.begin_log(); m_logger.ostream() << str << std::endl;
        bool Directed(){
            LOGGER("Directed() called.");
            return this->m_is_directed; 
        }
        bool GetWeight(const vertex_id& v1, const vertex_id& v2, edge_weight& _out){
            m_logger.begin_log(); m_logger.ostream() << "GetWeight(" << v1 << ", " << v2 << ", _out)" << std::endl; 
            return m_edge.GetWeight(v1, v2, _out);
        }
        void NewVertex(const value_type& _d){
            m_logger.begin_log(); m_logger.ostream() << "NewVertex(" << _d << ")" << std::endl; 
            m_vertex.push_back(Vertex(_d, ++m_max_id));
            m_edge.NewVertex(m_max_id);
        }
        bool DeleteVertex(const vertex_id& _v){
            m_logger.begin_log(); m_logger.ostream() << "DeleteVertex(" << _v << ")" << std::endl; 
            size_t _p = this->reverseMap(_v);
            if(_p == 0) return false;
            m_vertex.erase(m_vertex.begin() + _p - 1);
            return m_edge.DeleteVertex(_v);
        }
        bool SetWeight(const vertex_id& a, const vertex_id& b, const edge_weight& w = 0){
            m_logger.begin_log(); m_logger.ostream() << "SetWeight(" << a << ", " << b << ", " << w << ")" << std::endl; 
            return m_edge.SetWeight(a, b, w);
        }
        size_t reverseMap(const vertex_id& id){return m_edge.reverse_map(id);}
        bool GetVertex(const vertex_id& id, Vertex** _out){
            size_t x = this->reverseMap(id);
            if(x == 0) return false;
            *_out = &this->m_vertex[x - 1];
            return true;
        };
        bool GetVertex(const vertex_id& id, Vertex& _out){
            size_t x = this->reverseMap(id);
            if(x == 0) return false;
            _out = this->m_vertex[x - 1];
            return true;
        };
        bool update_vertex_msg(const vertex_id& id) //{
        {
            size_t loc = reverseMap(id);
            if(loc == 0) return false;
            Vertex& dest_vex = m_vertex[loc - 1];
            for(auto bi = m_edge.AdjBegin(id); bi != m_edge.AdjEnd(id); bi++){
                dest_vex.GetAdj().push_back(&m_vertex[*bi - 1]);
            }
            return true;
        } //}
        VertexIterator VertexBegin(){return m_vertex.begin();}
        VertexIterator VertexEnd  (){return m_vertex.end();}
        EdgeIterator   EdgeBegin()  {return EdgeIterator(this);}
        EdgeIterator   EdgeEnd  ()  {return EdgeIterator();}

        bool BFS(const vertex_id& begin_vertex, void* obj, void (*func)(Vertex& c_p, void* _somethingHanppend)) //{
        {
            std::queue<vertex_id> traverse_queue;
            for(auto bi = this->VertexBegin(); bi != this->VertexEnd(); ++bi)
                bi->Visited() = false;
            size_t x = this->reverseMap(begin_vertex);
            if(x != 0) traverse_queue.push(begin_vertex); else return false;
            m_vertex[x - 1].Visited() = true;
            while(!traverse_queue.empty()){
                size_t loc = this->reverseMap(traverse_queue.front());
                traverse_queue.pop();
                Vertex& vx = m_vertex[loc - 1];
                this->update_vertex_msg(vx.GetId());
                func(vx, obj);
                for(auto bi = vx.AdjecentsBegin(); bi != vx.AdjecentsEnd(); bi++){
                    if((*bi)->Visited() == false){
                        traverse_queue.push((*bi)->GetId());
                        (*bi)->Visited() = true;
                    }
                }
            }
            return true;
        } //}
        bool DFS(const vertex_id& begin_vertex, void* obj, void (*func)(Vertex& c_p, void* _somethingHanppend)) //{
        {
            std::stack<std::pair<Vertex*, typename Vertex::IterType>> traverse_stack;
            for(auto bi = this->VertexBegin(); bi != this->VertexEnd(); ++bi)
                bi->Visited() = false;
            size_t x = this->reverseMap(begin_vertex);
            if(x == 0) return false;
            Vertex* first = &m_vertex[x - 1];
            first->Visited() = true;
            func(*first, obj); // Visit
            if(x != 0) traverse_stack.push(std::make_pair(first, first->AdjecentsBegin())); else return false;
            while(!traverse_stack.empty()) {
                std::pair<Vertex*, typename Vertex::IterType>& top_elem = traverse_stack.top();
                for(;top_elem.second != top_elem.first->AdjecentsEnd() && (*top_elem.second)->Visited() == true;++top_elem.second){}
                if(top_elem.first->AdjecentsEnd() == top_elem.second) {
                    traverse_stack.pop();
                    continue;
                }
                Vertex*& cur = *top_elem.second++;
                func(*cur, obj);
                cur->Visited() = true;
                traverse_stack.push(std::make_pair(cur, cur->AdjecentsBegin()));
            }
            return true;
        } //}

        GTree<vertex_id, Vertex*>* DFS_GTree(const vertex_id& begin_vertex) //{ keep same logic between DFS and BFS
        {
            GTree<vertex_id, Vertex*>* ret_tree;
            std::stack<std::tuple<vertex_id, typename Vertex::IterType, GTree<vertex_id, Vertex*>*>> traverse_stack;
            for(auto bi = this->VertexBegin(); bi != this->VertexEnd(); ++bi)
                bi->Visited() = false;
            size_t x = this->reverseMap(begin_vertex);
            if(x == 0) return NULL;
            Vertex& first = m_vertex[x - 1];
            first.Visited() = true;
            ret_tree = new GTree<vertex_id, Vertex*>(begin_vertex, &first);
            traverse_stack.push(std::make_tuple(begin_vertex, first.AdjecentsBegin(), ret_tree));
            while(!traverse_stack.empty()) {
                std::tuple<vertex_id, typename Vertex::IterType, GTree<vertex_id, Vertex*>*> _top_elem = traverse_stack.top();
                size_t loc = this->reverseMap(std::get<0>(_top_elem));
                Vertex& vx = m_vertex[loc - 1];
                for(;std::get<1>(_top_elem) != vx.AdjecentsEnd() && (*std::get<1>(_top_elem))->Visited() == true; ++std::get<1>(_top_elem)){}
                if(vx.AdjecentsEnd() == std::get<1>(_top_elem)) {
                    traverse_stack.pop();
                    continue;
                }
                Vertex& vx_new = *(*std::get<1>(_top_elem));
                vx_new.Visited() = true;
                GTree<vertex_id, Vertex*>* current_tree = new GTree<vertex_id, Vertex*>(vx_new.GetId(), &vx_new);
                std::get<2>(_top_elem)->new_child(current_tree);
                traverse_stack.push(make_tuple(vx_new.GetId(), vx_new.AdjecentsBegin(), current_tree));
            }
            return ret_tree;
        } //}
        GTree<vertex_id, Vertex*>* BFS_GTree(const vertex_id& begin_vertex) //{ keep same logic between DFS and BFS
        {
            GTree<vertex_id, Vertex*>* ret_tree;
            std::queue<std::pair<vertex_id, GTree<vertex_id, Vertex*>*>> traverse_queue;
            for(auto bi = this->VertexBegin(); bi != this->VertexEnd(); ++bi)
                bi->Visited() = false;
            size_t x = this->reverseMap(begin_vertex);
            if(x != 0) traverse_queue.push(std::make_pair(begin_vertex, nullptr)); else return nullptr;
            m_vertex[x - 1].Visited() = true;
            while(!traverse_queue.empty()){
                std::pair<vertex_id, GTree<vertex_id, Vertex*>*> _pair = traverse_queue.front();
                size_t loc = this->reverseMap(_pair.first);
                Vertex& vx = m_vertex[loc - 1];
                GTree<vertex_id, Vertex*>* current_tree = new GTree<vertex_id, Vertex*>(_pair.first, &vx);
                if(_pair.second == nullptr) {
                    ret_tree = current_tree;
                } else {
                    _pair.second->new_child(current_tree);
                }
                traverse_queue.pop();
                this->update_vertex_msg(vx.GetId());
                for(auto bi = vx.AdjecentsBegin(); bi != vx.AdjecentsEnd(); bi++){
                    if((*bi)->Visited() == false) {
                        traverse_queue.push(std::make_pair((*bi)->GetId(), current_tree));
                        (*bi)->Visited() = true;
                    }
                }
            }
            return ret_tree;
        } //}
}; //}

// mimimum spaning tree
// short path from one peer to another peer
// short path from any peer to others
// ADT: TREE, PATH

template class DenseGraphMatrix<size_t>;
template class DenseGraph<double, size_t>;

#endif //_GRAPH_HPP
