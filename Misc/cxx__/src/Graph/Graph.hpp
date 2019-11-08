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
#include "../PriorityQueue/FibonacciHeap.hpp"
#include "../PriorityQueue/BinaryHeap.hpp"
#include "../Set/DisjointSet.hpp"
#include "../Matrix/matrix.hpp"

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
                void*                m_external_data;

            public:
                edge_weight m_prim_weight;
                vertex_id   m_prim_connected;

                typedef decltype(m_adjecents.begin()) IterType;
                auto AdjecentsBegin() {return m_adjecents.begin();}
                auto AdjecentsEnd  () {return m_adjecents.end  ();}
                value_type& GetData() {return this->m_msg;}
                vertex_id     GetId() const  {return this->m_id;}
                void          SetId(const vertex_id& id) {this->m_id = id;}
                std::vector<Vertex*>& GetAdj(){return this->m_adjecents;}
                void*&        GetExternalData(){return this->m_external_data;}

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
                void*   m_external_data;
            public:
                Edge(): m_vertex_f(nullptr), m_vertex_s(nullptr){}
                Edge(Vertex* a, Vertex* b): m_vertex_f(a), m_vertex_s(b){}
                Vertex& StartV(){return *m_vertex_f;}
                Vertex& EndV  (){return *m_vertex_s;}
                const Vertex& StartV() const{return *m_vertex_f;}
                const Vertex& EndV  () const{return *m_vertex_s;}
                void*&  GetExternalData(){return this->m_external_data;}
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
    static_assert(std::is_convertible<EW, int>::value, "weight type must be convertible for <int type>");
    static_assert(std::is_convertible<int, EW>::value, "weight type must be convertible for <int type>");
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
        inline vertex_id map2vertex_id(const size_t& pos) const {if(pos == 0 || pos > this->m_map.size()) return 0; return this->m_map[pos - 1];}

        adjecents_iterator AdjBegin(const vertex_id& id){return adjecents_iterator(this, id);}
        adjecents_iterator AdjEnd  (const vertex_id&)   {return adjecents_iterator();}

        void Shortest_path_closure();
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
        constexpr static bool NEGATIVE_WEIGHT = (std::numeric_limits<edge_weight>::min() < edge_weight(0));

    private:
        EdgeHolder                 m_edge;
        std::vector<Vertex>      m_vertex;
        vertex_id                m_max_id;
        const bool          m_is_directed;
#ifdef GRAPH_LOGGER_
        logger                   m_logger;
#endif // GRAPH_LOGGER__

        class edge_iterator //{
        {
            public:
                typedef size_t                  difference_type;
                typedef Edge                    value_type;
                typedef value_type&             reference;
                typedef value_type*             pointer;
                typedef std::input_iterator_tag iterator_category;

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
        GTree<vertex_id, Vertex*>* BFS_GTree_helper(const vertex_id& begin_vertex) //{
        {
            GTree<vertex_id, Vertex*>* ret_tree;
            std::queue<std::pair<vertex_id, GTree<vertex_id, Vertex*>*>> traverse_queue;
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
        GTree<vertex_id, Vertex*>* DFS_GTree_helper(const vertex_id& begin_vertex) //{
        {
            GTree<vertex_id, Vertex*>* ret_tree;
            std::stack<std::tuple<vertex_id, typename Vertex::IterType, GTree<vertex_id, Vertex*>*>> traverse_stack;
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

    public:
        using VertexIterator = decltype(m_vertex.begin());
        using EdgeIterator   = edge_iterator;

        DenseGraph(size_t num_vex = 0, bool _directed = false): 
            m_edge(num_vex, _directed), 
            m_vertex(num_vex), 
            m_max_id(num_vex), 
            m_is_directed(_directed)
#ifdef GRAPH_LOGGER_
            , m_logger("DenseGraph-" + random_string())
#endif // GRAPH_LOGGER__
            {
#ifdef GRAPH_LOGGER_
                m_logger.begin_log(); m_logger.ostream() << "Create an instance of DenseGraph" << std::endl;
#endif // GRAPH_LOGGER__
                for(vertex_id i = 1; i<=m_max_id; i++){
                    m_vertex[i - 1].SetId(i);
                }
            }

#define LOGGER(str) m_logger.begin_log(); m_logger.ostream() << str << std::endl;
        bool Directed(){
#ifdef GRAPH_LOGGER_
            LOGGER("Directed() called.");
#endif // GRAPH_LOGGER__
            return this->m_is_directed; 
        }
        bool GetWeight(const vertex_id& v1, const vertex_id& v2, edge_weight& _out){
#ifdef GRAPH_LOGGER_
            m_logger.begin_log(); m_logger.ostream() << "GetWeight(" << v1 << ", " << v2 << ", _out)" << std::endl; 
#endif // GRAPH_LOGGER__
            return m_edge.GetWeight(v1, v2, _out);
        }
        void NewVertex(const value_type& _d){
#ifdef GRAPH_LOGGER_
            m_logger.begin_log(); m_logger.ostream() << "NewVertex(" << _d << ")" << std::endl; 
#endif // GRAPH_LOGGER__
            m_vertex.push_back(Vertex(_d, ++m_max_id));
            m_edge.NewVertex(m_max_id);
        }
        bool DeleteVertex(const vertex_id& _v){
#ifdef GRAPH_LOGGER_
            m_logger.begin_log(); m_logger.ostream() << "DeleteVertex(" << _v << ")" << std::endl; 
#endif // GRAPH_LOGGER__
            size_t _p = this->reverseMap(_v);
            if(_p == 0) return false;
            m_vertex.erase(m_vertex.begin() + _p - 1);
            return m_edge.DeleteVertex(_v);
        }
        bool SetWeight(const vertex_id& a, const vertex_id& b, const edge_weight& w = 0){
#ifdef GRAPH_LOGGER_
            m_logger.begin_log(); m_logger.ostream() << "SetWeight(" << a << ", " << b << ", " << w << ")" << std::endl; 
#endif // GRAPH_LOGGER__
            return m_edge.SetWeight(a, b, w);
        }
        size_t reverseMap(const vertex_id& id){return m_edge.reverse_map(id);}
        vertex_id Map2VertexId(const size_t& pos) const {return this->m_edge.map2vertex_id(pos);}
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
            dest_vex.GetAdj().resize(0);
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
            for(auto bi : this->m_vertex) {
                this->update_vertex_msg(bi.GetId());
                bi.Visited() = false;
            }
            std::stack<std::pair<Vertex*, typename Vertex::IterType>> traverse_stack;
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

        GTree<vertex_id, Vertex*>* DFS_GTree(const vertex_id& begin_vertex) //{
        {
            for(auto bi : this->m_vertex){
                this->update_vertex_msg(bi.GetId());
                bi.Visited() = false;
            }
            return this->DFS_GTree_helper(begin_vertex);
        } //}
        GTree<vertex_id, Vertex*>* BFS_GTree(const vertex_id& begin_vertex) //{
        {
            for(auto bi = this->VertexBegin(); bi != this->VertexEnd(); ++bi)
                bi->Visited() = false;
            return this->BFS_GTree_helper(begin_vertex);
        } //}

    bool IsConnected() //{
    {
        size_t num_of_connected_node = 0;
        if(m_vertex.size() == 0) return true;
        vertex_id first_id = this->Map2VertexId(1);
        this->BFS(first_id, &num_of_connected_node, [](Vertex& v, void* n) -> void {
                    (void) v; // unused
                    int& nn = *(int*)n;
                    ++nn;
                });
        if(num_of_connected_node == m_vertex.size()) return true;
        return false;
    } //}

        class BinaryHeapEdgeVV: public BinaryHeap_IMP<Edge> //{
        {
            public:
                using KeyValueType = typename BinaryHeap_IMP<Edge>::KeyValueType;
                using ContainerType = typename BinaryHeap_IMP<Edge>::ContainerType;
                using ItemSize = typename BinaryHeap_IMP<Edge>::ItemSize;
                using ReturnIter = typename BinaryHeap_IMP<Edge>::ReturnIter;
            protected:
                DenseGraph* m_graph;
                bool kv_less(const KeyValueType& a, const KeyValueType& b) //{
                {
                     edge_weight aw; bool ab = m_graph->GetWeight(a.StartV().GetId(), a.EndV().GetId(), aw);
                     edge_weight bw; bool bb = m_graph->GetWeight(b.StartV().GetId(), b.EndV().GetId(), bw);
                     if(!(ab && bb)) throw *new std::runtime_error("Graph fault error.");
                     return aw < bw;
                } //}
                bool kv_equal(const KeyValueType& a, const KeyValueType& b) //{
                {
                     edge_weight aw; bool ab = m_graph->GetWeight(a.StartV().GetId(), a.EndV().GetId(), aw);
                     edge_weight bw; bool bb = m_graph->GetWeight(b.StartV().GetId(), b.EndV().GetId(), bw);
                     if(!(ab && bb)) throw *new std::runtime_error("Graph fault error.");
                     return aw == bw;
                } //}
            public:
                BinaryHeapEdgeVV(DenseGraph* dg): BinaryHeap_IMP<Edge>(), m_graph(dg){}
        }; //}
        class DisjointSetKruskal__: public DisjointSetForest_IMP<Vertex*> //{
        {
        public:
            using ElemType = typename DisjointSetForest_IMP<Vertex*>::ElemType;
            using ItemSize = typename DisjointSetForest_IMP<Vertex*>::ItemSize;
            using elem__   = typename DisjointSetForest_IMP<Vertex*>::__elem;
        protected:
             elem__* GetPointer(const ElemType& e){ return (elem__*)e->GetExternalData();}
             void    SetPointer(ElemType& e, elem__* p){e->GetExternalData() = p;}
        }; //}
        class FibonacciHeapVertexVV: public FibonacciHeap_IMP<Vertex*, edge_weight> //{
        {
            public:
                using KeyValueType  = typename FibonacciHeap_IMP<Vertex*, edge_weight>::KeyValueType;
                using NewValueType  = typename FibonacciHeap_IMP<Vertex*, edge_weight>::NewValueType;
                using ContainerType = typename FibonacciHeap_IMP<Vertex*, edge_weight>::ContainerType;
                using ItemSize      = typename FibonacciHeap_IMP<Vertex*, edge_weight>::ItemSize;
                using ReturnIter    = typename FibonacciHeap_IMP<Vertex*, edge_weight>::ReturnIter;
                using __elem        = typename FibonacciHeap_IMP<Vertex*, edge_weight>::__elem;
            protected:
                bool    kv_less(const KeyValueType& a, const KeyValueType& b)
                {
//                    std::cout << a->m_prim_weight << " < " << b->m_prim_weight << " = " << std::boolalpha << (a->m_prim_weight < b->m_prim_weight) << std::endl;
                    return a->m_prim_weight < b->m_prim_weight;
                }
                bool    kv_equal(const KeyValueType& a, const KeyValueType& b){return a->m_prim_weight == b->m_prim_weight;}
                __elem* GetPointer(const KeyValueType& e){__elem* ret = (__elem*)e->GetExternalData(); assert(ret != nullptr); return ret;}
                void    SetPointer(KeyValueType& e, __elem* p){e->GetExternalData() = p;}
                void    SetNewValue(KeyValueType& e, const NewValueType& v){e->m_prim_weight = v;}
            public:
                NewValueType GetValueType(KeyValueType e){return e->m_prim_weight;}
        }; //}

        bool __SingleSourceShortestPath_Dijkstra(const vertex_id& begin_v) //{
        {
            auto max = FibonacciHeap_IMP_HELP::max_elem_only<edge_weight>();
            FibonacciHeapVertexVV fbh;
            Vertex* begin_vertex; if(!this->GetVertex(begin_v, &begin_vertex)) return false;
            for(auto bi = this->m_vertex.begin(); bi != this->m_vertex.end(); ++bi){
                this->update_vertex_msg(bi->GetId());
                bi->m_prim_weight    = max;
                bi->m_prim_connected = -1;
                fbh.Add(&(*bi));
            }
            fbh.DecreaseKey(begin_vertex, 0);
            begin_vertex->m_prim_connected = begin_v;
            while(!fbh.empty()) {
                Vertex* vv = fbh.ExtractMin();
                if(vv->m_prim_connected == (vertex_id)-1) return true;
                for(auto bi = vv->AdjecentsBegin(); bi != vv->AdjecentsEnd(); ++bi){
                    edge_weight etmp;
                    if(!this->GetWeight(vv->GetId(), (*bi)->GetId(), etmp)) continue;
                    if((*bi)->GetExternalData() == nullptr) continue;
                    edge_weight vtmp = fbh.GetValueType(*bi);
                    if((*bi)->m_prim_weight + etmp < vtmp) {
                        fbh.DecreaseKey((*bi), (*bi)->m_prim_weight = etmp + (*bi)->m_prim_weight);
                        (*bi)->m_prim_connected = vv->GetId();
                    }
                }
            }
            return true;
        } //}
        bool __SingleSourceShortestPath_Bellman_Ford(const vertex_id& begin_v) //{ assuming no negative cycle
        {
            Vertex* begin_vertex; if(!this->GetVertex(begin_v, &begin_vertex)) return false;
            auto max = FibonacciHeap_IMP_HELP::max_elem_only<edge_weight>();
            for(auto bi = this->m_vertex.begin(); bi != this->m_vertex.end(); ++bi) {
                bi->m_prim_weight    = max;
                bi->m_prim_connected = 0;
            }
            begin_vertex->m_prim_weight = 0;
            begin_vertex->m_prim_connected = begin_v;
            for(size_t i = 1; i<this->m_vertex.size(); ++i) {
                for(auto bi = this->EdgeBegin(); bi != this->EdgeEnd(); ++bi) {
                    edge_weight www;
                    if(!this->GetWeight((*bi).StartV().GetId(), (*bi).EndV().GetId(), www)) continue;
                    if((*bi).StartV().m_prim_weight == max) continue; // avoid arithmetic overflow
                    edge_weight xxx = www + (*bi).StartV().m_prim_weight;
                    if( xxx < (*bi).EndV().m_prim_weight) {
                        (*bi).EndV().m_prim_weight    = xxx;
                        (*bi).EndV().m_prim_connected = (*bi).StartV().GetId();
                    }
                }
            }
            return true;
        } //}
        std::pair<std::vector<vertex_id>, edge_weight> ShortPathFromTo(const vertex_id& src, const vertex_id& dst) //{
        {
            std::vector<vertex_id> ret;
            edge_weight max__ = FibonacciHeap_IMP_HELP::max_elem_only<edge_weight>();
            if(NEGATIVE_WEIGHT){
                if(!__SingleSourceShortestPath_Bellman_Ford(src))return std::make_pair(ret, max__);
            }
            else {
                if(!__SingleSourceShortestPath_Dijkstra(src))return std::make_pair(ret, max__);
            }
            Vertex* vvv;
            if(!this->GetVertex(dst, &vvv)) return std::make_pair(ret, max__);
            if(vvv->m_prim_weight == max__) return std::make_pair(ret, max__);
            max__ = vvv->m_prim_weight;
            for(; vvv->m_prim_connected != src; this->GetVertex(vvv->m_prim_connected, &vvv))
                ret.push_back(vvv->GetId());
            if(ret.size() == 0) ret.push_back(dst);
            ret.push_back(src);
            return std::make_pair(ret, max__);
        } //}
//        void AllPairShortPath_Floyd_Warshall();
        std::pair<SquareMatrix<vertex_id>, SquareMatrix<edge_weight>> AllPairShortPath_Dijkstra_Bellman_Ford() //{ TODO NEGATIVE WEIGHT RESULT MAPPING
        {
            std::vector<edge_weight>  map_weight;
            std::vector<vertex_id>    path_map__;
            std::vector<edge_weight>  weight_map__;
            if(NEGATIVE_WEIGHT) { // remaping weight
                this->NewVertex(value_type());
                Vertex& last = m_vertex[m_vertex.size() - 1];
                for(auto bi = m_vertex.begin(); bi != m_vertex.end(); ++bi) {
                    if(last.GetId() != bi->GetId())
                        this->SetWeight(last.GetId(), bi->GetId(), edge_weight(0));
                }
                this->__SingleSourceShortestPath_Bellman_Ford(last.GetId());
                this->DeleteVertex(last.GetId());
                for(auto bi = m_vertex.begin(); bi != m_vertex.end(); ++bi)
                    map_weight.push_back(bi->m_prim_weight);
                for(auto bi = this->EdgeBegin(); bi != this->EdgeEnd(); ++bi) {
                    Vertex& a = (*bi).StartV();
                    Vertex& b = (*bi).EndV();
                    edge_weight w_tmp;
                    this->GetWeight(a.GetId(), b.GetId(), w_tmp);
                    size_t ap = this->reverseMap(a.GetId());
                    size_t bp = this->reverseMap(b.GetId());
                    w_tmp = w_tmp + map_weight[ap] - map_weight[bp];
                    this->SetWeight(a.GetId(), b.GetId(), w_tmp);
                }
            }
            for(auto vi = m_vertex.begin(); vi != m_vertex.end(); ++vi) {
                this->__SingleSourceShortestPath_Dijkstra(vi->GetId());
                for(auto bi = m_vertex.begin(); bi != m_vertex.end(); ++bi) {
                    path_map__.push_back(bi->m_prim_connected);
                    if(!NEGATIVE_WEIGHT) weight_map__.push_back(bi->m_prim_weight);
                }
            }
            SquareMatrix<edge_weight> path_map = std::move(path_map__);
            if(!NEGATIVE_WEIGHT) return std::make_pair(path_map, weight_map__);
            SquareMatrix<edge_weight> weight_map(path_map.ColumnSize());
            for(auto bi = this->EdgeBegin(); bi != this->EdgeEnd(); ++bi) {
                Vertex& a = (*bi).StartV();
                Vertex& b = (*bi).EndV();
                edge_weight w_tmp;
                this->GetWeight(a.GetId(), b.GetId(), w_tmp);
                size_t ap = this->reverseMap(a.GetId());
                size_t bp = this->reverseMap(b.GetId());
                w_tmp = w_tmp - map_weight[ap] + map_weight[bp];
                this->SetWeight(a.GetId(), b.GetId(), w_tmp);
            }
            return std::make_pair(path_map, weight_map__); // TODO, From here
        } //}

        std::vector<std::pair<vertex_id, vertex_id>> MST_Prim(edge_weight& out) //{
        {
            out = edge_weight(0);
            FibonacciHeapVertexVV fbh;
            vertex_id root = this->m_vertex[0].GetId();
            Vertex* root_v; this->GetVertex(root, &root_v);
            for(auto bi = this->m_vertex.begin(); bi != this->m_vertex.end(); ++bi){
                this->update_vertex_msg(bi->GetId());
                bi->m_prim_weight    = FibonacciHeap_IMP_HELP::max_elem_only<edge_weight>();
                bi->m_prim_connected = -1;
                fbh.Add(&(*bi));
            }
            root_v->m_prim_connected = 0;
            fbh.DecreaseKey(root_v, 0);
            std::vector<std::pair<vertex_id, vertex_id>> edge_keep;
            while(!fbh.empty()) {
                Vertex* vv = fbh.ExtractMin();
                if(vv->m_prim_connected == (vertex_id)-1) throw *new std::runtime_error("try to get MST of unconnnected graph");
                if(vv->m_prim_connected != 0) {
                    edge_keep.push_back(std::make_pair(vv->m_prim_connected, vv->GetId()));
                    edge_weight tmp_w;
                    this->GetWeight(vv->m_prim_connected, vv->GetId(), tmp_w);
                    out += tmp_w;
                }
                for(auto bi = vv->AdjecentsBegin(); bi != vv->AdjecentsEnd(); ++bi){
                    edge_weight etmp;
                    this->GetWeight(vv->GetId(), (*bi)->GetId(), etmp);
                    if((*bi)->GetExternalData() == nullptr) continue;
                    edge_weight vtmp = fbh.GetValueType(*bi);
                    if(etmp < vtmp) {
                        fbh.DecreaseKey(*bi, etmp);
                        (*bi)->m_prim_connected = vv->GetId();
                    }
                }
            }
            return edge_keep;
        } //}
        std::vector<std::pair<vertex_id, vertex_id>> MST_Prim(){edge_weight e; return MST_Prim(e);}
        std::vector<std::pair<vertex_id, vertex_id>> MST_Kruskal(edge_weight& weight_out) //{
        {
            DisjointSetKruskal__ DSK;
            BinaryHeapEdgeVV     PriorityQueueX(this);
            edge_weight          total_weight(0);
            std::vector<std::pair<vertex_id, vertex_id>> edge_keep;
            for(auto bi = this->VertexBegin(); bi != this->VertexEnd(); ++bi)
                DSK.MakeSet(&(*bi));
            PriorityQueueX.Add(this->EdgeBegin(), this->EdgeEnd());
            while(!PriorityQueueX.empty()) {
                Edge min = PriorityQueueX.ExtractMin();
                if(DSK.FindSet(&min.StartV()) == DSK.FindSet(&min.EndV()))
                    continue;
                edge_keep.push_back(std::make_pair(min.StartV().GetId(), min.EndV().GetId()));
                DSK.UnionWith(&min.StartV(), &min.EndV());
                edge_weight tmp_weight(0);
                this->GetWeight(min.StartV().GetId(), min.EndV().GetId(), tmp_weight);
                total_weight += tmp_weight;
            }
            if(edge_keep.size() != this->m_vertex.size() - 1) throw *new std::runtime_error("try to get MST of unconnnected graph");
            weight_out = total_weight;
            return edge_keep;
        } //}
        std::vector<std::pair<vertex_id, vertex_id>> MST_Kruskal() {edge_weight e; return MST_Kruskal(e);}
}; //}

// mimimum spaning tree
// short path from one peer to another peer
// short path from any peer to others
// ADT: TREE, PATH

template class DenseGraphMatrix<size_t>;
template class DenseGraph<double, size_t>;

#endif //_GRAPH_HPP
