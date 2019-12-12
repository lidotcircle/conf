#ifndef DUMMY_HEAD_HPP_
#define DUMMY_HEAD_HPP_

#include <bitset>
#include <iostream>

#include <cmath>
#include <cassert>
#include <cstdio>
#include <cstring>

constexpr double constexpr_ceil(double x)
{
    long i = x;
    return (i - x) >= 0 ? i : i + 1;
}
constexpr double constexpr_floor(double x)
{
    long i = x;
    return (x - i) >= 0 ? i : i - 1;
}

template<typename _IDT, size_t _fmagic, size_t _bitArea>
class file_head //{
{
    static_assert(std::is_integral<_IDT>::value, "");
    public:
        static constexpr size_t id_type_len = constexpr_ceil(sizeof(_IDT) / 8.0) * 8;
        static constexpr size_t fmagic_true = constexpr_ceil(_fmagic / 8.0) * 8;
        static constexpr size_t bit_area    = constexpr_ceil(_bitArea / 8.0) * 8;
        static constexpr size_t total_len   = id_type_len + fmagic_true + bit_area / 8;
        typedef _IDT id_type;

    private:
        unsigned char         m_magic[fmagic_true];
        unsigned char         m_first_id[id_type_len];
        std::bitset<bit_area> m_bit_switch;

    public:
        inline bool test (id_type i) {assert(i >= 0); return m_bit_switch.test(i);}
        inline void set  (id_type i) {assert(i >= 0 && i <= (id_type)bit_area); this->m_bit_switch.set(i);}
        inline void clear(id_type i) {assert(i >= 0 && i <= (id_type)bit_area); this->m_bit_switch.reset(i);}

        bool first_avil_id(id_type& out) // FIX
        {
            for(id_type i = 0; i<(id_type)bit_area; ++i) {
                if(!this->test(i)) {
                    out = i;
                    return true;
                }
            }
            return false;
        }

        bool write_to_file(FILE* out_file)
        {
            if(::fseek(out_file, 0, SEEK_SET)) return false;
            if(::fwrite(this, sizeof(file_head), 1, out_file) != 1) return false;
            return true;
        }

        bool read_from_file(FILE* in_file)
        {
            if(::fseek(in_file, 0, SEEK_SET)) return false;
            if(::fread(this, sizeof(file_head), 1, in_file) != 1) return false;
            return true;
        }

        bool operator[](const id_type& idx) const 
        {
            assert(idx >= 0 && idx < bit_area); 
            return this->m_bit_switch[idx];
        }

        inline id_type& first_id(){return *(id_type*)this->m_first_id;}
        
        file_head() = delete;
        ~file_head() = default;
        file_head(const std::string& what)
        {
            size_t slen = what.size() >= _fmagic ? _fmagic : what.size();
            ::memcpy(this->m_magic, what.c_str(), slen);
        }
        file_head(const char* what): file_head(std::string(what)){}
}; //}

template class file_head<int, 4, 20>;

#endif // DUMMY_HEAD_HPP_
