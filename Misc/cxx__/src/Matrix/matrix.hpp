#ifndef MATRIX_HPP_
#define MATRIX_HPP_

#include <cstdlib>
#include <cassert>
#include <cmath>
#include <limits>

#include <stdexcept>
#include <tuple>
#include <vector>
#include <iostream>
#include <iomanip>

namespace MATRIX_HELPER
{
    template<typename T, typename std::enable_if<std::numeric_limits<T>::min() >= 0, int>::type = 0>
    inline T abs(T a){return a;}
    template<typename T, typename std::enable_if<std::numeric_limits<T>::min() <  0, int>::type = 0>
    inline T abs(T a){return std::abs(a);}
}

template<typename V>
class GenericVector //{
{
    static_assert(std::is_convertible<V, double>::value, "V must be convertible to double");
    static_assert(std::is_convertible<double, V>::value, "V must be convertible to double");

    public:
        typedef V      ValueType;
        typedef size_t ItemSize;

    public:
        virtual const ValueType& operator[](const ItemSize& pos) const = 0;
        virtual ValueType& operator[](const ItemSize& pos) = 0;

        virtual GenericVector<ValueType>& GetEmptyVector() const = 0;
        virtual void ReInitializeWithSize(const ItemSize& size) = 0;

        virtual GenericVector<ValueType>& operator=(const std::vector<ValueType>& vv) //{
        {
            this->ReInitializeWithSize(vv.size());
            ItemSize i = 1;
            for(auto bi = vv.begin(); bi != vv.end(); ++bi, ++i)
                this->operator[](i) = *bi;
            return *this;
        } //}

        virtual GenericVector<ValueType>& operator=(const GenericVector<ValueType>& vv) //{
        {
            this->ReInitializeWithSize(vv.GetSize());
            for(ItemSize i = 1; i <= this->GetSize(); ++i)
                this->operator[](i) = vv[i];
            return *this;
        } //}

        virtual ~GenericVector(){};

        virtual ValueType norm2() //{
        {
            ValueType ret = 0;
            for(ItemSize i = 1; i<=this->GetSize(); ++i){
                ret += std::pow(MATRIX_HELPER::abs((*this)[i]), 2);
            }
            return std::sqrt(ret);
        } //}

        /// <summary>The size</summary>
        virtual ItemSize GetSize() const = 0;
        virtual void SetValue(ValueType (*func)(ItemSize)) //{
        {
            for(ItemSize i = 1; i<=this->GetSize(); ++i)
                this->operator[](i) = func(i);
            return;
        } //}
        virtual void ClearToZero() {this->SetValue([](ItemSize) -> ValueType { return 0;});}

        virtual GenericVector<ValueType>& operator+=(const GenericVector<ValueType>& _oth) //{
        {
            assert(this->GetSize() == _oth.GetSize());
            for(ItemSize i = 1; i<=this->GetSize(); ++i){
                this->operator[](i) += _oth.operator[](i);
            }
            return *this;
        } //}
        virtual GenericVector<ValueType>& operator-=(const GenericVector<ValueType>& _oth) //{
        {
            assert(this->GetSize() == _oth.GetSize());
            for(ItemSize i = 1; i<=this->GetSize(); ++i){
                this->operator[](i) -= _oth.operator[](i);
            }
            return *this;
        } //}
}; //}

template<typename V>
class CVector: public GenericVector<V> //{
{
    public:
        using ValueType     = typename GenericVector<V>::ValueType;
        using ItemSize      = typename GenericVector<V>::ItemSize;
        using DataContainer = std::vector<ValueType>;

    private:
        DataContainer m_data;
        ItemSize m_size;

    public:
        CVector(): m_data(), m_size(0) {}
        CVector(const ItemSize& s): m_data(s), m_size(s) {}
        ~CVector() = default;

        ItemSize GetSize() const {return this->m_size;}
        const ValueType& operator[](const ItemSize& pos) const  //{
        {
            if(pos <= 0 || pos > this->m_size) throw *new std::out_of_range("Vector access out of range.");
            return this->m_data[pos - 1];
        } //}
        ValueType& operator[](const ItemSize& pos) //{
        {
            if(pos <= 0 || pos > this->m_size) throw *new std::out_of_range("Vector access out of range.");
            return this->m_data[pos - 1];
        } //}

        CVector& GetEmptyVector() const{return *new CVector();}
        void ReInitializeWithSize(const ItemSize& size) //{
        {
            this->m_size = size;
            this->m_data.resize(this->m_size);
            return;
        } //}

        CVector<ValueType> operator+(const GenericVector<ValueType>& _oth) //{
        {
            CVector<ValueType> ret;
            ret = (*this);
            ret += _oth;
            return ret;
        } //}
        CVector<ValueType> operator-(const GenericVector<ValueType>& _oth) //{
        {
            CVector<ValueType> ret;
            ret = (*this);
            ret -= _oth;
            return ret;
        } //}
}; //}

#define ALMOST_ZERO 0.000000000001
template<typename V>
class GenericMatrix //{
{
    public:
        typedef size_t ItemSize;
        typedef V      ValueType;

        /// <summary> The implementation of this class should just delegate the origin data </summary>
        class GVector //{
        {
            public:
                typedef V      ValueType;
                typedef size_t ItemSize;

            public:
                /// <summary>Return the value</summary>
                /// <param name="pos">Range start at 1</param>
                virtual const ValueType& operator[](const ItemSize& pos) const = 0;
                virtual ValueType& operator[](const ItemSize& pos) {
                    return const_cast<ValueType&>(const_cast<const std::remove_pointer_t<decltype(this)>*>(this)->operator[](pos));
                }

                /// <summary>The size</summary>
                virtual ItemSize GetVectorSize() = 0;
        }; //}

    public:
        virtual ItemSize ColumnSize()       = 0;
        virtual ItemSize RowSize()          = 0;
        virtual ItemSize ColumnSize() const {
            return const_cast<typename std::remove_const_t<std::remove_pointer_t<decltype(this)>>*>(this)->ColumnSize();
        }
        virtual ItemSize RowSize() const {
            return const_cast<typename std::remove_const_t<std::remove_pointer_t<decltype(this)>>*>(this)->RowSize();
        }

        /// <summary> Change the size of this matrix, this may clean current data </summary>
        virtual void ReInitializeWithSize(const ItemSize& row, const ItemSize& column) = 0;
        virtual GenericMatrix& GetEmptyMatrix() = 0;

        virtual ~GenericMatrix(){};

        /// <summary> Transpose current matrix </summary>
        virtual void Transpose() //{
        {
            GenericMatrix& result = this->GetEmptyMatrix();
            result.ReInitializeWithSize(this->ColumnSize(), this->RowSize());
            for(ItemSize i=1; i<=this->RowSize(); ++i)
                for(ItemSize j=1; j<=this->ColumnSize(); ++j)
                    result.get(j, i) = this->get(i, j);
            *this = std::move(result);
            delete &result;
            return;
        } //}

        /// <summary> used to determine whether this matrix is SquareMatrix </summary>
        virtual bool IsSquareMatrix(){return this->ColumnSize() == this->RowSize();};

        /// <summary> If isn't square matrix raise error. </summary>
        virtual ValueType Determinant() //{
        {
            if(!this->IsSquareMatrix())
                throw *new std::logic_error("Determinant isn't avaliable for matrix that isn't square matrix");
            std::tuple<GenericMatrix*, GenericMatrix*, ItemSize> result = this->GaussElimination();
            ItemSize rank = std::get<2>(result);
            assert(rank <= this->RowSize());
            ValueType result_val = 0;
            if(rank != this->RowSize())
                goto determinant_return;
            result_val = 1;
            for(ItemSize s = 1; s<=this->RowSize(); ++s)
                result_val *= (*std::get<0>(result)).get(s, s);
determinant_return:
            delete std::get<0>(result);
            delete std::get<1>(result);
            return result_val;
        } //}

        /// <summary> utilize gauss eliminationo to get rank of matrix
        virtual ItemSize  Rank() //{
        {
            std::tuple<GenericMatrix*, GenericMatrix*, ItemSize> result = this->GaussElimination();
            ItemSize rank = std::get<2>(result);
            delete std::get<0>(result);
            delete std::get<1>(result);
            return rank;
        } //}

        void SetValue(ValueType (*func)(ItemSize i, ItemSize j)) //{
        {
            for(ItemSize i = 1; i<=this->RowSize(); ++i)
                for(ItemSize j = 1; j<=this->ColumnSize(); ++j)
                    this->get(i, j) = func(i, j);
        } //}
        void SetValue(ValueType (*func)(ItemSize i, ItemSize j, ValueType origin)) //{
        {
            for(ItemSize i = 1; i<=this->RowSize(); ++i)
                for(ItemSize j = 1; j<=this->ColumnSize(); ++j)
                    this->get(i, j) = func(i, j, this->get(i, j));
        } //}
        void SetValue(ValueType (*func)(ItemSize i, ItemSize j, ValueType oth), const GenericMatrix& matrix) //{
        {
            for(ItemSize i = 1; i<=this->RowSize(); ++i)
                for(ItemSize j = 1; j<=this->ColumnSize(); ++j)
                    this->get(i,j) = func(i, j, matrix.get(i, j));
        } //}

        /// <summary> gauss elimination - row transform </summary>
        virtual std::tuple<GenericMatrix*, GenericMatrix*, ItemSize> GaussElimination() //{
        {
            GenericMatrix& transf    = this->GetEmptyMatrix();
            GenericMatrix& copy_this = this->GetEmptyMatrix();
            transf.ReInitializeWithSize(this->RowSize(), this->RowSize());
            transf.SetValue([](ItemSize i, ItemSize j) -> ValueType {if(i == j) return 1; else return 0;});
            copy_this = *this;
            ItemSize rank = 0;
            for(ItemSize i = 1; i<=this->ColumnSize(); ++i) {
                ItemSize ix = rank + 1;
                for(; ix<=this->RowSize(); ix++) {
                    if(copy_this.get(ix, i) != 0) {
                        if(ix != rank + 1){
                            copy_this.Swap_Row(ix, rank + 1);
                            transf.   Swap_Row(ix, rank + 1);
                        }
                        break;
                    }
                }
                if(copy_this.get(rank + 1, i) == 0) // this column is all zero
                    continue;
                ++rank;
                if(rank == this->RowSize()) break; // row is full rank
                for(ItemSize i2 = rank+1; i2<=this->RowSize(); ++i2) {
                    ValueType factor2 = -copy_this.get(i2, i) / copy_this.get(rank, i);
                    if(factor2 == 0) continue;
                    copy_this.MultiplyAndAddTo_Row(factor2, rank, i2); // TODO
                    transf   .MultiplyAndAddTo_Row(factor2, rank, i2); // TODO
                }
            }
            return std::make_tuple(&copy_this, &transf, rank);
        } //}

        virtual GenericMatrix<ValueType>& GaussElimination_SquareMatrix_Inverse() //{
        {
            assert(this->ColumnSize() == this->RowSize()); // square matrix
            GenericMatrix& transf    = this->GetEmptyMatrix();
            GenericMatrix& copy_this = this->GetEmptyMatrix();
            transf.ReInitializeWithSize(this->RowSize(), this->RowSize());
            transf.SetValue([](ItemSize i, ItemSize j) -> ValueType {if(i == j) return 1; else return 0;});
            copy_this = *this;
            for(ItemSize i = 1; i<=this->ColumnSize(); ++i) {
                for(ItemSize ix = i; ix<=this->RowSize(); ix++) {
                    if(copy_this.get(ix, i) != 0) {
                        if(ix != i){
                            copy_this.Swap_Row(ix, i);
                            transf.   Swap_Row(ix, i);
                        }
                        break;
                    }
                }
                if(copy_this.get(i, i) == 0) // this column is all zero
                    throw *new std::logic_error("singular matrix");
                ValueType factor1 =  1 / copy_this.get(i, i);
                copy_this.MultiplyWith_Row(factor1, i);
                transf   .MultiplyWith_Row(factor1, i);
                for(ItemSize i2 = i+1; i2<=this->RowSize(); ++i2) {
                    ValueType factor2 = -copy_this.get(i2, i);
                    if(factor2 == 0) continue;
                    copy_this.MultiplyAndAddTo_Row(factor2, i, i2);
                    transf   .MultiplyAndAddTo_Row(factor2, i, i2);
                }
            }
            for(ItemSize i = 1; i<=this->RowSize(); ++i) {
                for(ItemSize j = i + 1; j<=this->ColumnSize(); ++j) {
                    copy_this.MultiplyAndAddTo_Column(-copy_this.get(i, j), i, j);
                    transf   .MultiplyAndAddTo_Column(-copy_this.get(i, j), i, j);
                }
            }
            delete &copy_this;
            return transf;
        } //}

        /// <summary> solve the linear equation Ax = b </summary>
        /// <param name="this">Arbitrary mxn matrix</param>
        /// <param name="vec"> Ax = b, here b = vec</param>
        /// <return> first vector is a solution of Ax = b. if existing more than 1 vector
        ///          which means the vectors behind first is kernel of A, Ax = 0.
        ///          When Ax = b doesn't exist a solution then return is empty vector. </return>
        virtual std::vector<GenericVector<ValueType>*> SolveLinearEquationEX(const GenericVector<ValueType>& vec) //{
        {
            std::tuple<GenericMatrix*, GenericMatrix*, ItemSize> solve_part = this->GaussElimination();
            GenericMatrix<ValueType>& Eliminated_matrix = *std::get<0>(solve_part);
            GenericMatrix<ValueType>& Transform_matrix  = *std::get<1>(solve_part);
            ItemSize& rank                              =  std::get<2>(solve_part);
#ifdef MATRIX_DEBUG
            std::cout << "E1" << std::endl << Eliminated_matrix << std::endl;
            std::cout << "T1" << std::endl << Transform_matrix  << std::endl;
#endif // MATRIX_DEBUG
            std::vector<GenericVector<ValueType>*> ret;
            GenericVector<ValueType>& new_b = Transform_matrix.multiply_with_vector(vec);
            if(rank < new_b.GetSize()) {
                for(ItemSize i = rank + 1; i<= new_b.GetSize(); ++i)
                    if(new_b[i] != 0) {
                        delete &Eliminated_matrix;
                        delete &Transform_matrix;
                        delete &new_b;
                        return ret;
                    }
            }
            if(rank == 0) { // A = 0 and b = 0, so full vector space is the solution
                GenericVector<ValueType>* new_vec = &vec.GetEmptyVector();
                new_vec->ReInitializeWithSize(this->RowSize());
                new_vec->ClearToZero();
                ret.push_back(new_vec);
                for(ItemSize i = 1; i<=this->ColumnSize(); ++i){
                    new_vec = &vec.GetEmptyVector();
                    new_vec->ReInitializeWithSize(this->RowSize());
                    new_vec->ClearToZero();
                    new_vec->operator[](i) = 1;
                    ret.push_back(new_vec);
                }
                delete &Eliminated_matrix;
                delete &Transform_matrix;
                delete &new_b;
                return ret;
            }
            Eliminated_matrix.Transpose();
            std::tuple<GenericMatrix*, GenericMatrix*, ItemSize> solve_part2 = Eliminated_matrix.GaussElimination();
            assert(std::get<2>(solve_part2) == rank);
            GenericMatrix<ValueType>& Transform_matrixP  = *std::get<1>(solve_part2);
            Transform_matrixP.Transpose();
#ifdef MATRIX_DEBUG
            std::cout << "Transposed E" << std::endl;
            std::cout << Eliminated_matrix << std::endl;
            std::cout << "E2" << std::endl;
            std::cout << *std::get<0>(solve_part2) << std::endl;
            std::cout << "T2" << std::endl;
            std::cout << Transform_matrixP << std::endl;
#endif
            for(ItemSize k = 1; k<=rank; ++k)
                new_b[k] /= std::get<0>(solve_part2)->get(k, k);
            GenericVector<ValueType>* xyz = &(Transform_matrixP.multiply_with_vector(new_b));
            ret.push_back(xyz); // particular solution
            for(ItemSize k = rank + 1; k<=this->ColumnSize(); ++k) {
                xyz = &(vec.GetEmptyVector());
                xyz->ReInitializeWithSize(this->ColumnSize());
                xyz->ClearToZero();
                xyz->operator[](k) = 1;
                ret.push_back(&(Transform_matrixP.multiply_with_vector(*xyz)));
                delete xyz;
            }
            delete &Transform_matrix;
            delete &Transform_matrixP;
            delete &Eliminated_matrix;
            delete std::get<0>(solve_part2);
            delete &new_b;
            return ret;
        } //}

        virtual std::vector<GenericVector<ValueType>*> Kernel() //{
        {
            CVector<ValueType> zero(this->ColumnSize());
            zero.ClearToZero();
            std::vector<GenericVector<ValueType>*> ret = this->SolveLinearEquationEX(zero);
            delete ret[0];
            ret.erase(ret.begin());
            return ret;
        } //}

        /// <summary> Row transformations </summary>
        virtual void MultiplyWith_Row(const ValueType& val, const ItemSize& pos) //{
        {
            if(pos > this->RowSize()) throw *new std::out_of_range("Matrix Operation out of range");
            for(ItemSize i = 1; i<=this->ColumnSize(); ++i){
                this->get(pos, i) *= val;
            }
            return;
        } //}
        virtual void AddTo_Row(const ItemSize& src, const ItemSize& dst) //{
        {
            if(src > this->RowSize()) throw *new std::out_of_range("Matrix Operation out of range");
            if(dst > this->RowSize()) throw *new std::out_of_range("Matrix Operation out of range");
            for(ItemSize i = 1; i<=this->ColumnSize(); ++i)
                this->get(dst, i) += this->get(src, i);
            return;
        } //}
        virtual void MultiplyAndAddTo_Row(const ValueType& val, const ItemSize& src, const ItemSize& dst) //{
        {
            if(src > this->RowSize()) throw *new std::out_of_range("Matrix Operation out of range");
            if(dst > this->RowSize()) throw *new std::out_of_range("Matrix Operation out of range");
            for(ItemSize i = 1; i<=this->ColumnSize(); ++i) {
                this->get(dst, i) += this->get(src, i) * val;
                if(MATRIX_HELPER::abs(this->get(dst, i)) < ALMOST_ZERO) this->get(dst, i) = 0;
            }
            return;
        } //}
        virtual void Swap_Row(const ItemSize& row1, const ItemSize& row2) //{
        {
            if(row1 > this->RowSize()) throw *new std::out_of_range("Matrix Operation out of range");
            if(row2 > this->RowSize()) throw *new std::out_of_range("Matrix Operation out of range");
            for(ItemSize i = 1; i<=this->ColumnSize(); ++i)
                std::swap(this->get(row1, i), this->get(row2, i));
            return;
        } //}
        virtual void Insert_Row(const ItemSize& new_row) = 0;
        virtual void Insert_Row(const ItemSize& new_row, const GenericVector<ValueType>& correspond_vector) //{
        {
            if(this->ColumnSize() != correspond_vector.GetSize())
                throw *new std::logic_error("Unmatch size between vector and matrix.");
            this->Insert_Row(new_row);
            for(ItemSize i = 1; i<=this->ColumnSize(); ++i)
                this->get(new_row, i) = correspond_vector[i];
            return;
        } //}
        /// <summary> Column transformations </summary>
        virtual void MultiplyWith_Column(const ValueType& val, const ItemSize& pos) //{
        {
            if(pos > this->ColumnSize()) throw *new std::out_of_range("Matrix Operation out of range");
            for(ItemSize i = 1; i<=this->RowSize(); ++i){
                this->get(i, pos) *= val;
            }
            return;
        } //}
        virtual void AddTo_Column(const ItemSize& src, const ItemSize& dst) //{
        {
            if(src > this->ColumnSize()) throw *new std::out_of_range("Matrix Operation out of range");
            if(dst > this->ColumnSize()) throw *new std::out_of_range("Matrix Operation out of range");
            for(ItemSize i = 1; i<=this->RowSize(); ++i)
                this->get(i, dst) += this->get(i, src);
            return;
        } //}
        virtual void MultiplyAndAddTo_Column(const ValueType& val, const ItemSize& src, const ItemSize& dst) //{
        {
            if(src > this->ColumnSize()) throw *new std::out_of_range("Matrix Operation out of range");
            if(dst > this->ColumnSize()) throw *new std::out_of_range("Matrix Operation out of range");
            for(ItemSize i = 1; i<=this->RowSize(); ++i) {
                this->get(i, dst) += this->get(i, src) * val;
                if(MATRIX_HELPER::abs(this->get(i, dst)) < ALMOST_ZERO) this->get(i, dst) = 0;
            }
            return;
        } //}
        virtual void Swap_Column(const ItemSize& col1, const ItemSize& col2) //{
        {
            if(col1 > this->ColumnSize()) throw *new std::out_of_range("Matrix Operation out of range");
            if(col2 > this->ColumnSize()) throw *new std::out_of_range("Matrix Operation out of range");
            for(ItemSize i = 1; i<=this->ColumnSize(); ++i)
                std::swap(this->get(i, col1), this->get(i, col2));
            return;
        } //}
        virtual void Insert_Column(const ItemSize& new_col) = 0;
        virtual void Insert_Column(const ItemSize& new_col, const GenericVector<ValueType>& correspond_vector) //{
        {
            if(this->RowSize() != correspond_vector.GetSize())
                throw *new std::logic_error("Unmatch size between vector and matrix.");
            this->Insert_Column(new_col);
            for(ItemSize i = 1; i<=this->RowSize(); ++i)
                this->get(i, new_col) = correspond_vector[i];
            return;
        } //}

        /// <summary> Matrix Multiply ... </summary>
        /// <return>  NewMatrix           </return>
        virtual GenericMatrix& operator*=(const GenericMatrix& _oth) //{
        {
            if(this->ColumnSize() != _oth.RowSize())
                throw *new std::logic_error("Unmatch Matrix Multiply");
            GenericMatrix& ret_matrix = GetEmptyMatrix();
            ItemSize i = this->RowSize();
            ItemSize j = _oth.ColumnSize();
            ret_matrix.ReInitializeWithSize(i, j);
            ItemSize k = this->ColumnSize();
            for(;i > 0;++i){
                for(;j > 0;++j){
                    ValueType accumulated_value = 0;
                    for(ItemSize s=1; s<=k; ++s){
                        accumulated_value += this->get(i, k) * (_oth.get(k, j));
                    }
                    ret_matrix.get(i, j) = accumulated_value;
                }
            }
            (*this) = ret_matrix;
            delete &ret_matrix;
            return *this;
        } //}

        /// <summary> multiply with vector </summary>
        /// <return> don't forget free space </return>
        virtual GenericVector<ValueType>& multiply_with_vector(const GenericVector<ValueType>& _oth) //{
        {
            if(this->ColumnSize() != _oth.GetSize())
                throw *new std::out_of_range("Unmatch vector and matrix in multiply");
            GenericVector<ValueType>& ret_matrix = _oth.GetEmptyVector();
            ret_matrix.ReInitializeWithSize(this->RowSize());
            for(ItemSize i = 1; i<=this->RowSize(); ++i){
                ValueType accum = 0;
                for(ItemSize j = 1; j<=this->ColumnSize(); ++j){
                    accum += this->get(i, j) * _oth[j];
                }
                ret_matrix[i] = accum;
            }
            return ret_matrix;
        } //}

        /// <summary> Common plus and minus </summary>
        virtual GenericMatrix<ValueType>& operator+=(const GenericMatrix<ValueType>& _oth) //{
        {
            assert(this->RowSize() == _oth.RowSize() && this->ColumnSize() == _oth.ColumnSize());
            for(ItemSize i = 1; i<=this->RowSize(); ++i){
                for(ItemSize j = 1; j<=this->ColumnSize(); ++j){
                    this->get(i, j) += _oth.get(i, j);
                }
            }
            return *this;
        } //}
        virtual GenericMatrix<ValueType>& operator-=(const GenericMatrix<ValueType>& _oth) //{
        {
            assert(this->RowSize() == _oth.RowSize() && this->ColumnSize() == _oth.ColumnSize());
            for(ItemSize i = 1; i<=this->RowSize(); ++i){
                for(ItemSize j = 1; j<=this->ColumnSize(); ++j){
                    this->get(i, j) -= _oth.get(i, j);
                }
            }
            return *this;
        } //}

        /// <summary> An inefficient implementation, this doesn't requrire the size is equal </summary>
        virtual GenericMatrix& operator=(const GenericMatrix& _oth) //{
        {
            this->ReInitializeWithSize(_oth.RowSize(), _oth.ColumnSize());
            for(ItemSize i=1; i<=this->RowSize(); ++i)
                for(ItemSize j=1; j<=this->ColumnSize(); ++j)
                    this->get(i, j) = _oth.get(i, j);
            return *this;
        } //}

        virtual ValueType& get(const ItemSize& row, const ItemSize& col) = 0;
        virtual const ValueType& get(const ItemSize& row, const ItemSize& col) const //{
        {
            return const_cast<std::remove_const_t<std::remove_pointer_t<decltype(this)>>*>(this)->get(row, col);
        } //}

        /// <summary>Get a vector that represent correspond row vector</summary>
        /// <exception>It may be raise a std::out_of_range exception </exception>
        /// <param name="pos">Range start at 1</param>
        virtual GVector& operator[](const ItemSize& pos) const = 0; // stupid API
}; //}

template<typename V>
std::ostream& operator<<(std::ostream& os, const GenericMatrix<V>& m) //{
{
    using ItemSize = typename GenericMatrix<V>::ItemSize;
    for(ItemSize i = 1; i<=m.RowSize(); ++i){
        for(ItemSize j = 1; j<=m.ColumnSize(); ++j){
            os << std::fixed << std::setprecision(2) << std::setw(8) << m.get(i, j);
        }
        os << std::endl;
    }
    return os;
} //}

template<typename V>
std::ostream& operator<<(std::ostream& os, const GenericVector<V>& m) //{
{
    using ItemSize = typename GenericMatrix<V>::ItemSize;
    os << "(";
    for(ItemSize i = 1; i<=m.GetSize() - 1; ++i){
            os << std::fixed << std::setprecision(2) << m[i] << ", ";
    }
    if(m.GetSize() >= 1)
        os << m[m.GetSize()];
    os << ")";
    return os;
} //}

template<typename V>
class GenericSquareMatrix: public GenericMatrix<V> //{
{
    public: 
        using ValueType = typename GenericMatrix<V>::ValueType;
        using ItemSize  = typename GenericMatrix<V>::ItemSize;

    public:
        ItemSize ColumnSize() {return this->RowSize();}
        virtual void ReInitializeWithSize(const ItemSize& size) = 0;
        void ReInitializeWithSize(const ItemSize& row, const ItemSize& column) //{
        {
            if(row != column) throw *new std::logic_error("SquareMatrix ReInitializationi error.");
            this->ReInitializeWithSize(row);
            return;
        } //}

}; //}

template<typename V>
class SquareMatrix: public GenericSquareMatrix<V> //{
{
    public: 
        using ValueType = typename GenericMatrix<V>::ValueType;
        using ItemSize  = typename GenericMatrix<V>::ItemSize;
        using GVector   = typename GenericMatrix<V>::GVector;

    private:
        using DataContainer = std::vector<ValueType>;
        ItemSize      m_row_col;
        DataContainer m_data;

    public:
        class GVectorImp: public GVector //{
        {
            public:
                using ItemSize  = typename GVector::ItemSize;
                using ValueType = typename GVector::ValueType;
            private:
                ItemSize       m_start_pos;
                DataContainer& m_delegated_container;
                ItemSize       m_size;
            public:
                GVectorImp() = delete;
                GVectorImp(ItemSize s_p, DataContainer& container, ItemSize col_size):
                    m_start_pos(s_p), m_delegated_container(container), m_size(col_size){}
                const ValueType& operator[](const ItemSize& pos) const {
                    return m_delegated_container[pos + m_start_pos];
                }
                ItemSize GetVectorSize() //{
                {
                    return this->m_size;
                } //}
        }; //}

        SquareMatrix(): m_row_col(0), m_data(){}
        SquareMatrix(const ItemSize& size): m_row_col(size), m_data(m_row_col * m_row_col){}
        SquareMatrix(const SquareMatrix<ValueType>& _oth): SquareMatrix(){*this = _oth;}
        SquareMatrix(DataContainer&& _oth, ItemSize row_col = 0): m_row_col(row_col), m_data(std::forward<DataContainer>(_oth)) //{
        {
            ItemSize real_s = m_data.size();
            if(this->m_row_col * this->m_row_col == real_s)
                return;
            real_s = std::sqrt(real_s);
            if(real_s * real_s != m_data.size()) throw *new std::logic_error("invalidate data to initialize matrix");
            this->m_row_col = real_s;
        } //}
        SquareMatrix(SquareMatrix<ValueType>&& _oth): SquareMatrix(){*this = std::move(_oth);}
        ~SquareMatrix(){};

        ItemSize RowSize() {return this->m_row_col;}

        void ReInitializeWithSize(const ItemSize& size) //{
        {
            this->m_row_col = size;
            this->m_data.resize(this->m_row_col * this->m_row_col);
        } //}
        SquareMatrix& GetEmptyMatrix() //{
        {
            SquareMatrix* ret = new SquareMatrix();
            return *ret;
        } //}
        SquareMatrix& operator=(SquareMatrix&& _oth) //{
        {
            this->m_row_col = _oth.m_row_col;
            this->m_data = std::move(_oth.m_data);
            return *this;
        } //}
        SquareMatrix<ValueType>& operator=(const SquareMatrix<ValueType>& _oth) //{
        {
            *static_cast<GenericMatrix<ValueType>*>(this) = _oth;
            return *this;
        } //}
        GVector& operator[](const ItemSize& pos) const //{ Dangerous using as .get(, ), Inevitable Memory Leak
        {
            return *new GVectorImp((this->m_row_col - 1) * pos - 1, const_cast<DataContainer&>(this->m_data), this->m_row_col);
        }; //}

        void Insert_Row(const ItemSize& new_row) //{
        {
            if(new_row <= 0 || new_row >= this->RowSize()) throw *new std::out_of_range("Invalidated new row.");
            auto iter = m_data.begin();
            iter += (new_row - 1);
            m_data.insert(iter, this->ColumnSize(), 0);
            ++this->m_row_col;
            return;
        } //}
        void Insert_Column(const ItemSize& new_col) //{
        {
            if(new_col <= 0 || new_col >= this->ColumnSize()) throw *new std::out_of_range("Invalidated new column.");
            DataContainer new_data;
            for(ItemSize i = 1; i<=this->RowSize(); ++i){
                for(ItemSize j = 1; j<=this->ColumnSize(); ++j){
                    if(j == new_col) new_data.push_back(0);
                    new_data.push_back(this->get(i, j));
                }
            }
            this->m_data = std::move(new_data);
            return;
        } //}

        ValueType& get(const ItemSize& row, const ItemSize& col) //{
        {
            if(row > this->RowSize() || col > this->ColumnSize() || row <= 0 || col <= 0)
                throw *new std::out_of_range("Invalidate Matrix access");
            return m_data[(row - 1) * m_row_col + col - 1];
        } //}

        CVector<ValueType> operator*(const GenericVector<ValueType>& _oth) //{
        {
            CVector<ValueType> ret;
            GenericVector<ValueType>& result = this->multiply_with_vector(_oth);
            *static_cast<GenericVector<ValueType>*>(&ret) = result;
            delete &result;
            return ret;
        } //}

        SquareMatrix<ValueType> operator+(const GenericMatrix<ValueType>& _oth) //{
        {
            SquareMatrix<ValueType> ret;
            ret = (*this);
            ret += _oth;
            return ret;
        } //}
        SquareMatrix<ValueType> operator-(const GenericMatrix<ValueType>& _oth) //{
        {
            SquareMatrix<ValueType> ret;
            ret = (*this);
            ret -= _oth;
            return ret;
        } //}
}; //}

template class SquareMatrix<double>;
template class CVector<double>;

#endif // MATRIX_HPP_
