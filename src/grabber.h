#ifndef GRABBER_H
#define GRABBER_H

#include "Rcpp.h"
#include "R.h"
#include <algorithm>

template<bool GETCOL>
struct base_grabber {
    static const bool getcol=GETCOL;

    base_grabber(SEXP data1) {
        raw_mat=VECTOR_ELT(data1, 0);
        dim=VECTOR_ELT(data1, 1);
        idx=Rcpp::as<int>(VECTOR_ELT(data1, 2));
        return;
    }

    int size() const {
        // Flipped around, because if we want the column,
        // then obviously we need the number of rows.
        if (GETCOL) {
            return dim[0];
        } else {
            return dim[1];
        }
    }
protected:
    Rcpp::RObject raw_mat;
    Rcpp::IntegerVector dim;
    int idx;
};


template<bool GETCOL, typename M>
struct ordinary_grabber : public base_grabber<GETCOL> {
    typedef typename M::stored_type value_type;
    static const unsigned int sexp_type=M::r_type::value;

    ordinary_grabber(SEXP data1) : base_grabber<GETCOL>(data1), mat(this->raw_mat) {}

    value_type operator()(int i) {
        if (GETCOL) {
            return mat(i, this->idx);
        } else {
            return mat(this->idx, i);
        }
    }

    void fill(value_type* out) {
        if (GETCOL) {
            auto C=mat.column(this->idx);
            std::copy(C.begin(), C.end(), out);
        } else {
            auto R=mat.row(this->idx);
            std::copy(R.begin(), R.end(), out);
        }
        return;
    }
protected:
    M mat;
};

template<bool GETCOL, typename V>
struct sparse_grabber : base_grabber<GETCOL> {
    typedef typename V::stored_type value_type;
    static const unsigned int sexp_type=V::r_type::value;

    sparse_grabber(SEXP data1) : base_grabber<GETCOL>(data1) {
        Rcpp::S4 tmp(this->raw_mat);
        x_=tmp.slot("x");
        i_=tmp.slot("i");
        p_=tmp.slot("p");
        return;
    }

    value_type operator()(int i) {
        if (GETCOL) {
            return search(i, this->idx);
        } else {
            return search(this->idx, i);
        }
    }

    void fill(value_type* out) {
        if (GETCOL) {
            std::fill(out, out+this->dim[0], 0);
            auto xIt=x_.begin();
            auto iIt=i_.begin();
            for (int x=p_[this->idx]; x!=p_[this->idx+1]; ++x) {
                out[*(iIt+x)]=*(xIt+x);
            }
        } else {
            for (int j=0; j<this->dim[1]; ++j) {
                out[j]=search(this->idx, j);
            }
        }
        return;
    }
protected:
    V x_;
    Rcpp::IntegerVector i_;
    Rcpp::IntegerVector p_;

    double search(int i, int j) {
        int start=p_[j], end=p_[j+1];
        auto last=i_.begin() + end;
        auto out=std::lower_bound(i_.begin() + start, last, i);
        if (out==last || *out != i) {
            return 0;
        } else {
            return x_[out - i_.begin()];
        }
    }
};

#endif
