#ifndef SPARSE_GRABBER_H
#define SPARSE_GRABBER_H

#include "Rcpp.h"
#include <algorithm>
#include "base_grabber.h"

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
