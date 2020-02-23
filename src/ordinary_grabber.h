#ifndef ORDINARY_GRABBER_H
#define ORDINARY_GRABBER_H

#include "Rcpp.h"
#include "base_grabber.h"

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

#endif
