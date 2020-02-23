#ifndef BASE_GRABBER_H
#define BASE_GRABBER_H

#include "Rcpp.h"
#include "R.h"

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

#endif
