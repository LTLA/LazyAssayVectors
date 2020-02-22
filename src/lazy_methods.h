#ifndef LAZY_METHODS_H 
#define LAZY_METHODS_H 

#include "Rcpp.h"
#include "R.h"
#include <algorithm>

#define class klass

extern "C" {
#include "R_ext/Altrep.h"
}

#undef klass

template<typename GRABBER>
struct lazy_vector_methods {
    static SEXP Materialize(SEXP vec) {
        SEXP data2=R_altrep_data2(vec);
        if (data2==R_NilValue) {
            SEXP data1=R_altrep_data1(vec);
            GRABBER grab(data1);

            int veclen=grab.size();
            data2=PROTECT(Rf_allocVector(GRABBER::sexp_type, veclen));
            grab.fill(static_cast<typename GRABBER::value_type*>(DATAPTR(data2)));

            R_set_altrep_data2(vec, data2);
            UNPROTECT(1);
        }
        return data2;
    }

    // ALTREP methods -------------------

    static R_xlen_t Length(SEXP vec) {
        SEXP data2=R_altrep_data2(vec);
        if (data2==R_NilValue) {
            SEXP data1=R_altrep_data1(vec);
            GRABBER grab(data1);
            return grab.size();
        } else {
            return LENGTH(data2);
        }
    }

    static Rboolean Inspect(SEXP x, int pre, int deep, int pvec, void (*inspect_subtree)(SEXP, int, int, int)){
        auto len=LENGTH(x);
        const char* dimname=(GRABBER::getcol ? "row" : "column");
        const char* typname=(GRABBER::sexp_type==REALSXP ? "double" : "integer");

        SEXP data2=R_altrep_data2(x);
        const char* mode=(data2==R_NilValue ? "lazy" : "materialized");
        Rprintf("%s %s (len=%d, type=%s)\n", mode, dimname, len, typname);

        return TRUE;
    }

    // ALTVEC methods ------------------

    static const void* Dataptr_or_null(SEXP vec){
        SEXP data2 = R_altrep_data2(vec);
        if (data2 == R_NilValue) {
            return nullptr;
        } else {
            return STDVEC_DATAPTR(data2);
        }
    }

    static void* Dataptr(SEXP vec, Rboolean writeable){
        return STDVEC_DATAPTR(Materialize(vec));
    }

    // ALTREAL/INTEGER methods -----------------

    static typename GRABBER::value_type value_Elt(SEXP vec, R_xlen_t i){
        SEXP data2=R_altrep_data2(vec);
        if (data2==R_NilValue) {
            SEXP data1=R_altrep_data1(vec);
            GRABBER grab(data1);
            return grab(i);
        } else {
            return static_cast<const typename GRABBER::value_type*>(STDVEC_DATAPTR(data2))[i];
        }
    }

    static R_xlen_t Get_region(SEXP vec, R_xlen_t start, R_xlen_t size, typename GRABBER::value_type* out){
        out = static_cast<typename GRABBER::value_type*>(Dataptr(vec, TRUE));
        R_xlen_t len = Length(vec) - start;
        return len > size ? len : size;
    }

    static void partial_Init(R_altrep_class_t* class_t) {
        R_set_altrep_Length_method(*class_t, Length);
        R_set_altrep_Inspect_method(*class_t, Inspect);
        R_set_altvec_Dataptr_method(*class_t, Dataptr);
        R_set_altvec_Dataptr_or_null_method(*class_t, Dataptr_or_null);
    }
};

#endif
