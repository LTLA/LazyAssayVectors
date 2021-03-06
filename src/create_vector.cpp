#include "Rcpp.h"
#include "R.h"
#include "R_ext/Rdynload.h"
#include "R_ext/Altrep.h"

#include "sparse_grabber.h"
#include "ordinary_grabber.h"
#include "other_grabber.h"
#include "lazy_methods.h"
#include "altrep_classes.h"

/***************************************************/

SEXP Make(R_altrep_class_t* class_t, SEXP mat, SEXP dim, SEXP idx) {
    MARK_NOT_MUTABLE(mat);
    MARK_NOT_MUTABLE(dim);
    MARK_NOT_MUTABLE(idx);

    SEXP data1=PROTECT(Rf_allocVector(VECSXP, 3));
    SET_VECTOR_ELT(data1, 0, mat);
    SET_VECTOR_ELT(data1, 1, dim);
    SET_VECTOR_ELT(data1, 2, idx);

    SEXP out=R_new_altrep(*class_t, data1, R_NilValue);
    UNPROTECT(1);
    return out;
}

template<typename METHODS>
void remaining_int_Init(R_altrep_class_t* class_t) {
    R_set_altinteger_Elt_method(*class_t, METHODS::value_Elt);
    R_set_altinteger_Get_region_method(*class_t, METHODS::Get_region);
}

template<typename METHODS>
void remaining_dbl_Init(R_altrep_class_t* class_t) {
    R_set_altreal_Elt_method(*class_t, METHODS::value_Elt);
    R_set_altreal_Get_region_method(*class_t, METHODS::Get_region);
}

template<typename METHODS>
void remaining_lgl_Init(R_altrep_class_t* class_t) {
    R_set_altlogical_Elt_method(*class_t, METHODS::value_Elt);
    R_set_altlogical_Get_region_method(*class_t, METHODS::Get_region);
}

/***************************************************/

typedef ordinary_grabber<true, Rcpp::IntegerMatrix> ordinary_int_col_grabber;

typedef ordinary_grabber<false, Rcpp::IntegerMatrix> ordinary_int_row_grabber;

typedef ordinary_grabber<true, Rcpp::NumericMatrix> ordinary_dbl_col_grabber;

typedef ordinary_grabber<false, Rcpp::NumericMatrix> ordinary_dbl_row_grabber;

typedef ordinary_grabber<true, Rcpp::LogicalMatrix> ordinary_lgl_col_grabber;

typedef ordinary_grabber<false, Rcpp::LogicalMatrix> ordinary_lgl_row_grabber;

typedef sparse_grabber<true, Rcpp::NumericVector> sparse_dbl_col_grabber;

typedef sparse_grabber<false, Rcpp::NumericVector> sparse_dbl_row_grabber;

typedef sparse_grabber<true, Rcpp::LogicalVector> sparse_lgl_col_grabber;

typedef sparse_grabber<false, Rcpp::LogicalVector> sparse_lgl_row_grabber;

typedef other_grabber<true, int, INTSXP> other_int_col_grabber;

typedef other_grabber<false, int, INTSXP> other_int_row_grabber;

typedef other_grabber<true, double, REALSXP> other_dbl_col_grabber;

typedef other_grabber<false, double, REALSXP> other_dbl_row_grabber;

typedef other_grabber<true, int, LGLSXP> other_lgl_col_grabber;

typedef other_grabber<false, int, LGLSXP> other_lgl_row_grabber;

/***************************************************/

typedef lazy_vector_methods<ordinary_int_col_grabber> lazy_ordinary_int_col_methods;

typedef lazy_vector_methods<ordinary_int_row_grabber> lazy_ordinary_int_row_methods;

typedef lazy_vector_methods<ordinary_dbl_col_grabber> lazy_ordinary_dbl_col_methods;

typedef lazy_vector_methods<ordinary_dbl_row_grabber> lazy_ordinary_dbl_row_methods;

typedef lazy_vector_methods<ordinary_lgl_col_grabber> lazy_ordinary_lgl_col_methods;

typedef lazy_vector_methods<ordinary_lgl_row_grabber> lazy_ordinary_lgl_row_methods;

typedef lazy_vector_methods<sparse_dbl_col_grabber> lazy_sparse_dbl_col_methods;

typedef lazy_vector_methods<sparse_dbl_row_grabber> lazy_sparse_dbl_row_methods;

typedef lazy_vector_methods<sparse_lgl_col_grabber> lazy_sparse_lgl_col_methods;

typedef lazy_vector_methods<sparse_lgl_row_grabber> lazy_sparse_lgl_row_methods;

typedef lazy_vector_methods<other_int_col_grabber> lazy_other_int_col_methods;

typedef lazy_vector_methods<other_int_row_grabber> lazy_other_int_row_methods;

typedef lazy_vector_methods<other_dbl_col_grabber> lazy_other_dbl_col_methods;

typedef lazy_vector_methods<other_dbl_row_grabber> lazy_other_dbl_row_methods;

typedef lazy_vector_methods<other_lgl_col_grabber> lazy_other_lgl_col_methods;

typedef lazy_vector_methods<other_lgl_row_grabber> lazy_other_lgl_row_methods;

/***************************************************/

R_altrep_class_t lazy_ordinary_int_col_t;
R_altrep_class_t lazy_ordinary_int_row_t;
R_altrep_class_t lazy_ordinary_dbl_col_t;
R_altrep_class_t lazy_ordinary_dbl_row_t;
R_altrep_class_t lazy_ordinary_lgl_col_t;
R_altrep_class_t lazy_ordinary_lgl_row_t;

R_altrep_class_t lazy_sparse_dbl_col_t;
R_altrep_class_t lazy_sparse_dbl_row_t;
R_altrep_class_t lazy_sparse_lgl_col_t;
R_altrep_class_t lazy_sparse_lgl_row_t;

R_altrep_class_t lazy_other_int_col_t;
R_altrep_class_t lazy_other_int_row_t;
R_altrep_class_t lazy_other_dbl_col_t;
R_altrep_class_t lazy_other_dbl_row_t;
R_altrep_class_t lazy_other_lgl_col_t;
R_altrep_class_t lazy_other_lgl_row_t;

// [[Rcpp::init]]
void init_lazy_vector(DllInfo* dll){
    // Ordinary methods.
    lazy_ordinary_int_col_t = R_make_altinteger_class("lazy_ordinary_int_col", "scater", dll);
    lazy_ordinary_int_col_methods::partial_Init(&lazy_ordinary_int_col_t);
    remaining_int_Init<lazy_ordinary_int_col_methods>(&lazy_ordinary_int_col_t);

    lazy_ordinary_int_row_t = R_make_altinteger_class("lazy_ordinary_int_row", "scater", dll);
    lazy_ordinary_int_row_methods::partial_Init(&lazy_ordinary_int_row_t);
    remaining_int_Init<lazy_ordinary_int_row_methods>(&lazy_ordinary_int_row_t);

    lazy_ordinary_dbl_col_t = R_make_altreal_class("lazy_ordinary_dbl_col", "scater", dll);
    lazy_ordinary_dbl_col_methods::partial_Init(&lazy_ordinary_dbl_col_t);
    remaining_dbl_Init<lazy_ordinary_dbl_col_methods>(&lazy_ordinary_dbl_col_t);

    lazy_ordinary_dbl_row_t = R_make_altreal_class("lazy_ordinary_dbl_row", "scater", dll);
    lazy_ordinary_dbl_row_methods::partial_Init(&lazy_ordinary_dbl_row_t);
    remaining_dbl_Init<lazy_ordinary_dbl_row_methods>(&lazy_ordinary_dbl_row_t);

    lazy_ordinary_lgl_col_t = R_make_altreal_class("lazy_ordinary_lgl_col", "scater", dll);
    lazy_ordinary_lgl_col_methods::partial_Init(&lazy_ordinary_lgl_col_t);
    remaining_lgl_Init<lazy_ordinary_lgl_col_methods>(&lazy_ordinary_lgl_col_t);

    lazy_ordinary_lgl_row_t = R_make_altreal_class("lazy_ordinary_lgl_row", "scater", dll);
    lazy_ordinary_lgl_row_methods::partial_Init(&lazy_ordinary_lgl_row_t);
    remaining_lgl_Init<lazy_ordinary_lgl_row_methods>(&lazy_ordinary_lgl_row_t);

    // Sparse methods.
    lazy_sparse_dbl_col_t = R_make_altreal_class("lazy_sparse_dbl_col", "scater", dll);
    lazy_sparse_dbl_col_methods::partial_Init(&lazy_sparse_dbl_col_t);
    remaining_dbl_Init<lazy_sparse_dbl_col_methods>(&lazy_sparse_dbl_col_t);

    lazy_sparse_dbl_row_t = R_make_altreal_class("lazy_sparse_dbl_row", "scater", dll);
    lazy_sparse_dbl_row_methods::partial_Init(&lazy_sparse_dbl_row_t);
    remaining_dbl_Init<lazy_sparse_dbl_row_methods>(&lazy_sparse_dbl_row_t);

    lazy_sparse_lgl_col_t = R_make_altreal_class("lazy_sparse_lgl_col", "scater", dll);
    lazy_sparse_lgl_col_methods::partial_Init(&lazy_sparse_lgl_col_t);
    remaining_lgl_Init<lazy_sparse_lgl_col_methods>(&lazy_sparse_lgl_col_t);

    lazy_sparse_lgl_row_t = R_make_altreal_class("lazy_sparse_lgl_row", "scater", dll);
    lazy_sparse_lgl_row_methods::partial_Init(&lazy_sparse_lgl_row_t);
    remaining_lgl_Init<lazy_sparse_lgl_row_methods>(&lazy_sparse_lgl_row_t);

    // Other methods.
    lazy_other_int_col_t = R_make_altinteger_class("lazy_other_int_col", "scater", dll);
    lazy_other_int_col_methods::partial_Init(&lazy_other_int_col_t);
    remaining_int_Init<lazy_other_int_col_methods>(&lazy_other_int_col_t);

    lazy_other_int_row_t = R_make_altinteger_class("lazy_other_int_row", "scater", dll);
    lazy_other_int_row_methods::partial_Init(&lazy_other_int_row_t);
    remaining_int_Init<lazy_other_int_row_methods>(&lazy_other_int_row_t);

    lazy_other_dbl_col_t = R_make_altreal_class("lazy_other_dbl_col", "scater", dll);
    lazy_other_dbl_col_methods::partial_Init(&lazy_other_dbl_col_t);
    remaining_dbl_Init<lazy_other_dbl_col_methods>(&lazy_other_dbl_col_t);

    lazy_other_dbl_row_t = R_make_altreal_class("lazy_other_dbl_row", "scater", dll);
    lazy_other_dbl_row_methods::partial_Init(&lazy_other_dbl_row_t);
    remaining_dbl_Init<lazy_other_dbl_row_methods>(&lazy_other_dbl_row_t);

    lazy_other_lgl_col_t = R_make_altreal_class("lazy_other_lgl_col", "scater", dll);
    lazy_other_lgl_col_methods::partial_Init(&lazy_other_lgl_col_t);
    remaining_lgl_Init<lazy_other_lgl_col_methods>(&lazy_other_lgl_col_t);

    lazy_other_lgl_row_t = R_make_altreal_class("lazy_other_lgl_row", "scater", dll);
    lazy_other_lgl_row_methods::partial_Init(&lazy_other_lgl_row_t);
    remaining_lgl_Init<lazy_other_lgl_row_methods>(&lazy_other_lgl_row_t);
}

// [[Rcpp::export(rng=false)]]
SEXP create_lazy_vector(SEXP mat, SEXP dim, SEXP idx, bool getcol, int matclass, int type) {
    /*
     * matclass = 0 is an ordinary matrix.
     * matclass = 1 is a dgCMatrix or lgCMatrix.
     * matclass = 2 is any other matrix.
     *
     * type = 0 is integer.
     * type = 1 is double.
     * type = 2 is logical.
     * type = -1 is some unknown type.
     */

    if (matclass==0) {
        if (type==0) {
            if (getcol) {
                return Make(&lazy_ordinary_int_col_t, mat, dim, idx);
            } else {
                return Make(&lazy_ordinary_int_row_t, mat, dim, idx);
            }
        } else if (type==1) {
            if (getcol) {
                return Make(&lazy_ordinary_dbl_col_t, mat, dim, idx);
            } else {
                return Make(&lazy_ordinary_dbl_row_t, mat, dim, idx);
            }
        } else if (type==2) {
            if (getcol) {
                return Make(&lazy_ordinary_lgl_col_t, mat, dim, idx);
            } else {
                return Make(&lazy_ordinary_lgl_row_t, mat, dim, idx);
            }
        }

    } else if (matclass==1) {
        if (type==1) {
            if (getcol) {
                return Make(&lazy_sparse_dbl_col_t, mat, dim, idx);
            } else {
                return Make(&lazy_sparse_dbl_row_t, mat, dim, idx);
            }
        } else if (type==2) {
            if (getcol) {
                return Make(&lazy_sparse_lgl_col_t, mat, dim, idx);
            } else {
                return Make(&lazy_sparse_lgl_row_t, mat, dim, idx);
            }
        }

    } else if (matclass==2) {
        if (type==0) {
            if (getcol) {
                return Make(&lazy_other_int_col_t, mat, dim, idx);
            } else {
                return Make(&lazy_other_int_row_t, mat, dim, idx);
            }
        } else if (type==1) {
            if (getcol) {
                return Make(&lazy_other_dbl_col_t, mat, dim, idx);
            } else {
                return Make(&lazy_other_dbl_row_t, mat, dim, idx);
            }
        } else if (type==2) {
            if (getcol) {
                return Make(&lazy_other_lgl_col_t, mat, dim, idx);
            } else {
                return Make(&lazy_other_lgl_row_t, mat, dim, idx);
            }
        }
    }

    throw std::runtime_error("lazy vectors not supported for this assay");
}
