#include "Rcpp.h"
#include "R.h"
#include "R_ext/Altrep.h"
#include "altrep_classes.h"

// [[Rcpp::export(rng=false)]]
SEXP check_lazy_vectors(Rcpp::List incoming) {
    Rcpp::LogicalVector output(incoming.size());
    for (R_xlen_t i=0; i<output.size(); ++i) {
        SEXP current=SEXP(incoming[i]);

        // There is probably a better way of doing this...
        if (
            R_altrep_inherits(current, lazy_ordinary_int_row_t) ||
            R_altrep_inherits(current, lazy_ordinary_dbl_col_t) ||
            R_altrep_inherits(current, lazy_ordinary_dbl_row_t) ||
            R_altrep_inherits(current, lazy_ordinary_lgl_col_t) ||
            R_altrep_inherits(current, lazy_ordinary_lgl_row_t) ||
            R_altrep_inherits(current, lazy_sparse_dbl_col_t) ||
            R_altrep_inherits(current, lazy_sparse_dbl_row_t) ||
            R_altrep_inherits(current, lazy_sparse_lgl_col_t) ||
            R_altrep_inherits(current, lazy_sparse_lgl_row_t) ||
            R_altrep_inherits(current, lazy_other_int_col_t) ||
            R_altrep_inherits(current, lazy_other_int_row_t) ||
            R_altrep_inherits(current, lazy_other_dbl_col_t) ||
            R_altrep_inherits(current, lazy_other_dbl_row_t) ||
            R_altrep_inherits(current, lazy_other_lgl_col_t) ||
            R_altrep_inherits(current, lazy_other_lgl_row_t)
        ) {
            output[i]=(R_altrep_data2(current)!=R_NilValue);
        } else {
            output[i]=1;
        }
    }
    return output;
}
