#' The LazyAssayVectors package
#'
#' Provides functions to lazily materialize rows or columns of any matrix class.
#' This allows us to extract rows or columns of large matrices containing experimental data
#' and store them in data.frames for use in frameworks like \pkg{ggplot2}.
#' Using the ALTREP system, individual vectors are only extracted from the matrix on an as-needed basis,
#' e.g., when requested by later \code{geom_} or \code{stat_} layers.
#'
#' @author Aaron Lun
#'
#' @importFrom Rcpp sourceCpp
#' @name LazyAssayVectors-pkg
#' @useDynLib LazyAssayVectors, .registration=TRUE
NULL
