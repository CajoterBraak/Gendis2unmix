#' @title Summary of a generalized discriminant analysis
#'
#' @description
#' \code{summary.gendis} summarizes the results of \code{\link{gendis}}.
#' @param  object an object of class gendis, created by \code{\link{gendis}}.
#' @param ... other optional arguments.
#' @return  GDF
#' @seealso \code{\link{gendis}}, \code{\link{print.gendis}}.
#' @example demo/demo1.r
#' @references van Franeker, J A. ter Braak, C J F. 1993.
#' A generalized discriminant for sexing fulmarine petrels from
#' external measurements. The Auk 110: pp 492-502ter Braak (2019)
#' @export
#'
#'
summary.gendis <- function(object, ...){
  cat("Overall results\n")
  with(object,{
    tt <- cbind(mean.males = mean.male,mean.female = mean.female,difference= mean.male - mean.female, within.sd = within.sd)
    cat("**** numbers per population ****\n")
    print(ind_mv)
    print(round(tt,3))
    cat("**** Correlation matrix ****\n")
    print(round(stats::cov2cor((Nind/(Nind-2*Np))*cov_overall),2))
    cat("**** generalized discriminant function ****\n")
    print(round(GDF,4))
    invisible(GDF)
  }
  )
}
