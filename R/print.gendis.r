
#' @title Printing results a generalized discriminant analysis
#'
#' @description
#' \code{print.gendis} prints the results of \code{\link{gendis}} in more detail than \code{summary.gendis}.
#' @param  x an object of class gendis, created by \code{\link{gendis}}.
#' @param ... other optional arguments
#' @return  list of within-sex correlations matrices per population (invisible)
#' @seealso \code{\link{gendis}}, \code{\link{summary.gendis}}, \code{\link{predict.gendis}}.
#' @example demo/demo1.r
#' @references van Franeker, J A. ter Braak, C J F. 1993.
#' A generalized discriminant for sexing fulmarine petrels from
#' external measurements. The Auk 110: pp 492-502ter Braak (2019)
#' @export
#'
#'
print.gendis <- function(x, ...){
  corlist <- list()
  with(x, {
    for (pop in 1:Np){
      cat("population", pop,", ", ind_mv[pop,1], "males", ind_mv[pop,2], "females \n")
      tt <- cbind(mean.males =   means.male[pop,],mean.female = means.female[pop,],
                  difference= means.male[pop,]-means.female[pop,],
                  within.sd = within.sds[pop,])
      print(round(tt,3))
     #sumlist[[pop]] <-
      cat("**** Correlation matrix ****\n")
      print(round(stats::cov2cor(cov_list[[pop]]),2))
      corlist[[pop]] <- cov2cor(cov_list[[pop]])
    }
  })
  invisible(corlist)
}
