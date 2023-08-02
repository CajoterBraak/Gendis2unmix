#' @title Predict function using a generalized discriminant function
#'
#' @description
#' \code{predict.gendis} applies a generalized discriminant function created with \code{\link{gendis}}
#'  to predict the sex (class) of each individual with measurements in \code{newdata}. From the  \code{\link{gendis}}
#'  object, the coefficients that define the generalized discriminant function (GDF) are applied to the
#'  \code{newdata} to obtain the discriminant scores.
#' @param  object an object of class gendis, typically created with \code{\link{gendis}}
#' @param  newdata a data frame with measurements on (new) individuals with variables used to create \code{object}.
#' The data should be from a single population. If your data are from multiple populations, use \code{predict} for each
#' subset (i.e. for each population).
#' @param type what to predict: the sex or class of each individual (default),
#' the generalized discriminant scores with cutpoint ("GDF" or "GDFscore") or
#' the full output of the unmixing algorithm \code{\link{unmix}}  ("cutpoint")
#' @param verbose logical (default = FALSE). If TRUE a plot of the density of the GDF is produced.
#' @param ... other optional arguments
#' @return  See argument \code{type}.
#' @details The discriminant score are a linear combination of the variables in \code{newdata}
#' that are shared with the variables used to create the \code{object}. The linear combination is defined by
#' the GDF coefficients. The discriminant scores are subjected to an unmixing algorithm. This algorithm (\code{\link{unmix}}) generates a cutpoint
#' below which individuals are predicted to be female (level 1 of \code{factor(sex)}) and above which they are predicted to be
#' male (level 2 of \code{factor(sex)}). The cutpoint is at the point of intersection of two normal densities with unequal
#' means and variances fitted to the discriminant scores (see \code{\link{unmix} for details}).
#' @example demo/demo2.r
#' @references van Franeker, J A. ter Braak, C J F. 1993.
#' A generalized discriminant for sexing fulmarine petrels from
#' external measurements. The Auk 110: pp 492-502, https://doi.org/10.2307/4088413 https://edepot.wur.nl/249350
#' @export
#'
#'
predict.gendis <- function(object, newdata, type = object$sex, verbose = FALSE, ...){
# type = sex, in ("cutpoint") or (GDF) GDFscores

  namV <- object$measurements

  idV <- sapply(namV, function(nam)which(names(newdata) %in% nam) )
  if (is.list(idV) )  stop(
    c("Predict.gendis error:
       names of measurements not in newdata:\n       names in newdata are:\n ",
      paste(names(newdata), collapse = " "),
      "\n       names in data are:\n ",       paste(namV, collapse = " "),"\n"
      ))
  newdata2 <- as.matrix(newdata[,idV])

  GDFscore <- c(newdata2%*% object$GDF[,2])
  attr(GDFscore,which = "classnames") <- object$classnames
  res <- unmix(GDFscore, verbose = verbose)
  attr(GDFscore,which = "cutpoint") <- res$cutpoint
  sex.predict <- c(ifelse(GDFscore > res$cutpoint, object$classnames[2],object$classnames[1]) )

  if (type == object$sex) result <- sex.predict else if (type == "cutpoint") {
     result <- res
  } else if (type %in% c("GDFscore","GDF", "GDFscores")) {
    result <- GDFscore
  } else {
    result <-object
    result$sex.predict <- sex.predict
    result$unmix <- res
    }
  return(result)
}



