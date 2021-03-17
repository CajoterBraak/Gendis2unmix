#' @title Calculates a generalized discriminant function
#'
#' @description
#' \code{gendis} calculates a generalized discriminant function to distinguish two classes,
#' typically sexes (male and female birds)
#' based on measurements of a number of indicators for individuals from each of the two sexes
#' from a series of different populations in which individuals may have a different mean size but
#' a common-within covariance matrix.
#' @param  population a name of the variable for the populations in the data (default "population")
#' @param  sex a name of the variable indicating the two classes to distinguish in the data (default "sex")
#' (0 vs 1 or "female" vs "male")
#' @param measurements character ("other_variables", default)  or character vector with names of
#' measurement variables. \code{gendis} maintains the order of the names.
#' @param verbose logical (default = FALSE)
#' @param data data frame with variables
#' @return  An object of class gendis which is a named list, among which,
#' \item{population}{name of variable indicating populations}
#' \item{sex}{name of variable indicating the two sexes or classes}
#' \item{classnames}{names for the classes of sex (level or value)}
#' \item{measurements}{names of the variables in the GDF}
#' \item{GDF}{the Generalized Discriminant Function, matrix with two columns differing in scaling of the GDF}
#' \item{mean.male}{overall mean of males (the second level of factor(sex))}
#' \item{mean.female}{overall mean of females (the first level of factor(sex))}
#' \item{within.sd}{overall within standard deviation}
#' \item{cov_overall}{overall within-group covariance matrix}
#' \item{means.male}{mean of males per population}
#' \item{means.female}{mean of females per population}
#' \item{within.sds}{within standard deviation per population}
#' \item{ind_mv}{number of males and females per population}
#' \item{cov_list}{within-group covariance matrix per population}
#' \item{Nind}{number of individuals}
#' \item{Np}{number of populations}
#' @seealso \code{\link{predict.gendis}}, \code{\link{summary.gendis}}, \code{\link{print.gendis}}.
#' @example demo/demo1.r
#' @references van Franeker, J A. ter Braak, C J F. 1993.
#' A generalized discriminant for sexing fulmarine petrels from
#' external measurements. The Auk 110: pp 492-502, https://doi.org/10.2307/4088413 https://edepot.wur.nl/249350
#' @export
#'
#'
gendis <- function(population = "population", sex = "sex", measurements = "other_variables",  verbose = FALSE,  data ){

  id_P <- which(names(data)%in% population )
  id_S <- which(names(data)%in% sex)

  if (length(id_P)==0 || length(id_S)==0 )    stop(
          c("Gendis error: population or sex variable name not in data:\n  names in data are\n    ",
           paste(names(data), collapse = " "),
           "\n    and should be quoted (i.e. a character) in call to Gendis"))
  sexnames <- levels(factor(data[,id_S]))
  if (length(sexnames) !=2 )    stop(
    c("Gendis error: the sex variable should have two levels or two unique values\n",
      "the current levels are:\n ", paste(sexnames, collapse = " ") ))

  if (length(measurements) ==1 ){
  id_vars <- (1:ncol(data))[-c(id_P,id_S)]
  namVs <- names(data)[id_vars] # variables to analyse
  } else {
    namVs <- measurements
    #id_vars <- which(names(data) %in% namVs)
    # retain the order of the variables given by measurements
    id_vars <- sapply(namVs, function(nam)which(names(data) %in% nam) )
  }
  if (is.list(id_vars) )  stop(
  c("Gendis error:
       names of measurements not in data:\n       names in data are\n    ",
         paste(names(data), collapse = " "),
                                         "\n    and should be quoted (i.e. a character) in call to Gendis"))
  p <- length(id_vars) # number of variables
  pops <- unique(data[,id_P])
  Np <- length(pops)
  Nind <- nrow(data)
  mcov <- matrix(0,nrow = p, ncol= p)
  ind_mv <- matrix(0,nrow = Np, ncol=2)
  f_means <- m_means <-  sdw <- matrix(0, nrow = Np,ncol = p)
  cov_list <- list()
  for (pop in seq_along(pops)){
    idp <- data[,id_P] == pops[pop]
    idsf <- data[,id_S]  %in% sexnames[1]
    idsm <- data[,id_S]  %in% sexnames[2]
    dat_female <- as.matrix(data[idp&idsf, id_vars])
    dat_male <-   as.matrix(data[idp&idsm, id_vars])
    ind_mv[pop,1]<- nrow(dat_male)
    ind_mv[pop,2]<- nrow(dat_female)
    f_means[pop,]<- f <- apply(dat_female,2, mean, na.rm = TRUE)
    m_means[pop,]<- m <- apply(dat_male,2, mean, na.rm = TRUE)
    dat_female <- sweep(dat_female, 2, f)
    dat_male <- sweep(dat_male, 2, m)
    dat_fm <- rbind(dat_female,dat_male)
    cov_list[[pop]] <- (t(dat_fm)%*%dat_fm)/nrow(dat_fm)
    sdw[pop,] <- sqrt(diag(cov_list[[pop]])*((nrow(dat_fm)/(nrow(dat_fm)-2))))
    if (verbose){
      cat("population", pop,", ", nrow(dat_male), "males", nrow(dat_female), "females \n")
      tt <- cbind(mean.males = m,mean.female = f,difference= m-f, within.sd = sdw[pop,])
      print(round(tt,3))
      cat("**** Correlation matrix ****\n")
      print(round(stats::cov2cor(cov_list[[pop]]),2))
    }
  }
  colnames(f_means) <- colnames(m_means) <- colnames(sdw) <- namVs
  rownames(ind_mv)  <- rownames(f_means) <- rownames(m_means) <- rownames(sdw) <- pops
  colnames(ind_mv) <- c("males","females")

  f_mean <- apply(f_means,2, mean, na.rm= TRUE)
  m_mean <- apply(m_means,2, mean, na.rm= TRUE)
  diff_mean <- m_mean - f_mean
  add <- function(x)Reduce("+",x)
  cov_overall  <- add(cov_list)/Np

  sdw_a <- sqrt(diag(cov_overall)*(Nind/(Nind-2*Np)))

  b<- solve(cov_overall,diff_mean)
  #1.1529/b[1]
  w <- b/b[1]
  #
  GDF <- cbind(b, w)
  if (verbose){
    cat("Overall results\n")
    tt <- cbind(mean.males = m_mean,mean.female = f_mean,difference=  m_mean - f_mean, within.sd = sdw_a)
    print(round(tt,3))
    cat("**** Correlation matrix ****\n")
    print(round(stats::cov2cor((Nind/(Nind-2*Np))*cov_overall),2))
    cat("**** generalized discriminant function ****\n")
    print(round(GDF,4))
  }

  if (sexnames[1]== "0" && sexnames[2]== "1") sexnames= c("female","male")
  result <- list(GDF=GDF, mean.male  = m_mean,  mean.female  = f_mean,
                      within.sds  = sdw,
                      means.male = m_means,means.female = f_means,
                      within.sd   = sdw_a, Nind = Nind, Np = Np, ind_mv = ind_mv,
                 cov_overall = cov_overall, cov_list = cov_list,
                 population = population, sex= sex,classnames = sexnames, measurements= namVs
                 )
  class(result)<- "gendis"
  return(result)
}
