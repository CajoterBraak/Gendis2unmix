#' @title Unmixing a distribution by decomposing it in two normal ones with unequal means and variances.
#'
#' @description
#' \code{unmix} generates a cutpoint below which individuals are predicted to be female
#' (level 1 of \code{factor(sex)}) and above which they are predicted to be
#' male (level 2 of \code{factor(sex)}). The cutpoint is at the point of intersection of two normal densities with unequal
#' means and variances fitted to argument \code{x}. This function is used internally in the \code{\link{predict.gendis}} function.
#' @param x a numeric vector of discriminant scores with optional attribute "classnames", e.g. c("female","male")
#' @param verbose logical (default = FALSE)
#' @return  A list consisting of
#' \itemize{
#' \item\code{cutpoint} point of equal density of the normal distributions
#' \item\code{p1} estimated probability of class 0 ("female"), informally: fraction of individuals in class 0
#' \item\code{p2} estimated probability of class 1 ("female"), informally: fraction of individuals in class 0
#' \item\code{m1} estimated mean of the normal distribution of class 0
#' \item\code{m2} estimated mean of the normal distribution of class 1
#' \item\code{v1} estimated variance of the normal distribution of class 0
#' \item\code{v2} estimated variance of the normal distribution of class 1
#' }
#' @details \code{unmix} is an EM algorithm following example 4.3.2 of Titterington et al. (1985). Alternatively,
#' library \code{flexmix} could have been used.
#' @example demo/demo3.r
#' @references Titterington, D.M., Smith, A.F.M. & Makov, U.E. (1985).
#' Statistical analysis of finite mixture distributions,
#' Wiley, 1985. pages 86/87, example 4.3.2
#'
#' van Franeker, J A. ter Braak, C J F. 1993.
#' A generalized discriminant for sexing fulmarine petrels from
#' external measurements. The Auk 110: pp 492-502, https://doi.org/10.2307/4088413 https://edepot.wur.nl/249350
#' @export
unmix <- function(x, verbose = FALSE){
  n <- length(x)
  if (verbose) plot(stats::density(x))
  cut=(min(x)+max(x))/2;
  xtemp = x[ x < cut];
  m1 = mean(xtemp);
  v1 = stats::var(xtemp);
  n1 = length(xtemp);
  xtemp = x[x >= cut] ;
  m2 = mean(xtemp);
  v2 = stats::var(xtemp);
  n2 =  length(xtemp)

  p1=n1/n;
  p2=n2/n;
  # start with equal initial variance
  vtot = (n1*v1 +n2*v2)/(n1+n2);
  v1=vtot;
  v2=vtot;

  niter <-0
  deviance= deltalik = 999999.0;
  if (verbose) cat(c("niter", "n1","n2", "deviance","deltalik\n"))
  while (deltalik > 0.005 && niter < 100 ){

    niter <- niter + 1
    f1=(exp((x-m1)*(x-m1)/(-2*v1)))/sqrt(v1);
    f2=(exp((x-m2)*(x-m2)/(-2*v2)))/sqrt(v2);
    pf1=p1*f1;
    pf2=p2*f2;
    pfm=pf1+pf2;
    prevdeviance=deviance;
    deviance=-2.0 * sum(log(pfm));
    deltalik=prevdeviance-deviance;
    w1=pf1/pfm;
    w2=pf2/pfm;
    n1=sum(w1);
    n2=sum(w2);
    #cout << niter<<" "<< n1 <<"  "<< n2<<
    #  "  " <<deviance <<"  "<< deltalik << endl;
    if (verbose) cat(c(niter, n1,n2, deviance,deltalik), "\n")
    p1=n1/n;
    p2=n2/n;
    m1=sum(w1*x)/n1;
    m2=sum(w2*x)/n2;
    v1=sum(w1*(x-m1)*(x-m1))/n1;
    v2=sum(w2*(x-m2)*(x-m2))/n2;
  }

  cutequal=(m1+m2)/2;

  if (abs(v2-v1)>0.0001*(v1+v2)){
    cut=sqrt( v1*v2*((m1-m2)*(m1-m2)+(v1-v2)*log(v1/v2)));
    cut=(m1*v2-m2*v1+cut)/(v2-v1);
    #cout << " cutpoint = "<< cut<<endl;
  } else {
    cut=cutequal;
  #  cout << " cutpoint = "<< cut<<endl;

  }

if (verbose)
# cout <<" p1 = "<<  p1 << " p2 = "  << p2 << endl;
# cout <<" m1 = "<<  m1 << " m2 = "  << m2 << endl;
# cout <<" v1 = "<<  v1 << " v2 = "  << v2 << endl;
  if (verbose) {
  classnames <- attr(x,which = "classnames")
  if (is.null(classnames)) classnames <- c("female", "male")
  cat(" fractions p, mean m and variance v of\n" )
  cat(c("      ",classnames[1], "   and    ", classnames[2]),"\n")

  cat(" p1 = ", p1, "p2 = ", p2, "\n")

  cat(" m1 = ", m1, "m2 = ", m2, "\n")
  cat(" v1 = ", m1, "v2 = ", v2, "\n")
  cat("cutpoint = ", cut, "\n")
}
  result <- list(cutpoint = cut, p1 =p1, p2 = p2, m1= m1, m2=m2,v1=v1,v2=v2)
  if (verbose) invisible(result) else
  return(result)
}

