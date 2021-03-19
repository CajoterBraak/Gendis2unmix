# Gendis2unmix

<!-- badges: start -->
<!-- badges: end -->

The goal of the R library Gendis2unmix is to sex birds on the basis of several measurements. 
The key feature is that the birds in different populations differ in size but that within populations
females are smaller in most measurements than male (or reversely). The predict function
for a set of unsexed birds from a new population therefore estimates
a new cutoff value which thus depends on the sizes of the birds in the new population.
The method has been described in van Franeker and ter Braak, C J F. (1993).
A generalized discriminant for sexing fulmarine petrels from external measurements. 
The Auk 110: pp 492-502, https://edepot.wur.nl/249350

## Installation

You can install the released version of Gendis2unmix from github by
invoking the R-code within an R-console:

``` r
install.packages("remotes")
remotes::install_github("CajoterBraak/Gendis2unmix")
```

