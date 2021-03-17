data("fulmarin")
result <- gendis(population = "population", sex = "sex",
                 measurements = "other_variables", verbose = FALSE ,  data=fulmarin )
data("JanMayenBirds")
GDFscores <- predict(result, newdata = JanMayenBirds, type = "GDF")
attr(GDFscores, which = "cutpoint") <- NULL # remove the cutpoint
str(GDFscores)
# note the attribute classnames with the names to be used
# for first and second level of the factor sex
# recalculate the cutpoint using unmix instead of predict.gendis
unmix(GDFscores,verbose = TRUE)



