data("fulmarin")
result <- gendis(population = "population", sex = "sex",
                 measurements = c("HB","BD2","TL","CL"), verbose = FALSE ,  data=fulmarin )
data("JanMayenBirds")
#get the measurements in the generalized discriminant function (GFD) from the new data
newdata <- as.matrix(JanMayenBirds[,  c("HB","BD2","TL","CL")])
# combine the measurements using the coefficients of the GDF
GDFscores <- newdata%*% result$GDF[,2]
attr(GDFscores,which = "classnames") <- result$classnames
# note the attribute classnames with the names to be used in the printout
# for first and second level of the factor sex
# Calculate the cutpoint using unmix instead of predict.gendis
unmix(GDFscores,verbose = TRUE)



