data("fulmarin")
str(fulmarin)
result <- gendis(population = "population", sex = "sex",
                 measurements = "other_variables", verbose = FALSE ,  data=fulmarin )
data("JanMayenBirds")
sex.predict <- predict(result, newdata = JanMayenBirds, verbose = TRUE)
# one false prediction: (number 32)
data.frame(sex = JanMayenBirds$DISSEX,  sex.predict)[seq(from=2, to = 37, by =5),]

predict(result, JanMayenBirds )
# same as default above
predict(result, JanMayenBirds, type = result$sex, verbose = FALSE)
# GDF score with cutpoint
predict(result, JanMayenBirds, type = "GDF", verbose = FALSE)
# unmix results only
predict(result, JanMayenBirds, type = "cutpoint", verbose = TRUE)



