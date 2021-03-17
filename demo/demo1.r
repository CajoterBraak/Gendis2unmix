data("fulmarin")
names(fulmarin)
result <- gendis(population = "population", sex = "sex",
                 measurements = "other_variables", verbose = FALSE ,  data=fulmarin )
result$GDF
summary(result)
print(result)

# populations may have names:
fulmarin$pop <- factor(c("a1","a2","a3","a4","a5","a6")[fulmarin$population])
levels(fulmarin$pop)
names(fulmarin)
result2 <- gendis(population = "pop", sex = "sex",
                  measurements = c("HB","B","TL","CL"), verbose = FALSE ,  data=fulmarin )
# all equal should not give numeric differences.
#all.equal(result, result2)

result2$GDF -result$GDF



