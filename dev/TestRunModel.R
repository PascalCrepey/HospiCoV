## Test run the model
library(HospiCov)


#build parameter object
params = Parameters$new()

vorhosp = PolyHosp$new()

#run the model
pop = vorhosp$getPopRegion("Bretagne")
res = runMod(params = params, sname = "test", population = pop)


ggplot(finalRes, aes(x = Time, y = Cases, color = AgeGroup)) +
  geom_line()
