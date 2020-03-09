## Test run the model
library(HospiCov)


#build parameter object
params = Parameters$new(R0 = 1.5)
params$preInfected = 100
vorhosp = PolyHosp$new()

#run the model
pop = vorhosp$getPopRegion("Ile-de-France")
finalRes = runMod(params = params$getList(), sname = "test", population = pop)

finalRes[, sum(Cases/N), by = AgeGroup]
ggplot(finalRes, aes(x = Time, y = Cases, color = AgeGroup)) +
  theme_classic() +
  geom_line()
ggplot(finalRes[, sum(Cases), by = "Time"], aes(x = Time, y = V1)) +
  theme_classic() +
  geom_line()
