library(ggplot2)
library(ggvoronoi)
library(ggmap)

# functions computing the voronoi tesselisations
register_google(key = "AIzaSyA8RT3PHkYKa26aTKCXv75Ey6OYy6xL1ys")

France = get_map(c(left = -5, top = 51, right = 8, bottom = 42))

ggmap(France)
