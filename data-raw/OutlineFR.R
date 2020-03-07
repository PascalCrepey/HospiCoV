
library(ggplot2)

outlineFr = ggplot2::map_data(map = "world", region = "france")

usethis::use_data(outlineFr)
