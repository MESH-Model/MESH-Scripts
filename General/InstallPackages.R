# This R script installs packages used for MESH Pre-processing

install.packages(c("tidyverse", "lubridate", "devtools", "zoo", "humidity", "rmarkdown"), repos = "https://muug.ca/mirror/cran/", dependencies = TRUE)
library(devtools)
install_github("CentreForHydrology/CRHMr")
install_github("ropensci/weathercan")
install_github("ropensci/tidyhydat")

