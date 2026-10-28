

install.packages(
  "sf",
  type = "source",
  lib = .libPaths()[1],
  repos = "https://cloud.r-project.org"
)

library(units)
library(sf)

devtools::install_github(
  "vanhasseltlab/pmxcopula", 
  build_vignettes = TRUE, 
  force = TRUE
  )

library(pmxcopula)
