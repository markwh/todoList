# todoList
# an R package for todo lists
# Mark Hagemann

library(devtools)

# Packages

use_package("stats")
use_package("dplyr")

# devtools utilities
use_testthat()
use_travis()
use_coverage()

vignette

use_vignette("todoList")
