library(targets)
library(tarchetypes)
library(renv)
library(conflicted)
library(janitor)
library(tidyverse)
library(patchwork)
library(magrittr)
library(MetBrewer)
library(ggraph)
library(igraph)
library(PNWColors)
library(grid)

# ggstream is a dependency of MoMAColors but was removed from CRAN
#ggstream_url <- "https://cran.r-project.org/src/contrib/Archive/ggstream/ggstream_0.1.0.tar.gz"
#install.packages(ggstream_url, repos = NULL, type = "source")
#devtools::install_github("BlakeRMills/MoMAColors")
library(MoMAColors)