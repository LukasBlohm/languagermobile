
library(tidyverse)
library(reticulate)

py_config()

source("R/fct_transformer.R")

# deps <- load_dependencies(model_path = NULL, language_in = "fr", language_out = "de")

deps <- setup_translator(model_path = NULL, language_pairs = "fr-de")


translate(dependencies = deps, language = "fr-de", input_text = "Maison", character_cut = 1200, verbose = TRUE)
