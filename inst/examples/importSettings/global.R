# load package
library(shiny)
library(locationmisc)

# set seed for reproducibility
set.seed(50)

# define layer names
layer_names <- sample(example_theme_names()$feature, 20)
