# set options
options("rgdal_show_exportToProj4_warnings" = "none")

# load package
library(shiny)
library(leaflet)
library(leafgl)
library(leaflet.extras2)
library(locationmisc)

# set seed for reproducibility
set.seed(200)
RandomFields::RFoptions(seed = 200)

# define global variables
## file paths
config_path <- NULL
spatial_path <- NULL
boundary_path <- NULL
attribute_path <- NULL

## data
dataset <- NULL
themes <- NULL
weights <- NULL
includes <- NULL
mode <- NULL

## widget variables
mm <- NULL
