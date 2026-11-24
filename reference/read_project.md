# Read project

Read a project from disk.

## Usage

``` r
read_project(
  path,
  spatial_path = NULL,
  attribute_path = NULL,
  boundary_path = NULL,
  mode = "project"
)
```

## Arguments

- path:

  `character` file path for the configuration file.

- spatial_path:

  `character` file path for spatial data. Defaults to `NULL` such that
  the file path is determined using the argument to `path`.

- attribute_path:

  `character` file path for attribute data. Defaults to `NULL` such that
  the file path is determined using the argument to `path`.

- boundary_path:

  `character` file path for boundary data. Defaults to `NULL` such that
  the file path is determined using the argument to `path`.

- mode:

  `character` mode for importing the objects. Defaults to `"project"`
  such that the mode is determined based on the contents of `path`. If
  the `mode` is `"advanced"`, then goal limits and mandatory include
  settings are disabled.

## Value

A `list` containing the following elements:

- name:

  A `character` value indicating the name.

- dataset:

  A
  [Dataset](https://ncc-cnc.github.io/wheretowork/reference/Dataset.md)
  object.

- themes:

  A `list` of
  [Theme](https://ncc-cnc.github.io/wheretowork/reference/Theme.md)
  objects.

- weights:

  A `list` of
  [Weight](https://ncc-cnc.github.io/wheretowork/reference/Weight.md)
  objects.

- includes:

  A `list` of
  [Include](https://ncc-cnc.github.io/wheretowork/reference/Include.md)
  objects.

- excludes:

  A `list` of
  [Exclude](https://ncc-cnc.github.io/wheretowork/reference/Exclude.md)
  objects.

- mode:

  A `character` value indicating the mode.

## Details

Note that the status field for themes and weights will automatically be
set to `FALSE` if the goal/weight values are zero. This is to ensure
that the application starts with a sensible combination of settings.

## Examples

``` r
# find data file paths
f1 <- system.file(
  "extdata", "projects", "sim_raster", "sim_raster_data.yaml",
  package = "wheretowork"
)
f2 <- system.file(
  "extdata", "projects", "sim_raster", "sim_raster_spatial.tif",
  package = "wheretowork"
)
f3 <- system.file(
  "extdata",  "projects", "sim_raster", "sim_raster_attribute.csv.gz",
  package = "wheretowork"
)
f4 <- system.file(
  "extdata",  "projects", "sim_raster", "sim_raster_boundary.csv.gz",
  package = "wheretowork"
)

# read project
x <- read_project(f1, f2, f3, f4)

# print project
print(x)
#> $name
#> [1] "Example GeoTIFF dataset"
#> 
#> $author_name
#> [1] "Richard Schuster"
#> 
#> $author_email
#> [1] "richard.schuster@natureconservancy.ca"
#> 
#> $dataset
#> Dataset
#>   paths:
#>     spatial: /tmp/RtmpiCuWTj/temp_libpath51b21a80aa17/wheretowork/extdata/projects/sim_raster/sim_raster_spatial.tif
#>     attribute: /tmp/RtmpiCuWTj/temp_libpath51b21a80aa17/wheretowork/extdata/projects/sim_raster/sim_raster_attribute.csv.gz
#>     boundary: /tmp/RtmpiCuWTj/temp_libpath51b21a80aa17/wheretowork/extdata/projects/sim_raster/sim_raster_boundary.csv.gz
#> 
#> $themes
#> $themes[[1]]
#> Theme
#>   id:        0d082c21-8df7-4648-a51b-834374517e05
#>   name:      Brown-necked Parrot (Brown-necked)
#>   feature: 
#>     Brown-necked Parrot (Brown-necked) habitat [status: TRUE, current: 0.81, goal: 0.62]
#>       variable: .../sim_raster_spatial.tif#Brown_necked_Parrot_Brown_necked_ [total: 483 ha]
#> 
#> $themes[[2]]
#> Theme
#>   id:        b5d6791f-08f1-4517-8424-0faad59a705c
#>   name:      Gough Island Finch
#>   feature: 
#>     Gough Island Finch habitat [status: TRUE, current: 0.83, goal: 0.54]
#>       variable: .../sim_raster_spatial.tif#Gough_Island_Finch [total: 25747.97 ha]
#> 
#> $themes[[3]]
#> Theme
#>   id:        62b42177-9f10-40d2-b180-e02e9850694b
#>   name:      Manus Boobook
#>   feature: 
#>     Manus Boobook habitat [status: TRUE, current: 0.79, goal: 0.5]
#>       variable: .../sim_raster_spatial.tif#Manus_Boobook [total: 23474.88 ha]
#> 
#> $themes[[4]]
#> Theme
#>   id:        f4aa4ac1-8beb-4664-81c4-a0b9715a8cfd
#>   name:      Flufftails
#>   feature: 
#>     Red-chested Flufftail [status: TRUE, current: 0.81, goal: 0.7]
#>       variable: .../sim_raster_spatial.tif#White_spotted_Flufftail_Northern_ [total: 16872.87 ha]
#>     Chestnut-headed Flufftail [status: TRUE, current: 0.81, goal: 0.7]
#>       variable: .../sim_raster_spatial.tif#White_spotted_Flufftail_Northern_ [total: 16872.87 ha]
#>     Tsingy Wood-Rail [status: TRUE, current: 0.81, goal: 0.73]
#>       variable: .../sim_raster_spatial.tif#White_spotted_Flufftail_Northern_ [total: 16872.87 ha]
#>     Streaky-breasted Flufftail [status: TRUE, current: 0.81, goal: 0.74]
#>       variable: .../sim_raster_spatial.tif#White_spotted_Flufftail_Northern_ [total: 16872.87 ha]
#>     Madagascar Flufftail [status: TRUE, current: 0.81, goal: 0.8]
#>       variable: .../sim_raster_spatial.tif#White_spotted_Flufftail_Northern_ [total: 16872.87 ha]
#>     Buff-spotted Flufftail [status: TRUE, current: 0.81, goal: 0.86]
#>       variable: .../sim_raster_spatial.tif#White_spotted_Flufftail_Northern_ [total: 16872.87 ha]
#>     White-winged Flufftail [status: TRUE, current: 0.81, goal: 0.76]
#>       variable: .../sim_raster_spatial.tif#White_spotted_Flufftail_Northern_ [total: 16872.87 ha]
#>     White-spotted Flufftail [status: TRUE, current: 0.81, goal: 0.64]
#>       variable: .../sim_raster_spatial.tif#White_spotted_Flufftail_Northern_ [total: 16872.87 ha]
#>     White-spotted Flufftail (Northern) [status: TRUE, current: 0.81, goal: 0.81]
#>       variable: .../sim_raster_spatial.tif#White_spotted_Flufftail_Northern_ [total: 16872.87 ha]
#> 
#> $themes[[5]]
#> Theme
#>   id:        815c52b5-5057-4797-89f2-ff4ed0c543df
#>   name:      Todies
#>   feature: 
#>     Puerto Rican Tody [status: TRUE, current: 0.79, goal: 0.6]
#>       variable: .../sim_raster_spatial.tif#Jamaican_Tody [total: 14614.97 ha]
#>     Jamaican Tody [status: TRUE, current: 0.79, goal: 0.73]
#>       variable: .../sim_raster_spatial.tif#Jamaican_Tody [total: 14614.97 ha]
#> 
#> 
#> $weights
#> $weights[[1]]
#> Weight
#>   id:       e663aecc-077d-4ba4-8edd-d9e3d1b7ceae
#>   name:     Population count
#>   variable: .../sim_raster_spatial.tif#Population_count [total: 174.9 ha]
#>   pane:  d0878796-462a-4104-8bfd-b8d4886fe4b9-Population_count
#>   current:  0.8
#>   visible:  TRUE
#>   invisible:  NA
#>   loaded:  TRUE
#>   hidden:  FALSE
#>   downloadable:  TRUE
#>   status:   FALSE
#>   factor:   0
#> 
#> $weights[[2]]
#> Weight
#>   id:       00ec5c09-83b2-4b04-bc55-474ce489fdbd
#>   name:     Transportation network
#>   variable: .../sim_raster_spatial.tif#Transportation_network [total: 140.56 ha]
#>   pane:  68d5fa04-3eac-4aa1-a60a-27b3dfc346c6-Transportation_network
#>   current:  0.82
#>   visible:  TRUE
#>   invisible:  NA
#>   loaded:  TRUE
#>   hidden:  FALSE
#>   downloadable:  TRUE
#>   status:   FALSE
#>   factor:   0
#> 
#> $weights[[3]]
#> Weight
#>   id:       bb08bcb0-71a2-4c84-9f27-c63a0d5e34ab
#>   name:     Sea level rise
#>   variable: .../sim_raster_spatial.tif#Sea_level_rise [total: 144.44 ha]
#>   pane:  04caba0a-fdaa-4b35-b899-195cc7c086c9-Sea_level_rise
#>   current:  0.79
#>   visible:  TRUE
#>   invisible:  NA
#>   loaded:  TRUE
#>   hidden:  FALSE
#>   downloadable:  TRUE
#>   status:   FALSE
#>   factor:   0
#> 
#> 
#> $includes
#> $includes[[1]]
#> Include
#>   id:       ff01a79a-3af2-465a-b5e0-a2bc983aed94
#>   name:     Protected landscapes
#>   variable: .../sim_raster_spatial.tif#Protected_landscapes [total: 182 ]
#>   pane:  a7707d89-1024-45ac-b026-a895bd16eb06-Protected_landscapes
#>   visible:  TRUE
#>   invisible:  NA
#>   loaded:  TRUE
#>   hidden:  FALSE
#>   downloadable:  TRUE
#>   status:   TRUE
#>   overlap:   NA
#> 
#> $includes[[2]]
#> Include
#>   id:       eeb5ae09-52cf-4037-abbb-eeb63346065a
#>   name:     OECM areas
#>   variable: .../sim_raster_spatial.tif#OECM_areas [total: 157 ]
#>   pane:  ff5e0e40-129a-489b-95fb-173f62fca369-OECM_areas
#>   visible:  TRUE
#>   invisible:  NA
#>   loaded:  TRUE
#>   hidden:  FALSE
#>   downloadable:  TRUE
#>   status:   TRUE
#>   overlap:   NA
#> 
#> 
#> $excludes
#> list()
#> 
#> $mode
#> [1] "advanced"
#> 
```
