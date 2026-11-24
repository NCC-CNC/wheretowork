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
#>     spatial: /tmp/RtmpqwxAqW/temp_libpath530240a470f5/wheretowork/extdata/projects/sim_raster/sim_raster_spatial.tif
#>     attribute: /tmp/RtmpqwxAqW/temp_libpath530240a470f5/wheretowork/extdata/projects/sim_raster/sim_raster_attribute.csv.gz
#>     boundary: /tmp/RtmpqwxAqW/temp_libpath530240a470f5/wheretowork/extdata/projects/sim_raster/sim_raster_boundary.csv.gz
#> 
#> $themes
#> $themes[[1]]
#> Theme
#>   id:        47cc0bf7-709c-42b1-8ebb-4552a54bccdb
#>   name:      Brown-necked Parrot (Brown-necked)
#>   feature: 
#>     Brown-necked Parrot (Brown-necked) habitat [status: TRUE, current: 0.81, goal: 0.62]
#>       variable: .../sim_raster_spatial.tif#Brown_necked_Parrot_Brown_necked_ [total: 483 ha]
#> 
#> $themes[[2]]
#> Theme
#>   id:        bb783262-b704-414d-8948-f0ed3f286572
#>   name:      Gough Island Finch
#>   feature: 
#>     Gough Island Finch habitat [status: TRUE, current: 0.83, goal: 0.54]
#>       variable: .../sim_raster_spatial.tif#Gough_Island_Finch [total: 25747.97 ha]
#> 
#> $themes[[3]]
#> Theme
#>   id:        c9ef03de-527f-43a5-a480-edbab2fd53ae
#>   name:      Manus Boobook
#>   feature: 
#>     Manus Boobook habitat [status: TRUE, current: 0.79, goal: 0.5]
#>       variable: .../sim_raster_spatial.tif#Manus_Boobook [total: 23474.88 ha]
#> 
#> $themes[[4]]
#> Theme
#>   id:        c8e0f48a-f1d4-43ba-86e4-d369a82536b3
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
#>   id:        d6529ee3-33fd-4c9a-b0d8-3a6dffe79678
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
#>   id:       c4b441ec-d97b-468f-8e13-a022ca2bba25
#>   name:     Population count
#>   variable: .../sim_raster_spatial.tif#Population_count [total: 174.9 ha]
#>   pane:  ccd9925c-380b-43d6-b34d-14fd99c1964e-Population_count
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
#>   id:       cc5344d5-f36c-4681-b47c-9e2e72711b24
#>   name:     Transportation network
#>   variable: .../sim_raster_spatial.tif#Transportation_network [total: 140.56 ha]
#>   pane:  85cb9cff-e8e6-42ff-8c75-f97c4731ba23-Transportation_network
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
#>   id:       c9dbb05b-ba7a-492d-b84b-be1dc9698343
#>   name:     Sea level rise
#>   variable: .../sim_raster_spatial.tif#Sea_level_rise [total: 144.44 ha]
#>   pane:  77bb96c3-6eba-4b88-a971-d4aeab16d3fa-Sea_level_rise
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
#>   id:       84f11bf6-dd1a-477f-9c20-d81c9f8183e4
#>   name:     Protected landscapes
#>   variable: .../sim_raster_spatial.tif#Protected_landscapes [total: 182 ]
#>   pane:  79395b62-497e-4e43-9331-d44d083fb4eb-Protected_landscapes
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
#>   id:       fd78d6d4-0a39-448f-a313-75dc9e742c10
#>   name:     OECM areas
#>   variable: .../sim_raster_spatial.tif#OECM_areas [total: 157 ]
#>   pane:  eda5ded5-0d6e-42e6-aae1-ec889af0f897-OECM_areas
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
