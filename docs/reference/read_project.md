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
#>     spatial: C:/Users/shaun.astbury/AppData/Local/Temp/RtmpuUTJMm/temp_libpath23344d0a1f4c/wheretowork/extdata/projects/sim_raster/sim_raster_spatial.tif
#>     attribute: C:/Users/shaun.astbury/AppData/Local/Temp/RtmpuUTJMm/temp_libpath23344d0a1f4c/wheretowork/extdata/projects/sim_raster/sim_raster_attribute.csv.gz
#>     boundary: C:/Users/shaun.astbury/AppData/Local/Temp/RtmpuUTJMm/temp_libpath23344d0a1f4c/wheretowork/extdata/projects/sim_raster/sim_raster_boundary.csv.gz
#> 
#> $themes
#> $themes[[1]]
#> Theme
#>   id:        d5af763f-f310-472c-a66c-fbde54510700
#>   name:      Brown-necked Parrot (Brown-necked)
#>   feature: 
#>     Brown-necked Parrot (Brown-necked) habitat [status: TRUE, current: 0.81, goal: 0.62]
#>       variable: .../sim_raster_spatial.tif#Brown_necked_Parrot_Brown_necked_ [total: 483 ha]
#> 
#> $themes[[2]]
#> Theme
#>   id:        aab893bc-f971-44d4-abea-3fd6ee852247
#>   name:      Gough Island Finch
#>   feature: 
#>     Gough Island Finch habitat [status: TRUE, current: 0.83, goal: 0.54]
#>       variable: .../sim_raster_spatial.tif#Gough_Island_Finch [total: 25747.97 ha]
#> 
#> $themes[[3]]
#> Theme
#>   id:        e737d34a-21df-4884-a466-5f7e469f7b6c
#>   name:      Manus Boobook
#>   feature: 
#>     Manus Boobook habitat [status: TRUE, current: 0.79, goal: 0.5]
#>       variable: .../sim_raster_spatial.tif#Manus_Boobook [total: 23474.88 ha]
#> 
#> $themes[[4]]
#> Theme
#>   id:        0d0035bb-25ce-4263-919e-5c357e9c4f16
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
#>   id:        5a3c72d2-9cac-409f-a931-7f4e725f7d6b
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
#>   id:       9ec322b1-43f8-41e0-99c9-9701bce8875b
#>   name:     Population count
#>   variable: .../sim_raster_spatial.tif#Population_count [total: 174.9 ha]
#>   pane:  52bbda8e-5eae-48f6-b0a9-02b22a00ccbb-Population_count
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
#>   id:       a7a4f806-d536-4a72-908a-10b9a9e82d6f
#>   name:     Transportation network
#>   variable: .../sim_raster_spatial.tif#Transportation_network [total: 140.56 ha]
#>   pane:  ae0f2cc1-0581-407b-90d6-1b17044bcf28-Transportation_network
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
#>   id:       b4844079-5bb0-443d-b4bb-050b4fadaa55
#>   name:     Sea level rise
#>   variable: .../sim_raster_spatial.tif#Sea_level_rise [total: 144.44 ha]
#>   pane:  8b3572d0-7a7c-454f-97e7-543b00213c61-Sea_level_rise
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
#>   id:       e3daeebb-bfc4-4d16-8900-ddb44a668e46
#>   name:     Protected landscapes
#>   variable: .../sim_raster_spatial.tif#Protected_landscapes [total: 182 ]
#>   pane:  b4fc1c12-704c-4bf0-971f-27a436ad098d-Protected_landscapes
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
#>   id:       f4d54c10-bf28-4db8-b66e-c8b339f11cd3
#>   name:     OECM areas
#>   variable: .../sim_raster_spatial.tif#OECM_areas [total: 157 ]
#>   pane:  05daadfb-3d17-43e3-a6b1-68ae9f600a9b-OECM_areas
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
