# Write project

Save a project to disk.

## Usage

``` r
write_project(
  x,
  dataset,
  path,
  name,
  spatial_path,
  attribute_path,
  boundary_path,
  mode = "advanced",
  user_groups = "public",
  author_name = NULL,
  author_email = NULL
)
```

## Arguments

- x:

  `list` of
  [Theme](https://ncc-cnc.github.io/wheretowork/reference/Theme.md),
  [Weight](https://ncc-cnc.github.io/wheretowork/reference/Weight.md),
  [Include](https://ncc-cnc.github.io/wheretowork/reference/Include.md),
  and
  [Exclude](https://ncc-cnc.github.io/wheretowork/reference/Exclude.md)
  objects.

- dataset:

  [Dataset](https://ncc-cnc.github.io/wheretowork/reference/Dataset.md)
  object.

- path:

  `character` file path to save the configuration file.

- name:

  `character` name for the scenario.

- spatial_path:

  `character` file path for the spatial data.

- attribute_path:

  `character` file path for the attribute data.

- boundary_path:

  `character` file path for the attribute data.

- mode:

  `character` mode for running the application. Defaults to
  `"advanced"`.

- user_groups:

  `character` vector of user groups than can access the dataset.
  Defaults to `"public"`.

- author_name:

  `character` name of the project author. Defaults to `NULL` such that
  no author name is encoded in the project configuration file. This
  means that the application will report default contact details.

- author_email:

  `character` email address of the project author. Defaults to `NULL`
  such that no email address is encoded in the project configuration
  file. This means that the application will report default contact
  details.

## Value

Invisible `TRUE` indicating success.

## Examples

``` r
 # find data file paths
 f1 <- system.file(
   "extdata", "projects", "sim_raster", "sim_raster_spatial.tif",
   package = "wheretowork"
 )
 f2 <- system.file(
   "extdata", "projects", "sim_raster", "sim_raster_attribute.csv.gz",
   package = "wheretowork"
 )
 f3 <- system.file(
   "extdata", "projects", "sim_raster", "sim_raster_boundary.csv.gz",
   package = "wheretowork"
 )

 # create new dataset
 d <- new_dataset(f1, f2, f3)

 # simulate themes and weights
 th <- simulate_themes(d, 1, 1, 2)
 w <- simulate_weights(d, 1)

 # combine themes and weights into a list
 l <- append(th, w)

 # save project
 write_project(
   x = l,
   name = "example",
   dataset = d,
   path = tempfile(),
   spatial_path = tempfile(fileext = ".tif"),
   attribute_path = tempfile(fileext = ".csv.gz"),
   boundary_path = tempfile(fileext = ".csv.gz")
 )
```
