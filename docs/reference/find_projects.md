# Find projects

Find project files in a directory.

## Usage

``` r
find_projects(x, user_groups = "public")
```

## Arguments

- x:

  `character` directory path.

- user_groups:

  `character` vector of project group names available that can be
  imported. Defaults to `"public"`.

## Value

[`tibble::tibble()`](https://tibble.tidyverse.org/reference/tibble.html)
object containing the configuration files and names of projects found in
the directory.

## Details

Projects that are missing user group information are automatically
assigned to the `"private"` user group. Additionally, note that
`"admin"` is a special user group that can access all projects.

## Examples

``` r
# find directory with built-in projects
d <- system.file("extdata", "projects", package = "wheretowork")

# list projects in directory
find_projects(d)
#> # A tibble: 3 × 3
#>   path                                                              name  status
#>   <chr>                                                             <chr> <lgl> 
#> 1 C:/Users/shaun.astbury/AppData/Local/Temp/RtmpuUTJMm/temp_libpat… Exam… TRUE  
#> 2 C:/Users/shaun.astbury/AppData/Local/Temp/RtmpuUTJMm/temp_libpat… Exam… TRUE  
#> 3 C:/Users/shaun.astbury/AppData/Local/Temp/RtmpuUTJMm/temp_libpat… Sout… TRUE  
```
