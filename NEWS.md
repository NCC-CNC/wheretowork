# wheretowork 1.0.0

### Notice
Official Where To Work version 1. 

Boundary data required for spatial clustering has changed to the prioritizr
8.0.4 format. This includes a better approach for rescaling data to avoid 
numerical issues during optimization. Earlier versions of the package 
recommended the use of the `scales::rescale()` to rescale such data. Now we use 
`prioritizr::rescale_matrix()` function to rescale boundary length data. 

Where To Work can still load older boundary data generated from previous 
projects, however it is recommended that new projects prep data with 
prioritizr >= 8.0.4 and wheretowork 1.0.0.

The `renv.lock` file only includes application dependencies from Imports, Depends 
and LinksTo found within the DESCRIPTION file. It does not include the Suggests 
packages needed for development. This reduces the bloat on deployment and 
installing wheretowork for data prep. The steps to contribute to package 
development include cloning wheretowork, running `renv::restore()`, and then
manually `renv::install` the Suggests packages referencing the correct version.
Example `renv::install(testthat@3.3.2.1.1)`. Gurobi is not a CRAN package. Follow 
the gurobi installation guide found on the prioritizr.net article:
https://prioritizr.net/articles/gurobi_installation_guide.html.

### Major changes
- requires R version 4.4.
- migrated from raster package to terra package.
- updated all CRAN package dependencies to latest version as of August, 2024.
- updated REMOTE leaflet package dependency to latest version as of August, 2024.
- added backwards compatibility for boundary data.
- removed leaflet.extras2::addHistory button. This feature is not compatible with
Shiny >= 1.7.0 when they removed the bundled copy of fontawesome to the 
{fontawesome} package.
- updated licence agreement.
- renv.lock file only records application dependencies from Imports, Depends and
LinksTo

### Minor changes and bug fixes
- The write_project function now includes the wheretowork and prioritizr package 
version number to the attribute.yaml.
- The read_project function checks if the prioritizr version number does 
not exist or is < version 8.0.4. If true, then update_bm method is called 
on the Dataset class to update the boundary data to the new format.
- replaced pryr with lobstr for tracking memory usage.
- removed readNamedRaster, this function was never called. 
- removed raster layer names as a .txt in writeNamedRaster function.
The terra::writeRaster function by default keeps layer names. 

### Infrastrucure changes
- updated to rocker/shiny:4.4.0 AS base in Dockerfile. 

# wheretowork 0.0.0.9000

- Initial package version.
