# wheretowork 1.1.3

### Minor changes and bug fixes
- fixed shapefile uploads, bug #396

# wheretowork 1.1.2

### Minor changes and bug fixes

- New `downloadable` field for `Features`, `Weights`, `Includes`, `Excludes` 
and `Solutions`. When `downloadable` is set to `FALSE`, the spatial layer will not 
be available for export. Sensitive layers not to be downloaded can be explicitly 
set in the [Where To Work data prep workflow](https://github.com/NCC-CNC/wtw-data-prep).

# wheretowork 1.0.2

### Minor changes and bug fixes

- Multiple layer download bug fix, issue #391.

# wheretowork 1.0.1

### Minor changes and bug fixes

- Updated wheretowork pkgdown site

# wheretowork 1.0.0

### Notice

- Official Where To Work version 1.

### Minor changes and bug fixes

- Boundary data required for spatial clustering has changed to the prioritizr 8.0.4 format. This includes a better approach for rescaling data to avoid numerical issues during optimization. Earlier versions of the package recommended the use of the `scales::rescale()` to rescale such data. Now we use `prioritizr::rescale_matrix()` function to rescale boundary length data.
- Where To Work can still load older boundary data generated from previous projects, however it is recommended that new projects prep data with prioritizr >= 8.0.4 and wheretowork 1.0.0.
- The `renv.lock` file only includes application dependencies from Imports, Depends and LinksTo found within the DESCRIPTION file. It does not include the Suggests packages needed for development. This reduces the bloat on deployment and installing wheretowork for data prep. The steps to contribute to package development include cloning wheretowork, running `renv::restore()`, and then manually `renv::install` the Suggests packages referencing the correct version. Example `renv::install(testthat@3.3.2.1.1)`. Gurobi is not a CRAN package. Follow the gurobi installation guide found on the prioritizr.net article: https://prioritizr.net/articles/gurobi_installation_guide.html.

### Major changes

- Requires R version 4.4.
- Migrated from raster package to terra package.
- Updated all CRAN package dependencies to latest version as of August, 2024.
- Updated REMOTE leaflet package dependency to latest version as of August, 2024.
- Added backwards compatibility for boundary data.
- Removed `leaflet.extras2::addHistory` button. This feature is not compatible with Shiny >= 1.7.0 when they removed the bundled copy of fontawesome to the fontawesome package.
- Updated license agreement.
- Updated `renv.lock` file only records application dependencies from Imports, Depends and LinksTo.

### Minor changes and bug fixes

- The `write_project` function now includes the wheretowork and prioritizr package version number to the `attribute.yaml`.
- The `read_project` function checks if the prioritizr version number does not exist or is < version 8.0.4. If true, then `update_bm` method is called on the Dataset class to update the boundary data to the new format.
- Replaced pryr with lobstr for tracking memory usage.
- Removed `readNamedRaster`, this function was never called.
- Removed raster layer names as a .txt in `writeNamedRaster` function. The `terra::writeRaster` function by default keeps layer names.

### Infrastrucure changes

- Updated to rocker/shiny:4.4.0 AS base in Dockerfile.

# wheretowork 0.0.0.9000

- Initial package version.
