# fix Shiny autoload issue
## Note that we can't actually use `golem::disable_autoload()` to achieve
## this because it creates a  "_disable_autoload.R" file which results
## violates R package conventions and means that the application cannot
## be sucessfully installed as an R package.
options(shiny.autoload.r = FALSE)

# active renv package management
source("renv/activate.R")

# set default browser as google-chrome if possible
if (file.exists("/usr/bin/google-chrome")) {
  options(browser = "/usr/bin/google-chrome")
}
