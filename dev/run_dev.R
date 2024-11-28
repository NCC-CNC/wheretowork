# Set options here
options(golem.app.prod = FALSE) # TRUE = production mode, FALSE = development mode

# Detach all loaded packages and clean your environment
golem::detach_all_attached()
# rm(list=ls(all.names = TRUE))

# Document and reload your package
golem::document_and_reload()

# Install package for asynchronous processing only on unix-type platforms.
# The "stop optimizing button" is hidden when running the application on 
# Windows (plan strategy: multisession).
# Must run app on a Linux machine to develop and debug async features 
# (plan strategy: multicore). 
if (!identical(getOption("quick"), TRUE) && 
  identical(.Platform$OS.type, "unix")){
  # THIS NOW INSTALLS THE LATEST VERSION OF PACKAGES ON CRAN. 
  # NOT WHAT WE WANT, FLAG FOR NOW.
  renv::install(".", upgrade = "never", force = TRUE)
}

# Run the application
run_app(options = list(launch.browser = TRUE))
