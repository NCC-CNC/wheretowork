
<!--- README.md is generated from README.Rmd. Please edit that file -->

# Where to Work: Interactive systematic conservation planning application

[![lifecycle](https://img.shields.io/badge/Lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![Docker
Status](https://img.shields.io/docker/cloud/build/naturecons/wheretowork?label=Docker%20build)](https://hub.docker.com/r/naturecons/wheretowork)

The *Where to Work* application is a decision support tool to help
prioritize conservation efforts for the [Nature Conservancy of
Canada](https://natureconservancy.ca/en/).

## Usage

The application is [available online
here](https://natureconservancy.ca/en/). **Please note that you must use
[Google Chrome](https://www.google.com/chrome/) for it to work.**.

## Installation

The application is available as an online service provided by the
[Nature Conservancy of Canada](https://natureconservancy.ca/en/). As
such, you don’t need to install it on your computer to use: just go to
[this link and it’s ready to go](https://natureconservancy.ca/en/). If
you need to run the application on your own computer, then you can run
it using [Docker](https://www.docker.com/) or the [R statistical
computing environment](https://www.r-project.org/).

### Using Docker

To run the application using this method, you will need to install the
[Docker Engine](https://www.docker.com/) software ([see here for
instructions](https://docs.docker.com/get-docker/)). After completing
this step, you can install the application from the [DockerHub
repository](https://hub.docker.com/repository/docker/naturecons/wheretowork).
Specifically, please use the following system command to install the
latest official version of the application:

``` bash
docker run -dp 3838:3838 --name wheretowork -it naturecons/wheretowork:latest
```

You can then view the application by opening the following link in
[Google Chrome](https://www.google.com/chrome/):
<http://localhost:3838>. After you have finished using the application,
you can terminate it using the following system command. **Note that if
you don’t terminate the application once you are finished using it, then
it will continue running in the background.**

``` bash
docker rm -f wheretowork
```

Additionally, the latest development version can be installed using the
following system command. Please note that while developmental versions
may contain additional features not present in the official version,
they may also contain defects.

``` bash
docker run -dp 3838:3838 --name wheretowork -it naturecons/wheretowork:devel
```

Similar to the official version, you can access the developmental
version of the application by opening the following link in [Google
Chrome](https://www.google.com/chrome/): <http://localhost:3838>. Also
note that you should terminate the developmental version of the
application once you are finished using it (using the same code above
for the official version).

### Using R

To run the application using this method, you will need to install the
[R statistical computing environment](https://www.r-project.org/). After
completing the installation, you can install the latest official version
of the application by using the following R code:

``` r
if (!require(remotes)) install.packages("remotes")
remotes::install_github("NCC-CNC/wheretowork@*release")
```

You can then use the following R code to run the application and
automatically open it in your web browser:

``` r
wheretowork::run_app()
```

Additionally, the latest development version can be installed using the
following R code. Please note that while developmental versions may
contain additional features not present in the official version, they
may also contain defects.

``` r
if (!require(remotes)) install.packages("remotes")
remotes::install_github("NCC-CNC/wheretowork")
```

## Contributing

The application is a [Shiny web
application](https://mastering-shiny.org/) developed using the [R
statistical computing environment](https://www.r-project.org/).
Specifically, it uses the [`golem`
framework](https://thinkr-open.github.io/golem/). This means that the
application is effectively an [R package](https://r-pkgs.org/) that
contains code for defining and launching the application (see
[here](https://engineering-shiny.org/) for more details). The R code
files (located in the `./R` directory) are organized the following
system of naming conventions:

-   `app_*`: Defines the web application:
    -   `app_config.R`: Imports configuration settings.
    -   `app_global.R`: Initializes the application. It performs a
        similar to the `global.R` file in typical Shiny applications.
    -   `app_server.R`: Defines the (back-end) server-side logic for the
        application. It performs a similar role to the `server.R` file
        in typical Shiny applications.
    -   `app_ui.R`: Defines the (font-end) user interface for the
        application. It performs a similar role to the `ui.R` file in
        typical Shiny applications.
-   `server_*`: Defines components used to assemble the server-side
    logic for the application.
-   `ui_`\*: Defines functions used to programmatically create HTML
    elements for the front-end of the application.
-   `class_*`: Defines object orientated programming classes used in the
    back-end of the application. These classes are implemented using [R6
    class system](https://r6.r-lib.org/).
-   `fct_*`: Defines R functions used in the back-end of the
    application. These files contain code used to perform analyses and
    manipulate the classes.
-   `widget_*`: Defines custom widgets used by the application. These
    widgets are implemented using the [`htmlwidgets`
    framework](https://www.htmlwidgets.org/).
-   `utils_*`: Defines utility R functions used in the back-end of the
    application.

## Getting help

If you have any questions about using the *Where to Work* application or
suggestions for improving it, please contact [Dr. Richard
Schuster](https://www.richard-schuster.com/)
(<richard.schuster@natureconservancy.ca>) or [Prof. Joe
Bennett](https://carleton.ca/bennett-lab/lab-members/)
([JosephBennett@cunet.carleton.ca](mailto:mailto:JosephBennett@cunet.carleton.ca)).
