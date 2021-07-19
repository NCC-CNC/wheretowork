
<!--- README.md is generated from README.Rmd. Please edit that file -->

# Where to Work: Interactive application for systematic conservation planning

[![lifecycle](https://img.shields.io/badge/Lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![R-CMD-check-Ubuntu](https://img.shields.io/github/workflow/status/NCC-CNC/wheretowork/Ubuntu/master.svg?label=Ubuntu)](https://github.com/NCC-CNC/wheretowork/actions)
[![R-CMD-check-Windows](https://img.shields.io/github/workflow/status/NCC-CNC/wheretowork/Windows/master.svg?label=Windows)](https://github.com/NCC-CNC/wheretowork/actions)
[![R-CMD-check-Mac-OSX](https://img.shields.io/github/workflow/status/NCC-CNC/wheretowork/Mac%20OSX/master.svg?label=Mac%20OSX)](https://github.com/NCC-CNC/wheretowork/actions)
[![Docker
Status](https://img.shields.io/docker/cloud/build/naturecons/wheretowork?label=Docker%20build)](https://hub.docker.com/r/naturecons/wheretowork)
[![Coverage
Status](https://codecov.io/github/NCC-CNC/wheretowork/coverage.svg?branch=master)](https://codecov.io/github/NCC-CNC/wheretowork?branch=master)

The *Where to Work* application is a decision support tool to help
prioritize conservation efforts for the [Nature Conservancy of
Canada](https://natureconservancy.ca/en/).

## Installation

The latest official version can be installed from
[DockerHub](https://hub.docker.com/repository/docker/naturecons/wheretowork).
After [installing Docker on your
system](https://docs.docker.com/get-docker/), please use the following
system command to install the application:

``` bash
docker run -dp 3838:3838 -it naturecons/wheretowork:latest
```

You can then view the application by opening the following link in
[Google Chrome](https://www.google.com/chrome/):
<http://localhost:3838>.

Alternatively, the latest development version can be installed using the
following system command. Please note that while developmental versions
may contain additional features not present in the official version,
they may also contain defects.

``` bash
docker run -dp 3838:3838 -it naturecons/wheretowork:devel
```

You can then view the development version of the application – similar
to the official version – by opening the following link in [Google
Chrome](https://www.google.com/chrome/): <http://localhost:3838>.

## Usage

TODO.

## Contributing

The application is a [Shiny web application](https://shiny.rstudio.com/)
developed using the [R statistical computing
environment](https://www.r-project.org/). Specifically, it uses the
[`golem` framework](https://thinkr-open.github.io/golem/). This means
that the application is effectively an [R package](https://r-pkgs.org/)
that contains code for defining and launching the application (see
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
