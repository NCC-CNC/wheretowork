
<!--- README.md is generated from README.Rmd. Please edit that file -->

# Where to Work: Interactive systematic conservation planning application

[![lifecycle](https://img.shields.io/badge/Lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![R-CMD-check-Ubuntu](https://img.shields.io/github/actions/workflow/status/ncc-cnc/wheretowork/R-CMD-check-ubuntu.yaml?branch=master&label=Ubuntu)](https://github.com/NCC-CNC/wheretowork/actions)
[![R-CMD-check-Windows](https://img.shields.io/github/actions/workflow/status/ncc-cnc/wheretowork/R-CMD-check-windows.yaml?branch=master&label=Windows)](https://github.com/NCC-CNC/wheretowork/actions)
[![Docker
Status](https://img.shields.io/docker/cloud/build/naturecons/wheretowork?label=Docker%20build)](https://hub.docker.com/r/naturecons/wheretowork)
[![Coverage
Status](https://codecov.io/github/NCC-CNC/wheretowork/coverage.svg?branch=master)](https://codecov.io/github/NCC-CNC/wheretowork?branch=master)

The *Where To Work* application is a decision support tool to help
prioritize conservation efforts for the [Nature Conservancy of
Canada](https://natureconservancy.ca/en/). It provides an interactive
interface for conducting systematic conservation planning exercises, and
uses mathematical optimization algorithms to generate solutions.

## Usage

The application is [available online](https://ncc.carleton.ca). Please
note that you must use [Google Chrome](https://www.google.com/chrome/)
for it to work.

<table>

<tr>

<td>

<img class="screenshot" src="man/figures/screenshot.png" align="center" width="100%"/>
</td>

</tr>

</table>

## Installation

The application is available as an online service provided by the
[Nature Conservancy of Canada](https://natureconservancy.ca/en/). If you
need to run the application on your own computer, then you can run it
using the [R statistical computing
environment](https://www.r-project.org/),
[Docker](https://www.docker.com/), or [Docker
Compose](https://docs.docker.com/compose/).

### Using R

To use this method, you will need to install the [R statistical
computing environment](https://www.r-project.org/). After completing the
installation, you can install the application using the following R
code:

``` r
if (!require(remotes)) install.packages("remotes")
remotes::install_github("NCC-CNC/wheretowork")
```

You can then use the following R code to start the application and open
it in your web browser:

``` r
wheretowork::run_app()
```

### Using Docker

To use this method, you will need to install [Docker
Engine](https://www.docker.com/) ([see here for
instructions](https://docs.docker.com/get-docker/)). After completing
this step, you can install the application from the [DockerHub
repository](https://hub.docker.com/repository/docker/naturecons/wheretowork).
Specifically, please use the following system command:

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

### Using Docker Compose

To use this method, you will need to install [Docker
Engine](https://www.docker.com/) ([see here for
instructions](https://docs.docker.com/get-docker/)) and Docker Compose
([see here for instructions](https://docs.docker.com/compose/install/)).
After installing both programs, you can install the application by
[cloning this
repository](https://docs.github.com/en/github/creating-cloning-and-archiving-repositories/cloning-a-repository-from-github/cloning-a-repository)
and then using the following system commands:

``` bash
docker-compose pull
docker-compose up -d
```

You can then view the application by opening the following link in
[Google Chrome](https://www.google.com/chrome/):
<http://localhost:3838>. After you have finished using the application,
you can terminate it using the following system command. **Note that if
you don’t terminate the application once you are finished using it, then
it will continue running in the background.**

``` bash
docker-compose down
```

## Contributing

The application is a [Shiny web
application](https://mastering-shiny.org/) developed using the [R
statistical computing environment](https://www.r-project.org/).
Specifically, it uses the [`golem`
framework](https://thinkr-open.github.io/golem/). This means that the
application is effectively an [R package](https://r-pkgs.org/) that
contains code for defining and launching the application ([see here for
more details](https://engineering-shiny.org/)). The R code files
(located in the `./R` directory) are organized using the following
naming conventions:

- `app_*`: Defines the web application:
  - `app_config.R`: Imports configuration settings.
  - `app_global.R`: Initializes the application. It performs a similar
    to the `global.R` file in typical Shiny applications.
  - `app_server.R`: Defines the (back-end) server-side logic for the
    application. It performs a similar role to the `server.R` file in
    typical Shiny applications.
  - `app_ui.R`: Defines the (font-end) user interface for the
    application. It performs a similar role to the `ui.R` file in
    typical Shiny applications.
- `server_*`: Defines components used to assemble the server-side logic
  for the application.
- `ui_`\*: Defines functions used to programmatically create HTML
  elements for the front-end of the application.
- `class_*`: Defines object orientated programming classes used in the
  back-end of the application. These classes are implemented using [R6
  class system](https://r6.r-lib.org/).
- `fct_*`: Defines R functions used in the back-end of the application.
  These files contain code used to perform analyses and manipulate the
  classes.
- `widget_*`: Defines custom widgets used by the application. These
  widgets are implemented using the [`htmlwidgets`
  framework](https://www.htmlwidgets.org/).
- `utils_*`: Defines utility R functions used in the back-end of the
  application.

## Getting help

Thank you for checking out this application. If you encounter any
software defects (e.g. application crashes, unexpected behavior, or
spelling mistakes), please feel free to post them on the [issue
tracker](https://github.com/NCC-CNC/wheretowork/issues). If you have any
questions about using this application, please contact [Dr. Richard
Schuster](https://www.richard-schuster.com/)
(<richard.schuster@natureconservancy.ca>) or [Prof. Joe
Bennett](https://carleton.ca/bennett-lab/lab-members/)
([JosephBennett@cunet.carleton.ca](mailto:mailto:JosephBennett@cunet.carleton.ca)).
