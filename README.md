
<!--- README.md is generated from README.Rmd. Please edit that file -->

## locationmisc: ‘Shiny’ widgets for systematic conservation planning

[![lifecycle](https://img.shields.io/badge/Lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/experimental)

### Overview

This package provides classes, widgets, and functions for building web
applications for systematic conservation planning under the ‘Shiny’
framework.

### Installation

The package is only available from an [online coding
repository](https://github.com/NCC-CNC/locationmisc). To install it, use
the following R code.

``` r
if (!require(devtools))
  install.packages("devtools")
devtools::install_github("NCC-CNC/locationmisc")
```

### Usage

This package provides the following widgets: \* `sidebar()`: Constructs
a widget that forms a sidebar on a `leaflet` map. It contains the
following widgets for customizing its appearance: \* `sidebarTab()`
Constructs a widget that forms a tab in a sidebar. This is used to group
together widgets (e.g. those specified below and standard `shiny`
widgets) that have related functionality (e.g. a side bar tab could
contain widgets for controlling the appearance of a `leaflet` map). \*
`newSolutionPanel()`: Constructs a widget for creating new solutions. It
forms a panel that contains – in addition to standard `shiny` widgets
(e.g. a button to generate a new solution) – the following widgets for
setting goals and weights: \* `singleThemeGoal()`: Constructs a widget
for controlling the goal for a theme with a single feature. \*
`multiThemeGoal()`: Constructs a widget for controlling the goals for a
theme with multiple features. \* `weightFactor()`: Constructs a widget
for controlling the factor for a weight. \* `mapPanel()`: Constructs a
widget for controlling the overall appearance of a `leaflet` map. It
forms a panel that contains – in addition to standard `shiny` widgets
(e.g. a button to hide all solutions) – the following widgets: \*
`tableOfContents()`: Constructs a container widget that contains widgets
for controlling the appearance of a particular map component
(e.g. theme, solution, weight). Critically, the order of these widgets
within this widget controls their relative plotting order on the
`leaflet` map. It forms a panel containing the following widgets: \*
`singleThemeVisual()`: Constructs a widget for controlling the
appearance of a theme with a single feature on a `leaflet` map. \*
`multiThemeVisual()`: Constructs a widget for controlling the appearance
of a theme with multiple features on a `leaflet` map. \*
`weightVisual()`: Constructs a widget for controlling the appearance of
a weight on a `leaflet` map. \* `solutionVisual()`: Constructs a widget
for controlling the appearance of a solution on a `leaflet` map. It also
contains buttons to inspect the statistics describing the
performance/area/representation of solutions too.
