# Read App Configuration

Read App Configuration

## Usage

``` r
get_golem_config(
  value,
  config = Sys.getenv("GOLEM_CONFIG_ACTIVE", Sys.getenv("R_CONFIG_ACTIVE", "default")),
  use_parent = TRUE
)
```

## Arguments

- value:

  Value to retrieve from the configuration file.

- config:

  GOLEM_CONFIG_ACTIVE value. If unset, R_CONFIG_ACTIVE. If unset,
  "default".

- use_parent:

  Logical, scan the parent directory for configuration file.
