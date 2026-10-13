# Result class

Result class

Result class

## Details

Definition for the Result class.

## See also

[`new_result()`](https://ncc-cnc.github.io/wheretowork/reference/new_result.md).

## Public fields

- `id`:

  `character` value.

- `values`:

  `numeric` vector.

- `area`:

  `numeric` value.

- `perimeter`:

  `numeric` value or `NA_real_`.

- `theme_coverage`:

  `numeric` value.

- `weight_coverage`:

  `numeric` value.

- `include_coverage`:

  `numeric` value.

- `exclude_coverage`:

  `numeric` value.

- `theme_settings`:

  `data.frame` object.

- `weight_settings`:

  `data.frame` object.

- `include_settings`:

  `data.frame` object.

- `exclude_settings`:

  `data.frame` object.

- `parameters`:

  `list` of
  [Parameter](https://ncc-cnc.github.io/wheretowork/reference/Parameter.md)
  objects.

## Methods

### Public methods

- [`Result$new()`](#method-Result-new)

- [`Result$print()`](#method-Result-print)

- [`Result$repr()`](#method-Result-repr)

- [`Result$clone()`](#method-Result-clone)

------------------------------------------------------------------------

### Method [`new()`](https://rdrr.io/r/methods/new.html)

Create a new Result object.

#### Usage

    Result$new(
      id,
      values,
      area,
      perimeter,
      theme_coverage,
      weight_coverage,
      include_coverage,
      exclude_coverage,
      theme_settings,
      weight_settings,
      include_settings,
      exclude_settings,
      parameters
    )

#### Arguments

- `id`:

  `character` value.

- `values`:

  `numeric` vector.

- `area`:

  `numeric` value.

- `perimeter`:

  `numeric` value or `NA_real_`.

- `theme_coverage`:

  `numeric` vector.

- `weight_coverage`:

  `numeric` vector.

- `include_coverage`:

  `numeric` vector.

- `exclude_coverage`:

  `numeric` vector.

- `theme_settings`:

  `logical` value.

- `weight_settings`:

  `tbl_df` data frame.

- `include_settings`:

  `tbl_df` data frame.

- `exclude_settings`:

  `tbl_df` data frame.

- `parameters`:

  `list` of
  [Parameter](https://ncc-cnc.github.io/wheretowork/reference/Parameter.md)
  objects.

#### Returns

A new Result object.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print the object.

#### Usage

    Result$print(...)

#### Arguments

- `...`:

  not used.

------------------------------------------------------------------------

### Method `repr()`

Generate a `character` summarizing the representation of the object.

#### Usage

    Result$repr(start = "[", end = "]")

#### Arguments

- `start`:

  `character` symbol used to start the setting list. Defaults to `"["`.

- `end`:

  `character` symbol used to start the setting list. Defaults to `"]"`.

#### Returns

`character` value.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Result$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
