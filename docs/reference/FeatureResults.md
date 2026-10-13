# Feature results class

Feature results class

Feature results class

## Details

Definition for the `FeatureResults` class.

## See also

[`new_feature_results()`](https://ncc-cnc.github.io/wheretowork/reference/new_feature_results.md).

## Public fields

- `id`:

  `character` value.

- `feature`:

  [Feature](https://ncc-cnc.github.io/wheretowork/reference/Feature.md)
  object.

- `status`:

  `logical` value.

- `current`:

  `numeric` value.

- `goal`:

  `numeric` value.

- `held`:

  `numeric` value.

## Methods

### Public methods

- [`FeatureResults$new()`](#method-FeatureResults-new)

- [`FeatureResults$print()`](#method-FeatureResults-print)

- [`FeatureResults$repr()`](#method-FeatureResults-repr)

- [`FeatureResults$clone()`](#method-FeatureResults-clone)

------------------------------------------------------------------------

### Method [`new()`](https://rdrr.io/r/methods/new.html)

Create a new `FeatureResults` object.

#### Usage

    FeatureResults$new(id, feature, held)

#### Arguments

- `id`:

  `character` value.

- `feature`:

  [Feature](https://ncc-cnc.github.io/wheretowork/reference/Feature.md)
  value.

- `held`:

  `character` value.

#### Returns

A new `FeatureResults` object.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print the object.

#### Usage

    FeatureResults$print(...)

#### Arguments

- `...`:

  not used.

------------------------------------------------------------------------

### Method `repr()`

Generate a `character` summarizing the representation of the object.

#### Usage

    FeatureResults$repr(start = "[", end = "]")

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

    FeatureResults$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
