# Provenance class

Provenance class

Provenance class

## Details

Definition for the Provenance class.

## See also

[`new_provenance()`](https://ncc-cnc.github.io/wheretowork/reference/new_provenance.md).

## Public fields

- `name`:

  `character` value.

- `icon`:

  `character` value

- `color`:

  `character` value.

- `description`:

  `character` value

## Methods

### Public methods

- [`Provenance$new()`](#method-Provenance-new)

- [`Provenance$print()`](#method-Provenance-print)

- [`Provenance$repr()`](#method-Provenance-repr)

- [`Provenance$get_widget_data()`](#method-Provenance-get_widget_data)

- [`Provenance$export()`](#method-Provenance-export)

- [`Provenance$clone()`](#method-Provenance-clone)

------------------------------------------------------------------------

### Method [`new()`](https://rdrr.io/r/methods/new.html)

Create a Provenance object.

#### Usage

    Provenance$new(name, icon, color, description)

#### Arguments

- `name`:

  `character` value.

- `icon`:

  `numeric` value

- `color`:

  `character` value.

- `description`:

  `numeric` value.

#### Returns

A Provenance object.

------------------------------------------------------------------------

### Method [`print()`](https://rdrr.io/r/base/print.html)

Print the object.

#### Usage

    Provenance$print(...)

#### Arguments

- `...`:

  not used.

------------------------------------------------------------------------

### Method `repr()`

Generate a `character` summarizing the representation of the object.

#### Usage

    Provenance$repr(...)

#### Arguments

- `...`:

  not used.

#### Returns

`character` value.

------------------------------------------------------------------------

### Method `get_widget_data()`

Get widget data.

#### Usage

    Provenance$get_widget_data()

#### Returns

`list` object.

------------------------------------------------------------------------

### Method `export()`

Get widget data.

#### Usage

    Provenance$export()

#### Returns

`character` value.

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    Provenance$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
