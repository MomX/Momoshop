# Launch Momoshop Interactive Pipeline Builder

Momoshop provides an interactive interface for building, previewing, and
exporting image processing pipelines using the magick package. The
workflow is: Load images → Add operations → Preview results → Export
code.

## Usage

``` r
Momoshop()
```

## Value

No return value. Launches the Shiny application in the default browser.

## Key Features

- Real-time preview with live parameter adjustment (debounced 500ms)

- Responsive mosaic layout that adapts to window width

- Separate and Composite view modes for detailed comparison

- Reproducible R code generation for all pipelines

- Full pipeline control (edit, reorder, enable/disable operations)

## Available Operations

- Colorspace:

  Extract channels, convert between RGB/HSL/HSV/Gray

- Pixel Operations:

  Modulate brightness/saturation/hue, negate, enhance, normalize

- Geometry:

  Rotate, flip horizontal/vertical, trim

- Segmentation & Filters:

  Blur, threshold, quantize, morphology, edge detection

## Examples

``` r
if (FALSE) { # \dontrun{
# Launch the application
Momoshop()
} # }
```
