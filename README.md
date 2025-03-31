# lsma: Landscape Structure Multiscale Analysis

`lsma` is an R package designed to perform multi-scale landscape analysis. It provides tools to decouple nested spatial patterns and assess landscape metrics, helping landscape ecologists and conservation scientists analyze spatial data efficiently.

## Features

- Tools for calculating landscape metrics across multiple scales.
- Nested and decoupled approaches for landscape analysis.
- Integration with `raster`, `sf`, and `lsma` for handling spatial and raster data.
- Experimental support for parallel processing using the `future` package.

## Installation

You can install the development version of `lsma` from GitHub:

```r
# Install from GitHub
remotes::install_github("wilsonfrantine/lsma")
```

## How to Use

Four simple steps to analyze your data:
1. Load the raster data, sampling points and set your buffer sizes
2. Choose the best strategy for your experiment (coupled, decoupled, decouple_single)
3. Calculate the metric for each scale with calculate_metrics() from any landscapemetrics package function

### Example

```r
# Load the package
library(lsma)
library(raster)
library(sf)

# Step 1: Load the data
  # 1.1 Your raster
  landscape_raster <- raster("path/to/your_raster")
  
  # 1.2 Your sampling points
  path_to_shape_file <- system.file("extdata/pnts.shp", package="lsma")
  points <- read_points(path_to_shape_file, type="shp")
  
  # Set your buffer sizes (ex. 100m, 500m, 1000m)
  buffers <- c(100, 500, 1000)

# Step 2: Choose your landscape strategy analysis (coupled_scales, decouple_scales, decouple_single_scale) 
  scales <- decouple_scales(landscape_raster, points, buffers)

# Step 3: Calculate landscape metrics on the decoupled scales
  metrics <- calculate_metrics(scales)

```

The package also has functions to plot the extracted scales for visual check and publication, as well as basic metric plots.

For detailed usage examples and tutorials, please check out our [vignettes](https://wilsonfrantine.github.io/lsma/).

## Parallel Computation

The package offers support for parallel computation using the `future` package. To enable parallel processing, set up the plan as follows:

```r
library(future)
plan(multisession)

# Then run your code...
metrics <- calculate_metrics(large_landscape)
```

To return to sequential processing, use:

```r
plan(sequential)
```

## How to Cite

If you use the `lsma` package in your research, please cite it as follows:

```
Frantine-Silva, W. (2024). lsma: A package for landscape structure multi-scale analysis. R package version 0.3.0. https://doi.org/10.5281/zenodo.13997058
```

**BibTeX citation:**
```bibtex
@Manual{,
  title = {lsma: A package for landscape structure multi-scale analysis},
  author = {Wilson Frantine-Silva},
  year = {2024},
  note = {R package version 0.3.1},
  doi = {10.5281/zenodo.13997058},
  url = {https://doi.org/10.5281/zenodo.13997058}
}
```

## Reporting Bugs

This is a preview version. If you encounter any bugs or issues, please report them on the [GitHub issue tracker](https://github.com/wilsonfrantine/lsma/issues), or contact me at wilsonfrantine@gmail.com.
