# lsma: Landscape Structure Multiscale Analysis

`lsma` is an R package designed to perform multi-scale landscape analysis. It provides tools to decouple nested spatial patterns and assess landscape metrics, helping landscape ecologists and conservation scientists analyze spatial data efficiently.

## Features

- Tools for calculating landscape metrics across multiple scales.
- Nested and decoupled approaches for landscape analysis.
- Integration with `raster`, `sf`, and `lsma` for handling spatial and raster data.
- Experimental support for parallel processing using the `future` package.

## Installation

You can install the development version of `lsma` from GitHub:
Always check the line below:

```r
# install.packages("devtools")
devtools::install_github("wilsonfrantine/lsma@refactor-v1")
```

## How to Use

Three simple steps to analyze your data:
1. Load the raster data, sampling points and set your buffer sizes
2. Choose the best strategy for your experiment (coupled, decoupled, decouple_single)
3. Calculate the metric for each scale with calculate_metrics() from any landscapemetrics package function

## 🚀 Quick Start

    library(lsma)
    
    # Load example raster and points
    r <- terra::rast(system.file("extdata/raster.grd", package = "landscapeDecoupler"))
    p <- terra::vect(sf::st_read(system.file("extdata/pnts.shp", package = "landscapeDecoupler"), quiet = TRUE))
    
    # Extract landscapes
    ls <- extract_landscapes(r, p, buffers = c(500, 1000, 2000), strategy = "nested")
    
    # Visualize
    view_ls(ls)
    
    # Calculate metrics
    metrics <- calculate_metrics(ls, metric = "pland", level = "class")
    
    # Fit models
    models <- multimodel(metrics, bio_data = data_frame, model_type = "lm", model_formula = "responses ~ predictors")

The package also has functions to plot the extracted scales for visual check and publication, as well as basic metric plots.

For detailed usage examples and tutorials, please check out our [vignettes](https://wilsonfrantine.github.io/lsma/).

## How to Cite

If you use the `lsma` package in your research, please cite it as follows:

```
Frantine-Silva, W. (2024). lsma: A package for landscape structure multi-scale analysis. R package version 0.4.0. https://doi.org/10.5281/zenodo.13997058
```

**BibTeX citation:**
```bibtex
@Manual{,
  title = {lsma: A package for landscape structure multi-scale analysis},
  author = {Wilson Frantine-Silva},
  year = {2024},
  note = {R package version 0.4.0},
  doi = {10.5281/zenodo.13997058},
  url = {https://doi.org/10.5281/zenodo.13997058}
}
```

## Reporting Bugs

This is a preview version. If you encounter any bugs or issues, please report them on the [GitHub issue tracker](https://github.com/wilsonfrantine/lsma/issues), or contact me at wilsonfrantine@gmail.com.
