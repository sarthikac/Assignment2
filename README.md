# Task 2 - Cedars-Sinai

This R package creates Sankey diagrams to visualize stroke risk factors over time, matching the formatting of the Excel chart provided in the assignment. 

## Installation

```r
# Install directly from GitHub
devtools::install_github("sarthikac/Assignment2")
```
## Usage
```r
library(Assignment2)
plot_sankey_diagram(stroke_data, vertical_gap = TRUE, horizontal_gap = TRUE)
```

## Features
- Generates Sankey diagrams matching Excel’s styling.
- Optional vertical and horizontal gaps for white space.
- Packaged function (plot_sankey_diagram) with arguments for flexibility.
- Includes a vignette explaining the project and demonstrating usage.

Author
Sarthika Chimmula
