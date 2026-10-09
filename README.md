# ReactiveAtlantis

Integrated calibration tools for Atlantis ecosystem models.

## Overview

ReactiveAtlantis provides a modern, unified web application for tuning, parameterization, 
and analysis of Atlantis ecosystem models. All tools are accessible through a single 
interface with consistent styling and deterministic color schemes.

### Key Features

* **Unified Interface**: All calibration tools in one application
* **Modern Design**: Clean, readable interface with professional styling
* **Deterministic Colors**: Consistent color assignments across tools and sessions
* **Interactive Analysis**: Real-time visualization and parameter exploration
* **Model Skill Assessment**: Quantitative metrics for model performance

### Available Tools

* Visualization and analysis of input, output, and initial conditions
* Interactive modification of Atlantis configuration files
* Parameter simulation and calibration support
* Model skill assessment against observed data

## Installation

```R
# Install from GitHub
install.packages('devtools')
library("devtools")
install_github('Atlantis-Ecosystem-Model/ReactiveAtlantis', force=TRUE, dependencies=TRUE)

# Load the package
library("ReactiveAtlantis")
```

## Running ReactiveAtlantis

### Unified Application (Recommended)

The recommended way to use ReactiveAtlantis is through the unified application, which combines all calibration tools into a single modern interface:

```R
library(ReactiveAtlantis)
launch_reactiveatlantis()
```

This will open an interactive browser-based application where you can:

* Upload your Atlantis model files through file inputs
* Navigate between different analysis tools using tabs
* Visualize results with consistent, modern styling
* Compare multiple outputs side by side

All calibration tools are accessible from the main navigation:
* **Compare Outputs** - Visualize and compare biomass between simulations
* **Predation** - Analyze predator-prey interactions through time
* **Food Web** - Explore food web structure and trophic levels
* **Recruitment** - Estimate recruitment and primary production
* **Growth** - Analyze limitation factors for primary producers
* **Catch Analysis** - Visualize harvest outputs and model skill assessment
* **Feeding Matrix** - Calibrate predator-prey availability matrices
* **Mortality** - Explore natural, fishing, and predation mortality

### Command-Line Usage (Legacy)

The original command-line functions are still available for scripting and batch processing:

```R
## Compare outputs and biomass visualization
compare(nc.current, nc.out.old = NULL, grp.csv, bgm.file, cum.depths)

## Predation analysis
predation(biom, grp.csv, diet.file, bio.age = NULL)

## Predator-prey interactions
feeding.mat(prm.file, grp.file, nc.initial, bgm.file, cum.depths)

## Food web and trophic levels
food.web(diet.file, grp.file, diet.file.bypol = NULL)

## Growth of primary producers
growth.pp(nc.initial, grp.csv, prm.file, nc.current)

## Recruitment analysis
recruitment.cal(nc.initial, nc.current, yoy.file, grp.file, prm.file)

## Harvest outputs and skill assessment
catch(grp.csv, fsh.csv, catch.nc, ext.catch.by.fleet = NULL, ext.catch.total = NULL)

## Mortality analysis
mortality(grp.file, prm.file, SpeMort, PredMort)
```

## Authors

* **Javier Porobic**

## License

This project is licensed under [GPL3](https://www.gnu.org/licenses/gpl-3.0.en.html)
