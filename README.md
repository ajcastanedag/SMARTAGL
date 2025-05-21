# SMARTAGL: Smart Above Ground Level Flight Path Planner

![SMARTAGL Visualization](images/flight_profile.png)

## Table of Contents

* [Core Features](#core-features)
* [Installation](#installation)
* [Quick Start](#quick-start)
* [Variable Reference](#variable-reference)
* [UgCS Integration](#ugcs-integration)
* [Visualization](#visualization)
* [Examples](#examples)
* [License](#license)

## Core Features

* Terrain-aware drone path planning
* Constant AGL altitude maintenance
* UgCS-compatible CSV exports
* 2D profile and 3D interactive maps
* Multi-CRS support

## Installation

```r
# Install from GitHub
if (!require("devtools")) install.packages("devtools")
devtools::install_github("yourusername/SMARTAGL")

# Load package
library(SMARTAGL)
```

## Quick Start

```r
# 1. Define coordinates and DSM path
deploy <- c(570560.87, 5513697.85) # UTM coordinates
land <- c(570467.18, 5513850.40)
dsm_path <- "path/to/dsm.tif"

# 2. Generate flight path (50m AGL)
flight <- create_flight_path(
  deploy_coords = deploy,
  land_coords = land,
  dsm_path = dsm_path,
  H = 50
)

# 3. Visualize
plot_flight_path(flight) # 2D profile
plot_flight_map(flight)  # Interactive map

# 4. Export for UgCS
export_flight_path(flight, "flight_plan.csv")
```

## Variable Reference

### Input Variables

| Variable       | Type        | Description                     | Constraints            |
| -------------- | ----------- | ------------------------------- | ---------------------- |
| deploy\_coords | numeric\[2] | Deployment (x,y) in project CRS | Must match DSM CRS     |
| land\_coords   | numeric\[2] | Landing (x,y) in project CRS    | Must match DSM CRS     |
| dsm\_path      | string      | Path to Digital Surface Model   | GeoTIFF format         |
| N              | integer     | Sample points along path        | ≥2, default = 100      |
| H              | numeric     | AGL height (meters)             | ≥0, default = 10       |
| crs\_proj      | string      | Projected CRS                   | Default = "EPSG:32632" |

### Output Structure

The flight path object contains:

```r
list(
  deploy_coords = c(x, y),       # Original deployment
  land_coords = c(x, y),         # Original landing
  sampled_points = sf object,    # All sampled points
  terrain_line = sf object,      # Raw terrain profile
  simple_shift_line = sf object, # Basic AGL offset
  smart_agl_line = sf object,    # Optimized path
  final_coords = data.frame(     # UgCS-ready waypoints
    X = easting,
    Y = northing,
    Z = altitude_amsl
  ),
  parameters = list(             # Input parameters
    N = N,
    H = H,
    crs = crs_proj
  )
)
```

## UgCS Integration

Exported CSV contains these required columns:

| Column       | Type    | UgCS Mapping      |
| ------------ | ------- | ----------------- |
| WP           | int     | Waypoint sequence |
| Latitude     | decimal | WGS84 latitude    |
| Longitude    | decimal | WGS84 longitude   |
| AltitudeAMSL | decimal | AMSL altitude     |
| Speed        | decimal | Flight speed      |
| Picture      | boolean | Camera trigger    |

## Visualization

### 2D Elevation Profile

```r
plot_flight_path(flight)
```

### 3D Interactive Map

```r
plot_flight_map(flight)
```

## Examples

* Simulated path over mountainous terrain
* Comparison of fixed-height vs. true-AGL flight plans

