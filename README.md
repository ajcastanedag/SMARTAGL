# SMARTAGL: Smart Above Ground Level Flight Path Planning

## Overview

SMARTAGL is an R package that generates terrain-aware drone flight paths maintaining constant height above ground level (AGL). The output is specifically formatted for seamless import into **UgCS** flight planning software.

## Key Features

- **UgCS-Compatible Outputs**: CSV waypoint format matches UgCS route import requirements
- Terrain-adaptive flight paths with constant AGL altitude
- 3D path optimization that accounts for elevation changes
- Interactive visualization tools
- Support for various coordinate reference systems

## Installation

```r
# Install from GitHub
devtools::install_github("ajcastanedag/SMARTAGL")

# Load the package
library(SMARTAGL)
```

# 1. Define starting and landing points
```r
deploy_coords <- c(-110289.7,-62576.0)
land_coords <- c(-110077.1,-62963.2)
```

# 2. Create flight path
```r
# Create flight path
flight_path <- create_flight_path(
  deploy_coords = deploy_coords,
  land_coords = land_coords,
  dsm_path = "../SMARTAGL/Sample/DSM_Port.tif",
  N = 50,
  H = 80,
  crs_proj = "EPSG:3763"
)
```

# 3. Visualize
```r
plot_flight_path(flight) # 2D profile
plot_flight_map(flight)  # Interactive map
```

![SMARTAGL Flight Path Visualization](Sample/profile.png) 

# 4. Export for UgCS
```r
export_flight_path(flight, "ugcs_mission.csv")
```
