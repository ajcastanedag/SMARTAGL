# SMARTAGL: Smart Above Ground Level Flight Path Planning

![SMARTAGL Flight Path Visualization](images/flight_profile.png) 

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

# 1. Load your DSM and define points
dsm_path <- "path/to/your_dsm.tif"
deploy <- c(570560.87, 5513697.85) # UTM coordinates
land <- c(570467.18, 5513850.40)

# 2. Create flight path (50m AGL)
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
export_flight_path(flight, "ugcs_mission.csv")
