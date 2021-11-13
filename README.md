# CorrelCon21: Introduction to Spatial Analysis with R

This is the repository containing code and data used in the hands-on session "Introduction to Spatial Analysis with R" during the CorrelCon 2021 on November 13th, 2021.


### How to use
Download the zip-folder "spaital_analysis_R_intro". If you double click on the .Rproj file in the folder an R-project should open and the script "correlcon_intro_spatial_analysis.R" should run out of the box since it contains all necessary data in the "datasets" folder. This script will produce the map we are aiming to create during our talk.

### Data Sources
- Bikesharing [trips](https://data.deutschebahn.com/dataset/data-call-a-bike/resource/b51f1366-15a1-4176-bbc0-74c2722faf9c.html) & [stations](https://data.deutschebahn.com/dataset/data-call-a-bike/resource/4007a81d-bb3d-46ed-9929-e3744d714aae.html): Open Data Portal from Deutsche Bahn
- Landuse: Openstreetmap data downloaded for Hamburg from [Geofabrik Download Server](https://download.geofabrik.de/europe/germany/hamburg.html)
- Shortest paths between stations: Extracted from [CycleStreetsAPI](https://www.cyclestreets.net/api/) using Rpackage "[cyclestreets](https://cran.r-project.org/web/packages/cyclestreets/index.html)"

Note: All data sources were already slightly prepared & cleaned and sampled for easier plotting, contact us for details. The script "routing_with_cyclestreets.R" contains the code that connects all stations with the shortest path and also straight lines. This is slightly advanced but we included it anyways in case you are interested how to produce the data..  

### Code Sources
This analysis is create by using a collection of great and helpful packages:

```
sf # simple features packages, a load of data wrangling functions for spatial data, usable with tidyverse
sp # simple features packages, a load of data wrangling functions for spatial data
tidyverse (dplyr & stringi mainly)

# for plotting:
cowplot
ggplot2
ggforce
scales
glue
"patchwork")
extrafont)

stplanr # for creating straight lines between stations
cyclestreets # the R package that connects to the cycle streets api
```



![testesttestttest](https://user-images.githubusercontent.com/68635246/141644627-2e4e090f-eeab-4b17-bbf0-1a3b805e6b49.png)
