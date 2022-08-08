# code_for_AOH

Scripts to produce AOH maps for terrestrial and riparian birds and mammals with the [aoh package](https://github.com/prioritizr/aoh) in R 4.2.1
at 100 m and 1 km resolutions.

We also provide code to validate the 100 m AOH maps using GDAL and R.

Each R script starts with a brief description of its use.


## Where to download the data inputs


Range maps, habitat preferences and altitudinal ranges for mammals can be downloaded from the [IUCN Red List API, May 2022](https://apiv3.iucnredlist.org/).

Data for birds are available from BirdLife International.

Species range maps were saved as geopackages (a single geopackage per species).

Once you download the data you will need to modify the folder structure as needed.


## IUCN Red List Habitats Classification Scheme 

The IUCN Red List Habitats Classification Scheme ([Version 3.1](https://www.iucnredlist.org/resources/habitat-classification-scheme#:~:text=Suitability%20options%20are%3A,unknown%20importance%20to%20the%20species)) includes 18 broad Level 1 habitat categories, and a much larger number of Level 2 and 3 sub-categories.

For each habitat coded for each species, the suitability, importance and seasonality are coded (all options of suitability and importance are included). 

The IUCN habitat Level 1 codes are matched to the land-cover map classes with the help of a crosswalk table (see the section below, [Crosswalk](#crosswalk)).

We match the seasonality options to the seasonality values from the range maps (e.g. a habitat coded as non-breeding iss matched to range polygons with the attribute non-breeding).



## Range maps 

Each species range map from the IUCN Red List may comprise multiple polygons. 
Each polygon has attributes for presence, origin and seasonality (IUCN 2021) ([https://nc.iucnredlist.org](https://nc.iucnredlist.org/redlist/content/attachment_files/Mapping_Standards_Version_1.19_2021.pdf)).

We calculate a single AOH map for all vertebrates that do not migrate.
However, for all migratory species we produce separate breeding and non-breeding AOH maps according to the KBA Guidelines:

*   presence = 1 (Extant), and 2 (Probably Extant);
*   origin = 1 (Native), 2 (Reintroduced) and 6 (Assisted Colonisation);
*   seasonality = for non-migratory species: any season code (1 (Resident), 2 (Breeding Season), 3 (Non-breeding Season), 4 (Passage) and 5 (Seasonal Occurrence Uncertain)). For migratory species for the breeding range: 1, 2 and 5; and for the non-breeding range: 1, 3 and 5.


Movement patterns are available for the majority of species from the IUCN.
For those species where the movement patterns are missing (mostly because these are unknown), we inferred the migratory status from the seasonality of the range maps.
If there were at least two polygons in the range map, one with seasonality equal to 2 and the other  with seasonality equal to 3,
the missing movement patterns is classified as migratory.
In all the other cases, the missing movement patterns are classified as not migratory.


We transforme the coordinate reference system (CRS) of the range maps to World Behrmann ([ESRI:54017](https://epsg.io/54017)), which is a case of the cylindrical equal-area map projection with standard parallels set at 30° north and south.

## Upper and lower elevation limits

The IUCN Red List documents the Upper and Lower elevation limits (measured in meters above sea level) to indicate the altitudinal ranges suitable for each species. 
For > 1,000 bird species, Occasional Upper and Lower elevation limits are also reported. 
When available, we use these values when creating AOH maps.

Before running the computations, we verify potential errors in the altitudinal ranges and apply the following checks:

*   if any value is missing, we replace missing lower elevation values with -500 m (3496 species) and upper elevation with 9000 m 
*   if the lower elevation is higher than the upper elevation, we assigne -500 m to the lower elevation and 9000 m to the upper elevation 
*   if the difference between upper and lower elevation is less than 50 m, we adjust both values so that the difference was at least 50 m 
*   if the lower elevation was less than -500 m or the higher above 9000 m, we set the values to -500 m and 9000 m, respectively.



## Land-cover map

A global map of the vegetation can be derived from satellite images.
The land-cover map is input data in the AOH map production, providing the geographical distribution of land-cover classes. 
Each land-cover map has its own legend and, as such, needs a specific crosswalk table to link the land-cover classes with the habitat classification codes from the IUCN.
Here we used the  Copernicus land-cover product at a ~100 m resolution (CGLS-LC100) and set the CRS to World Behrmann.


## Global digital elevation model (DEM)

A global Digital Elevation Model (DEM) represents the topographic surface of the Earth's bare ground.
We used EarthEnv-DEM90 available at [EarthEnv project](https://www.earthenv.org/DEM.html) with ~ 90 m resolution to identify the suitable elevations for each species.
We transformed the CRS to World Behrmann to match with the land-cover layers, the global DEM and the range maps.


## Crosswalk 

To translate the land-cover classes to the IUCN habitat codes suitable for each species we use the Crosswalk tables statistically derived by  Lumbierres et al 2021 for CGLS-LC100.
We use the values with the highest thresholds which have the lowest commission errors without increasing the omission error rate.

The level 1 map of habitat codes derived from CGLS-LC100 can be downloaded at [10.5281/zenodo.5146072](https://doi.org/10.5281/zenodo.5146072) or [10.5281/zenodo.6622064](https://doi.org/10.5281/zenodo.6622064).
 

## Un-mappable habitats

Some species occur in habitats that are not mappable using satellite imagery.
Rarely mappable IUCN habitat types are not found within the CGLS-LC100 masked to the range map of the species.

## Spatial and temporal scale

We produce two sets of AOH maps: one set with ~100 m resolution and another with ~1 km resolution.
In the maps with ~100 m resolution, the values of a pixel are binary, where 0 indicates that a cell is unsuitable and 1 that a cell is suitable.

The maps at ~1 km resolution are fractional. Each grid cell has a value between 0 and 1, indicating the percentage of suitable habitat per grid cell within the range map.




# Validation


The validation procedure estimates the rate of omission or commission errors within each AOH map.

The validation used here compares two metrics (Dahal et al 2021):

1.  model prevalence, defined as the proportion of grid cells inside the range map retained in the AOH map (AOH/range ratio); for example, a model prevalence of 0.6 indicates that the AOH covers 60% of the mapped range
1.  point prevalence, defined as the the proportion of independent point localities falling inside the AOH map for those species where presence point localities data are available; for example, a point prevalence of 0.6 indicates that 60% of point localities fall within the AOH.

To calculate point prevalence we download presence-only information at the global scale for mammals (GBIF, Global Biodiversity Information Facility, www.gbif.org) and  birds (eBird basic dataset, Cornell Lab of Ornithology, www.ebird.org).

We only validate the ~100 m resolution AOH maps.
For migratory species, we combine the breeding and non-breeding ranges into a single AOH map and only validate the integrated map.


## Estimating model prevalence

We estimate model prevalence (the dependent variable) as a function of the elevation range of the species (upper minus lower elevation range), mid-point of the elevation range, number of habitats preferred by the species according to the IUCN Red List, seasonality and the geographical realm of the species.
Family nested into Class (Aves or Mammalia) is included as random intercepts.
The results of the statistical analyses can detect some species with a larger or smaller AOH than would be expected from their characteristics.
 
We define these species as outliers.
For these outliers we produce an AOH map only removing unsuitable habitats and an AOH map only removing unsuitable elevation, in addition to the AOH map removing both unsuitable habitats and elevations.
The comparison of these maps could indicate if any potential error is related with an inaccuracy in habitat or elevation coding. 
Our results can be used by Red List Assessors, who can further examine the outliers.


## Estimating point prevalence

Presence-only data gathered from citizen science projects such as GBIF and eBird are used to estimate the AOH point prevalence.
Point prevalence is the proportion of presence localities over the total number of data points that fall inside the AOH map of each species.
We only include species with a minimum of 50 data points and create a buffer of 300 m around each point locality to account for observation errors.
When the point prevalence is higher than the model prevalence, the AOH map performs better than a random map in predicting the geographical distribution of suitable habitats for that species.


# References

Buchhorn, Marcel, Myroslava Lesiv, Nandin-Erdene Tsendbazar, Martin Herold, Luc Bertels, and Bruno Smets. 2020. “Copernicus Global Land Cover Layers—Collection 2.” Remote Sensing 12 (6): 1044.


Dahal, Prabhat Raj. 2022. “Phd Thesis: Production and Validation of Updated Area of Habitat Maps for Terrestrial Birds and Mammals.” Sapienza University of Rome.


Dahal, Prabhat Raj, Maria Lumbierres, Stuart HM Butchart, Paul F Donald, and Carlo Rondinini. 2021. “A Validation Standard for Area of Habitat Maps for Terrestrial Birds and Mammals.” Geoscientific Model Development Discussions, 1–25.


Lumbierres, Maria, Prabhat Raj Dahal, Moreno Di Marco, Stuart HM Butchart, Paul F Donald, and Carlo Rondinini. 2021. “Translating Habitat Class to Land Cover to Map Area of Habitat of Terrestrial Vertebrates.” Conservation Biology.


R Core Team. 2022. R: A Language and Environment for Statistical Computing. Vienna, Austria: R Foundation for Statistical Computing. https://www.R-project.org/.


Robinson, Natalie, James Regetz, and Robert P Guralnick. 2014. “EarthEnv-Dem90: A Nearly-Global, Void-Free, Multi-Scale Smoothed, 90m Digital Elevation Model from Fused ASTER and SRTM Data.” ISPRS Journal of Photogrammetry and Remote Sensing 87: 57–67.


Standards, KBA, and. Appeals Committee. 2019. “Guidelines for Using a Global Standard for the Identification of Key Biodiversity Areas, Version 1.1.” Prepared by the KBA Standards and Appeals Committee of the IUCN Species Survival Commission and IUCN World Commission on Protected Areas.























