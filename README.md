# LANDISII
### Jen Baron, PhD Student
#### University of British Columbia, Faculty of Forestry

Paramaterizing LANDISII simulation model for the East Kootenays, BC

Values in modern.csv are based on the British Columbia Wildfire Service historical fire perimeters dataset (https://catalogue.data.gov.bc.ca/dataset/fire-perimeters-historical) using fire events from 1940-2020, intersected with provincial BEC (https://catalogue.data.gov.bc.ca/dataset/bec-map) and provided PEM aspect layers. 

- Zone - One of five zones (Columbia, Elk, Kootenay North, Kootenay South, Purcell)
- Region_Name - Name of BEC subregion (abbreviations from provincial BEC layer)
- Aspect - warm/cool aspect, as identified using the provided PEM layer
- number_fires - Number of fire events within the zone and subregion, across both warm and cool aspects
- number_fires.aspect- Number of fire events within the zone and region, subdivided into warm and cool aspect
- mean_fs_BEC - Mean size (ha) of fire events occuring within the area of each zone and BEC subregion, across both warm and cool aspects
- mean_fs_BEC.aspect - Mean size (ha) of fire events occuring within the area of each zone and BEC subregion, subdivided into warm and cool aspect
- min_fs_BEC - Minimum size (ha) of fire events occuring within the area of each zone and BEC subregion, across both warm and cool aspects
- min_fs_BEC.aspect - Minimum size (ha) of fire events occuring within the area of each zone and BEC subregion, subdivided into warm and cool aspect
- max_fs_BEC - Maximum size (ha) of fire events occuring  within the area of each zone and BEC subregion, across both warm and cool aspects
- max_fs_BEC.aspect - Maximum size (ha) of fire events occuring within the area of each zone and BEC subregion, subdivided into warm and cool aspect
- FRI - Fire rotation interval (years); the time necessary for fire to burn the equivalent of the total area, across both warm and cool aspects
- FRI.aspect - Fire rotation interval (years); the time necessary for fire to burn the equivalent of the total area, subdivided into warm and cool aspect


Missing values (NA) indicate that there were no fire events recorded in the BCWS historical fire perimeters dataset within a given BEC subzone (BEC subzone & aspect for .aspect values).
