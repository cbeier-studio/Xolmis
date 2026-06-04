# Occurrence Points

**Occurrence Points** are a repository of geographic coordinates associated with a species or an individual. These points are essential for spatial and geographic analyses, such as determining a species’ distribution range, estimating individual territories, studying habitat use, and analyzing movement patterns.

Occurrence points are available as a submodule within **Sightings**, **Individuals**, and **Surveys**. You can access these modules from the main menu under **Fieldwork**.

## Adding and editing points

When creating or editing an occurrence point, the following fields are available:

| Field | Required | Description |
| --- | --- | --- |
| **Date** | Yes | Date when the point was obtained |
| **Time** |  | Time when the point was obtained |
| **Name** | Yes | Identifier of the point |
| **Longitude** |  | Longitude coordinate (X axis), in decimal degrees |
| **Latitude** |  | Latitude coordinate (Y axis), in decimal degrees |
| **Coordinate accuracy** |  | Accuracy of the geographic coordinates |
| **Altitude** |  | Elevation above sea level, in meters |
| **Observer** |  | Person who obtained the point |
| **Taxon** | Yes | Taxon to which the record belongs |
| **Individual** |  | Individual associated with the point |
| **Sighting** |  | Sighting associated with the point |
| **Survey** |  | Survey event associated with the point |
| **Notes** |  | Any additional information about the point |

## Tips and best practices

- **Always record coordinate accuracy when possible**: GPS accuracy is essential for reliable spatial analyses. Accuracy values help filter imprecise points and prevent misinterpretation on maps.
- **Use consistent point names**: Identifiers such as “P01”, “P02”, “Roost 1”, or “Foraging 3” make searches, analyses, and later reviews easier.
- **Prefer decimal degree coordinates**: Xolmis works natively with this format, reducing conversion errors.
- **Include altitude when relevant**: Mountain ecology, microhabitat studies, and behavioral analyses can greatly benefit from elevation data.
- **Link the point to an individual or sighting whenever possible**: This enables analyses such as trajectories, home ranges, activity hotspots, and spatial use patterns.
- **Review suspicious coordinates**: Points far outside the expected area, swapped latitude/longitude values, or coordinates like 0,0 should be corrected or discarded.

## Relation to other modules

Occurrence Points are not an isolated catalog. They are directly integrated with:

- **[Individuals](individuals.md)**: points associated with a specific individual can be used to estimate territory, home range, and movement patterns.  
- **[Sightings](sightings.md)**: survey records may include multiple points for different species.  
- **[Surveys](surveys.md)**: points are often obtained during survey events, enabling spatial analyses based on sampling effort.  

By managing occurrence points in Xolmis, researchers ensure that these data remain organized, standardized, and ready for spatial, ecological, and behavioral analyses.
