# Sightings

The **Sightings** module is used to record bird observations made during fieldwork. A sighting represents a **direct or indirect detection of a bird**, whether by visual observation, vocalization, capture, or other evidence. Sightings are fundamental for biodiversity monitoring, as they allow researchers to document species presence, abundance, and behavior in a standardized way.

Open the Sightings module in the main menu: **Sampling → Sightings**. Alternatively, sightings can be viewed grouped by survey in the **[Surveys](surveys.md)** module.

## Adding or editing a sighting

When creating or editing a sighting record, the following fields are available:

| Field | Required | Description |
| --- | --- | --- |
| **Survey** |  | Survey to which the sighting is linked |
| **Observer** |  | Observer selected from the Researchers table |
| **Method** | Yes | Method used when the bird was sighted (e.g., point count, transect, mist net) |
| **Locality** | Yes | Site where the bird was sighted (linked to Gazetteer) |
| **Longitude** |  | Longitude coordinate of the sighting |
| **Latitude** |  | Latitude coordinate of the sighting |
| **Date** | Yes | Date of the sighting |
| **Time** |  | Time when the bird was sighted |
| **Taxon** | Yes | Taxon observed (species, subspecies, etc.) |
| **Individual** |  | Specific individual sighted (if already registered) |
| **Quantity** |  | Number of individuals sighted |
| **Distance** |  | Distance from the transect or observer, in meters |
| **Detection type** |  | How the bird was detected (see below) |
| **Breeding/behavior code** |  | [eBird breeding and behavior code](https://support.ebird.org/en/support/solutions/articles/48000837520-ebird-breeding-and-behavior-codes#anchorDefinitions) |
| **Mackinnon list number** |  | Mackinnon list in which the sighting is included |
| **Captured** |  | Check if the bird/taxon was captured |
| **Seen** |  | Check if the bird/taxon was seen |
| **Heard** |  | Check if the bird/taxon was heard |
| **Photographed** |  | Check if the bird/taxon was photographed |
| **Audio recorded** |  | Check if the bird/taxon was recorded in audio |
| **Nr. of new captures** |  | Number of birds captured for the first time |
| **Nr. of recaptures** |  | Number of birds captured again |
| **Nr. of unbanded** |  | Number of birds captured without bands |
| **Nr. of males** |  | Number of males observed |
| **Nr. of females** |  | Number of females observed |
| **Nr. of birds not sexed** |  | Number of birds whose sex could not be determined |
| **Nr. of adults** |  | Number of adults observed |
| **Nr. of immatures** |  | Number of immatures or juveniles observed |
| **Nr. of birds not aged** |  | Number of birds whose age could not be determined |
| **Record is on eBird** |  | Check if the sighting is also recorded on eBird |
| **Outside the sampling** |  | Check if the sighting was made outside regular sampling effort |
| **Notes** |  | Any additional information about the sighting |

## Detection types

Detection types describe **how the bird was detected** during the sighting:

- **S** – Song  
- **C** – Call  
- **V** – Seen  
- **W** – Wing flapping  
- **D** – Drumming  
- **F** – Flying  

These codes help standardize observations and make them comparable across surveys.

## Best practices

- **Always link sightings to surveys**: This ensures that observations are contextualized within sampling events.  
- **Record detection type**: Helps distinguish between visual and acoustic detections, improving data quality.  
- **Use breeding/behavior codes**: When applicable, record breeding evidence to enrich ecological datasets.  
- **Document effort**: Note if the sighting occurred outside regular sampling to avoid bias in analyses.  
- **Add media evidence**: Photographs and audio recordings strengthen the reliability of records.  
- **Record sex and age when possible**: These details are valuable for demographic and population studies.  
- **Use notes for unusual observations**: Document rare behaviors, habitat conditions, or disturbances.  

## Relation to other modules

Sightings are interconnected with several parts of Xolmis:

- **[Surveys](surveys.md)**: Sightings are grouped by survey events.  
- **[Individuals](individuals.md)**: Sightings can be linked to specific registered individuals.  
- **[Captures](captures.md)**: Captured birds are also recorded as sightings.  
- **[Projects](projects.md)**: Sightings contribute to project datasets and reports.  
- **[eBird](http://www.ebird.org) integration**: Records can be cross-referenced with eBird for broader data sharing.  

By managing sightings in Xolmis, researchers ensure that bird observations are standardized, traceable, and ready for ecological and conservation analyses.
