# Individuals

The **Individuals** module is used to register and manage information about specific birds. Each record represents a single bird, identified by its taxon and optionally by a band (ring). This module is central to long-term monitoring, as it allows you to track individuals across captures, sightings, nests, and other records.

Open the Individuals module in the main menu: **Fieldwork → Individuals**.

## Adding or editing an individual

When creating or editing an individual record, the following fields are available:

| Field | Required | Description |
| --- | --- | --- |
| **Taxon** | Yes | The taxon of the individual (species, subspecies, etc.) |
| **Band** |  | The bird band (ring) assigned to the individual |
| **Banding date** |  | Date when the bird was banded |
| **Double band** |  | Additional band, if the bird carries more than one |
| **Removed band** |  | Band that was removed and replaced |
| **Band change date** |  | Date when the band was replaced |
| **Right tarsus** |  | Bands and colored bands on the right tarsus (below the joint) |
| **Left tarsus** |  | Bands and colored bands on the left tarsus (below the joint) |
| **Right leg** |  | Bands and colored bands on the right leg (above the joint) |
| **Left leg** |  | Bands and colored bands on the left leg (above the joint) |
| **Birth date** |  | Date of birth of the individual (can be approximate) |
| **Death date** |  | Date of death of the individual (can be approximate) |
| **Sex** |  | Sex of the individual: Male, Female, Unknown |
| **Age** |  | Age category of the individual (see below) |
| **Nest** |  | Nest where the bird was born (links to the Nests module) |
| **Father** |  | The bird registered as the father of the individual |
| **Mother** |  | The bird registered as the mother of the individual |
| **Recognizable markings** |  | Description of visible and distinctive markings (plumage, scars, deformities) |
| **Notes** |  | Any additional information about the individual |

## Age categories

The **Age** field allows you to classify the developmental stage of the bird. This classification is important for demographic studies and population monitoring.

- **Unknown** – Age not determined  
- **Adult** – Fully mature bird  
- **Juvenile** – Young bird, not yet fully mature  
- **Fledgling** – Bird that has recently left the nest but is still dependent  
- **Nestling** – Bird still in the nest, dependent on parents  
- **First year** – Bird in its first calendar year  
- **Second year** – Bird in its second calendar year  
- **Third year** – Bird in its third calendar year  
- **Fourth year** – Bird in its fourth calendar year  
- **Fifth year** – Bird in its fifth calendar year  

## Best practices

- **Always link bands**: If the bird is banded, record the band details to ensure traceability across modules.  
- **Use nests for pedigree**: Linking individuals to nests, fathers, and mothers helps build family trees and track lineage.  
- **Record markings**: Distinctive markings are useful for re-identification in the field, especially if bands are lost.  
- **Approximate dates**: If exact birth or death dates are unknown, approximate values can still provide valuable demographic information.  
- **Update status**: Keep records updated when individuals die, lose bands, or change identifiers.

## Relation to other modules

The Individuals module is interconnected with several other parts of Xolmis:

- **[Bands](bands.md)**: Each individual can be linked to one or more bands.  
- **[Nests](individuals.md)**: Birth information connects individuals to nests, allowing pedigree tracking.  
- **[Sightings](sightings.md) and [captures](captures.md)**: Observations reference individuals, enabling longitudinal studies.  
- **[Specimens](specimens.md)**: Individuals may be associated with collected samples.  

By maintaining accurate individual records, you ensure that all ornithological data in Xolmis is consistent and traceable across modules.
