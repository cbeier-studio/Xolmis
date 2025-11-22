# Methods

The **Methods** module is used to register and manage the sampling methods applied during fieldwork. A method defines **how data is collected**, ensuring that surveys, captures, and observations are standardized and comparable. By documenting methods, Xolmis allows researchers to maintain consistency across projects and facilitates integration with external platforms such as eBird.

Open the Methods module in the main menu: **Fieldwork → Methods**.

## Adding or editing a method

!!! info
    Some methods are preloaded and cannot be edited. These methods are provided to ensure **standardization** and **compatibility** with Xolmis Mobile and external datasets. Preloaded methods cover common sampling techniques such as point counts, transects, mist netting, and opportunistic observations.

When creating or editing a method record, the following fields are available:

| Field | Required | Description |
| --- | --- | --- |
| **Name** | Yes | Full name of the method (e.g., "Point count", "Mist netting") |
| **Abbreviation** | Yes | Short abbreviation used for quicker typing and identification |
| **Category** |  | Category for grouping methods (e.g., "Visual", "Capture", "Acoustic") |
| **Method name on eBird** |  | Equivalent name of the method on eBird, used for import/export compatibility |
| **Description** |  | Brief description of the method to help identify it |
| **Recommended uses** |  | Typical cases where the method is applied (e.g., "Best for forest understory species") |
| **Notes** |  | Any additional information about the method |

## Method categories

The **Category** field is used to group methods into broader classes, making it easier to filter, organize, and compare sampling techniques.  
Below is a suggested list of categories commonly used in ornithological fieldwork:

- **Visual** – Methods based on direct observation of birds (e.g., point counts, transects, opportunistic sightings).  
- **Acoustic** – Methods that rely on sound recordings or auditory detections (e.g., song playback, autonomous recording units).  
- **Capture** – Methods involving physical capture of individuals (e.g., mist netting, traps, hand captures).  
- **Marking** – Methods focused on marking or tagging individuals (e.g., banding/ringing, telemetry, PIT tags).  
- **Specimen collection** – Methods involving collection of biological material (e.g., voucher specimens, feather sampling, blood sampling).  
- **Breeding monitoring** – Methods used to monitor nests, eggs, or nestlings (e.g., nest checks, cavity inspections, camera traps at nests).  
- **Behavioral observation** – Methods designed to record specific behaviors (e.g., focal animal sampling, activity budgets).  
- **Habitat assessment** – Methods that document environmental or habitat variables associated with bird presence (e.g., vegetation plots, habitat transects).  
- **Remote sensing** – Methods using technology to detect or monitor birds (e.g., drones, radar ornithology, satellite telemetry).  
- **Opportunistic** – Non-standardized methods where data is collected incidentally (e.g., casual observations, incidental records).  
- **Experimental** – Methods applied in controlled experiments or manipulations (e.g., playback experiments, food supplementation).  
- **Other** – For methods that do not fit neatly into the categories above.  

## Best practices

- **Use clear names and abbreviations**: Ensure that methods are easily recognizable by all team members.  
- **Group methods by category**: Helps organize and filter methods when managing surveys.  
- **Document recommended uses**: Indicate the contexts where the method is most effective (habitat type, target species, time of day).  
- **Maintain compatibility**: Use the eBird equivalent name when applicable to facilitate data exchange.  
- **Avoid duplication**: Check if a method already exists before creating a new one.  
- **Use notes for details**: Record specific adaptations or variations of the method used in your project.  

## Relation to other modules

Methods are interconnected with several parts of Xolmis:

- **[Surveys](surveys.md)**: Each survey requires a method to define how data was collected.  
- **[Imports](importing-data.md)/[Exports](exporting-data.md)**: Methods ensure compatibility when importing data from eBird or exporting datasets to external tools.  

By managing methods in Xolmis, you guarantee that sampling events are properly documented, standardized, and comparable across different studies.
