# Importing Nesting Data

![Import nesting data dialog](img/import-nesting-data.png)

Nesting data can be imported in **CSV** format.  
Each CSV file must follow a **predefined schema** to ensure that the data can be validated and correctly integrated into the system.

## Generate files

You can generate empty CSV files with the right schema to fill with data.

1. Select in the main menu **File → Import → Nesting Data**.  
2. Click the **Generate files** button.  
3. Select the folder and inform a file name, then click the **Save** button.  
4. The files will be created in the selected folder. If the option *Open file after export* is enabled, the files will be opened in the default application.

## How to import

1. Select **File → Import → Nesting Data**.  
2. The dialog will open, allowing you to choose one or more files for **Nests**, **Nest revisions**, and **Eggs**.  
   - There are **three types of nesting data** available for import.  
   - You must select at least one file, but you may select all three.  
3. Once the import begins, the dialog will display progress and results.

This feature ensures that nesting records collected externally can be integrated into the **Nests**, **Nest revisions**, and **Eggs** modules.

## CSV Schemas

Below are the outlines of the three CSV schemas.

### 1. Nest schema

| Column | Description |
| --- | --- |
| **field_number** | field identifier for the nest |
| **taxon** | bird species taxon |
| **male** | male individual identifier |
| **female** | female individual identifier |
| **latitude** | in decimal degrees |
| **longitude** | in decimal degrees |
| **altitude** | in meters |
| **locality** | study site name |
| **height_above_ground** | nest height above ground, in meters |
| **support_plant_1** | main plant supporting the nest |
| **support_plant_2** | secondary plant supporting the nest |
| **max_internal_diameter** | maximum internal diameter of the nest, in millimeters |
| **min_internal_diameter** | minimum internal diameter of the nest, in millimeters |
| **max_external_diameter** | maximum external diameter of the nest, in millimeters |
| **min_external_diameter** | minimum external diameter of the nest, in millimeters |
| **internal_height** | internal height of the nest, in millimeters |
| **external_height** | external height of the nest, in millimeters |
| **plant_center_distance** | distance from nest to plant center, in centimeters |
| **plant_edge_distance** | distance from nest to plant edge, in centimeters |
| **nest_cover** | description of nest cover (e.g., exposed, partially covered) |
| **max_plant_diameter** | maximum diameter of supporting plant, in centimeters |
| **min_plant_diameter** | minimum diameter of supporting plant, in centimeters |
| **plant_height** | height of supporting plant, in meters |
| **plant_dbh** | diameter at breast height of supporting plant, in centimeters |
| **productivity** | productivity status (e.g., successful, failed) |
| **nest_fate** | fate of the nest (e.g., fledged, depredated) |
| **philornis_larvae** | presence of *Philornis* larvae |
| **found_stage** | stage when the nest was found (egg, nestling, etc.) |
| **cause_of_loss** | cause of nest loss |
| **loss_stage** | stage when loss occurred |
| **found_day** | day when the nest was found |
| **last_day_active** | last day the nest was active |
| **last_seen** | last observation date |
| **nest_age** | estimated age of the nest |
| **nest_days_egg** | number of days in egg stage |
| **nest_days_nestling** | number of days in nestling stage |
| **notes** | any additional information |

### 2. Nest revision schema

| Column | Description |
| --- | --- |
| **nest** | nest identifier |
| **date** | revision date |
| **observer** | abbreviation of the observer |
| **status** | nest status at revision |
| **eggs_tally** | number of eggs |
| **nestlings_tally** | number of nestlings |
| **photos** | photo references |
| **notes** | any additional information |

### 3. Egg schema

| Column | Description |
| --- | --- |
| **nest** | nest identifier |
| **date** | date of measurement |
| **egg_num** | egg number |
| **length** | egg length, in millimeters |
| **width** | egg width, in millimeters |
| **mass** | egg mass, in grams |
| **shape** | egg shape description |
| **color** | egg color description |
| **photos** | photo references |
| **notes** | any additional information |
