# Sampling plots

A **sampling plot** represents a specific site regularly used for sampling activities, such as a trail, a mist net station, a tower, or a fixed point of observation. Sampling plots are essential for organizing spatial data, ensuring consistency in monitoring, and facilitating comparisons across surveys and projects.

!!! note "Toponyms vs. Sampling plots"  
    Although similar, these concepts are not the same:

    - **Toponyms** are well-known named sites (e.g., "Serra do Cipó").
    - **Sampling plots** are more specific sites, usually located *inside* a locality (toponym), and often named by the researcher (e.g., "Trail A", "Mist net station 3").
    
    A single locality can contain multiple sampling plots.

Open the Sampling plots module in the main menu: **Geo → Sampling plots**.

## Adding or editing a sampling plot

When creating or editing a sampling plot record, the following fields are available:

| Field | Required | Description |
| --- | --- | --- |
| **Name** | Yes | Short name to identify the sampling plot |
| **Abbreviation** | Yes | Abbreviation used for quicker typing and imports |
| **Locality** | Yes | Locality (toponym) where the sampling plot is located |
| **Longitude** |  | Longitude coordinate of the sampling plot |
| **Latitude** |  | Latitude coordinate of the sampling plot |
| **Description** |  | Brief description of the sampling plot (e.g., habitat type, accessibility) |
| **Notes** |  | Any additional information about the sampling plot |

## Permanent nets

If the sampling plot is a **mist net station**, it can include **permanent nets**: nets that are always kept or used in the same spot. This ensures consistency in sampling effort and allows long-term monitoring of bird populations.

| Field | Required | Description |
| --- | --- | --- |
| **Mistnet number** | Yes | Mist net ID number or code |
| **Longitude** | Yes | Longitude coordinate of the mist net |
| **Latitude** | Yes | Latitude coordinate of the mist net |
| **Notes** |  | Any additional information about the mist net (e.g., habitat description, maintenance notes) |

## Best practices

- **Use clear names and abbreviations**: Helps quickly identify plots during fieldwork and data entry.  
- **Record precise coordinates**: Use GPS devices to ensure accuracy, especially for permanent nets.  
- **Provide descriptive context**: Add habitat type, vegetation, or accessibility notes to enrich ecological analyses.  
- **Standardize mist net numbering**: Maintain consistent IDs across surveys to avoid confusion.  
- **Link plots to localities**: Always associate sampling plots with a broader locality for hierarchical organization.  
- **Update notes regularly**: Document changes in habitat, accessibility, or sampling conditions.  

## Relation to other modules

Sampling plots are interconnected with several parts of Xolmis:

- **[Surveys](surveys.md)** – Sampling plots define where surveys are conducted.  
- **[Captures](captures.md)** – Mist net captures are linked to specific plots and nets.  
- **[Sightings](sightings.md)** – Observations can be associated with defined sampling plots.  

By managing sampling plots in Xolmis, researchers ensure that spatial data is **organized, traceable, and comparable**, supporting robust ecological and ornithological studies.

## Example workflow

1. Create a sampling plot named *Trail A* inside the locality *Reserva Natural X*.  
2. Add three permanent mist nets with precise GPS coordinates.  
3. Link captures and sightings to these nets during surveys.  
4. Export coordinates to GIS for spatial analysis of sampling effort.  

This workflow demonstrates how sampling plots provide the **spatial backbone** for organizing field data in Xolmis.
