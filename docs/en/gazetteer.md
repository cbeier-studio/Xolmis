# Gazetteer

The **Gazetteer** is a catalog of toponyms (geographical names or known sites). Toponyms are a fundamental part of Xolmis because they are referenced in many other modules.

Once added to the Gazetteer, a toponym can be searched and selected to fill fields in related modules such as sightings, sampling plots, nests, or specimens.

You can access the Gazetteer from the main menu: **Geo → Gazetteer**.

## Hierarchy of toponyms

Toponyms in Xolmis follow a hierarchical structure, defined by their type and their parent toponym. This hierarchy ensures that sites are organized consistently and can be grouped or filtered correctly.

``` mermaid
flowchart LR
    A[Country] --> B[State]
    B --> C[Region]
    C --> D[Municipality]
    D --> E[District]
    E --> F[Locality]
```

!!! example
    A locality such as *Parque Nacional Aparados da Serra* would be linked to the municipality *Cambará do Sul*, which belongs to the state *Rio Grande do Sul*, within the country *Brazil*.

## Adding and editing toponyms

When creating or editing a toponym, the following fields are available:

| Field | Required | Description |
| --- | --- | --- |
| **Name** | Yes | Official site name |
| **Abbreviation** |  | Abbreviation or short code, if applicable |
| **Type** | Yes | Type of toponym: country, state, region, municipality, district, locality |
| **Longitude** |  | Longitude coordinate (X axis), in decimal degrees |
| **Latitude** |  | Latitude coordinate (Y axis), in decimal degrees |
| **Altitude** |  | Elevation above sea level, in meters |
| **Parent toponym** |  | Parent site in the hierarchy (e.g., a municipality belongs to a region) |
| **Full name** | Yes | Complete hierarchical name (automatically generated once parent is defined) |
| **Site name on eBird** |  | Equivalent site name on eBird, used for data import |

!!! info "Site hierarchy"
    The hierarchy is used to display site names grouped like a tree, for example in [Quick filters](search-and-filtering-data.md#quick-filters). It is essential to assign the correct **Parent toponym** for each entry, otherwise the structure will be inconsistent and filters may not work properly.

## Tips and best practices

- **Start broad, then refine**: always begin by adding countries, then states/provinces, and only afterwards municipalities and localities.  
- **Use official names**: prefer standardized names (ISO or government sources) to avoid duplicates.  
- **Coordinates help**: adding latitude/longitude makes it easier to integrate with maps and spatial queries.  
- **Import helper**: Xolmis provides a helper tool to automatically add countries and their states/provinces. You select the countries you need, and the system fills them into the Gazetteer. After that, you only need to add municipalities and specific localities.  
- **Consistency matters**: once a toponym is created, it will be reused across modules. Take care to define it correctly to avoid errors later.

## Relation to other modules

The Gazetteer is not an isolated catalog. It is directly linked to:

- **[Sampling plots](sampling-plots.md)**: each plot must be tied to a locality or higher-level toponym.  
- **[Sightings](sightings.md) and [specimens](specimens.md)**: all records reference a site from the Gazetteer.  
- **[Captures](captures.md)**: associated with specific localities.  

Because of these dependencies, filling the Gazetteer correctly is one of the first steps when [starting a new database](first-steps.md).
