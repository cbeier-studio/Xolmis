# Gazetteer

A gazetteer is a catalog of toponyms or known site names. Toponyms are used in many modules of Xolmis and, once they were added to the Gazetteer, they can be searched and selected to fill other modules. Open the Gazetteer in the main menu **Geo → Gazetteer**.

The toponyms follow a hierarchy, defined by the type of toponym and the parent toponym:

> Country
    ⨽ State
        ⨽ Region
            ⨽ Municipality
                ⨽ District
                    ⨽ Locality

## Adding and editing

| Field | Required | Description |
| --- | --- | --- |
| Name | Yes | Site name |
| Abbreviation | | Site abbreviation, if it has one |
| Type | Yes | Type of toponym: country, state, region, municipality, district, locality |
| Longitude | | X axis/longitude of the geographical coordinate, in decimal degrees |
| Latitude | | Y axis/latitude of the geographical coordinate, in decimal degrees |
| Altitude | | Altitude at sea level, in meters |
| Parent toponym | | Parent site in the hierarchy |
| Full name | Yes | Site name with hierarchy |
| Site name on eBird | | Site name as it is on eBird, for import purposes |

!!! info "Site hierarchy"

    The site hierarchy is used to display site names grouped like a tree, as in [Quick filters](search-and-filtering-data.md#quick-filters). Thus is important to assign the correct **Parent toponym** for each toponym.
