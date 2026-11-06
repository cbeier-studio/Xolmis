# Taxa

The **Taxa** module is dedicated to viewing ornithological taxonomy. A **taxon** is a name within a taxonomic rank (such as species, genus, family, or order) in a hierarchical structure. In Xolmis, ornithological taxa can be **viewed but not edited**, ensuring consistency with recognized taxonomic standards. This module provides charts, maps, and reports for each taxon, helping researchers explore biodiversity patterns and link records throughout the database.

Open the Taxa module from the main menu: **Taxonomy → Taxa**.

![Taxa module screen](img/taxa-module.png)

## Searching taxa

You need to run a search to display taxa. The search box is in the top-left corner of the module and includes an **options button**. You can choose:

- **Show only recorded taxa** - Displays only taxa that have records in the database.
- **Show all taxa** - Displays the complete taxonomy, including taxa with no records.

This flexibility lets you focus either on your dataset or on the broader taxonomic hierarchy.

!!! info
    Xolmis uses the [Clements Checklist](https://www.birds.cornell.edu/clementschecklist/), which is also used by [eBird](https://www.ebird.org/). This ensures compatibility with one of the world's most widely used ornithological databases. The unified [AviList](https://www.avilist.org/) taxonomy is expected to eventually be integrated into the Clements Checklist, so Xolmis does not plan to migrate to it in the near future.

## Taxon information

When you select a taxon, the module displays detailed information in a header, including:

- **Taxonomic rank:** species, genus, family, and so on.
- **Scientific name:** the scientific name of the taxon.
- **Authority:** the name of the author who described the taxon.
- **Conservation status:** the taxon's global conservation status (IUCN Red List).
- **Vernacular names:** the English name and the name in Xolmis's current language, when available.
- **Taxonomic hierarchy:** order, family, genus, and species.

The remaining information is available in the tabs below the header.

## Summary

The **Summary** tab presents general information about the selected taxon, including:

- **Indicators:**
    - Total records
    - Number of observations
    - Number of individuals
    - Number of captures
    - Number of nests
    - Number of eggs
    - Number of specimens
- **Geographical distribution**
- **Synonyms** (alternative names used in the literature)
- **External links:** Open in your default browser for quick access to scientific articles, images, distribution maps, and conservation information.
    - [Birds of the World](https://birdsoftheworld.org)
    - [eBird](https://ebird.org)
    - [Wikiaves](https://www.wikiaves.com.br)
    - [IUCN Red List](https://www.iucnredlist.org)
    - [GBIF](https://www.gbif.org)
    - [Google Search](https://www.google.com)
    - [Google Images](https://www.google.com/images)
    - [Google Scholar](https://scholar.google.com)
- **Charts:**
    - Records per year
    - Seasonality

### Records per year chart

The **records per year chart** displays:

- **X axis** - Years.
- **Y axis** - Number of records.
- **Colors** - Different record types stacked in each bar.

This chart helps track long-term trends in data collection, species monitoring, and research effort.

### Seasonality chart

The **seasonality chart** is a bar chart showing:

- **X axis** - Months of the year.
- **Y axis** - Number of records.
- **Colors** - Each record type is represented by a different color, stacked in each bar.

This chart is useful for identifying seasonal patterns in species occurrence, breeding activity, or migration.

## Occurrence

The **Occurrence** tab presents information about taxa found in different locations and periods.

### Map of records

The **map view** displays all records with geographical coordinates as colored points.

- Each record type is represented by a different color.
- The map can be zoomed and panned for exploration.
- Currently, the map is **view-only**, but future updates may include interactive tools for filtering and analysis.

This visualization helps identify spatial distribution patterns of taxa across surveys and projects.

## Reproduction

The **Reproduction** tab provides information about the reproductive biology of taxa, including nest fates, productivity, and reproductive success.

- **Indicators:**
    - Total nests
    - Average nest productivity
- **Charts:**
    - Nest fates

### Nest fates chart

The **nest fates chart** presents:

- A **donut chart** showing the proportions of nest outcomes:
    - Unknown
    - Lost
    - Success
- The **average nest productivity** is displayed on the left, providing insight into reproductive success rates.

This visualization supports ecological and conservation studies by summarizing breeding outcomes across taxa.

## Best practices

- **Use “Show only recorded taxa”** to focus on species present in your dataset.
- **Cross-check external sources** (eBird, IUCN, GBIF) for up-to-date taxonomy and conservation status.
- **Use charts and maps** to identify ecological patterns and research gaps.
- **Monitor nest fates** to evaluate reproductive success and habitat quality.

## Relation to other modules

The Taxa module is interconnected with several parts of Xolmis:

- **[Sightings](sightings.md)** - Observations linked to taxa.
- **[Captures](captures.md)** - Banding and measurement records associated with taxa.
- **[Nests](nests.md) and [Eggs](eggs.md)** - Reproductive data linked to specific species.
- **[Reports](print-data.md) and [Exports](exporting-data.md)** - Taxon summaries can be included in reports and exported for external analysis.

By using the Taxa module, researchers gain a **centralized view of taxonomy**, ensuring compatibility with global standards and enabling deeper ecological insights.
