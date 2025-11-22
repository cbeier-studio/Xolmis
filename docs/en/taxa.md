# Taxa

The **Taxa** module is dedicated to the consultation and visualization of ornithological taxonomy. A **taxon** is a name within a taxonomic rank (category), such as species, genus, family, or order, and it follows a hierarchical structure. In Xolmis, ornithological taxa can be **viewed but not edited**, ensuring consistency with recognized taxonomic standards. This module offers charts, maps, and reports related to each taxon, helping researchers explore biodiversity patterns and link records across the database.

Open the Taxa module in the main menu: **Taxonomy → Taxa**.

## Searching taxa

To display taxa, you need to run a search. The search box is located at the top left of the module and includes an **options button**. You can choose to:

- **Show only the recorded taxa** – Displays only taxa that have records in the database.  
- **Show all taxa** – Displays the complete taxonomy, even if no records exist for a given taxon.  

This flexibility allows you to focus either on your dataset or on the broader taxonomic hierarchy.

!!! info
    The taxonomy adopted in Xolmis is the [Clements Checklist](https://www.birds.cornell.edu/clementschecklist/), which is also used by [eBird](https://www.ebird.org/). This ensures compatibility with one of the most widely used ornithological databases worldwide. The [AviList](https://www.avilist.org/) unified taxonomy will eventually be integrated into the Clements Checklist, so Xolmis does not plan to change to it in the near future.

## Taxon information

When a taxon is selected, the module displays detailed information, including:

- **Scientific name** and **authority**  
- **Taxonomic rank** (species, genus, family, etc.)  
- **IUCN Red List category** (conservation status)  
- **Common names** in English, Spanish, and Portuguese  
- **Geographical distribution**  
- **Taxonomic hierarchy** (parent and subordinate taxa)  
- **Synonyms** (alternative names used in literature)  
- **Subordinate taxa** (e.g., species within a genus)  

Additionally, the module shows **links to other modules** containing records of the selected taxon, along with the number of records in each. Clicking on a module name opens it directly, filtered to display the records of the chosen taxon.

## Searching a taxon on the web

With a taxon selected, you can quickly search for external information using the buttons on the right toolbar. Currently, supported websites include:

- [Google Search](https://www.google.com)  
- [Google Images](https://www.google.com/images)  
- [Google Scholar](https://scholar.google.com)  
- [Birds of the World](https://birdsoftheworld.org)  
- [eBird](https://ebird.org)  
- [Wikiaves](https://www.wikiaves.com.br)  
- [IUCN Red List](https://www.iucnredlist.org)  
- [GBIF](https://www.gbif.org)  

The search opens in your default internet browser, allowing quick access to scientific articles, images, distribution maps, and conservation information.

## Map of records

The **map view** displays all records with geographical coordinates as colored dots.

- Each record type is represented by a different color.  
- The map can be zoomed and panned for exploration.  
- Currently, the map is **view-only**, but future updates may include interactive tools for filtering and analysis.  

This visualization helps identify spatial distribution patterns of taxa across surveys and projects.

## Chart of seasonality

The **seasonality chart** shows a bar chart with:

- **X axis** – Months of the year.  
- **Y axis** – Number of records.  
- **Colors** – Each record type is represented by a different color, stacked within each bar.  

This chart is useful for identifying seasonal patterns in species occurrence, breeding activity, or migration.

## Chart of records per year

The **records per year chart** displays:

- **X axis** – Years.  
- **Y axis** – Number of records.  
- **Colors** – Different record types stacked within each bar.  

This chart helps track long-term trends in data collection, species monitoring, and research effort.

## Chart of nest fates

The **nest fates chart** presents:

- A **donut chart** showing proportions of nest outcomes:  
    - Unknown  
    - Lost  
    - Success  
- On the left, the **mean nest productivity** is displayed, providing insight into reproductive success rates.  

This visualization supports ecological and conservation studies by summarizing breeding outcomes across taxa.

## Best practices

- **Use “Show only recorded taxa”** to focus on species present in your dataset.  
- **Cross-check with external sources** (eBird, IUCN, GBIF) for updated taxonomy and conservation status.  
- **Leverage charts and maps** to identify ecological patterns and research gaps.  
- **Monitor nest fates** to evaluate reproductive success and habitat quality.  

## Relation to other modules

The Taxa module is interconnected with multiple parts of Xolmis:

- **[Sightings](sightings.md)** – Observations linked to taxa.  
- **[Captures](captures.md)** – Banding and measurement records associated with taxa.  
- **[Nests](nests.md) and [Eggs](eggs.md)** – Breeding data tied to specific species.  
- **[Reports](print-data.md) and [exports](exporting-data.md)** – Taxa summaries can be included in reports and exported for external analysis.  

By using the Taxa module, researchers gain a **centralized view of taxonomy**, ensuring compatibility with global standards and enabling deeper ecological insights.
