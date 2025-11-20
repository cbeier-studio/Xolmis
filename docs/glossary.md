# Glossary

Here are some terms used throughout this documentation.

**Add button**

:   Button with a plus sign icon :material-plus-circle: located in toolbars and its function is to add new *records* or items.

**Band (Ring)**

:    A *band* (also called a *ring*) is a small, uniquely numbered metal or plastic tag attached to a bird’s leg for identification. In ornithology, bands are used to mark individual birds, allowing researchers to track movements, survival, and behavior over time. In Xolmis, band records store details such as band number, material, size, and issuing authority, ensuring that each marked bird can be reliably linked to its corresponding individual record.

**Batch dialog**

:   A *dialog* with the purpose of insert data *records* in batches, especially for the cases when many *records* have data in common.

**Boolean**

:    A *boolean* is a data type with only two possible values: **true** or **false**. In Xolmis, boolean fields are used for yes/no options, switches, or logical conditions.

**Cell**

:    A *cell* is the smallest unit of a *data grid* or *spreadsheet*, located at the intersection of a *row* and a *column*. Each cell contains a single value or piece of information corresponding to one field of a *record*. In Xolmis, cells are used to display and edit individual data points within grids, allowing precise control over the content of each *record*.

**Column**

:    A *column* is a vertical set of *cells* in a data *grid*, representing one field or attribute of a *record*. In Xolmis, columns define which data fields are visible in the grid and can be customized for readability.

**Connection**

:    A *connection* refers to the active link between the application and a specific *database*. In Xolmis, the term is often used almost synonymously with *database*, since each connection points to one dataset file or server.

**CSV**

:    *CSV* (Comma-Separated Values) is a plain text format used to store tabular data, where each line represents a *row* and values are separated by commas (or semicolons). In Xolmis, CSV files are commonly used for importing or exporting *grids* of *records*.  

     **Example**
     ```
     id,name,species
     1,John Doe,Turdus rufiventris
     2,Jane Smith,Tyrannus melancholicus
     ```

**Data grid**

:    A *data grid* (or just *grid*) is the tabular interface used to display *records* in *rows* and *columns*. In Xolmis, grids resemble *spreadsheets* but are directly linked to the *database*, allowing filtering, sorting, and editing of *records*.

**Database**

:    A *database* is an organized collection of structured information stored electronically. In Xolmis, the database holds all *records*, *modules*, and relationships, serving as the central repository of data.

**DBF**

:    *DBF* (Database File) is a legacy file format originally used by dBASE and widely supported in *GIS* software. In Xolmis, DBF files may appear as attribute tables linked to shapefiles, storing tabular data associated with geographic features.

**Delete button**

:   Button with a trashcan icon :material-delete: located in toolbars and its function is to delete the selected *record* or item.

**Dialog**

:   It is a type of window that opens from the *main window*, and generally can not be maximized. It includes the *edit dialogs*.

**Edit button**

:   Button with a pencil icon :material-pencil: located in toolbars and its function is to edit the selected *record* or item.

**Edit dialog**

:   A *dialog* with the purpose of insert or edit data *records*. Each *module* have its own edit dialog.

**Encoding**

:    *Encoding* is the process of converting information into a specific format for storage, transmission, or interpretation. In Xolmis, encoding ensures that characters, symbols, and data are correctly represented across systems.

**Expedition**

:    An *expedition* is a planned field trip or campaign carried out by researchers or teams to collect data in a specific region and time period. In Xolmis, expeditions serve as organizational units that group *surveys*, captures, and observations under the same fieldwork effort, providing context for location, duration, and participants.

**Gazetteer**

:   It is a catalog of *toponyms* or named places on a map.

**GeoAssist**

:   Name of the tool used to help fill geographical coordinates in records.

**GeoJSON**

:    *GeoJSON* is a *JSON*-based format for encoding geographic data structures, such as points, lines, and polygons. In Xolmis, GeoJSON is used for interoperability with web mapping applications and modern *GIS* tools.

**GIS**

:    *GIS* (Geographic Information System) is a framework for capturing, storing, analyzing, and visualizing spatial or geographic data. In Xolmis, GIS integration allows researchers to map *localities*, *sampling plots*, and *expeditions*, and to analyze spatial relationships among *records*.

**GPX**

:    *GPX* (GPS Exchange Format) is an *XML*-based format for storing GPS data, including waypoints, tracks, and routes. In Xolmis, GPX files can be imported to record *survey* paths or *sampling plot* coordinates.

**Home**

:   The *module* that opens when Xolmis starts. Same as *start page*. Represented by a purple house button :material-home: at the top left of *main window*.

**Individual**

:    An *individual* represents a single bird that has been identified and tracked within the system. In Xolmis, an individual *record* is used to maintain continuity across multiple observations, captures, or *samples* of the same bird. This allows researchers to link data such as banding information, measurements, and life history events to one unique organism, ensuring consistency and traceability throughout the *database*.

**JSON**

:    *JSON* (JavaScript Object Notation) is a lightweight data-interchange format that uses key-value pairs and arrays. In Xolmis, JSON is used for structured data exchange between *modules* and external applications.  

     **Example**
     ```json
     {
       "id": 1,
       "name": "John Doe",
       "species": "Turdus rufiventris"
     }
     ```

**KML**

:    *KML* (Keyhole Markup Language) is an *XML*-based format for representing geographic data, such as points, lines, and polygons. In Xolmis, KML files can be used to visualize *localities*, *sampling plots*, or *expeditions* in mapping tools like Google Earth.

**KMZ**

:    *KMZ* is a compressed version of a *KML* file, packaged as a *ZIP* archive. In Xolmis, KMZ files are useful for sharing larger geographic datasets with reduced file size.

**Locality**

:    A *locality* is a specific geographic place where a *record* (such as a capture, *sighting*, or *sample*) occurs. In Xolmis, localities are linked to the *gazetteer* and represent the precise site of data collection, usually defined within a *municipality* or administrative unit. Localities provide spatial context for *records*, ensuring that observations and *samples* can be accurately mapped, analyzed, and compared across *surveys* and *expeditions*.

**Lookup field**

:    A *lookup field* is a data entry field that retrieves values from another table or list. In Xolmis, lookup fields ensure consistency by allowing users to select predefined options instead of typing free text.

**Main menu**

:    The *main menu* is the primary navigation interface of the application. In Xolmis, it provides access to *modules*, tools, and settings, serving as the starting point for all operations.

**Main window**

:   The Xolmis window from where all *modules* and features can be used.

**Mist net**

:    A *mist net* is a fine mesh net used in ornithology to safely capture birds for research. In Xolmis, mist net records document effort, dimensions, and usage during *surveys* and *expeditions*.

**Module**

:   Any feature of Xolmis that opens in a tab on the *main window*. It can be an inventory of some type of data or a tool to manage data.

**Municipality**

:    A *municipality* is the lowest administrative division with local government authority, typically responsible for managing a city, town, or district. In Xolmis, municipalities are linked to the *gazetteer* and represent the geographic unit below states or provinces, used to organize *localities* consistently.  

**New button**

:   Button at the top left of *main window*, represented as a purple plus sign :material-plus-circle:. It opens a menu with options to create new *records* of many types.

**ODS**

:    *ODS* (OpenDocument Spreadsheet) is a file format for *spreadsheets* based on the OpenDocument standard, commonly used by LibreOffice and OpenOffice. In Xolmis, ODS files can be used to import or export tabular data in an open, non-proprietary format.

**Outlier**

:    An *outlier* is a data point that differs significantly from other observations in a dataset. Outliers may occur due to natural variation, measurement error, or unusual conditions. In Xolmis, identifying outliers is important for data validation and analysis, since they can affect statistical results, highlight exceptional biological events, or reveal inconsistencies in data entry.

**Parent taxon**

:   *Taxon* of higher rank in the botanical or zoological *taxa* hierarchy, to which other *taxa* are subordinated.

**Parent toponym**

:   *Toponym* of higher rank in the *gazetteer* hierarchy, to which other *toponyms* are subordinated.

**Permit**

:    A *permit* is an official authorization issued by a competent authority that allows researchers or institutions to carry out specific activities, such as capturing, banding, or sampling wild birds. In Xolmis, permit *records* store details about the issuing institution, validity period, and scope of the authorization, ensuring that all fieldwork complies with legal and ethical requirements.

**_Philornis_ sp.**

:    *Philornis sp.* refers to parasitic flies of the genus *Philornis*, known for affecting birds by laying larvae that feed on nestlings. In Xolmis, *records* of *Philornis* sp. are important for documenting parasitism events and their impact on bird populations.

**Quick Entry**

:   Name of the tool to insert data records in batches using a *spreadsheet*-like grid. It differs from *batch dialogs* in allowing to add *records* without data in common.

**Record**

:    A *record* is a single entry in the *database*, representing one unit of information (e.g., a capture, a sighting, a specimen). In Xolmis, records are the fundamental data objects stored and managed across *modules*.

**Related data**

:   In some *modules* each *record* can have one or more lists of related *records*. The related data is located at the bottom of the *module* tab and *main window*, and have a bar with a button to add related *records* :material-plus-circle: and the tabs of related types of *records*. The related data is shown or hidden clicking the related tabs. Each related tab shows the number of related *records*, if it have any.

**Report**

:    A *report* is a formatted output generated from the *database*, summarizing or presenting data for analysis, publication, or decision-making. In Xolmis, reports can be exported or printed directly from *data grids* and *modules*.

**Row**

:    A *row* is a horizontal line of *cells* in a *data grid* or *spreadsheet*. Each row represents a single *record*, with its values distributed across the *columns*. In Xolmis, rows are used to display and edit *records* within *grids*, allowing users to view multiple entries at once and navigate through datasets efficiently.

**Sample**

:    A *sample* is a material taken from a *specimen* or environment for analysis, such as feathers, blood, or tissue. In Xolmis, collected samples are the same as *specimens*.

**Sampling plot**

:    A *sampling plot* is a defined geographic area where systematic data collection takes place. In Xolmis, sampling plots are linked to *surveys* and represent the spatial unit of effort, allowing researchers to record captures, sightings, or environmental measurements consistently within a bounded location.

**Schema**

:    A *schema* is the structural definition of how data is organized in the *database*, including tables, fields, and relationships. In Xolmis, the schema ensures consistency across *modules* and defines how *records* interconnect.

**Sighting**

:    A *sighting* is the observation (or hearing) of one or more birds in the field, without necessarily capturing them. In Xolmis, sighting *records* document the *taxon* observed, the *locality*, date, time, and observer, providing valuable data for monitoring populations, distribution, and behavior. Sightings complement capture *records* by expanding knowledge of species presence and abundance in surveyed areas.

**Specimen**

:    A *specimen* is an individual organism (or part of it) collected, observed, or documented for study. In Xolmis, a specimen record represents a collected *sample*.

**Spreadsheet**

:    A *spreadsheet* is a file or interface that organizes data in *rows* and *columns*, typically used for manual entry and calculations. In Xolmis, spreadsheet-like tools (such as *Quick Entry*) allow bulk data input before importing into the *database*.

**Start page**

:   Same as *home*.

**Survey**

:    A structured sampling event in which data is collected following a defined protocol. In Xolmis, a survey represents the organizational unit that groups together captures, sightings, mist nets, and other related *records*. It provides the temporal and spatial context for fieldwork, ensuring that all associated data can be linked to the same sampling effort.

**Synonym**

:    A *synonym* is an alternative scientific name that has been historically applied to a taxon but is not currently the accepted name. In *taxonomy*, synonyms arise when species are reclassified or when multiple names have been published for the same organism. In Xolmis, synonyms are stored to ensure that older *records* remain traceable and can be correctly linked to the accepted taxon.

**Taxon**

:   A *taxon* (plural: *taxa*) is a unit of biological classification, representing a group of organisms that share common characteristics. It can refer to any rank in the taxonomic hierarchy, such as species, genus, family, order, or higher categories.

**Taxonomy**

:    *Taxonomy* is the scientific system of classifying organisms into hierarchical categories such as species, genus, family, and order. In Xolmis, the taxonomy *module* organizes taxa and ensures that *record*s are linked to standardized classifications.

**Toponym**

:   A *toponym* is the name given to a geographic place, such as a city, river, mountain, or region. Toponyms are linguistic labels that identify locations and often carry historical, cultural, or descriptive meaning. In Xolmis, toponyms are used in the *gazetteer* to standardize *locality* names, ensuring that *records* are consistently linked to recognized geographic entities.

**Validation**

:    *Validation* is the process of checking whether data entered into the system meets required rules or formats. In Xolmis, validation ensures consistency, prevents errors, and maintains the integrity of *records*.

**Vernacular name**

:    A *vernacular name* is the common or popular name used by the general public to refer to a species, often varying by language, region, or culture. In Xolmis, vernacular names provide accessible references to *taxa* alongside scientific names, helping users identify species more easily in different contexts.

**WGS84**

:    *WGS84* (World Geodetic System 1984) is the standard coordinate reference system used globally for mapping and GPS. In Xolmis, all geographic data (*localities*, coordinates, maps) are stored and displayed using WGS84 to ensure compatibility with external tools and datasets.

**Wizard**

:    A *wizard* (assistant) is a guided interface that helps users complete complex tasks step by step. In Xolmis, wizards simplify operations such as importing data, creating *records*, or configuring *modules*.

**XLSX**

:    *XLSX* is the default *spreadsheet* file format used by Microsoft Excel, based on the Office Open XML standard. In Xolmis, XLSX files are often used for exchanging data with researchers who rely on Excel.

**XML**

:    *XML* (Extensible Markup Language) is a markup format that uses tags to define structured data. In Xolmis, XML can be used for data exchange and integration with other systems.  

     **Example**
     ```xml
     <record>
       <id>1</id>
       <name>John Doe</name>
       <species>Turdus rufiventris</species>
     </record>
     ```

**ZIP**

:    *ZIP* is a compressed file format that bundles one or more files into a single archive, reducing storage size and simplifying distribution. In Xolmis, ZIP files are often used to package *databases* or geographic files (such as *KML*/*KMZ*) for sharing and backup.
