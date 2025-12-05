# Xolmis

A free, open source, multi-platform ornithological data manager, built using Lazarus/FPC.

> [!IMPORTANT]
> Xolmis is in the early stages of development and is subject to some breaking changes.

Xolmis is an ornithological data management system intended for researchers and birdwatchers. It was designed to aggregate separated data files (_e.g._ CSV files and Excel spreadsheets), which may end up being lost over time. In addition to the objective of aggregating different types of data collected in research and observations, it also aims to facilitate the curation, crossing and sharing of data, while increasing its consistency and security.

![Xolmis screenshot](https://github.com/user-attachments/assets/8079e3ce-4cd2-40e8-aac6-54616410eb65)


:dove: _Xolmis_ is a genus of Neotropical passerines. Today it is represented by two species: [_Xolmis irupero_](https://www.wikiaves.com.br/wiki/noivinha) and [_Xolmis velatus_](https://www.wikiaves.com.br/wiki/noivinha-branca).

## Features

Here is a list of the main features:

- [x] Management of database connections.
- [ ] Start page with dashboard.
- [x] Management of expeditions and samplings.
- [x] Lists of sightings and samples collection.
- [x] Bands and markings management (Brazilian system/CEMAVE).
- [x] Management of known individuals and captures (including feathers and molt data).
- [x] Record of breeding biology data.
- [x] Record of institutions, researchers, and projects.
- [x] Management of permits.
- [x] Record of toponyms (gazetteer) and sampling plots.
- [x] Updated bird taxonomies (Clements/eBird).
- [x] Geographical coordinates converter (WGS84) from/to Decimal Degrees, DMS, and UTM formats.
- [x] Import data from [Xolmis Mobile](https://github.com/cbeier-studio/xolmis_mobile) (JSON file).
- [x] Import eBird records (CSV file).
- [x] Import banding journals and captures (CSV file).
- [ ] Import nests (CSV file).
- [ ] Import data from files with different schemas (CSV, JSON, ODS, XLSX, XML, and DBF formats).
- [x] Import and export geographical coordinates from/to file (CSV, KML, GPX, and GeoJSON formats).
- [x] Export data to file (CSV, JSON, ODS, XLSX, and XML formats).
- [x] Show geographical coordinates on a map.
- [x] Column summaries.
- [x] Attachment of images, audio recording, videos, documents and links to the field records.
- [ ] Print and export data to PDF.

See the [Milestones](https://github.com/cbeier-studio/Xolmis/milestones) for more information on planned features.

## Technology Stack

- **Language:** Free Pascal  
- **IDE:** Lazarus  
- **Database:** SQLite  
- **Configuration:** JSON (`jsonconf` package)  
- **Documentation:** FPDoc (developer), MkDocs + Material for MkDocs (user)  

## Download and installation

See the [Wiki](https://github.com/cbeier-studio/Xolmis/wiki/Installing) for options and instructions of how to install.

> [!NOTE]
> Linux and MacOS versions are not released yet. They need more work.
> Let me know if you are interested in helping.

## How to contribute

### Giving feedback

You can create [pull requests](https://github.com/cbeier-studio/xolmis/pulls) directly and give feedback using the [GitHub Issues](https://github.com/cbeier-studio/xolmis/issues). All suggestions, bugs reported and general issues are much appreciated.

### Coding

If you want to help in Xolmis development, please send me a message to [hello@christianbeier.studio](mailto:hello@christianbeier.studio).

For requirements and packages needed, see the [Wiki](https://github.com/cbeier-studio/Xolmis/wiki/Installing).

## License

Xolmis is available under the [GPL-3.0 license](https://github.com/cbeier-studio/xolmis/blob/main/LICENSE).

We use data from [Countries States Cities Database](https://github.com/dr5hn/countries-states-cities-database), available under the [ODbL v1.0](https://github.com/dr5hn/countries-states-cities-database/blob/master/LICENSE).

Icons used in Xolmis were adapted from [Fluent UI System Icons](https://github.com/microsoft/fluentui-system-icons) and [Fluent Emoji](https://github.com/microsoft/fluentui-emoji). Both were made available by Microsoft Corporation under the [MIT license](https://github.com/microsoft/fluentui-system-icons/blob/main/LICENSE).

## Acknowledgements

Xolmis is developed to support ornithological research, conservation, and citizen science.  
We thank all contributors, institutions, and communities engaged in bird monitoring and ecological studies.
Special thanks to our sponsors!

### Platinum Sponsor

[![Alianza del Pastizal - Platinum Sponsor](/docs/img/alianza-del-pastizal.png)](http://www.alianzadelpastizal.org.br)
