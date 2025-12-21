# Importing data

Xolmis is designed to be a **flexible data repository**, capable of integrating information collected from different sources and formats. Importing data allows you to bring external records into the system, whether they come from fieldwork, other platforms, or geographic tools. There are several methods to import files into Xolmis, depending on the file’s origin and format. Each option is outlined below.

!!! note
      To import data, the **user must have permission** for that. See details in [Users](users.md).

## Import wizard

![Import wizard dialog](img/import-wizard.png)

The **Import wizard** guides you through all steps required to bring external data into Xolmis safely and consistently.  
It is designed to validate, preview, and map data before it is inserted into the database, reducing errors and ensuring compatibility with Xolmis tables.

The wizard is divided into five steps:

1. **Source and destination selection**  
2. **General import settings**  
3. **Field mapping**  
4. **Import progress**  
5. **Completion**

Each step is described in detail below.

### 1. Source and destination selection

In the first step, you choose:

- **Source file:** the external file you want to import (see the supported file types below).
- **Destination table:** the Xolmis table where the data will be inserted (e.g., *Sightings*, *Individuals*, *Nests*, *Eggs*, *Specimens*, etc.).
- **Import settings:** optionally, you can choose a saved import profile.

This step ensures that the wizard loads the correct structure and prepares the appropriate field mapping options.

### 2. General import settings

This step configures how the file should be interpreted.  
Different formats expose different options, but common settings include:

| Option | Description | Default |
| --- | --- | --- |
| **Import strategy** | How to treat duplicate data. | Append |
| **Error handling** | How to handle errors during import. | Abort on first error |
| **File encoding** | UTF-8 or system encoding. | System encoding |
| **First row as header** | First row contains column names. | Yes |
| **Delimiter** | Column delimiter: Comma, Semicolon, Tab, Other. | Semicolon |
| **Sheet or tab** | Which sheet to read from spreadsheet file. | first sheet |
| **Decimal separator** | Comma or period (dot). | Comma |
| **Records key path** | Path to JSON/XML key containing the list of records. | |
| **Records XPath** | Name of the XML tag containing one record. | |

| Option | CSV/TSV | ODS/XLSX | JSON | XML | DBF |
| --- | :-: | :-: | :-: | :-: | :-: |
| **Import strategy** | :material-check: | :material-check: | :material-check: | :material-check: | :material-check: |
| **Error handling** | :material-check: | :material-check: | :material-check: | :material-check: | :material-check: |
| **File encoding** | :material-check: |  | :material-check: | :material-check: | :material-check: |
| **First row as header** | :material-check: | :material-check: |  |  |  |
| **Delimiter** | :material-check: |  |  |  |  |
| **Sheet or tab** |  | :material-check: |  |  |  |
| **Decimal separator** | :material-check: | :material-check: | :material-check: | :material-check: |  |
| **Records key path** |  |  | :material-check: | :material-check: |  |
| **Records XPath** |  |  |  | :material-check: |  |

These settings ensure that Xolmis interprets the file correctly before mapping fields.

### 3. Field mapping

In this step, you define how each column from the source file corresponds to fields in the destination table.

The wizard automatically:

- lists all fields from the source file  
- lists all fields from the destination table  
- attempts to **infer data types** (integer, float, date, time, boolean, text)  
- suggests mappings when possible  

You can adjust each mapping manually. For each field, the following options are available:

| Option | Description | Default |
| --- | --- | --- |
| **Source field** | Column name from the imported file (read only). | |
| **Target field** | Field in the Xolmis table. | |
| **Import** | Whether this field should be imported. | |
| **Primary or corresponding field** | Field used as reference to check duplicate records. | No |
| **Data type** | Automatically inferred; can be overridden. | Text |
| **Lookup table** | If the source field value needs to be searched in another table. | |
| **Lookup field** | Which field to search in the lookup table. | |
| **Null values** | How to treat null values: Ignore, Default value, Mean value, Median value, Mode value. | Ignore |
| **Array fields** | How to treat array fields: Ignore, JSON string. | Ignore |
| **Trim value**| Trim spaces from start and end of the value. | Yes |
| **Boolean value** | Force treatment of field as boolean. | No |
| **Text case** | Transform text case: Original case, Lower case, Upper case, Sentence case, Title case. | Original case |
| **Remove accents** | Remove text diacritics. | No |
| **Normalize whitespace** | Remove tabs and double spaces in the middle of text. | Yes |
| **Replace chars** | Replace text in all text values. | No |
| **Round value** | Round decimal numbers to a defined precision. | No |
| **Scale value** | Multiply or divide value by a scale value. Useful to convert units. | No |
| **Extract date part** | Extract year, month or day. | No |
| **Convert coordinates** | Convert geographical coordinates to decimal degrees. | No |
| **Split coordinates** | When the source coordinates have longitude and latitude in the same value. | No |

The goal of this step is to ensure that every imported value matches the structure and constraints of the Xolmis database.

### 4. Import progress

Once the mapping is confirmed, the wizard begins the import process.

The progress screen displays:

- **Number of processed rows**  
- **Warnings** (non-critical issues)  
- **Errors** (rows that could not be imported)

If errors occur, the wizard provides:

- a list of problematic rows  
- the reason for each error  
- an option to export the error report for review

This allows you to correct issues and re-import only the affected rows if needed.

### 5. Completion

When the import finishes, the wizard shows a summary:

- Total rows processed  
- Rows successfully imported  
- Rows skipped  
- Rows with errors  
- Destination table  
- Time elapsed

You can then:

- **Open the destination table** to review the imported data  
- **Save the import profile** for future use  
- **Export the error report** (if applicable)  
- **Start a new import**

### Saving and reusing import profiles

The wizard allows you to save your import configuration as a **profile**, which includes:

- file format settings  
- delimiter, encoding, date/time formats  
- field mappings  
- transformations

Profiles are stored in the Xolmis database and can be exported/imported as JSON files.

This is especially useful for recurring imports with the same structure.

### Supported file formats

Xolmis currently supports importing data from multiple file types, each suited to different use cases:

- **CSV (Comma-Separated Values)**
      - Widely used for tabular data.
      - Compatible with most spreadsheet and statistical software.
      - Simple and lightweight, but does not support complex structures.
- **TSV (Tab-Separated Values)**
      - Similar to CSV, but uses tabs as delimiters.
      - Useful when data fields contain commas, reducing parsing errors.
- **JSON (JavaScript Object Notation)**
      - Ideal for structured data exchange and integration with APIs.
      - Human-readable and machine-friendly.
      - Supports hierarchical data.
- **XML (eXtensible Markup Language)**
      - Suitable for interoperability with legacy systems.
      - Schema-based validation possible.
      - Verbose but standardized.
- **ODS (OpenDocument Spreadsheet)**
      - Open standard for spreadsheet documents.
      - Preserves formatting, formulas, and multiple sheets.
      - Good alternative to proprietary formats.
- **XLSX (Microsoft Excel)**
      - Native format for Microsoft Excel.
      - Supports rich spreadsheet features such as styles, formulas, and multiple sheets.
      - Widely used in professional environments.
- **DBF (Database File)**
      - Legacy format used by dBASE and compatible applications.
      - Still common in some GIS and database workflows.
      - Useful for interoperability with older systems.

### Tips for choosing the right format

- Use **CSV/TSV** for simple tabular data and maximum compatibility.  
- Use **JSON/XML** for structured or hierarchical data, especially when integrating with other systems.  
- Use **ODS/XLSX** when spreadsheet features (formulas, formatting, multiple sheets) are required.  
- Use **DBF** if working with legacy databases or GIS applications that rely on this format.

## Xolmis Mobile

Xolmis has a mobile companion app, [Xolmis Mobile](xolmis-mobile.md), which enables the collection of data directly in the field. Data collected with the app can be exported as text files in **JSON** format, ready to be imported into the desktop version of Xolmis.

![Import from Xolmis Mobile dialog - file selection](img/import-xolmis-mobile1.png) ![Import from Xolmis Mobile dialog - data review](img/import-xolmis-mobile2.png)

To import JSON files from Xolmis Mobile:

1. Open the import wizard in the main menu: **File → Import → Xolmis mobile**.  
2. In the dialog, choose the source file. The system will automatically validate the file and indicate whether it is acceptable. Optionally, you can select an **expedition** to associate with the imported data.
3. Click **Next** to proceed. The records from the file will be listed.  
      - You can uncheck records you do not want to import.  
      - Some fields may be filled automatically if matching data already exists.  
      - **Observer** and **Locality** are required fields.  
      - The **Record** column corresponds to the database record for each file entry.  
4. Click **Next** again to start the import.  
      - Records without a corresponding database entry will be inserted.  
      - Records with a match will be updated.  
5. The progress will be displayed in the dialog. You can stop the process at any time by clicking **Cancel**.  
6. When finished, the dialog will show the result.  
      - If errors occurred, no data will be saved.  
      - You can retry with **Try Again** or save the import log with **Save Log** to investigate issues.

## eBird records

Xolmis supports importing data exported from the **eBird** platform in **CSV** format. To import eBird records:

1. Select **File → Import → eBird records**.  
2. Locate and select the desired file.  
3. The import process will begin automatically, and a message will confirm completion.  

This feature allows you to integrate citizen science data with your own research database.

## Banding data

![Import banding data dialog](img/import-banding-data.png)

Select in the main menu **File → Import → Banding Data**. See details in [Importing Banding Data](importing-banding-data.md).

## Nests data

![Import nests data dialog](img/import-nesting-data.png)

Select in the main menu **File → Import → Nests Data**. See details in [Importing Nesting Data](importing-nesting-data.md).

## Geographical coordinates

Files containing geographical coordinates can be imported and used across various tables in Xolmis via the [GeoAssist](adding-and-editing-data.md#geoassist). This allows you to integrate spatial information directly into your ornithological records.

### Supported file formats

Xolmis currently supports the following formats:

- **CSV (Comma-Separated Values)**  
  Tabular format where latitude and longitude are stored in columns.  
  Best for simple datasets exported from spreadsheets or statistical software.

- **KML/KMZ (Keyhole Markup Language / Compressed KML)**  
  Widely used in Google Earth and other GIS tools.  
  Supports points, lines, polygons, and metadata.  
  KMZ is the compressed version of KML, useful for larger datasets.

- **GPX (GPS Exchange Format)**  
  Standard format for GPS devices and applications.  
  Ideal for importing tracks, routes, and waypoints recorded in the field.

- **GeoJSON**  
  Modern, lightweight format for representing geographical features in JSON.  
  Well-suited for web applications and APIs.  
  Supports complex geometries and properties.

### How to import

1. Select **File → Import → Geographical Coordinates**.  
2. Choose the file you wish to import.  
3. The system will process the file, validate the data, display progress, and notify you upon completion.

### Typical use cases

- Importing **GPS tracks** from field expeditions.  
- Adding **waypoints** for sampling plots or observation sites.  
- Loading **predefined polygons** for study areas or conservation zones.  
- Integrating **spatial datasets** from external GIS or mapping tools.

## Best practices

- **Validate files before importing**: Ensure that the format and structure match the expected requirements.
- **Keep backups**: Always maintain a copy of the original files before importing.
- **Use logs**: Save import logs when errors occur to diagnose problems.
- **Check duplicates**: Avoid importing the same file multiple times to prevent redundant records.
- **Prefer standardized formats**: Use CSV, JSON, KML, or GPX whenever possible for maximum compatibility.

## Relation to other modules

Imported data integrates seamlessly with other parts of Xolmis:

- **[Sightings](sightings.md), [Nests](nests.md) and [Specimens](specimens.md)**: Mobile data feed directly into sighting, nest and specimen records.  
- **[Surveys](surveys.md) and [Expeditions](expeditions.md)**: Imported coordinates and survey data enrich fieldwork records.  
- **[Reports](print-data.md) and [Exports](exporting-data.md)**: Once imported, data can be analyzed and exported again for external use.  

By using the import features, Xolmis becomes a central hub for managing ornithological data collected from diverse sources.

*[CSV]: Comma Separated Values
*[DBF]: Database File
*[GPX]: GPS Exchange Format
*[JSON]: JavaScript Object Notation
*[KML]: Keyhole Markup Language
*[KMZ]: Compressed Keyhole Markup Language
*[ODS]: Open Document Spreadsheet
*[TSV]: Tab Separated Values
*[XLSX]: Microsoft Excel spreadsheet
*[XML]: Extensible Markup Language
