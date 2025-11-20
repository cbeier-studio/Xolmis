# Importing data

Xolmis is designed to be a **flexible data repository**, capable of integrating information collected from different sources and formats. Importing data allows you to bring external records into the system, whether they come from fieldwork, other platforms, or geographic tools. There are several methods to import files into Xolmis, depending on the file’s origin and format. Each option is outlined below.

!!! note
      To import data, the **user must have permission** for that. See details in [Users](users.md).

## Import wizard

The **Import wizard** is a general tool that guides the user step by step through the import process. It is being expanded to support multiple file types and sources, ensuring that data can be validated before insertion. Future updates will include support for bulk imports, error handling, and mapping of fields between external files and Xolmis tables.

## Xolmis Mobile

Xolmis has a mobile companion app, [Xolmis Mobile](xolmis-mobile.md), which enables the collection of data directly in the field. Data collected with the app can be exported as text files in **JSON** format, ready to be imported into the desktop version of Xolmis.

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

Xolmis supports importing data exported from the **eBird** platform. To import eBird records:

1. Select **File → Import → eBird records**.  
2. Locate and select the desired file.  
3. The import process will begin automatically, and a message will confirm completion.  

This feature allows you to integrate citizen science data with your own research database.

## Banding data

Banding data can be imported in **CSV format**. To import:

1. Select **File → Import → Banding Data**.  
2. The dialog will open, allowing you to choose one or more files.  
      - There are three types of banding data available for import.  
      - You must select at least one file, but you may select all three.  
3. Once the import begins, the dialog will display progress and results.  

This feature ensures that banding records collected externally can be integrated into the Individuals and Captures modules.

## Nests data

The import of **nest data** is under development. Future versions will allow importing nest records from external sources, linking them to individuals, localities, and projects.

## Geographical coordinates

Files containing geographical coordinates, such as **KML** and **GPX** files, can be imported and used across various tables in Xolmis via the [GeoAssist](adding-and-editing-data.md#geoassist).  

To import:

1. Select **File → Import → Geographical Coordinates**.  
2. Choose the file you wish to import.  
3. The system will process the file, display progress, and notify you upon completion.  

This feature is especially useful for importing GPS tracks, waypoints, or predefined sampling plots.

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
