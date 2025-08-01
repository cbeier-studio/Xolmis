# Importing data

There are various methods to import files into Xolmis, which vary based on the file's origin and format. Each option is outlined below.

## Import wizard

!!! note ""

    Working on it!

## Xolmis Mobile

Xolmis have a mobile companion, the [Xolmis Mobile](https://github.com/cbeier-studio/xolmis_mobile) app. It enables the collection of data in the field and its exportation in the form of text files, either in CSV or JSON format. Here is how to import these files in JSON format.

Begin by opening the import wizard located in the main menu: **File > Import > Xolmis mobile**. The dialog that opens should appear as shown below.

Choose a file in the source file field. The file will be accessed, and the dialog will display if it is OK or not. Click the **Next** button to proceed.

The records of the file to be imported will be listed. The user can uncheck the records that do not want to import. If possible, some info will be filled automatically. The observer and locality columns are required. The record column corresponds to the database record for each file record. Click the **Next** button to proceed with the import. File records without corresponding database record will be inserted, otherwise they will be updated.

The importation process will then commence and display its progress in the dialog. You can halt the importation at any time by clicking the **Cancel** button located at the bottom of the dialog.

Upon completion, the dialog will display the result, whether successful or not. If a problem occurred, no data will be saved. You may attempt the process again by clicking the **Try Again** button. Alternatively, you can save the import log by clicking the **Save Log** button to investigate the cause of the problem before retrying.

## eBird records

To import a file exported from the eBird platform, simply select the menu option **File > Import > eBird records**. This will open a dialog box to locate and select the desired file for import. Once the file is selected, the import process will begin and a message will be displayed upon completion. It is just that simple!

## Banding data

To import banding data in CSV format, select the menu option **File > Import > Banding Data**. This action will open the corresponding dialog.

There are three types of banding data available for import. You must select at least one file to initiate the import, but you have the option to select all three. Once the import begins, the dialog will display the progress and results of the process.

## Nests data

!!! note ""

    Working on it!

## Geographical coordinates

Files containing geographical coordinates, such as KML and GPX files, can be imported and utilized across various tables in Xolmis via the [Coordinates Editor](adding-and-editing-data.md#geoassist).

Simply click on the menu option **Files > Import > Geographical Coordinates** and choose the file you wish to import. The system will display the progress and notify you upon completion.
