# Coordinates converter

Xolmis includes a tool to **convert geographical coordinates** (currently only in the WGS84 reference system). It is accessible in the top menu: **Geo → Coordinates converter**, and opens in a new tab. This tool is useful for standardizing coordinates across different formats, ensuring consistency in fieldwork, surveys, and data analysis.

!!! warning "Important"
    Coordinates must be entered in the following order and separated by `;` (semicolon): **longitude; latitude**  

    Entering values in the wrong order will result in incorrect conversions and potentially misleading spatial data.

![Coordinates converter](img/coordinates-converter.png)

## Options

At the top of the left panel, select:

1. The **source format** (the format of the coordinates you have).  
2. The **target format** (the format you want to convert to).  

Available formats:

- **Decimal degrees (DD)** – e.g., `-43.1729; -22.9068`  
- **Degrees, minutes, and seconds (DMS)** – e.g., `43°10'22"W; 22°54'24"S`  
- **Universal Transverse Mercator (UTM)** – requires additional parameters:  
    - **UTM zone** - e.g., `22J`
    - **Hemisphere** (North or South)  

When converting to DMS or UTM, you can enable the option **Add units/zone to the result** to include symbols (°, ', ") or zone identifiers in the output.

## Getting coordinates to convert

### From file

You can load coordinates from a file. The file must contain only longitude and latitude values separated by semicolons.

### From clipboard

Copy coordinates from another application (e.g., Microsoft Excel) and paste them into the left text editor using the **Paste** button or the shortcut ++ctrl+v++.  

After loading, Xolmis automatically strips symbols and attempts to guess the format.

### Swap values

Coordinates must follow the sequence **longitude; latitude**. If your data is in the opposite order (**latitude; longitude**), click the **Swap values** button to correct them automatically.

## Convert coordinates

Once the source and target formats are defined:

1. Click the **Convert** button.  
2. The converted coordinates will appear in the right text editor.  
3. If errors are detected, they will be indicated in the right panel with the corresponding line number.  
4. Correct the errors in the left editor and run the conversion again.  

## After conversion

### Save to file

Click the **Save** button to export the converted coordinates to a CSV file. You will be prompted to choose a filename and location.

### Copy to clipboard

Click the **Copy** button to copy all converted coordinates to the clipboard. You can then paste them into other applications (e.g., spreadsheets, GIS software).

### Add to GeoAssist

Click **Add to GeoAssist** to send the converted coordinates directly to the [GeoAssist](adding-and-editing-data.md#geoassist). They will then be available for use within Xolmis modules.

## Reset the converter

Click the **Clear all** button to empty both text editors and start a new conversion process.

## Best practices

- **Always check the order of values**: Longitude must come before latitude.  
- **Validate formats before conversion**: Ensure the source format matches the data you are importing.  
- **Use Swap values carefully**: Only apply it if you are certain the data is inverted.  
- **Keep backups of original files**: Avoid overwriting raw data; store converted coordinates separately.  
- **Add units/zone when exporting**: Helps prevent confusion when sharing data with collaborators.  

## Relation to other modules

The Coordinates converter integrates with several modules:

- **[GeoAssist](adding-and-editing-data.md#geoassist)** – Use converted coordinates directly in data entry.

By using the converter, researchers can maintain **accuracy, consistency, and interoperability** of spatial data across Xolmis and external GIS tools.
