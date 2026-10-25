# Print data

Most modules in Xolmis include a **print button** :material-printer: that opens a menu with options to generate printable reports. This functionality allows researchers to produce hard copies or digital files (PDF) of their data, useful for documentation, sharing, or archiving.

!!! note
      To print data, the **user must have permission** for that. See details in [Users](users.md).


## Print preview

Other reports are generated as an **HTML page** and opened in the **default web browser** of your computer or operating system. The browser acts as the print preview window, so its own features are available, such as:

- **Zoom in/out** – Adjust the view of the report.  
- **Search in page** – Find text in the report (`Ctrl+F`).  
- **Print and save as PDF** – Use the browser print dialog (`Ctrl+P`).  

!!! note
    The report is a temporary file and does not need internet access. If you want to keep it, save it as PDF.

### Send to printer

In the browser, press `Ctrl+P` (or use the browser menu **Print**) to send the report to a printer. In the print dialog, you can:

- Select the printer to use.  
- Define which pages to print.  
- Set the number of copies.  

The page size and orientation chosen in the report settings dialog are used as the default for printing. Check that the browser's *Margins* option is set to *Default* and that *Background graphics* is enabled if you want the colors of the report.

### Export PDF

In the browser print dialog, choose **Save as PDF** (or **Microsoft Print to PDF**) as the destination and click **Save**. Then:

- Choose the destination folder.  
- Define the file name.  

Exporting to PDF is recommended for sharing reports digitally or archiving them in project documentation.

### Report settings and profiles

Before a report is generated, the **report settings dialog** lets you choose the page size, page orientation and color theme. For modules with many columns, such as **Captures**, it also offers a **Report profile** to keep the list readable:

| Profile | Content |
|---------|---------|
| **All columns** | Every column available for the report. |
| **Summary** | Location, date, taxon, band, age, sex, status and notes. |
| **Morphometry** | Measurements: wing chord, tail, tarsus, bill, skull, weight, etc. |
| **Molt and breeding condition** | Molt, fat, brood patch, cloacal protuberance, skull ossification, etc. |
| **Samples and photos** | Samples collected, blood parameters and photography data. |

Every profile repeats the identification columns (locality, date/time, bander, type, taxon and band), so each row keeps its context. The profile name is added to the report title.

## Best practices

- **Split wide reports with profiles**: In Captures, generate one report per profile instead of a single report with all columns.  
- **Limit columns when possible**: Hide unnecessary columns before printing to reduce page count.  
- **Use PDF for sharing**: Export reports as PDF to ensure compatibility across devices and operating systems.  
- **Check printer settings**: Configure page size, orientation, and margins to avoid formatting issues.  
- **Archive reports**: Save important reports as PDF to maintain a permanent record of your data.  

## Relation to other modules

The print functionality is available across most modules, including:

- **[Sightings](sightings.md) and [Captures](captures.md)** – Generate reports of field observations and banding data.  
- **[Nests](nests.md) and [Eggs](eggs.md)** – Print breeding records for analysis or submission to institutions.  
- **[Projects](projects.md) and [Permits](permits.md)** – Produce administrative reports for management or compliance.  
- **[Institutions](institutions.md) and [Researchers](researchers.md)** – Export lists for collaboration or documentation.  

By using the print tools in Xolmis, researchers can easily transform digital records into **shareable, standardized reports**, supporting both scientific communication and administrative needs.
