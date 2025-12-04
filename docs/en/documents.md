# Documents and links

The **Documents and links** feature allows researchers to attach external files or references to records in Xolmis. This ensures that supporting materials such as reports, datasets, presentations, or online resources are directly linked to ornithological data, making the database a comprehensive repository of information.

!!! danger
    Before adding any documents, go to [Settings](settings.md) in the **Media** section and set the **documents location**. Documents are saved with a file path relative to this location. Changing the location afterwards can cause problems in accessing files.

    {==A solution for dynamic relocation is under development.==}

## Toolbar

| Icon | Button | Function |
| --- | --- | --- |
| :material-plus-circle: | Add document/link | Insert new documents from files or link URL |
| :material-pencil: | View and edit document/link info | Edit the selected document or link info |
| :material-open-in-new: | View document/link | Opens the selected document or link in the default application |
| :material-delete: | Delete document/link | Delete the selected document or link |

## Adding documents

You can add documents in two ways:

1. **Using the add button**  
      - Click the **Add** button :material-plus-circle: and the option **Add documents** at the top toolbar of the documents and links side panel.  
      - Select one or more files to attach to the current record.  
      - Click **Open** to confirm.  
      - The system will show the progress of adding the files.  

2. **Drag and drop**  
      - Drag files from your file explorer.  
      - Drop them directly into the documents and links side panel.
      - The system will show the progress of adding the files.  

3. **Edit metadata**
      - If metadata is present (e.g., creation date), it will be automatically extracted.
      - Link the documents to records using the dialog that opened.
      - Other information must be edited manually afterwards.

This flexibility allows quick integration of documents into the database.

## Adding link

- Click the **Add** button :material-plus-circle: and the option **Add link** at the top toolbar of the documents and links side panel.  
- Fill the metadata in the dialog that opened, and click the **Save** button.

## Editing document or link info

To edit document or link information:

1. Select the document or link in the side panel.  
2. Click the **Edit** button :material-pencil: in the toolbar.  
3. A dialog will open with editable fields.  

![Edit document or link dialog](img/edit-document-link-dialog.png)

| Field | Required | Description |
| --- | --- | --- |
| **Document type** |  | Type of document (see details below) |
| **Document title** |  | Name or title given to the document |
| **Document date** | Yes | Date of creation of the document |
| **Document time** |  | Time of creation of the document |
| **Document file/URL** | Yes | Relative file path of the document or external URL |
| **License type** |  | Type of license applied to the document |
| **License year** |  | Year the document was licensed |
| **License owner** |  | Rights holder of the document |
| **License notes** |  | Additional notes about licensing |
| **License URL** |  | Link to the license text |

### Document types

Documents can be classified into the following types:

- **URL** – External web link (e.g., article, dataset, online resource).  
- **Document** – Text documents (DOC, TXT, ODT).  
- **Spreadsheet** – Data tables (XLSX, ODS, CSV).  
- **Presentation** – Slides (PPT, ODP).  
- **PDF** – Portable Document Format files.  
- **Code** – Scripts or source code files.  
- **Image** – Photographs, diagrams, or figures.  
- **Audio** – Sound or voice recordings.  
- **Database** – External database files.  
- **GIS** – Geographic Information System files (e.g., shapefiles, GeoJSON).  
- **Other** – Any other type not listed above.  

### License types

Licensing ensures proper use and sharing of images:

- **Copyright** – All rights reserved.  
- **CC BY** – Creative Commons with attribution.  
- **CC BY-SA** – Creative Commons with attribution and derivatives under the same license.  
- **CC BY-ND** – Creative Commons with attribution and no derivatives.  
- **CC BY-NC** – Creative Commons with attribution and no commercial use.  
- **CC BY-NC-SA** – Creative Commons with attribution, no commercial use, and derivatives under the same license.  
- **CC BY-NC-ND** – Creative Commons with attribution, no commercial use, and no derivatives.  
- **CC0** – Public domain (Creative Commons).  
- **Commercial** – Custom license with contractual terms.

## Best practices

- **Set the documents location first**: Avoid broken links by configuring the storage path before adding files.  
- **Use descriptive titles**: Helps identify documents quickly in lists and reports.  
- **Record licensing information**: Ensures compliance with copyright and data-sharing policies.  
- **Prefer relative paths**: Keep documents organized in the designated media folder for portability.  
- **Link relevant resources**: Attach reports, datasets, or references that complement the record.  
- **Use URLs for dynamic content**: When referencing online resources that may be updated, prefer URLs instead of static files.  

## Relation to other modules

Documents and links can be attached in several modules:

- **[Projects](projects.md)** – Add proposals, reports, or budgets.  
- **[Permits](permits.md)** – Attach scanned permits or authorization letters.  
- **[Expeditions](expeditions.md) and [Surveys](surveys.md)** - Attach maps, field notes, or supporting files.
- **[Sightings](sightings.md), [Captures](captures.md) and [Specimens](specimens.md)** – Link field notes, spreadsheets, or supporting files.  
- **[Nests](nests.md)** – Attach diagrams, measurements, or photographic evidence.  
- **[Researchers](researchers.md)** – Store CVs, publications, or institutional documents.  

By managing documents and links in Xolmis, researchers ensure that all supporting materials are **organized, accessible, and directly connected to ornithological data**.

## Example workflow

1. A researcher uploads a **PDF report** summarizing a survey.  
2. The file is attached to the **Survey record** in Xolmis.  
3. Licensing information is added (Creative Commons, year, owner).  
4. Later, another collaborator opens the survey record and directly accesses the report.  

This workflow demonstrates how documents and links strengthen collaboration and preserve valuable context for scientific records.
