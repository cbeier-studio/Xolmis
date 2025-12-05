# Images

The **Images** feature allows researchers to attach photographs and other visual files to records in selected modules. Images are essential for documenting taxa, fieldwork, specimens, nests, eggs, and other ecological evidence. By linking images directly to records, Xolmis ensures that multimedia data is organized, traceable, and available for future analysis.

Open the images side panel by clicking the image icon :material-image: on the right toolbar (if available in the module).

![Images view](img/images-view.png)

!!! danger
    Before adding any images, go to [Settings](settings.md) in the **Media** section and set the **images location**. Images are saved with a file path relative to this location. Changing the location afterwards can cause problems in accessing files.

    {==A solution for dynamic relocation is under development.==}

## Toolbar

| Icon | Button | Function |
| --- | --- | --- |
| :material-plus-circle: | Add image | Insert new images from files |
| :material-pencil: | View and edit image info | Edit the selected image info |
| :material-image-search: | View image | Opens the selected image in the image viewer |
| :material-delete: | Delete image | Delete the selected image |

## Adding images

You can add images in two ways:

1. **Using the add button**  
      - Click the **Add** button :material-plus-circle: at the top toolbar of the images side panel.  
      - Select one or more image files to attach to the current record.  
      - Click **Open** to confirm.  
      - The system will show the progress of adding the images.  
2. **Drag and drop**  
      - Drag image files from your file explorer.  
      - Drop them directly into the images side panel.
      - The system will show the progress of adding the images.  

- **Edit metadata**
      - If metadata is present (e.g., creation date, GPS coordinates), it will be automatically extracted.
      - Link the images to records using the dialog that opened.
      - Other information must be edited manually afterwards.

This flexibility allows quick integration of field photographs into the database.

## Editing image info

To edit image information:

1. Select the image in the side panel.  
2. Click the **Edit** button :material-pencil: in the toolbar.  
3. A dialog will open with editable fields.  

| Field | Required | Description |
| --- | --- | --- |
| **Subtitle** |  | Short description of the image |
| **Author** |  | Person who created the image |
| **Image date** | Yes | Date when the image was taken |
| **Image time** |  | Time when the image was taken |
| **Image type** |  | Category of the image (see below) |
| **Image file** | Yes | Relative file path of the image |
| **Locality** |  | Site where the image was taken |
| **Coordinates precision** |  | Precision of the geographical coordinates |
| **Longitude** |  | Longitude coordinate of the image |
| **Latitude** |  | Latitude coordinate of the image |
| **Taxon** |  | Taxon represented in the image |
| **License type** |  | Type of image license |
| **License year** |  | Year the image was licensed |
| **License owner** |  | Rights holder of the image |
| **License notes** |  | Additional notes about licensing |
| **License URL** |  | Link to the license text |

### Image types

Images can be classified into categories that describe their content:

- **Bird in hand** – flank, belly, back, wing spread, tail spread, head, feet/bands  
- **Free bird** – perched, flying, swimming, foraging/feeding, copulating, building nest, displaying, in nest, vocalizing, agonistic behavior  
- **Dead bird**  
- **Flock**  
- **Nest**  
- **Egg**  
- **Nestling**  
- **Ectoparasite**  
- **Footprint**  
- **Feather**  
- **Feces**  
- **Food**  
- **Environment**  
- **Fieldwork**  
- **Team**

These categories help standardize image classification and facilitate searches and reports.

### Coordinates precision

- **Exact** – Coordinates are precise, generally obtained using GPS or similar devices.  
- **Approximated** – Coordinates are less precise, usually estimated from maps.  
- **Reference coordinate** – Coordinates are very imprecise, representing a central point of a larger area (e.g., farmyard, municipality center).  

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

## Image viewer

The **Image viewer** allows you to preview and interact with images attached to records in Xolmis. It provides several options to adjust visualization, perform quick edits, and manage images directly from the system.

![Image viewer](img/image-viewer-screen.png)

### Available options

| Icon | Button | Function |
| --- | --- | --- |
| :material-magnify-plus: | Zoom in | Increases the zoom level of the image. |
| :material-magnify-minus: | Zoom out | Decreases the zoom level of the image. |
| :material-fit-to-screen: | Zoom to fit | Zoom level to fit image to window size. |
| :material-numeric-1-box: | Zoom to real size | Zoom level to image real size. |
| :material-image-edit: | Open in default editor | Opens the image in the default image editor of your operating system. |
| :material-rotate-right-variant: | Rotate right | Rotates the image clockwise. |
| :material-rotate-left-variant: | Rotate left | Rotates the image counterclockwise. |
| :material-flip-horizontal: | Flip horizontally | Flips the image horizontally. |
| :material-flip-vertical: | Flip vertically | Flips the image vertically. |
| :material-content-copy: | Copy to clipboard | Copies the image to the clipboard for use in other applications. |
| :material-content-save: | Save as | Saves the image to a chosen location with a new filename. |
| :material-chevron-right: | Next image | Advances to the next image in the record. |
| :material-chevron-left: | Previous image | Returns to the previous image in the record. |

### How it works

- The viewer opens when you select an image from a record.  
- You can navigate between multiple images attached to the same record using the **Next** and **Previous** buttons.  
- Quick actions like **Rotate** and **Flip** are applied only to the visualization, not to the stored file, unless you explicitly save the image again.  
- The **Open in default editor** option allows you to perform advanced edits using external software.  

## Best practices

- **Set the images location first**: Avoid broken links by configuring the storage path before adding files.  
- **Use descriptive subtitles**: Helps identify images quickly in lists and reports.  
- **Record metadata in the field**: Note taxon, locality, and context during image capture to minimize manual editing later.  
- **Respect licensing**: Always define license type and owner to ensure compliance with copyright.  
- **Link to taxa and locality**: Strengthens the ecological value of images by connecting them to species and habitats.  
- **Standardize image types**: Use the predefined categories to ensure consistency across datasets.  

## Relation to other modules

Images can be attached in several modules:

- **[Sightings](sightings.md)** – Document observed individuals.  
- **[Captures](captures.md)** – Record morphological details of captured birds.  
- **[Individuals](individuals.md)** – Associate images with specific banded birds.  
- **[Nests](nests.md) and [Eggs](eggs.md)** – Document breeding evidence and nest structures.  
- **[Surveys](surveys.md)** – Attach supporting photographs.  

By managing images in Xolmis, researchers integrate visual evidence into ornithological studies, enriching datasets with multimedia content and supporting long-term ecological monitoring.
