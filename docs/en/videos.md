# Videos

The **Videos** feature allows researchers to attach video recordings to records in selected modules. Videos are valuable for documenting bird behavior, habitat conditions, fieldwork activities, or any other dynamic evidence that complements textual and photographic data. By linking videos directly to records, Xolmis ensures that multimedia information is organized, traceable, and available for future analysis.

Open the videos side panel by clicking the video camera icon :material-video: in the right toolbar (if available in the module).

!!! danger
    Before adding any recordings, go to [Settings](settings.md) in the **Media** section and set the **video files location**. Videos are saved with a file path relative to this location. Changing the location afterwards can cause problems in accessing files.

    {==A solution for dynamic relocation is under development.==}

## Toolbar

Icon | Button | Function
--- | --- | ---
:material-plus-circle: | Add video | Insert new videos from files
:material-pencil: | View and edit video info | Edit the selected video info
:material-play: | Play video | Opens the selected video file in the default video player
:material-delete: | Delete video | Delete the selected video

## Adding videos

You can add videos in two ways:

1. **Using the add button**  
      - Click the **Add** button :material-plus-circle: in the toolbar of the videos side panel.  
      - Select one or more video files to attach to the current record.  
      - Click **Open** to confirm.  
      - The system will show the progress of adding the recordings.  

2. **Drag and drop**  
      - Drag video files from your file explorer.  
      - Drop them directly into the videos side panel.  
      - The system will show the progress of adding the recordings.  

3. **Edit metadata**
      - If metadata is present (e.g., creation date, GPS coordinates), it will be automatically extracted.
      - Link the videos to records using the dialog that opens.
      - Other information must be edited manually afterwards.

This flexibility allows quick integration of field recordings into the database.

## Editing video info

To edit video information:

1. Select the video in the side panel.  
2. Click the **Edit** button :material-pencil: in the toolbar.  
3. A dialog will open with editable fields.  

| Field | Required | Description |
| --- | --- | --- |
| **Subtitle** |  | Short description of the recording |
| **Author** |  | Person who created the recording |
| **Recording date** | Yes | Date when the recording was made |
| **Recording time** |  | Time when the recording was made |
| **Recording type** |  | Type of recording (see below) |
| **Recording file** | Yes | Relative file path of the recording |
| **Locality** |  | Site where the recording was made |
| **Longitude** |  | Longitude coordinate of the recording |
| **Latitude** |  | Latitude coordinate of the recording |
| **Taxon** |  | Taxon represented in the recording |
| **Camera model** |  | Model of the camera used |
| **Context of the recording** |  | Behavior or activity of the individuals recorded |
| **Distance (m)** |  | Distance from the individuals to the camera |
| **License type** |  | Type of license applied to the recording |
| **License year** |  | Year of the license |
| **License owner** |  | Rights holder of the recording |
| **License notes** |  | Additional notes about licensing |
| **License URL** |  | Link to the license text |

### Recording types

Recording types classify the nature of the video captured. Examples include:

- **Behavioral** – Displays specific behaviors such as foraging, vocalizing, mating, or nest building.  
- **Environmental** – Shows habitat conditions, vegetation, or weather context.  
- **Fieldwork** – Documents research activities, mist net setup, or team operations.  
- **Morphological** – Focuses on physical traits of individuals (e.g., plumage, banding process).  
- **Nest monitoring** – Records nest activity, eggs, or nestlings.  
- **Other** – Any video not covered by the categories above.  

### License types

Licensing ensures proper use and sharing of videos:

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

- **Set video location first**: Avoid broken links by configuring the storage path before adding files.  
- **Use descriptive subtitles**: Helps identify recordings quickly in lists and reports.  
- **Record metadata in the field**: Note taxon, locality, and context during recording to minimize manual editing later.  
- **Respect licensing**: Always define license type and owner to ensure compliance with copyright.  
- **Link to taxa and locality**: Strengthens the ecological value of videos by connecting them to species and habitats.  
- **Keep file sizes manageable**: Large video files may slow down performance; compress when possible.  

## Relation to other modules

Videos can be attached in several modules:

- **[Sightings](sightings.md)** – Document observed behaviors or environmental context.  
- **[Captures](captures.md)** – Record handling procedures or morphological details.  
- **[Individuals](individuals.md)** – Associate videos with specific banded birds.  
- **[Nests](nests.md)** – Monitor breeding activity and nestling development.

By managing videos in Xolmis, researchers integrate dynamic evidence into ornithological studies, enriching datasets with multimedia content and supporting long-term ecological monitoring.
