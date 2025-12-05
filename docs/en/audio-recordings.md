# Audio recordings

The **Audio recordings** feature allows researchers to attach sound files to records in selected modules. This is particularly useful for documenting bird vocalizations, environmental sounds, or other acoustic evidence that complements field observations. By linking audio recordings directly to taxa, sightings, or captures, Xolmis ensures that multimedia data is integrated and traceable.

Open the audio recordings side panel by clicking the microphone icon :material-microphone: on the right toolbar (if available in the module).

!!! danger
    Before adding any recordings, go to [Settings](settings.md) in the **Media** section and set the audio files location. Recordings are saved with a file path relative to this location. Changing the location afterwards can cause problems in accessing files.

    {==A solution for dynamic relocation is under development.==}

## Toolbar

| Icon | Button | Function |
| --- | --- | --- |
| :material-plus-circle: | Add recording | Insert new audio recordings from files |
| :material-pencil: | View and edit recording info | Edit the selected recording info |
| :material-play: | Play recording | Opens the selected recording file in the default audio player |
| :material-delete: | Delete recording | Delete the selected audio recording |

## Adding audio recordings

You can add audio recordings in two ways:

1. **Using the add button**  
      - Click the **Add** button :material-plus-circle: in the toolbar of the audio recordings side panel.  
      - Select one or more files to attach to the current record.  
      - Click **Open** to confirm.  
      - The system will display the progress of adding the recordings.  
2. **Drag and drop**  
      - Drag audio files from your file explorer.  
      - Drop them directly into the audio recordings side panel.  
      - The system will display the progress of adding the recordings.  

- **Edit metadata**
      - If metadata is present (e.g., creation date), it will be automatically extracted.
      - Link the audio recordings to records using the dialog that opened.
      - Other information must be edited manually afterwards.

This flexibility allows quick integration of field recordings into the database.

## Editing audio recording info

To edit the information of an audio recording:

1. Select the recording in the side panel.  
2. Click the **Edit** button :material-pencil: in the toolbar.  
3. A dialog will open with editable fields.  

| Field | Required | Description |
| --- | --- | --- |
| **Subtitle** |  | Short description of the recording |
| **Author** |  | Person who created the recording |
| **Recording date** | Yes | Date when the recording was made |
| **Recording time** |  | Time when the recording was made |
| **Recording type** |  | Type of sound recorded (see below) |
| **Recording file** | Yes | Relative file path of the recording |
| **Locality** |  | Site where the recording was made |
| **Longitude** |  | Longitude coordinate of the recording |
| **Latitude** |  | Latitude coordinate of the recording |
| **Taxon** |  | Taxon represented in the recording |
| **Recorder model** |  | Model of the recording device |
| **Microphone model** |  | Model of the microphone used |
| **Filter model** |  | Model of the filter used |
| **Context of the recording** |  | Behavior or activity of the individuals recorded |
| **# individuals** |  | Number of individuals recorded |
| **Distance (m)** |  | Distance from the individuals to the microphone |
| **Temperature** |  | Ambient temperature during recording |
| **Cloud cover** |  | Percentage of cloud cover |
| **Precipitation** |  | Weather conditions: none, fog, mist, drizzle, rain |
| **Wind speed (bft)** |  | Wind speed in Beaufort scale |
| **Relative humidity** |  | Air humidity percentage |
| **Playback was used** |  | Check if playback was used before recording |
| **License type** |  | Type of license applied to the recording |
| **License year** |  | Year of the license |
| **License owner** |  | Rights holder of the recording |
| **License notes** |  | Additional notes about licensing |
| **License URL** |  | Link to the license text |

## Recording types

Recording types classify the nature of the audio captured. Examples include:

- **Song** – Full vocalization of a bird.  
- **Call** – Shorter communication sounds.  
- **Alarm** – Distress or warning calls.  
- **Flight call** – Vocalizations during flight.  
- **Duet** – Coordinated vocalizations between individuals.  
- **Environmental** – Background sounds or habitat acoustics.  
- **Other** – Any sound not covered by the categories above.  

### License types

Licensing ensures proper use and sharing of audio recordings:

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

- **Set media location first**: Avoid issues by configuring the audio storage path before adding files.  
- **Use descriptive subtitles**: Helps quickly identify recordings in lists and reports.  
- **Record metadata in the field**: Note taxon, locality, and context during recording to minimize manual editing later.  
- **Standardize equipment info**: Document recorder and microphone models for reproducibility.  
- **Respect licensing**: Always define license type and owner to ensure proper usage and sharing.  
- **Link to taxa and locality**: Strengthens the ecological value of recordings by connecting them to species and habitats.  

## Relation to other modules

Audio recordings can be attached in several modules:

- **[Sightings](sightings.md)** – Link vocalizations to observed individuals.  
- **[Individuals](individuals.md)** – Associate recordings with specific banded birds.  

By managing audio recordings in Xolmis, researchers integrate acoustic data into ornithological studies, enriching datasets with multimedia evidence and supporting long-term ecological monitoring.
