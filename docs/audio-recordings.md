# Audio recordings

Some modules support attach audio recordings to records. Open the audio recordings side panel clicking in the microphone icon :material-microphone: on the right toolbar, if present.

!!! danger

    Before adding any recordings, go to [Settings](settings.md) in the Media section and set the audio files location. The recordings added save the file path relative to this location. Changing the location afterwards can be problematic, at the moment. We are working on a solution.

## Adding audio recordings

You can add audio recordings clicking in the add button :material-plus-circle: at the toolbar of the audio recordings side panel. Then select all the files that you want to attach to the current record and click **Open**. The progress of adding the recordings is shown. Some audio files have metadata, that is used to get some info about the recording, such as date of creation. Other audio recording info must be edited manually afterwards.

Alternatively, you can add audio recordings simply dragging the files from a file explorer and dropping on the audio recordings side panel.

## Editing audio recording info

To edit the audio recording info, click in the edit button :material-pencil: at the toolbar of the audio recordings side panel.

Field | Required | Description
--- | --- | ---
Subtitle | | Description of the recording
Author | | Person that created the recording
Recording date | Yes | Date of creation of the recording
Recording time | | Time of creation of the recording
Recording type | | Which was recorded, see details below
Recording file | Yes | Relative file path of the recording
Locality | | Site where the recording was created
Longitude | | X axis/longitude of the geographical coordinate
Latitude | | Y axis/latitude of the geographical coordinate
Taxon | | Taxon represented in the recording
Recorder model | | Model of the recorder used
Microphone model | | Model of the microphone used
Filter model | | Model of the filter used
Context of the recording | | What the individuals recorded were doing
\# individuals | | Number of individuals recorded
Distance (m) | | Distance of the individuals while recording, in meters
Temperature | | Temperature when recorded
Cloud cover | | Percent of cloud cover when recorded
Precipitation | | Precipitation when recorded: none, fog, mist, drizzle, rain
Wind speed (bft) | | Wind speed when recorded, in Beaufort scale
Relative humidity | | Percent of humidity in the air when recorded
Playback was used | | Check if a playback was used before recording
License type | | Type of recording license
License year | | Year the recording was licensed
License owner | | Recording rights owner
License notes | | Notes about the licensing
License URL | | URL to the license text

### Recording types
