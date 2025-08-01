# Images

Some modules support attach images to records. Open the images panel clicking in the image icon :material-image: on the right toolbar, if present.

!!! danger

    Before adding any images, go to [Settings](settings.md) in the Media section and set the images location. The images added save the file path relative to this location. Changing the location afterwards can be problematic, at the moment. We are working on a solution.

## Adding images

You can add images clicking in the plus sign :material-plus-circle: at the top toolbar of the images panel. Then select all the images that you want to attach to the current record and click **Open**. The progress of adding the images is shown. Some image files have metadata, that is used to get some info about the image, such as date of creation and geographical coordinates. Other image info must be edited manually afterwards.

Alternatively, you can add images simply dragging the files from a file explorer and dropping on the images panel.

## Editing image info

To edit the image info, click in the pencil :material-pencil: at the top toolbar of the images panel.

Field | Required | Description
--- | --- | ---
Subtitle | | Description of the image
Author | | Person that created the image
Image date | Yes | Date of creation of the image
Image time | | Time of creation of the image
Image type | | Which is depicted in the image, see details below
Image file | Yes | Relative file path of the image
Locality | | Site where the image was created
Coordinates precision | | Precision of the geographical coordinates
Longitude | | X axis/longitude of the geographical coordinate
Latitude | | Y axis/latitude of the geographical coordinate
Taxon | | Taxon represented in the image
License type | | Type of image license
License year | | Year the image was licensed
License owner | | Image rights owner
License notes | | Notes about the licensing
License URL | | URL to the license text

### Image types

- Bird in hand - flank
- Bird in hand - belly
- Bird in hand - back
- Bird in hand - wing spread
- Bird in hand - tail spread
- Bird in hand - head
- Bird in hand - feet/bands
- Free bird - perched
- Free bird - flying
- Free bird - swimming
- Free bird - foraging/feeding
- Free bird - copulating
- Free bird - building nest
- Free bird - displaying
- Free bird - in nest
- Free bird - vocalizing
- Free bird - agonistic behavior
- Dead bird
- Flock
- Nest
- Egg
- Nestling
- Ectoparasite
- Footprint
- Feather
- Feces
- Food
- Environment
- Fieldwork
- Team

### Coordinates precision

- **Exact**: geographical coordinates are very precise, generally obtained using some device.
- **Approximated**: geographical coordinates are not precise, generally obtained using a map.
- **Reference coordinate**: geographical coordinates are very imprecise, generally using a central point for a bigger area, such as the farmyard of a farm or the urban area of a municipality.

### License types

- **Copyright**: all rights reserved.
- **CC BY**: Creative Commons with attribution.
- **CC BY-SA**: Creative Commons with attribution and derivatives with same license.
- **CC BY-ND**: Creative Commons with attribution and no derivatives.
- **CC BY-NC**: Creative Commons with attribution and no commercial use.
- **CC BY-NC-SA**: Creative Commons with attribution, no commercial use and derivatives with same license.
- **CC BY-NC-ND**: Creative Commons with attribution, no commercial use and no derivatives.
- **CC0**: public domain (Creative Commons).
- **Commercial**: license with customized contracts.
