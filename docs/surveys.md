# Surveys

Open the Surveys module in the main menu **Sampling → Surveys**.

## Adding or editing

Field | Required | Description
--- | --- | ---
Expedition | | Expedition to what the survey is linked
Survey date | Yes | Date of sampling
Duration | | Duration of sampling, in minutes
Starting time | | Time when the sampling was started
Ending time | | Time when the sampling has ended
Method | Yes | Which method was used for sampling
Locality | Yes | In which locality the sampling occurred
Mist net station | | Station used if the sampling method was banding
Project | | Project to which the sampling is associated
Longitude (initial) | | X axis/longitude of the (initial) geographical coordinate
Latitude (initial) | | Y axis/latitude of the (initial) geographical coordinate
End longitude | | X axis/longitude of the ending geographical coordinate
End latitude | | Y axis/latitude of the ending geographical coordinate
Number of observers | | How many observers or banders were sampling
Sample identifier | | Identifier for point, transect, list, etc.
Area | | Total area sampled, in hectares
Distance | | Total distance sampled, in kilometers
Number of mist nets | | Total number of mist nets used for banding
Mist net effort | | Total effort of capture using mist nets (automatic)
Environment description | | Brief description of the surroundings
Mist net checking times | | Times of each event of mist net checking
Notes | | Any additional info about the sampling

## Survey members

Field | Required | Description
--- | --- | ---
Researcher | Yes | Person that participate in the sampling
Visitor | | If the person is not part of the team

## Nets

Field | Required | Description
--- | --- | ---
Permanent net | | Mist net from which get the number and coordinates
Mist net number | Yes | Identifier of the mist net
Longitude | | X axis/longitude of the geographical coordinate
Latitude | | Y axis/latitude of the geographical coordinate
Mist net length | | Length of the opened mist net, in meters
Mist net height | | Height of the mist net, in meters
Mesh size | | Size of the mesh, in millimeters
Mist net area | | Total area of the opened mist net (automatic)
Survey date | Yes | Date when the mist net was open
Total open time | | Total time in which the mist net was open (automatic)
Opening time 1-4 | Yes | Time when the mist net was opened
Closing time 1-4 | Yes | Time when the mist net was closed
Notes | | Any other info about the mist net

## Weather log

Field | Required | Description
--- | --- | ---
Date | Yes | Date of the weather assessment
Time | | Time of the weather assessment
Moment | Yes | Moment of the sampling when the weather was assessed: start, middle, end
Cloud cover | | Percentage of sky covered by clouds
Temperature | | Temperature in Celsius degrees
Precipitation | | Type of precipitation: None, Fog, Mist, Drizzle, Rain
Accumulated rainfall | | In millimeters
Wind speed (bft) | | Wind speed, in Beaufort scale
Wind speed (km/h) | | Wind speed, in km/h
Relative humidity | | Percentage of humidity in the air
Atmospheric pressure | | In millipascal (mPa)
Notes | | Any other info about the weather

## Vegetation

Field | Required | Description
--- | --- | ---
Date | Yes | Date of the vegetation sampling
Time | | Time of the vegetation sampling
Longitude | | X axis/longitude of the geographical coordinate
Latitude | | Y axis/latitude of the geographical coordinate
Distribution (herbs) | Yes | Type of distribution, see below for details
Proportion (herbs) | | Proportion of this vegetation stratum
Average height (herbs) | | Average height of this vegetation stratum, in centimeters
Distribution (shrubs) | Yes | Type of distribution, see below for details
Proportion (shrubs) | | Proportion of this vegetation stratum
Average height (shrubs) | | Average height of this vegetation stratum, in centimeters
Distribution (trees) | Yes | Type of distribution, see below for details
Proportion (trees) | | Proportion of this vegetation stratum
Average height (trees) | | Average height of this vegetation stratum, in centimeters
Notes | | Any other information about the vegetation

### Distribution types

- (0) None
- (1) Rare
- (2) Few sparse individuals
- (3) Only one patch
- (4) Only one patch and some isolated individuals
- (5) Many sparse individuals
- (6) Only one patch and many isolated individuals
- (7) Few patches
- (8) Few patches and isolated individuals
- (9) Many patches evenly distributed
- (10) Many patches evenly distributed with sparse individuals
- (11) Evenly distributed isolated individuals in high density
- (12) Continuous cover with some gaps in evaluated stratum
- (13) Continuous and dense cover
- (14) Continuous and dense cover with clear edge with other stratum
