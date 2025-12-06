# Importing Banding Data

![Import banding data dialog](img/import-banding-data.png)

Banding data can be imported in **CSV** format.  
Each CSV file must follow a **predefined schema** to ensure that the data can be validated and correctly integrated into the system.

## Generate files

You can generate empty CSV files with the right schema to fill with data.

1. Select in the main menu **File → Import → Banding Data**.
2. Click the **Generate files** button.
3. Select the folder and inform a file name, click the **Save** button.
4. The files will be created in the selected folder. If the option *Open file after export* is enabled, the files will be opened in the default application.

## How to import

1. Select **File → Import → Banding Data**.  
2. The dialog will open, allowing you to choose one or more files for **Field journal**, **Field efforts**, and **Captures**.  
   - There are **three types of banding data** available for import.  
   - You must select at least one file, but you may select all three.  
3. Once the import begins, the dialog will display progress and results.

This feature ensures that banding records collected externally can be integrated into the **Surveys**, **Net efforts**, **Individuals**, and **Captures** modules.

## CSV Schemas

Below are the outlines of the three CSV schemas.

### 1. Field journal schema

| Column | Description |
| --- | --- |
| **LOCALITY** | study site name |
| **NET STATION** | banding station name |
| **SAMPLING DATE** | date of fieldwork |
| **START TIME** | time when the first net was opened |
| **END TIME** | time when the last net was closed |
| **LONGITUDE** | in decimal degrees |
| **LATITUDE** | in decimal degrees |
| **TEAM** | abbreviations of researchers comma-separated |
| **NOTES** | any observations or additional information |
| **WEATHER TIME 1** | time when the weather was accessed the first time |
| **WEATHER MOMENT 1** | moment of sampling: S (Start), M (Middle), E (End) |
| **CLOUD COVER 1** | proportion of sky covered by clouds, in percentage |
| **PRECIPITATION 1** | type of precipitation: N (None), F (Fog), M (Mist), D (Drizzle), R (Rain) |
| **TEMPERATURE 1** | in Celsius degrees |
| **WIND SPEED 1** | in Beaufort scale |
| **HUMIDITY 1** | relative humidity, in percentage |
| **WEATHER TIME 2** | time when the weather was accessed the second time |
| **WEATHER MOMENT 2** | moment of sampling: S (Start), M (Middle), E (End) |
| **CLOUD COVER 2** | proportion of sky covered by clouds, in percentage |
| **PRECIPITATION 2** | type of precipitation: N (None), F (Fog), M (Mist), D (Drizzle), R (Rain) |
| **TEMPERATURE 2** | in Celsius degrees |
| **WIND SPEED 2** | in Beaufort scale |
| **HUMIDITY 2** | relative humidity, in percentage |
| **WEATHER TIME 3** | time when the weather was accessed the third time |
| **WEATHER MOMENT 3** | moment of sampling: S (Start), M (Middle), E (End) |
| **CLOUD COVER 3** | proportion of sky covered by clouds, in percentage |
| **PRECIPITATION 3** | type of precipitation: N (None), F (Fog), M (Mist), D (Drizzle), R (Rain) |
| **TEMPERATURE 3** | in Celsius degrees |
| **WIND SPEED 3** | in Beaufort scale |
| **HUMIDITY 3** | relative humidity, in percentage |
| **WEATHER TIME 4** | time when the weather was accessed the fourth time |
| **WEATHER MOMENT 4** | moment of sampling: S (Start), M (Middle), E (End) |
| **CLOUD COVER 4** | proportion of sky covered by clouds, in percentage |
| **PRECIPITATION 4** | type of precipitation: N (None), F (Fog), M (Mist), D (Drizzle), R (Rain) |
| **TEMPERATURE 4** | in Celsius degrees |
| **WIND SPEED 4** | in Beaufort scale |
| **HUMIDITY 4** | relative humidity, in percentage |

### 2. Field efforts schema

| Column | Description |
| --- | --- |
| **LOCALITY** | study site name |
| **NET STATION** | banding station name |
| **SAMPLING DATE** | date of effort |
| **NET NUMBER** | number of the mist net in the field |
| **LONGITUDE** | in decimal degrees |
| **LATITUDE** | in decimal degrees |
| **OPEN TIME 1** | time when the net was opened the first time |
| **CLOSE TIME 1** | time when the net was closed the first time |
| **OPEN TIME 2** | time when the net was opened the second time |
| **CLOSE TIME 2** | time when the net was closed the second time |
| **OPEN TIME 3** | time when the net was opened the third time |
| **CLOSE TIME 3** | time when the net was closed the third time |
| **OPEN TIME 4** | time when the net was opened the fourth time |
| **CLOSE TIME 4** | time when the net was closed the fourth time |
| **NOTES** | any additional information about the net |

### 3. Captures schema

| Column | Description |
| --- | --- |
| **LOCALITY** | study site name |
| **STATION** | banding station name |
| **DATA** | banding date |
| **RECORDER** | abbreviation of researcher which was writing down the data |
| **BANDER** | person responsible for banding |
| **CAP TIME** | time of capture |
| **NET SITE NAME** | net number or name |
| **NEW_RECAP** | nature of capture (new capture, recapture, same day, etc.) |
| **BAND_CODE** | alphabetic code of band size (one letter) |
| **BAND NUMBER** | unique band number for the band size |
| **RIGHT LEG** | bands combination in the right leg |
| **LEFT LEG** | bands combination in the left leg |
| **SPECIES NAME** | scientific name |
| **CP** | cloacal protuberance code |
| **BP** | brood patch code |
| **FAT** | subcutaneous fat code |
| **BODY MOLT** | body molt code |
| **FF MOLT** | flight feathers molt code |
| **FF WEAR** | flight feathers wear code |
| **RIGHT WING** | length of right wing chord, in millimeters |
| **FIRST SECONDARY** | length of right wing first secondary chord, in millimeters |
| **TAIL** | length of tail, in millimeters |
| **TARSUS LENGTH** | in millimeters |
| **RIGHT TARSUS DIAMETER** | in millimeters |
| **WEIGHT** | in grams |
| **MOLT LIMITS** | codes for each molt limit |
| **SKULL** | skull ossification code |
| **CYCLE CODE** | molt cycle code |
| **HOW AGED** | code of how the bird was aged |
| **SEX** | sex code |
| **HOW SEXED** | code of how the bird was sexed |
| **STATUS** | status code of the bird at release |
| **ESCAPED** | if the bird escaped without completing the measurements |
| **NOTES** | any additional information about the bird |
| **REMOVED BAND** | code and number of the removed band if it was replaced |
| **PHOTOGRAPHER** | slash-separated abbreviations of researchers that photographed the bird |
| **INITIAL PHOTO NUMBER** | number of the first photo filename for this bird |
| **FINAL PHOTO NUMBER** | number of the last photo filename for this bird |
| **CAMERA NAME** | identification of the camera used |
| **PHOTO NAME FORMULA** | standardized filename to rename photo files |
| **CRANIO** | length of skull, in millimeters |
| **CULMEN EXPOSTO** | length of exposed culmen, in millimeters |
| **NP** | distance of nostril to bill tip, in millimeters |
| **LARGURA BICO** | bill width, in millimeters |
| **ALTURA BICO** | bill height, in millimeters |
| **SANGUE** | if a blood sample was collected |
| **PENAS** | if a feather sample was collected |
| **LONGITUDE** | in decimal degrees |
| **LATITUDE** | in decimal degrees |
| **KIPPS** | Kipp's distance, in millimeters |
| **GLICOSE** | glucose measurement |
| **HEMOGLOBINA** | hemoglobin measurement |
| **HEMATOCRITO** | hematocrit measurement |
| **GPS NUMBER** | number of the GPS tracking device |