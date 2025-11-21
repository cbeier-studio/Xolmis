# Captures

The **Captures** module is used to record detailed information about birds captured during fieldwork. Each capture represents a specific event in which an individual bird was handled, measured, banded, or sampled. Captures are essential for demographic studies, health monitoring, and long-term tracking of individuals.

Open the Captures module in the main menu: **Fieldwork → Captures**. Captures can also be viewed grouped by individual in the **[Individuals](individuals.md)** module, allowing you to track the capture history of each bird.

## Adding or editing a capture

When creating or editing a capture record, the following fields are available:

| Field | Required | Description |
| --- | --- | --- |
| **Individual** | Yes | The individual bird to which the capture is linked |
| **Survey** |  | Sampling event in which the capture occurred |
| **Locality** | Yes | Site where the capture took place (linked to Gazetteer) |
| **Capture date** | Yes | Date of the capture |
| **Capture time** |  | Time of the capture |
| **Bander** | Yes | Person who banded the bird |
| **Annotator** | Yes | Person who recorded the capture data |
| **Type** | Yes | Type of capture (see details below) |
| **Mist net** |  | Mist net in which the capture occurred |
| **Longitude** |  | Longitude coordinate (decimal degrees) |
| **Latitude** |  | Latitude coordinate (decimal degrees) |
| **Taxon** |  | Taxon of the bird captured |
| **Band** |  | Band assigned to the bird |
| **Removed band** |  | Band that was replaced |
| **Right tarsus** |  | Bands and color combinations on the right tarsus (below the joint) |
| **Left tarsus** |  | Bands and color combinations on the left tarsus (below the joint) |
| **Age** |  | Age category of the bird (see below) |
| **Escaped** |  | Check if the bird escaped before completing measurements |
| **Status** |  | Status of the bird when released (see below) |
| **Cloacal protuberance** |  | Code for cloacal protuberance (see below) |
| **Brood patch** |  | Code for brood patch (see below) |
| **Subcutaneous fat** |  | Code for subcutaneous fat (see below) |
| **Body molt** |  | Code for body molt (see below) |
| **Flight feathers molt** |  | Code for flight feathers molt (see below) |
| **Flight feathers wear** |  | Code for flight feathers wear (see below) |
| **Right wing chord** |  | Measurement in millimeters |
| **First secondary chord** |  | Measurement in millimeters |
| **Tail length** |  | Measurement in millimeters |
| **Tarsus length** |  | Measurement in millimeters |
| **Tarsus diameter** |  | Measurement in millimeters |
| **Weight** |  | Bird weight in grams |
| **Skull length** |  | Measurement in millimeters |
| **Exposed culmen** |  | Measurement in millimeters |
| **Nostril to bill tip** |  | Measurement in millimeters |
| **Bill width** |  | Measurement in millimeters |
| **Bill height** |  | Measurement in millimeters |
| **Total length** |  | Measurement in millimeters |
| **Total culmen** |  | Measurement in millimeters |
| **Philornis larvae quantity** |  | Number of *Philornis* larvae parasitizing the bird |
| **Kipp's index** |  | Automatically calculated: right wing chord – first secondary chord |
| **Molt limits** |  | Codes for molt limits (see below) |
| **Skull ossification** |  | Code for skull ossification (see below) |
| **Molt cycle** |  | Code for molt cycle (see below) |
| **How was aged** |  | Codes for how the bird was aged (see below) |
| **Sex** |  | Sex of the bird: male, female, unknown |
| **How was sexed** |  | Codes for how the bird was sexed (see below) |
| **Notes** |  | Any additional information about the capture |
| **Blood** |  | Check if blood samples were collected |
| **Feather(s)** |  | Check if feathers were collected |
| **Feces** |  | Check if feces were collected |
| **Parasite(s)** |  | Check if parasites were collected |
| **Audio(s)** |  | Check if audio recordings were made |
| **Photos** |  | Check if the bird was photographed |
| **Claw** |  | Check if claw samples were collected |
| **Specimen (whole)** |  | Check if the bird was collected as a specimen |
| **Photographer 1–2** |  | People who photographed the bird |
| **Camera** |  | Camera used |
| **First photo number** |  | Sequential number of the first photo |
| **Last photo number** |  | Sequential number of the last photo |
| **Field number** |  | Identifier for the capture |
| **Hemoglobin** |  | Measurement in g/dL |
| **Hematocrit** |  | Measurement in mm³ |
| **Glucose** |  | Measurement in mg/dL |

## Capture types

- **New capture** – First time the bird is captured  
- **Recapture** – Bird captured again after a previous event  
- **Same day** – Bird recaptured on the same day  
- **Change band** – Band replaced during capture  
- **Unbanded** – Bird captured without a band  

## Age categories

- **Unknown** – Age not determined  
- **Adult** – Fully mature bird  
- **Juvenile** – Young bird, not yet fully mature  
- **Fledgling** – Bird that has recently left the nest but is still dependent  
- **Nestling** – Bird still in the nest, dependent on parents  
- **First year** – Bird in its first calendar year  
- **Second year** – Bird in its second calendar year  
- **Third year** – Bird in its third calendar year  
- **Fourth year** – Bird in its fourth calendar year  
- **Fifth year** – Bird in its fifth calendar year

## Status types

- **N** – Normal  
- **I** – Injured  
- **W** – Wing sprain  
- **X** – Stressed (not flying)  
- **D** – Dead  

## Morphological and physiological codes

### Cloacal protuberance

- U, N, S, M, L

### Brood patch

- F, N, V, W, O

### Subcutaneous fat

- N, T, L, H, S, B, G, V

### Body molt

- N, T, S, H, G, A, F

### Flight feathers molt

- N, S, A

### Flight feathers wear

- N, S, L, M, H, X

### Skull ossification

- N, T, L, H, G, A, F

## Molt limits

Codes used to describe molt limits observed during examination:

- N – No molt limits found  
- U – Undetermined  
- P – Primary flight feathers  
- S – Secondary flight feathers  
- D – Primary coverts  
- G – Greater coverts  
- V – Primaries vs. greater coverts  
- R – Rectrices  
- L – Lesser coverts  
- M – Median coverts  
- B – Body plumage  
- C – Carpal covert vs. alula covert/lesser alula  
- A – Alula covert vs. lesser alula  
- Y – Limits present but undetermined location  

## Molt cycle codes

### Cycle

- U – Unknown  
- D – Definitive  
- F – First cycle  
- S – Second cycle  
- T – Third cycle  
- 4 – Fourth cycle  
- 5 – Fifth cycle  

### Molt

- C – Not molting  
- P – Molting (pre)  
- A – After a given plumage  

### Plumage

- U – Unknown  
- J – Juvenal  
- S – Supplemental  
- F – Formative  
- B – Basic  
- A – Alternate  

## How was sexed or aged

### Physical differences

- B – Brood patch  
- C – Cloacal protuberance  
- @ – Egg in oviduct  
- E – Eye color  
- I – Mouth/bill color or striations (hummingbirds)  
- G – Gape  
- $ – Feet or legs  
- S – Skull ossification  
- Q – Measurements (details in notes)  
- Y – Symmetrical flight feather molt  

### Plumage characters

- K – Definitive basic plumage  
- A – Definitive alternate plumage  
- F – Formative plumage  
- J – Juvenal plumage  
- M – Molt limits  
- P – Plumage (sexual dimorphism)  
- L – Plumage color patch length/extent  

### Feather characters

- W – Feather wear  
- V – Feather shape  
- R – Prejuvenal molt  
- = – Fault bar alignment  
- \# – Growth bar alignment  

### Undetermined or remaining

- O – Other (behavior, copulation; add notes)  
- U – Undetermined after examination  
- X – Age/sex determination not attempted  
- Z – Less precise age (<95%) but high certainty  

## Best practices

- **Ensure accurate linkage**: Always link captures to the correct individual and locality to maintain data consistency across modules.  
- **Record complete measurements**: Even if some values are approximate, recording them helps build robust datasets for population studies.  
- **Use standardized codes**: Apply the provided codes for molt, fat, ossification, and other attributes to ensure comparability with other datasets.  
- **Document escapes**: Mark birds that escaped before measurements were completed, so data gaps are properly flagged.  
- **Update band information**: If a band is replaced, record the removed band and the change date to preserve traceability.  
- **Add notes**: Use the notes field to capture unusual observations, behaviors, or conditions not covered by standard fields.  
- **Collect samples responsibly**: When blood, feathers, or other samples are taken, ensure proper handling and ethical compliance.  
- **Photographic records**: Always record photo numbers and photographer details to maintain a clear link between images and capture events.  
- **Check status codes**: Record the bird’s condition at release (normal, injured, stressed, etc.) to support health monitoring.  
- **Consistency across surveys**: Apply the same measurement protocols in all captures to avoid bias in long-term datasets.

## Relation to other modules

The Captures module is closely integrated with other parts of Xolmis:

- **[Individuals](individuals.md)**: Each capture is linked to a registered individual, allowing longitudinal tracking.  
- **[Bands](bands.md)**: Banding information is updated during captures, including new bands, replacements, or removals.  
- **[Gazetteer](gazetteer.md)**: Locality data ensures captures are tied to precise geographic sites.  
- **[Surveys](surveys.md) and [sampling plots](sampling-plots.md)**: Captures are part of broader sampling events, enabling ecological and statistical analyses.  
- **[Specimens](specimens.md)**: Biological material collected during captures (blood, feathers, parasites) is stored and managed in related modules.  
- **[Projects](projects.md) and [permits](permits.md)**: Captures may be associated with specific research projects and require proper permits.

By maintaining detailed and standardized capture records, Xolmis supports reliable ornithological research, enabling comparisons across time, sites, and species.
