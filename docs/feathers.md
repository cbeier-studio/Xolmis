# Feathers

The **Feathers** module is used to record samples of feathers collected from birds. Feather sampling is an important method in ornithology, as feathers can provide information about molt cycles, growth rates, diet, contaminants, and even genetic material. Each feather record is linked to a taxon and locality, and can also be grouped by individual in the **Individuals** module.

Open the Feathers module in the main menu: **Fieldwork → Feathers**. Alternatively, view feathers grouped by individual in the **[Individuals](individuals.md)** module.

## Adding or editing a feather record

When creating or editing a feather record, the following fields are available:

| Field | Required | Description |
| --- | --- | --- |
| **Date** | Yes | Date when the sampling occurred |
| **Time** |  | Time of the sampling |
| **Taxon** | Yes | Species from which the feather was sampled |
| **Locality** | Yes | Site where the feather was sampled (linked to Gazetteer) |
| **Observer** |  | Person who collected the feather |
| **Source** |  | Source of the sample (see details below) |
| **Symmetry** |  | Whether the molt/growth of the feather is symmetrical or asymmetrical |
| **Feather trait** |  | Feather type from which the sample was taken (see details below) |
| **Feather number** |  | For flight feathers, the number within the feather trait (e.g., primary 1, secondary 5) |
| **Side** |  | Body side from which the feather was sampled: right, left, or not applicable |
| **% grown** |  | Percentage of feather growth relative to its total length |
| **Length** |  | Feather length measurement, in millimeters |
| **Area** |  | Feather surface area measurement, in square millimeters |
| **Mass** |  | Feather mass measurement, in milligrams |
| **Rachis width** |  | Width of the rachis (central shaft), in millimeters |
| **Growth bar width** |  | Width of a pair of growth bars, in millimeters |
| **Barb density** |  | Density of barbs, measured in barbs per centimeter |
| **Feather age** |  | Age of the feather in relation to the molt cycle (see details below) |
| **Notes** |  | Any additional information about the feather |

### Source types

- **Unknown** – Source not determined  
- **Capture** – Feather collected during a capture event  
- **Sighting** – Feather collected during a field sighting (without capture)  
- **Photo** – Feather identified or documented from a photograph  

### Feather traits

Feathers can be sampled from different traits, each with specific biological meaning:

- **Body** – Contour feathers from the body  
- **Primary** – Primary flight feathers (outer wing)  
- **Secondary** – Secondary flight feathers (inner wing)  
- **Rectrix** – Tail feathers  
- **Primary covert** – Coverts overlying the primaries  
- **Greater covert** – Larger coverts overlying the secondaries  
- **Median covert** – Medium-sized coverts overlying the secondaries  
- **Lesser covert** – Smaller coverts overlying the secondaries  
- **Carpal covert** – Coverts near the carpal joint  
- **Alula** – Small feathers on the alula (thumb-like structure of the wing)  

### Feather age

Feather age relates to the molt cycle and helps determine the bird’s stage of development:

- **Unknown** – Age not determined  
- **Nestling** – Feather from a bird still in the nest  
- **Fledgling** – Feather from a bird that has recently left the nest  
- **Adult** – Feather from a fully mature bird  
- **First year** – Feather from a bird in its first calendar year  
- **Second year** – Feather from a bird in its second calendar year  
- **Third year** – Feather from a bird in its third calendar year  
- **Fourth year** – Feather from a bird in its fourth calendar year  
- **Fifth year** – Feather from a bird in its fifth calendar year  

## Adding a new batch

![New batch of feathers dialog](img/batch-feathers-dialog1.png)

The **Add new batch** dialog is used to insert multiple feather records at once, streamlining the process of documenting molt and growth data. Instead of adding feathers individually, you can group them into a batch linked to a specific sampling event.

| Field | Required | Description |
| --- | --- | --- |
| **Date** | Yes | Date when the sampling occurred. This defines the temporal context of the batch. |
| **Time** |  | Time of the sampling, useful for precise field documentation. |
| **Capture** |  | Capture event to which the feathers are related. Links the batch to a specific capture record. |
| **Sighting** |  | Sighting event to which the feathers are related. Links the batch to an observation record. |
| **Taxon** | Yes | Species from which the feathers were sampled. Must be selected from the taxonomy list. |
| **Locality** | Yes | Site where the feathers were sampled, linked to the Gazetteer for consistency. |
| **Observer** |  | Person who collected the feather sample. Helps track responsibility and provenance. |
| **Source** |  | Source of the sample (e.g., field collection, museum specimen, rehabilitation center). |
| **Symmetry** |  | Indicates whether feather molt/growth is symmetrical or asymmetrical. |
| **P1 to P10** |  | Percent growth of each primary feather (from P1 to P10). |
| **S1 to S9** |  | Percent growth of each secondary feather (from S1 to S9). |
| **R1 to R6** |  | Percent growth of each rectrix feather (from R1 to R6). |

For each flight feather with a percent growth value entered in the form, Xolmis automatically creates a corresponding feather record in the database. This streamlines the process of documenting molt progress for individual birds, making data entry faster and more consistent.

## Best practices

- **Record precise measurements**: Length, mass, and rachis width are critical for comparative studies.  
- **Note feather symmetry**: Asymmetrical growth can indicate stress or nutritional issues.  
- **Use feather numbers for flight feathers**: Helps identify molt patterns and growth sequences.  
- **Link feathers to individuals**: Whenever possible, associate feather samples with specific birds to track molt and health.  
- **Add detailed notes**: Document unusual characteristics such as damage, parasites, or pigmentation anomalies.  
- **Combine with other modules**: Feathers can be linked to captures, individuals, and projects, enriching the dataset.  

## Relation to other modules

The Feathers module is interconnected with other parts of Xolmis:

- **[Individuals](individuals.md)**: Feathers can be grouped by individual birds.  
- **[Captures](captures.md)**: Feathers collected during captures are recorded here.  
- **[Permits](permits.md)**: Feather sampling may require permits depending on local regulations.  

By maintaining detailed feather records, Xolmis supports studies on molt cycles, growth rates, and ecological conditions, providing valuable insights into bird biology.
