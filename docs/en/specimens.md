# Specimens

A **specimen** represents a collected biological or ecological sample, which can take many forms. Specimen data are fundamental for linking field records to physical evidence and for depositing samples in scientific collections maintained by museums, universities, and research institutions. By documenting specimens in Xolmis, researchers ensure that samples are traceable, standardized, and available for future studies.

The **Specimens** module allows you to record specimens and the preparations derived from them. To access the module, go to **Fieldwork → Specimens**.

## Adding or editing specimens

To add a new specimen:

- Click the add button :material-plus-circle: in the module toolbar.  
- Alternatively, click the purple plus sign :material-plus-circle: at the top right of the window and select **New specimen**.  
- You can also add specimens in batches using [Quick Entry](adding-and-editing-data.md#quick-entry).  

To edit a specimen:

- Select it in the grid and click the edit button :material-pencil: in the toolbar.  
- Or double-click the record, right-click and select **Edit**, or use the shortcut ++ctrl+e++.  

### Specimen fields

| Field | Required | Description |
| --- | --- | --- |
| **Field number** | Yes | Unique code used to identify the sample in the field |
| **Type** | Yes | Type of sample collected: Whole carcass, Partial carcass, Nest, Bones, Egg, Parasites, Feathers, Blood, Claw, Swab, Tissues, Feces, Regurgitate |
| **Collection date** | Yes | Date when the sample was collected |
| **Locality** | Yes | Site where the sample was collected |
| **Longitude** |  | Longitude coordinate of the collection site |
| **Latitude** |  | Latitude coordinate of the collection site |
| **Taxon** | Yes | Taxon to which the sample pertains |
| **Individual** |  | Individual bird linked to the sample |
| **Nest** |  | Nest linked to the sample |
| **Egg** |  | Egg linked to the sample |
| **Notes** |  | Any additional information about the sample |

## Collectors

A **collector** is a researcher who collected the specimen.  
A specimen can have multiple collectors associated with it.

To add a collector:

- Click the add button :material-plus-circle: in the related navigation bar.  
- Use [Quick Entry](adding-and-editing-data.md#quick-entry) for batch additions.  

To edit a collector:

- Click the edit button :material-pencil: in the toolbar.  
- Or double-click the collector, right-click and select **Edit**.  

### Collector fields

| Field | Required | Description |
| --- | --- | --- |
| **Collector** | Yes | Person who collected the specimen |

## Sample preparations

Specimens often undergo **preparations and procedures**, resulting in new data or derivative samples. Preparations document how the specimen was processed and preserved, ensuring traceability in scientific collections.

To add a preparation:

- Click the add button :material-plus-circle: in the related navigation bar.  
- Use [Quick Entry](adding-and-editing-data.md#quick-entry) for batch additions.  

To edit a preparation:

- Click the edit button :material-pencil: in the toolbar.  
- Or double-click the preparation, right-click and select **Edit**.  

### Preparation fields

| Field | Required | Description |
| --- | --- | --- |
| **Accession number** | Yes | Accession number assigned after preparation |
| **Duplicate/part number** |  | Sequential duplicate number for the accession |
| **Type** | Yes | Type of preparation (see list below) |
| **Preparation date** |  | Date when the preparation was made |
| **Preparer** |  | Person who prepared the sample |
| **Notes** |  | Any additional information about the preparation |

### Preparation types

- **Skin (standard)** – Scientific taxidermy keeping the bill with the skin.  
- **Skin (shmoo)** – Scientific taxidermy removing the whole skull.  
- **Skin (mounted)** – Mounted taxidermy, depicting the specimen in a lifelike posture.  
- **Wing open** – Detached open wing.  
- **Skeleton (whole)** – Complete skeleton.  
- **Skeleton (partial)** – Incomplete skeleton.  
- **Nest** – Whole nest preserved.  
- **Egg** – Eggshell preserved.  
- **Parasites** – Parasites removed from the specimen.  
- **Feathers** – Feathers removed from the specimen.  
- **Blood (dry)** – Blood sample dried on filter paper.  
- **Blood (wet)** – Blood sample stored in alcohol or other liquid.  
- **Blood (smear)** – Blood smeared on a microscope slide.  
- **Sexing** – Result of sex determination from a blood sample.  
- **Genetic sequencing** – Result of DNA sequencing.  
- **Microbial culture** – Culture derived from swab samples.  
- **Tissues** – Tissue samples from organs.  
- **Eyes** – Whole eyes preserved.  
- **Tongue** – Whole tongue preserved.  
- **Syrinx** – Whole syrinx preserved.  
- **Gonads** – Whole gonads preserved.  
- **Stomach** – Whole stomach and its contents preserved.  

## Best practices

- **Use consistent field numbers**: Ensure each specimen has a unique identifier.  
- **Record precise locality and coordinates**: Essential for ecological and biogeographical studies.  
- **Link specimens to individuals, nests, or eggs**: Strengthens traceability across modules.  
- **Document collectors and preparers**: Provides accountability and historical context.  
- **Specify preparation types clearly**: Helps future researchers understand how the specimen was processed.  
- **Add detailed notes**: Record unusual conditions, preservation methods, or contextual information.  

## Relation to other modules

Specimens are interconnected with several parts of Xolmis:

- **[Surveys](surveys.md)** – Specimens are linked to survey events.  
- **[Captures](captures.md)** – Specimens may originate from capture records.  
- **[Individuals](individuals.md)** – Specimens can be tied to specific banded birds.  
- **[Nests](nests.md) and [Eggs](eggs.md)** – Samples may derive from breeding records.  

By managing specimens in Xolmis, researchers ensure that collected samples are **traceable, standardized, and scientifically valuable**, supporting long-term ornithological and ecological research.
