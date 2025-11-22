# Nests

The **Nests** module is used to record and manage information about bird nests. It allows researchers to document nest characteristics, productivity, supporting structures, and breeding outcomes. By maintaining detailed nest records, Xolmis supports studies on breeding biology, reproductive success, and habitat preferences.

Open the Nests module in the main menu: **Fieldwork → Nests**.

## Adding or editing a nest

When creating or editing a nest record, the following fields are available:

| Field | Required | Description |
| --- | --- | --- |
| **Taxon** | Yes | Taxon of the nest owner (species or subspecies) |
| **Field number** | Yes | Identifier of the nest, usually assigned in the field |
| **Fate** |  | Outcome of the nest (see details below) |
| **Nest encounter date** |  | Date when the nest was first found |
| **Last date** |  | Last date when the nest was observed active |
| **Project** |  | Project to which the nest is linked |
| **Observer** | Yes | Person who found and monitored the nest |
| **Locality** | Yes | Site where the nest is located (linked to Gazetteer) |
| **Longitude** |  | Longitude coordinate of the nest |
| **Latitude** |  | Latitude coordinate of the nest |
| **Nest description** |  | Brief description of the nest (materials, placement, etc.) |
| **Productivity** |  | Number of fledglings that successfully left the nest |
| **Nest shape** |  | Shape of the nest (see details below) |
| **Support type** |  | Structure or substrate where the nest was built |
| **Height at ground level** |  | Nest height above ground, in centimeters |
| **Support plant 1–2** |  | Plant species used as support for the nest |
| **Plant height** |  | Height of the support plant, in centimeters |
| **Stem thickness** |  | Thickness of the supporting stem, in centimeters |
| **Greater plant diameter** |  | Maximum diameter of the support plant |
| **Lesser plant diameter** |  | Minimum diameter of the support plant |
| **Days building** |  | Number of days spent building the nest |
| **Days incubating** |  | Number of days eggs were incubated |
| **Nestling-days** |  | Number of days nestlings remained in the nest |
| **Total active-days** |  | Total number of days the nest was active |
| **Lesser internal diameter** |  | Minimum internal diameter of the nest |
| **Greater internal diameter** |  | Maximum internal diameter of the nest |
| **Lesser external diameter** |  | Minimum external diameter of the nest |
| **Greater external diameter** |  | Maximum external diameter of the nest |
| **Internal height** |  | Internal height of the nest |
| **External height** |  | External height of the nest |
| **Distance from plant edge** |  | Position of the nest relative to the edge of the support plant |
| **Distance from plant center** |  | Position of the nest relative to the center of the support plant |
| **Cover** |  | Percentage of cover above the nest (stems, branches, leaves) |
| **Notes** |  | Any additional information about the nest |

### Nest fates

- **Unknown** – Fate not determined.  
- **Lost** – Nest failed (predation, abandonment, destruction).  
- **Success** – Nest succeeded (fledglings left the nest).  

### Nest shapes

- **Scrape** – Simple depression in the ground.  
- **Cup** – Typical open cup-shaped nest.  
- **Plate** – Flat nest structure.  
- **Sphere** – Enclosed spherical nest.  
- **Pendent** – Hanging nest.  
- **Platform** – Large flat nest, often on elevated structures.  
- **Mound** – Nest built as a mound of material.  
- **Burrow** – Nest inside a burrow or tunnel.  
- **Cavity** – Nest inside a cavity (tree hole, rock crevice, etc.).  

### Support types

- **Ground** – Nest placed directly on the ground.  
- **Herb/bush** – Nest supported by herbaceous plants or bushes.  
- **Branch/fork** – Nest placed on a branch or fork of a tree.  
- **Leaves** – Nest supported by leaves.  
- **Ledge** – Nest placed on a ledge.  
- **Rock/cliff** – Nest built on rocks or cliffs.  
- **Ravine** – Nest located in ravines.  
- **Nest box** – Artificial nest box.  
- **Anthropic** – Human-made structures (buildings, poles, etc.).  
- **Other** – Any other support type not listed.  

## Nest owners

Nest owners are the individuals associated with the nest.

| Field | Required | Description |
| --- | --- | --- |
| **Role** | Yes | Role of the individual at the nest (see details below) |
| **Individual** | Yes | Individual that participated in the nest |

### Roles

- **Breeding male** – Male responsible for breeding.  
- **Breeding female** – Female responsible for breeding.  
- **Helper** – Individual assisting in nest care.  
- **Offspring** – Young bird associated with the nest.  
- **Unknown** – Role not determined.  

## Nest revisions

Nest revisions are periodic checks of nest status. Open the Nest revisions in a separate tab in the main menu: **Breeding → Nest revisions**.

| Field | Required | Description |
| --- | --- | --- |
| **Nest** | Yes | Nest to which the revision is linked |
| **Nest revision date** | Yes | Date when the nest was revised |
| **Nest revision time** |  | Time when the nest was revised |
| **Observer 1–2** | Yes | Observers who performed the revision |
| **Stage** | Yes | Stage of the nest (see details below) |
| **Status** | Yes | Status of the nest: inactive, active, unknown |
| **Number of eggs (host)** |  | Number of eggs from the nest owner taxon |
| **Number of nestlings (host)** |  | Number of nestlings from the nest owner taxon |
| **Nidoparasite taxon** |  | Taxon of the brood parasite (if present) |
| **Number of nidoparasite eggs** |  | Number of eggs from the brood parasite |
| **Number of nidoparasite nestlings** |  | Number of nestlings from the brood parasite |
| **Parasitized by *Philornis* sp. larvae** |  | Check if nestlings were parasitized by *Philornis* botfly larvae |
| **Notes** |  | Any other information about the nest |

### Nest stages

- **Building** – Nest construction in progress.  
- **Laying** – Eggs being laid.  
- **Incubating** – Eggs being incubated.  
- **Hatching** – Eggs hatching.  
- **Nestling** – Nestlings present in the nest.  
- **Inactive** – Nest no longer active.  
- **Unknown** – Stage not determined.  

## Eggs

For details on egg records, see the [eggs](eggs.md) documentation.

## Best practices

- **Record nest encounter date**: Always note when the nest was first found.  
- **Monitor regularly**: Use nest revisions to track changes in stage and productivity.  
- **Document fate**: Recording nest fate is essential for reproductive success studies.  
- **Measure carefully**: Nest dimensions and plant characteristics provide valuable ecological data.  
- **Note parasitism**: Brood parasites and *Philornis* larvae can significantly affect productivity.  
- **Link to projects**: Associate nests with projects for better organization and reporting.  
- **Use notes for context**: Add details about habitat, disturbances, or unusual observations.  

## Relation to other modules

The Nests module is interconnected with other parts of Xolmis:

- **[Eggs](eggs.md)** – Detailed records of eggs are linked to nests.  
- **[Individuals](individuals.md)** – Breeding individuals are associated with nests.  
- **[Projects](projects.md)** – Nests can be linked to specific research projects.  
- **[Permits](permits.md)** – Nest monitoring may require permits depending on regulations.  

By managing nests in Xolmis, researchers can build a comprehensive dataset on breeding biology, enabling long-term monitoring and comparative studies across species and habitats.
