# Nests

## Adding or editing

| Field | Required | Description |
| --- | --- | --- |
| Taxon | Yes | Taxon of the nest owner |
| Field number | Yes | Identifier of the nest |
| Fate | | Fate of the nest, see details below |
| Nest encounter date | | Date when the nest was found |
| Last date | | Last date when the nest was seen active |
| Project | | Project to which the nest is linked |
| Observer | Yes | Person who found and monitored the nest |
| Locality | Yes | Site where the nest is |
| Longitude | | X axis/longitude of the geographical coordinate |
| Latitude | | Y axis/latitude of the geographical coordinate |
| Nest description | | Brief description of the nest |
| Productivity | | Nest productivity as the number of fledglings that left the nest |
| Nest shape | | Type of shape of the nest, see details below |
| Support type | | Type of structure/substrate where the nest was built |
| Height at ground level | | In centimeters |
| Support plant 1-2 | | Plant species used as support for the nest |
| Plant height | | In centimeters |
| Stem thickness | | In centimeters |
| Greater plant diameter | | In centimeters |
| Lesser plant diameter | | In centimeters |
| Days building | | In how many days the nest was built |
| Days incubating | | In how many days the eggs were incubated |
| Nestling-days | | In how many days the nestlings left the nest |
| Total active-days | | The total number of days in which the nest was active |
| Lesser internal diameter | | Minimum diameter of the nest measured in the internal part |
| Greater internal diameter | | Maximum diameter of the nest measured in the internal part |
| Lesser external diameter | | Minimum diameter of the nest measured in the external part |
| Greater external diameter | | Maximum diameter of the nest measured in the external part |
| Internal height | | Height of the nest measured in the internal part |
| External height | | Height of the nest measured in the external part |
| Distance from plant edge | | Position of the nest relative to the edge of the support plant |
| Distance from plant center | | Position of the nest relative to the center of the support plant |
| Cover | | Percentage of cover above the nest by stems, branches, and leaves |
| Notes | | Any other info about the nest |

### Nest fates

- Unknown
- Lost
- Success

### Nest shapes

- Scrape
- Cup
- Plate
- Sphere
- Pendent
- Platform
- Mound
- Burrow
- Cavity

### Support types

- Ground
- Herb/bush
- Branch/fork
- Leaves
- Ledge
- Rock/cliff
- Ravine
- Nest box
- Anthropic
- Other

## Nest owners

| Field | Required | Description |
| --- | --- | --- |
| Role | Yes | Role of the individual at the nest, see details below |
| Individual | Yes | Individual that participated in the nest |

### Roles

- Breeding male
- Breeding female
- Helper
- Offspring
- Unknown

## Nest revisions

| Field | Required | Description |
| --- | --- | --- |
| Nest | Yes | Nest to which the revision is linked to |
| Nest revision date | Yes | Date when the nest was revised |
| Nest revision time | | Time when the nest was revised |
| Observer 1-2 | Yes | The observers who did the revision |
| Stage | Yes | In which stage the nest was, see details below |
| Status | Yes | Status of the nest: inactive, active, unknown |
| Number of eggs (host) | | Number of eggs from the taxon that owns the nest |
| Number of nestlings (host) | | Number of nestlings from the taxon that owns the nest |
| Nidoparasite taxon | | Taxon of the nidoparasite |
| Number of nidoparasite eggs | | Number of eggs from the nidoparasite |
| Number of nidoparasite nestlings | | Number of nestlings from the nidoparasite |
| Parasitized by Philornis sp. larvae | | Check if the nestling were parasitized by _Philornis_ botfly's larvae |
| Notes | | Any other info about the nest |

### Nest stages

- Building
- Laying
- Incubating
- Hatching
- Nestling
- Inactive
- Unknown

## Eggs

See the [eggs](eggs.md) documentation.
