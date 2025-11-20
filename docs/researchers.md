# Researchers

The **Researchers** module is used to register and manage information about people who participated in data collection, analysis, or any other activity related to ornithological projects. A researcher can be a field assistant, technician, student, professor, or collaborator from an institution. By documenting researchers, Xolmis ensures that all records are properly attributed, strengthening accountability, traceability, and collaboration.

Open the Researchers module in the main menu: **Management → Researchers**.

## Adding or editing a researcher

When creating or editing a researcher record, the following fields are available:

| Field | Required | Description |
| --- | --- | --- |
| **Profile picture** |  | Optional profile picture of the person |
| **Name** | Yes | Full name of the person |
| **Citation** | Yes | Citation form of the name (used in reports and publications) |
| **Abbreviation** | Yes | Unique abbreviation used to type faster and import data |
| **Treatment** |  | Title or treatment: Mr., Ms., Dr., BSc., MSc., PhD, etc. |
| **Gender** |  | Gender of the person (he, she, he/she) |
| **Date of birth** |  | Date of birth, used to show birthdays |
| **Date of death** |  | Optional date of death |
| **RG** |  | RG document number (Brazil) – will soon be deprecated |
| **CPF** |  | CPF document number (Brazil), equivalent to National ID Card (USA) |
| **E-mail** |  | Personal or professional email address |
| **Telephone** |  | Landline phone number |
| **Mobile phone** |  | Mobile phone number |
| **Postal code** |  | CEP (Brazil) or ZIP Code (USA) |
| **Address** |  | Postal address where the person lives |
| **Address detail** |  | Complementary details such as building or apartment number |
| **Neighborhood** |  | Part of the city where the person lives |
| **Municipality** |  | County, city, or village |
| **State** |  | State or province |
| **Country** |  | Country of residence |
| **Institution** |  | Institution where the person works |
| **Department** |  | Department where the person is allocated |
| **Role** |  | Role of the person in the institution/department |
| **Lattes** |  | Lattes profile string (Brazilian academic CV system) |
| **Orcid** |  | ORCID profile string (international researcher identifier) |
| **X (Twitter)** |  | Profile name on X (formerly Twitter) |
| **Instagram** |  | Instagram profile name |
| **Website** |  | Personal or professional website address |
| **Notes** |  | Any other information about the person |

## Best practices

- **Use consistent citation forms**: Ensure that names are standardized for reports and publications.  
- **Assign unique abbreviations**: Abbreviations are critical for quick data entry and imports; avoid duplicates.  
- **Keep contact information updated**: Researchers may change institutions, emails, or phone numbers.  
- **Link institutional data**: Always connect researchers to their institutions for better traceability.  
- **Record identifiers (ORCID, Lattes)**: Facilitates integration with academic and scientific platforms.  
- **Use notes for context**: Add details such as areas of expertise, fieldwork experience, or collaborations.  

## Relation to other modules

Researchers are interconnected with several parts of Xolmis:

- **[Sightings](sightings.md), [Captures](captures.md), and [Nests](nests.md)** – Records are attributed to the researchers who collected the data.  
- **[Projects](projects.md)** – Researchers are linked to projects as participants or coordinators.  
- **[Institutions](institutions.md)** – Researchers are affiliated with institutions, strengthening organizational context.  
- **[Reports](print-data.md)** – Citations and abbreviations are used in generated reports and exports.  

By managing researchers in Xolmis, teams ensure that contributions are properly documented, fostering collaboration and recognition in ornithological research.

## Example workflow

1. Add a new researcher (*Maria Silva*) with her ORCID and Lattes profiles.  
2. Link her to her institution (*Universidade Federal de Pelotas*) and department (*Biologia*).  
3. Attribute her as the observer in a **Sightings record** and as the bander in a **Captures record**.  
4. Generate a report where her name appears in the citation format (*Silva, M.*).  

This workflow demonstrates how researcher records provide **traceability, consistency, and proper attribution** across the entire database.
