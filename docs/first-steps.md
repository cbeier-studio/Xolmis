# Starting a new database

When you create a new database in Xolmis, the first step is to establish the foundational modules that will support all subsequent data entry. These modules are interconnected, and the order in which you fill them is important because each depends on the information provided in the previous ones.

!!! success "Once done"

    If you have a large amount of data, inserting everything may take some time — so hang in there! This step is important to ensure that your database is complete and ready for use. The good news is that you will probably only need to do this once, at the beginning. Afterwards, new records can be added individually or in small batches whenever necessary, making the process much lighter. Once the initial setup is finished, all your data will always be available for quick access and reliable use in the database.

!!! info

    If more users will use the same database, you can add them via main menu **File → [Manage users](users.md)**.

## Step by step setup

### 1. Gazetteer

Begin by defining the geographic hierarchy of your study area. The gazetteer organizes places such as countries, states, regions, municipalities, districts, and localities. This structure is essential because all sampling plots and observations will be linked to these locations. See details in [Gazetteer](gazetteer.md).

!!! tip

    To make it easier to enter basic geographic data (countries, states, and municipalities), Xolmis offers a helper tool. Simply choose the countries you need, them and their states or provinces will be added automatically to the gazetteer. When selecting a state, you can also pick the municipalities (cities) you want to include. After this step, all that remains is to enter the specific localities.

### 2. Sampling plots

Once the gazetteer is complete, create your sampling plots. These represent specific sites within the gazetteer where fieldwork is conducted. Sampling plots depend on the gazetteer entries to ensure accurate spatial references. See details in [Sampling plots](sampling-plots.md).

### 3. Institutions

Register the institutions involved in your work (universities, research centers, museums, etc.). Institutions are required to associate researchers and projects with their organizational affiliations. See detail in [Institutions](institutions.md).

### 4. Researchers

Add the researchers who will contribute data. Each researcher could be linked to an institution. This ensures that all records are properly attributed and traceable. See details in [Researchers](researchers.md).

## Optional modules

Depending on your project’s scope, you may also want to configure the following modules before entering ornithological data:

- [**Projects**](projects.md): Define research projects to group related activities and datasets.
- [**Permits**](permits.md): Record permits and authorizations required for fieldwork.
- [**Methods**](methods.md): Document specialized methodologies used in your research.
- [**Bands (Rings)**](bands.md): Register bird ring series for marking and tracking individuals.

## Entering Ornithological Data

After completing the setup of the core modules (and optional ones if needed), you are ready to [insert](adding-and-editing-data.md) or [import](importing-data.md) ornithological data. Sightings, captures, specimens, and other records will reference the gazetteer, sampling plots, institutions, and researchers you have already defined. This relational structure ensures consistency and prevents errors, as each module provides the necessary context for the others.
