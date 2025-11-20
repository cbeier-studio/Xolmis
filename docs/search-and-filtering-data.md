# Search and filtering data

Efficiently finding information in Xolmis is essential when working with large datasets. The system provides powerful **search** and **filtering** tools that can be used independently or in combination, allowing you to quickly locate records and refine results.

## Search data

The **search field** appears in the top-right corner of the window whenever a tab with data is active. Click the search field or press ++ctrl+f++ to set the focus.

Simply type the value you want to search. Results are updated as you type, supporting **partial matches** and **modifiers** for more precise queries.

### Search the exact value

To search for an exact match, start typing `=` (equal sign) followed by the value. 

!!! example
    `=abc` will return only records that exactly match "abc".

### Search value starting with

By default, searches return partial matches regardless of whether the value is at the beginning, middle, or end.  
To restrict results to values starting with the typed text, use `:` (colon).  

!!! example
    `:abc` will return only values beginning with "abc".

### Multiple words search

You can search for multiple words by separating them with spaces.  
This is useful for composite names.  

!!! example
    
    - Searching for `pip` will return all taxa containing "Pipile".  
    - Searching for `pip jac` will return only "Pipile jacutinga".

### Syllabic search

If you only remember fragments of a word or are unsure of the spelling, use the connector `+` (plus) to combine syllables. Example: `abro+pus` will return all taxa containing "Abroscopus".

## Filtering data

Search and filters can be applied simultaneously, making it possible to refine results with high precision.

### Quick filters

Quick filters are accessible from the right toolbar. Click the funnel button :material-filter: to open a side panel with contextual filtering options.

#### Boolean filters

Boolean filters present three options:

- **All** – Equivalent to disabling the filter.  
- **Yes** – Shows only records where the field value is `True`.  
- **No** – Shows only records where the field value is `False`.

#### Tree filters

Tree filters display values in a hierarchical structure with checkboxes. They are used for fields such as:

- **Dates** (year → month → day)  
- **Toponyms** (country → state → municipality)  
- **Taxa** (order → family → species)  

You can check one or multiple values to filter results.

#### List filters

List filters are presented as a **combobox**. You can select only one value at a time, making them useful for categorical fields.

#### Lookup filters

Lookup filters combine a text field with a search button. You can start typing directly or click the button to open a **find dialog**. Select the desired value to apply the filter. Only one value can be selected at a time.

## Best practices

- **Combine search and filters**: Use both simultaneously for maximum precision.  
- **Use modifiers wisely**: Exact (`=`), starting (`:`), and syllabic (`+`) searches help refine results.  
- **Leverage tree filters**: Ideal for hierarchical data such as taxa or localities.  
- **Document your workflow**: Note which filters were applied when exporting or reporting data.  
- **Reset filters when needed**: Always check if filters are active to avoid missing records.  

## Relation to other modules

Search and filtering are available across all major modules in Xolmis:

- **[Individuals](individuals.md)** – Find specific birds by band, taxon, or markings.  
- **[Captures](captures.md)** – Filter by date, locality, or capture type.  
- **[Surveys](surveys.md)** – Narrow down sampling events by method, project, or location.  
- **[Sightings](sightings.md)** – Search by taxon, detection type, or observer.  
- **[Projects](projects.md) and [permits](permits.md)** – Quickly locate administrative records.  

By mastering search and filtering, you can navigate large datasets efficiently, ensuring that analyses and reports are accurate and reproducible.
