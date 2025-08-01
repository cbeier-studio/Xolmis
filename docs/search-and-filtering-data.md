# Search and filtering data

## Search data

The search field appears in the right-top corner of the window when a tab with data if active. Click the search field or type ++ctrl+f++ to set the focus.

Just type the value that you want to search in the field. You get the search results as you type, and it supports partial values.

### Search the exact value

To search the exact value, start typing `=` (equal) and the value you are looking for. _E.g._: `=abc`

### Search value starting with

The search finds partial values by default, no matter it is in the start, middle or end of the result value.

To search only values starting with the typed part, start typing `:` (colon) and the part you are looking for. _E.g._: `:abc`

### Multiple words search

If the text you are searching have more than one word, you can use the partial search for each word separating them with spaces. It helps to find composite names. _E.g._: searching for `pip` will return all the `Pipile` species and other taxa, while searching for `pip jac` will return only `Pipile jacutinga`.

### Syllabic search

If you remember only some parts of a word or do not remember how some syllables are spelled, you could search for two or more parts of the same word using the connector `+` (plus). _E.g._: searching for `abro+pus` will return all `Abroscopus` taxa.

## Filtering data

Search and filter values can be used simultaneously.

### Quick filters

The quick filters are accessible at the right toolbar, clicking on the funnel button, it opens a panel with contextual filtering options.

#### Boolean filters

The boolean filters present three options:

* **All**: it is the same as disabling the filter.
* **Yes**: filter for records where the field value is `True`.
* **No**: filter for records where the field value is `False`.

#### Tree filters

The values to filter are presented in a hierarchical tree with checkboxes for each value. Check one or multiple values to filter.

#### List filters

The filter is presented as a combobox and you can select only one value at a time.

#### Lookup filters

An edit with a button is presented. Start typing in the edit or click the button to open the find dialog. Find and select the value you want to search. You can select one value at a time.
