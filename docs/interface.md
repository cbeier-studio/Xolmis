# User interface

## Main window breakdown

The typical Xolmis window with a module opened is as follows:

### Main menu

The main menu is the topmost bar in the window. The menu options are divided in:

Menu | Description
--- | ---
File | Options to manage databases and users, settings and import
Sampling | Open modules related to sampling
Individuals | Open modules related to individual birds
Breeding | Open modules related to nesting
Management | Open modules related to institutions, people, projects, and permits
Geo | Open modules related to geography
Taxonomy | Open modules related to zoological and botanical taxonomies
Help | Options to user manual, feedback, check updates, and about the application

### Navigation and search bar

The navigation bar is at the top of the window, just below the main menu. It presents a home button :material-home: and a new button :material-plus-circle: at the left, then the tabs opened, and at the right a search box :octicons-search-16: and a notifications button :material-bell:.

### Main records panel

The main records panel is the upper grid of records of the window, with the main records toolbar at the top of the grid and the main records status bar at the bottom of the grid.

#### Main records toolbar

The main records toolbar presents buttons for record actions, as follows:

Icon | Button | Function
--- | --- | ---
:material-plus-circle: | New record | Insert new record using the edit dialog for the module
:material-table-large-plus: | Quick Entry | Open Quick Entry dialog, used to insert records in batches
varies | Add batch of... | Contextual button to insert records using a batch dialog for selected modules
:material-pencil: | Edit record | Edit the selected record using the edit dialog for the module
:material-checkbox-multiple-marked: | Mark/unmark all records | Menu with options for mark or unmark all records, or invert the selection
:material-share: | Export records | Open the export data dialog
:material-printer: | Print records | Menu with options to print grid and other reports
:material-filter-remove: | Clear all filters | Clear data filtering and search
:material-delete: | Delete record | Delete the selected record

!!! note

    The icons presented here and the icons on application may differ, but are similar.

#### Main records status bar

The main records status bar shows the number os records found in the current query, at the left. Aligned to the right, we have some buttons, as listed:

Icon | Button | Function
--- | --- | ---
:material-checkbox-multiple-marked-circle: | Record verifications | Open the record verifications dialog and shows the current record status
:material-history: | Record history | Open the record history dialog
:material-refresh: | Refresh | Refresh the data query
:material-page-first: | First record | Go to the first record
:material-chevron-left: | Previous record | Go to the previous record
:material-chevron-right: | Next record | Go to the next record
:material-page-last: | Last record | Go to the last record

### Related records panel

The related records panel is an expandable panel at the lower part of the window. It has a navigation bar, a toolbar, a records grid, a status bar, and may have a side panel.

#### Related records navigation bar

The navigation bar presents a button to add records :material-plus-circle:, when clicked, if the related records panel is collapsed, it opens a menu with options to add new related records. The add button is followed by navigation tabs of the related modules. Click a tab to expand the related records panel and navigate between related modules, or click again the active tab to collapse it. If the panel is expanded, clicking the add button will open the related module edit dialog, without opening a menu.

#### Related records toolbar

Above the related records grid, is a toolbar, similar to the main records toolbar. Just in case, the available controls are:

Icon | Button/Control | Function
--- | --- | ---
:material-table-large-plus: | Quick Entry | Open Quick Entry dialog, used to insert related records in batches
varies | Add batch of... | Contextual button to insert related records using a batch dialog for selected modules
none | Quick add edit | Search box to quickly select a value and insert in the related module
:material-pencil: | Edit record | Edit the selected related record using the edit dialog for the module
:material-share: | Export records | Open the export data dialog
:material-dock-right: | Show/hide side panel | Open or close the related records side panel
:material-delete: | Delete record | Delete the selected related record

#### Related records status bar

Below the related records grid, is the status bar, similar to the main records status bar and with the same elements.

#### Related records side panel

Currently, only projects budget presents a side panel, with the purpose of showing the budget balance and rubric balance values.

### Side panel and toolbar

Aligned to the right side of the window is a vertical toolbar. Clicking on a button of this toolbar opens the side panel, and clicking the active button again closes it. The side toolbar buttons are:

Icon | Button | Panel function
--- | --- | ---
:material-text-box: | Record details | Shows all the fields and values of the record vertically
:material-filter: | Quick filters | Options to filter the records of the module
:material-image: | Images | Images attached to the selected record
:material-microphone: | Audio recordings | Audio recordings attached to the selected record
:material-video: | Videos | Videos attached to the selected record
:material-attachment: | Documents and links | Documents and/or links attached to the selected record
:material-map: | Map | Display geographical coordinates of the selected record on a map
:material-list-box: | Summary | Show counts or average values for the selected column of the main grid
:material-table-column: | Columns | Visibility settings of main grid columns
:material-recycle: | Recycle bin | Shows and manages deleted records

Each side panel could have its own toolbar.

#### Quick filters toolbar

Icon | Button | Function
--- | --- | ---
:material-filter-remove: | Clear all filters | Clear data filtering and search

#### Images toolbar

Icon | Button | Function
--- | --- | ---
:material-plus-circle: | Add image | Insert new images from files
:material-pencil: | View and edit image info | Edit the selected image info
:material-image-search: | View image | Opens the selected image in the image viewer
:material-delete: | Delete image | Delete the selected image

#### Audio recordings toolbar

Icon | Button | Function
--- | --- | ---
:material-plus-circle: | Add recording | Insert new audio recordings from files
:material-pencil: | View and edit recording info | Edit the selected recording info
:material-play: | Play recording | Opens the selected recording file in the default audio player
:material-delete: | Delete recording | Delete the selected audio recording

#### Videos toolbar

Icon | Button | Function
--- | --- | ---
:material-plus-circle: | Add video | Insert new videos from files
:material-pencil: | View and edit video info | Edit the selected video info
:material-play: | Play video | Opens the selected video file in the default video player
:material-delete: | Delete video | Delete the selected video

#### Documents and links toolbar

Icon | Button | Function
--- | --- | ---
:material-plus-circle: | Add document/link | Insert new documents from files or link URL
:material-pencil: | View and edit document/link info | Edit the selected document or link info
:material-open-in-new: | View document/link | Opens the selected document or link in the default application
:material-delete: | Delete document/link | Delete the selected document or link

#### Map toolbar

Icon | Button | Function
--- | --- | ---
:material-share: | Export map points | Export map points to file

#### Columns toolbar

Icon | Button | Function
--- | --- | ---
:material-table-row-height: | Increase row height | Increases the row height in the grids
:material-table-row-height: | Decrease row height | Decreases the row height in the grids
:material-table-row-height: | Default row height | Sets the row height in the grids back to default value
:material-table-column-width: | Auto adjust columns' width | Auto adjust the width of columns by the widest value
:material-arrow-down: | Move column down | Move the selected column down in the list
:material-arrow-up: | Move column up | Move the selected column up in the list
:material-checkbox-multiple-marked: | Mark/unmark all columns | Sets all columns visible or invisible
:material-table-column-remove: | Hide column | Sets the selected column invisible

#### Recycle bin toolbar

Icon | Button | Function
--- | --- | ---
:material-delete-restore: | Restore selected | Restores the selected record (reactivates record)
:material-delete-forever: | Delete permanently | Delete the selected record from the database permanently

### Main status bar

The main window has a status bar at the very bottom of the window. It displays some useful information, such as the current database connection, the current user, some temporary messages, and the application version.
