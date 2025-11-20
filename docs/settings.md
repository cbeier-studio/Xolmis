# Settings

The **Settings** module allows you to customize the appearance, behavior, and data management preferences of Xolmis. It is accessible in the main menu: **File → Settings**. Opening this option displays a dialog with a list of categories on the left. Each category contains specific settings that can be adjusted according to your workflow.

## General

This section contains general system settings that affect the overall behavior of Xolmis.

| Setting | Description | Default |
| --- | --- | --- |
| **Start page** | Defines which tab opens when Xolmis starts. | Coordinates converter |
| **Use <Enter/Return> key to jump to the next control** | If enabled, ++enter++ acts like the ++tab++ key, moving to the next field. | Enabled |
| **Confirm data insert/edit cancellation** | Prompts a confirmation dialog when canceling data entry or editing. Useful if you often cancel by mistake. | Disabled |
| **Clear deleted records automatically (in days)** | Deleted records are kept in a "recycle bin" and permanently removed after the defined period. Can be set to *Never*. | 60 days |
| **Check for Xolmis updates** | Defines how often Xolmis checks for updates. | Daily |

!!! note
    Deleted records remain inactive in the database until the configured period (30, 60, 90, or 120 days). If you do not want inactive records to be permanently deleted, set the option to **Never**.  

    {==Images, audio recordings, videos, and documents are always deleted permanently without being inactivated.==}

## Appearance

This section controls the visual aspects of Xolmis.

| Setting | Description | Default |
| --- | --- | --- |
| **Theme** | Choose between light or dark themes. The *Auto* option adapts to the operating system theme. | Auto |
| **Grid row height** | Defines the row height in data grids. | 25 pixels |
| **Use conditional formatting in grids** | Highlights values in grids to indicate errors or groupings. | Enabled |
| **Highlight outlier measurements in captures** | If conditional formatting is enabled, highlights potential outliers in capture measurements. | Enabled |

## Collection

This section manages how collected data is displayed and curated.

| Setting | Description | Default |
| --- | --- | --- |
| **Language of vernacular names** | Sets the language of common names displayed in taxon search results. | English |
| **Show synonyms in search results** | If enabled, synonyms are shown in search results, but Xolmis always uses the accepted name. | Disabled |

## Media

This section defines how media files (images, audio, video, documents) are stored and handled.

| Setting | Description | Default |
| --- | --- | --- |
| **Images location** | Folder path for storing images. | `.\images\` |
| **Audio files location** | Folder path for storing audio recordings. | `.\sounds\` |
| **Videos location** | Folder path for storing videos. | `.\videos\` |
| **Documents location** | Folder path for storing documents. | `.\attachments\` |
| **Open files after export** | If enabled, opens exported files in the default application. | Disabled |

!!! danger
    Changing media locations after adding files is **not recommended**. Media paths are stored as relative references, and altering the location may cause inconsistencies.
    
    {==A solution for dynamic relocation is under development.==}

## Security and privacy

This section contains settings related to login and technical support.

| Setting | Description | Default |
| --- | --- | --- |
| **Remember connection from last session** | Loads the last used connection in the login dialog. | Enabled |
| **Remember user from last session** | Loads the last used username in the login dialog. | Enabled |
| **Allow Xolmis to log events for technical support** | Creates a log file of activities to help investigate problems. | Disabled |

!!! tip
    If you experience errors or crashes, enable **Allow Xolmis to log events for technical support**. This will record technical details that can be sent to support. **No personal or sensitive data is recorded or transmitted.** If Xolmis is running smoothly, keep this setting disabled.

## Backup and restore

This section manages backup and restore options for your data.  
Backups are essential to protect against data loss and ensure that your ornithological records remain safe and recoverable.

| Setting | Description | Default |
| --- | --- | --- |
| **Backup files folder** | Folder path where backup files are stored. | `.\backup\` |
| **Create backup when Xolmis closes** | Defines if backups are created automatically when closing Xolmis. Options: *Never*, *Daily*, *Weekly*, *Monthly* | Weekly |

In addition to the automatic options above, two buttons are available for manual control:

### Create backup

Immediately generates a backup of the current database and saves it in the folder defined in **Backup files folder**. Use this option before performing major edits, imports, or maintenance tasks to ensure you have a safe restore point.

### Restore

Allows you to select a previously created backup file and restore it as the active database. This option is useful if you need to roll back changes, recover from corruption, or return to a known stable state.

!!! warning
    Restoring a backup will replace the current database with the selected backup file. Always verify that the backup is correct before proceeding.

!!! tip
    If you use a cloud drive (Google Drive, OneDrive, Dropbox, etc.), set a subfolder inside it as the backup folder. This way, backups will be synchronized automatically and kept safe.

!!! note
    Currently, Xolmis does not manage backup storage. You must manually check storage usage and delete older backups if necessary. A more automated solution is planned for future versions.

## Best practices

- **Review settings periodically**: Adjust preferences as your workflow evolves.  
- **Keep backups safe**: Store backups in cloud drives or external devices.  
- **Use conditional formatting**: Helps quickly identify errors or outliers in data.  
- **Avoid changing media paths**: Prevent inconsistencies by keeping default media folders.  
- **Enable logging only when needed**: Use technical logs for troubleshooting, not for routine use.  

## Relation to other modules

Settings affect the behavior of all modules in Xolmis:

- **General** – Controls startup behavior and record lifecycle.  
- **Appearance** – Defines how data grids and reports are displayed.  
- **Collection** – Influences taxon searches and data curation.  
- **Media** – Manages how images, audio, and documents are linked to records.  
- **Security and privacy** – Impacts login and troubleshooting.  
- **Backup and restore** – Ensures data safety and recovery.  

By configuring settings properly, researchers can tailor Xolmis to their workflow, ensuring **efficiency, consistency, and data security**.
