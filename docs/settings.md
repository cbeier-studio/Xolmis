# Settings

The Xolmis' settings are accessible in the main menu **File > Settings**. It opens a dialog where you can customize the appearance and behavior of Xolmis. It has a list of categories on the left. Each setting is explained below.

## General

Setting | Description | Default
--- | --- | ---
Start page | Which tab opens in Xolmis startup | Coordinates converter
Use <Enter/Return> key to jump to the next control | If enabled, ++enter++ is used like ++tab++ key | Enabled
Confirm data insert/edit cancellation | If enabled, it will prompt if you really want to cancel inserting/editing a record. It could be useful if you accidentally cancel data inserting/editing very often | Disabled
Clear deleted records automatically (in days) | A deleted record is kept in the "recycle bin" and is definitely deleted after this period of time, or never | 60 days
Check for Xolmis updates | Check for updates periodically | Daily

!!! note

    Deleted records are kept inactivated in the database. On every startup, Xolmis will check if there are inactive records with more than 30, 60, 90 or 120 days, as configured in the Settings, then permanently delete them. If you do not want inactive records to be permanently deleted automatically, set the _Clear deleted records automatically (in days)_ setting to **Never**.

    {==Images, audio recordings and documents are always deleted permanently without inactivating the records.==}  

## Appearance

Setting | Description | Default
--- | --- | ---
Theme | Set light or dark themes. The _Auto_ option sets the theme according to the operating system | Auto
Use conditional formatting in grids | Highlight some values on grids to indicate some error or grouping | Enabled
Highlight outlier measurements in captures | If _Use conditional formatting in grids_ is enabled, highlight in yellow the capture measurements that may be outliers | Enabled

## Collection

Setting | Description | Default
--- | --- | ---
Language of vernacular names | Set the language of vernacular names displayed in taxon search results | English
Show synonyms in search results | If enabled, it will show synonyms on search results, but Xolmis always use the accepted name | Disabled

## Media

Setting | Description | Default
--- | --- | ---
Images location | Folder path where all the images are stored | `.\images\` in install folder
Audio files location | Folder path where all the audio recordings are stored | `.\sounds\` in install folder
Documents location | Folder path where all document files are stored | `.\attachments\` in install folder
Open files after export | If enabled, opens exported file in the default application for each filetype | Disabled

!!! danger

    Changing the media locations when you already added some media to the database is **not recommended**, at the moment. The media is stored in the database using relative paths, and changing the media location could lead to inconsistences. We are working on a solution.

## Security and privacy

Setting | Description | Default
--- | --- | ---
Remember connection from last session | If enabled, it will load the last used connection on the login dialog | Enabled
Remember user from last session | If enabled, it will load the last used username on the login dialog | Enabled
Allow Xolmis to log events for technical support | If enabled, it will write a log file of activities, which could be used to investigate problems | Disabled

!!! tip

    If you are experiencing some errors and crashes while using Xolmis, you can enable _Allow Xolmis to log events for technical support_. It will probably record the errors and you can open the log file and send to the support. This helps finding the problem and fixing it. **No personal or sensitive data is recorded in the log or sent to the support**. If Xolmis is running smoothly, you can keep this setting disabled.

## Backup and restore

Setting | Description | Default
--- | --- | ---
Backup files folder | Folder path where all backup files will be stored | `.\backup\` in install folder
Create backup when Xolmis closes | Periodically creates a backup file on closing Xolmis, or never | Weekly

!!! tip

    If you have a cloud drive (such as Google Drive, OneDrive, Dropbox etc.), you can set a subfolder inside it to store Xolmis' backups. Then it will synchronize the backups to the cloud automatically and keep the backups safe.

!!! note

    Xolmis does not control the backup files created and stored at the moment. You need to periodically check how much storage space the backups are using and manually delete older backups if you need to free up some storage space. We have plans to implement a more automatic solution for this in the future.
