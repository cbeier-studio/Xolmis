# Connections

Xolmis works with [SQLite](http://www.sqlite.org) database files. Each database file is an independent set of data and the user may have multiple databases, which we call connections. Only one connection can be opened at a time.

## Managing connections

Open the connections dialog in the menu **File → Manage connections**.

### Creating a new database

To create a new database file and connection, click the plus sign :material-plus-circle: and select **New database**, which will open a dialog. Alternatively, you can open this dialog in the menu **File → New database**.

Click on the folder icon of the Database file field to inform the filename for the new database. Give a Connection name to the database. Optionally, the author and the description of the database can be informed as well. Click the **Create database** button to create the database file and proceed to the next step.

All databases have an admin user. Set the Admin user password for the new database. Click the **Apply** button to save the password and proceed to the next step.

It is not recommended to use the Admin user for regular work on Xolmis. A standard user needs to be created, informing a username and password. Click the **Create user** button to finish the database creation process.

### Adding connection for an existing database

To create a connection, click on the plus icon :material-plus-circle:, which will open the Edit connection dialog. Fill the fields according to the list below:

Field | Required | Description
--- | --- | ---
Connection name | Yes | a short name to identify the connection.
Database type | Yes | currently, only the SQLite is available (it is planned to add support to other databases).
Remote database | | check if the database is in another computer.
Server | | the IP address or hostname of the remote computer where the database is located. The default is localhost.
Port | | the communication port in the remote computer.
Database file | Yes | the database file path or the database name at the server.
Username and password | | if the database needs authentication, fill with your credentials.

To save the new connection click on the **Save** button at the bottom of the dialog. Or click on **Cancel** to discard the connection.

### Editing an existing connection

To edit an existing connection, click on the button with a pencil icon :material-pencil:. It will open the Edit connection dialog, then modify what you need and click on the Save button at the bottom of the dialog to apply the changes.

### Deleting a connection

Select the connection that you want to delete in the list and click on the trashcan icon :material-delete:. Confirm if you really want to delete the connection or not.

## Opening a connection

When the Xolmis app starts, it shows the Connection dialog. Select a connection from the list and inform your username and password for the selected connection.

!!! tip

    If you frequently connect to the same database or only have one connection, you can enable the option to save the last used connection in the Settings. This also applies to the username.

While running the Xolmis app, you can connect to another database file just clicking on **File → Connect to database**. It will close the current connection and open the selected one.
