# Users

A user is anyone who connects to and interacts with a Xolmis database file. Each database connection has its own set of users. To open a Xolmis connection, you must log in with a username and password. This username is recorded with any changes made to the data, providing both security and change tracking.

## User management dialog

To manage the database users, open it via main menu **File → Manage users**. It opens a dialog with a list of users of the current database file, with a toolbar at the top.

![User management dialog](img/users-dialog.png)

Icon | Button | Function
--- | --- | ---
:material-plus-circle: | Add user | Create a new user in the current database
:material-pencil: | Edit user | Edit the information and permissions of the selected user
:material-key-variant: | Change password | Change password of the selected user
:material-refresh: | Refresh records | Reload the user list
:material-delete: | Delete user | Delete the selected user

## Adding and editing

To add a new user, click on the add button :material-plus-circle:. And to edit the selected user, click on the edit button :material-pencil: or double-click the user to be edited. It will open the edit dialog.

![Edit user dialog](img/edit-user-dialog.png)

Field | Required | Description
--- | --- | ---
Username | Yes | Name that will be used to login and displayed elsewhere
Fullname | | Optionally, you can add the user real name
Access level | Yes | Defines some privileges or restrictions. It is explained below
Manage collection | | Allow the user to edit the collection data
Print reports | | Allow the user to generate and print or export reports
Export data | | Allow the user to export data
Import data | | Allow the user to import data

### Access levels

The access levels give the user the permissions to access or restrict some functionalities of Xolmis.

Level | Description
--- | ---
Administrator | Have the highest privileges, can access all functionalities of Xolmis. Must be used only for important tasks, like database maintenance. Not recommended to use when editing data
Standard | Is the default level. Have access to almost all functionalities of Xolmis. Recommended to the everyday use and editing data
Visitor | Have some restrictions, as it can only visualize data, not edit it

!!! note "`admin` user"

    The `admin` user is created when creating a new database file, so all databases have it. It must **not** be used to trivial tasks, like editing data.

## Change user password

Click on the key button :material-key-variant:, it will prompt for authentication using the selected user current password, then open the dialog to change password. Inform a new password and the password confirmation. Click on **Save** button to apply the change.

![Change password dialog](img/change-password-dialog.png)

!!! note

    If you try to change the password of another user that is not the one your are logged in, the authentication of that user is required.

## Deleting a user

To delete a user, click on the trashcan :material-delete: button. It will prompt for confirmation, then click **Yes** if you want to proceed.

!!! warning

    If the user has already been used to perform editing tasks, it is not recommended to delete it. If you delete a user, you can lose some track changes info.

!!! info

    The `admin` user can not be deleted.
