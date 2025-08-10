# Bands (Rings)

The bands (or bird rings) are a method to identify individual birds. Each band is engraved with a unique identifier code. Xolmis is prepared to work with the Brazilian National Banding System (SNA - Sistema Nacional de Anilhamento), where each band is engraved with the size code (a letter) and a unique number for that size.

!!! note

    We have plans to include other banding systems, thus we need contributors familiarized with these systems. Contact us if you are interested in contributing.

Open the Bands module in the main menu **Individuals → Bands**.

## Adding or editing

Field | Required | Description
--- | --- | ---
Size | Yes | Size of the band in CEMAVE standard (Brazil)
Number | Yes | Unique number for the band
Prefix | | Additional prefix of the band
Suffix | | Additional suffix of the band
Type | Yes | Type of band
Color | | Color of the band
Status | Yes | Status or fate of the band
Reported | | Check if the band fate was already reported
Source | Yes | How the band was obtained or retrieved
Supplier | Yes | The supplier of the band
Requester | Yes | The requester of the band
Carrier | | Person who is/was with the band
Project | | If the band is linked to a specific project
Notes | | Any other info about the band

## New batch

This dialog allows you to add bands in batches that share common data such as size, type, source, etc. Every number from the Start number to the Final number will be added as an individual band record.

Field | Required | Description
--- | --- | ---
Size | Yes | The size code of the batch of bands
Type | Yes | Type of band
Start number | Yes | First band number of the batch sequence
Final number | Yes | Last band number of the batch sequence
Project | | Project to which the bands are linked to
Source | Yes | How the band was obtained or retrieved
Order number | | Protocol number of the order of this batch of bands
Order date | | Date when the bands were ordered
Receipt date | | Date when the bands were received
Supplier | Yes | The supplier of the batch of bands
Requester | Yes | The requester of the batch of bands
Carrier | | Person who will keep the bands
Sender | | Person who transferred the batch of bands

## Transfer bands to...

To transfer a batch of bands to another bander click on the More options button :material-dots-horizontal:. It opens a dialog with the following fields:

Field | Required | Description
--- | --- | ---
Transfer data | Yes | The date when the transfer was done
Size | Yes | The size code letter of the batch of bands
Start number | Yes | First band number of the batch sequence
Final number | Yes | Last band number of the batch sequence
Requester | Yes | The bander to whom the bands will be transferred

Click the **Save** button to record the transfers for each band number, from Start number to Final number. To view the transfer records, access the band history of each transferred band.

## Band history

The band history dialog displays the transactions for a specified band, which include ordering, receiving, transferring, using, and reporting losses.

## Bands balance

The display indicates the quantity of bands you possess for each size, as well as the average number of bands utilized per day of banding and the highest number used in a single day. These figures help predict when to reorder bands, highlighting the row in yellow, or indicate when all bands of a particular size have been used, by turning the row red.

Right-click on the list to access the popup menu. Choose the **Refresh** option or press the ++f5++ key to update the values. Selecting **Export CSV** will save the list as a CSV file. The **Print** option will generate a report that can be printed or saved as PDF.

*[CSV]: Comma-Separated Values
