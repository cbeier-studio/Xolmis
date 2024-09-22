Usage of the test program "multiread"

/0/ Download the program ExifTool from 
    https://www.sno.phy.queensu.ca/~phil/exiftool/
    Unzip the binary and copy it as "exiftool.exe" to the folder "tools" of
    the fpexif installation.
    
/1/ Enter, in the top edit line, the path to the image directory to be analyzed.

/2/ Press "Read files" -- The names of the image files found are listed in the
    treeview at the left.
    
/3/ Uncheck the files to be skipped from the test.

/4/ Press "Create txt files". The program uses ExifTool to extract EXIF metadata 
    from the checked picture files; the information is stored in text files
    bearing the image filename, but extension txt.
    
/5/ Press "Run test". The program tries to read the EXIF meta data by using
    the fpexif routines, compares the results with the exiftool reference
    data and lists the differences in the memo.
    
    Some minor differences are possible because both programs may use different
    strings in case of enumerated values.
    
