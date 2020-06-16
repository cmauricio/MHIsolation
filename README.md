# MHIsolation

The folder 'Data' contains 4 files
- Cleandata.csv
- Labels.csv
- Variables.csv
- Structure.csv

Note that the CSV data is in European format, with column separators being ';' and decimal separators being ','

# Cleandata.csv
This file contains all thedata acquired during the study. The data has been pre-processed so that it was cleaned from information provided by people who should not have participated (i.e. people who were underaged but marked the informed consent as if they were adults). In addition, the names of the countries have been standardised so that all have the same format (e.g. US, USA, United States, United States of America, all become one entry).
No more changes or information filtering were done to this database.
The database is numerically indexed. 

# Labels.csv
This file contains the index codes for the answers of the different questions presented in Cleandata.csv

# Variables.csv 
This file contains the index codes used for each of the questions (columns) in Cleandata.CSV

# Structure.csv
This file contains the whole structure of the formularie, including the type and subtype of questions and the different choices related to each selection question.

