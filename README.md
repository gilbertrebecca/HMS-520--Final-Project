# HMS-520--Final-Project

Group members:
Asrat Arja Wolde
Rebecca Gilbert
Susan Campbell

Format: 
Option 3 - analyzing a dataset that uses data wrangling and modeling tools in R

Goals:
We want to better understand how indicators of social vulnerability covary by U.S. state and, within the state of Washington, by county.
CDC provides an excellent compiliation of indicators related to vulnerability, pulled from the U.S. census.
We will first read CDC's documentation about the Social Vulnerability Index (SVI) to gain familiarity with the variables.
Next, we will read in data, select variables of interest, and peforme data cleaning and checks.
We will produce some simple visuals (maybe including violin plots and others) and summary stats to orient ourselves to the data.
Then we will choose some analyses of interest and develop models to investigate them. 
We will consider linear regression and linear mixed effects modeling primarily, and will also consider logistic regression if appropriate. 
Finally, we will develop visuals (likely scatterplots) to show model fit and produce any appropriate summaries.
Woohoo!

Plans and timeline:
Group meeting Nov 29 to develop outline of project
Proposal submitted on or before Dec 1st
Group programming session on Dec 4th - import and clean data and initial exploration
Individual programming session on Dec 5th - analyses, visualizations, and final results
Group meeting to develop and record presentation on Dec 13th
Submit presentation on or before Dec 15th
Submit GitHub repository on or before Dec 15th

Accessing data:
Navigate to CDC's ATSDR Social Vulnerability Indexi site (link: https://www.atsdr.cdc.gov/placeandhealth/svi/data_documentation_download.html). Under Year, select 2018. Under Geography, select "United States." Under Geography Type, select Counties. Choose the radio button labeled "CSV File (table data)" and click Go. The documentation for 2018 is available at this same site.

You must save the data locally and point the script to the file path at the variable called "location_of_data" on line 10.
