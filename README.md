# Viewing the Dashboard
1. Navigate to https://mattlampl.shinyapps.io/PGH-HFPA-Dashboard/
2. Click the ***Get Data*** button in the upper left-hand corner.  
   *Note: Be patient while the data is being fetched. It may take 1-2 minutes to gather all of the data from the census.*
3. Once the data has been gathered, you will see a popup message indicating that the data has successfully been retrieved.

# Food Security

Tools and scripts for generating data about food security for the City of Pittsburgh. See https://pittsburghpa.gov/dcp/food-access-programs.

## Healthy Food Priority Areas

These are census tracts in which the risks of food insecurity are the most critical. Methodology for how these have been determined can be found in the [FeedPGH Report](https://pittsburghpa.gov/dcp/food-access-programs).

## Setting up the HFPA Dashboard

In order to properly access the census data required for the HFPA dashboard, you must first request an API Key. Navigate to [Census API Key signup page](https://api.census.gov/data/key_signup.html) and follow the isntructions to sign up for a key.

*Note: The API Key should be a 40 character string with numerical and alphabetical characters*

Once a key has been generated, you will then have to add it to the `get_data.R` file. Find the line underneath the `#Census API Key` comment, and paste the key within the quotes where the code reads `ENTER KEY HERE`.

*Note: The API Key must be within the quotes or you will get an error.*


## Running the HFPA Dashboard

Open `app.R` and `get_data.R` in R Studio. Make sure that all packages listed below are installed on your machine:

 - `shiny`
 - `tidyverse`
 - `tidycensus`
 - `plotly`
 - `sf`
 - `tmap`
 - `shinyWidgets`
 
 Click the `Run App` button in the upper right-hand corner of R Studio. This should open up the application in your default web browser.

