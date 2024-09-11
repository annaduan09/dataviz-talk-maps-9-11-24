# Philly Dataviz Meetup Talk - Maps and Code

This repository contains the code and data used to generate the figures for my presentation at the Philly Dataviz Meetup on September 11, 2024. The repository is organized as follows:

## code: The code for data processing and map creation is contained within the `code` folder:  
The primary script, `maps.R`, handles the ingestion, processing, and visualization of the data. The script is thoroughly commented to guide users through the steps involved.

## data: The two data sources not processed in maps.R can be found in the `data` folder. This includes:  
- Voucher Data: Provided by the Philadelphia Housing Authority.
- Yelp Data: Accessed via the Yelp Fusion API in `yelp-api-workflow.Rmd`.

## maps: The final map outputs are stored in the maps folder.  

## slides: The slides used in the presentation are available in PDF format in the slides folder.  

For further details or to reproduce the figures from the talk, please refer to the comments within the maps.R script.