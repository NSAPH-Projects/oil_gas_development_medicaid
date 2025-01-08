README  

 

This repository contains code to support the manuscript “Impact of Boom and Bust Economies from Oil and Gas Development on Psychiatric Hospitalizations among Medicaid Beneficiaries.” This paper uses Medicaid claims to calculate changes in psychiatric hospitalizations among U.S. counties with oil and gas development following booms and busts in production.  

 

Code: 
NBER_data_processing.R: This file is used to process public population estimates from NBER.  

USDA_oilgascounty_processing.R: This file is used to create boom/bust indicators from UDSA oil and gas data  

manuscript_OGD_development_maps_tables.R: This file is used to create tables and figures from public data sources, including USDA OGD data. 

oil_gas_dev_DID_all.R: This is the primary code file used for analysis 

 

Data: 
Medicare data used in this study cannot be shared nor made publicly available due to restrictions in the Data Use Agreement with the CMS. Researchers interested in Medicare data should request data from CMS directly and complete separate Data Use Agreements. 

 

Medicare patient individual-level data used for this analysis are stored at a Level-3 secured data platform on Research Computing Environment, supported by the Institute for Quantitative Social Science in the Faculty of Arts and Sciences at Harvard University. Code used to process these data can be found here: https://github.com/NSAPH-Data-Processing/medicaid_mental_health_aggregated 

 
Data used for county-level oil and gas production are available from the United States Department of Agriculture at https://www.ers.usda.gov/data-products/county-level-oil-and-gas-production-in-the-united-states/. 


Other public data sources used directly from source include:  

The CDC Social Vulnerability Index available from: https://www.atsdr.cdc.gov/place-health/php/svi/svi-data-documentation-download.html  

Census County-to-County Migration Flow tables available from: https://www.census.gov/data/tables/2000/demo/geographic-mobility/county-to-county-migration-flows.html  

Census TIGER files available from https://www.census.gov/geographies/mapping-files/2010/geo/tiger-line-file.html 

U.S. Census SAIPE datasets available from: https://www.census.gov/programs-surveys/saipe/data/datasets.html  

 

All the analyses are run on 
R version 3.4.1 (2024-06-14) -- "Race for Your life" 
Copyright © 2024 The R Foundation for Statistical Computing 
Platform: x86_64-pc-linux-gnu (64-bit) 

 

Terms of Use: 
By using the contents on this Github repo and the article, you agree to cite our paper. 

 

[citation TBD] 

 

Contact Us: 

Mary Willis: mwillis@bu.edu 

Nina Cesare: ncesare@bu.edu  

 

Acknowledgments 

We recognize the support of the National Studies in Air Pollution and Health (NSAPH) Lab at Harvard University for computing resources and support.  
 