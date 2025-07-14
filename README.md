---
output:
  word_document: default
  pdf_document: default
  html_document: default
---
# Fall_Flow_Redd_Dewatering
code to create one(ish) page summary of Fall Flow scenarios and shallow redd dewatering

**September 21, 2023**

## Primary Authors:                                                     
 Lisa Elliott
 U.S. Bureau of Reclamation, Bay-Delta Office, Science Division
 lelliott@usbr.gov

Chase Ehlo
 U.S. Bureau of Reclamation, Bay-Delta Office, Science Division
 cehlo@usbr.gov
 
## Disclaimers:
The script used to generate this document was developed by Bureau of Reclamation for the purposes of providing decision support for real-time Fall Flow reductions. The resulting document is for the purpose of comparing proposed alternatives and does not imply a specific policy position. It is for the purpose of comparing alternatives. No warranty expressed or implied is made regarding the display or utility of the code for other purposes, nor on all computer systems, nor shall the act of distribution constitute any such warranty. The U.S. Bureau of Reclamation or the U.S. Government shall not be held liable for improper or incorrect use of the code described and/or contained herein.

## How to run the code:
**Upload** the latest *'Shallow Redd yyyy Winter-run yyyy-mm-dd.xlsx* from Calfish.org or CDFW into *'input_data/shallow_redds'*. Make sure that dates are clearly in "%Y-%m-%d" format.  

If interested in including own scenarios **Update** the latest scenario *'FallFlowsyyyymmdd_Draft 1.xlsx* file with any new flow scenarios of interest in the *Keswick Flow Alternatives* sheet locaed at *'input_data/flow_scen'*. Make sure that cell formatting for all flows is "General" (no commas), and make sure the new scenario is labeled as 'Y' in the *Desired Scenarios* sheet.  Add description to Scenario Description. 

**Check** for an update on the YYYY_WR_INTERNET_CARCASS-REDDS_counts_as_of_M_dd_YY.xlsx file from calfish.org. Check the "Reporting" sheet for the "To date, unexpanded redd count" (cell B88 as of 2023).  If file has been updated since the last download (check date of latest file under *input_data/shallow_redds/ReddCount*) download and add the new spreadsheet to this folder.  

**Update** input lines for report generation. All inputs that need to be adjusted are in the first chunk of code in 'source_code/import_clean.R'. Inputs to update:

1. wy = water year of analysis 
2. yr = year of analysis  
3. exp_fac = the analysis year's expected expansion number based on the linear relationship between yearly expansions  vs recapture rate of tagged female salmon (see WR EXPANSIONS for Dewatered Redd Calculations 9-11-23.xlsx from CDFW) 



