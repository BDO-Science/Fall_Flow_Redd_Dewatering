---
output:
  word_document: default
  pdf_document: default
  html_document: default
---
# Fall_Flow_Redd_Dewatering
code to create one page summary of Fall Flow scenarios and shallow redd dewatering

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
**Update** the Redds.csv file with the latest shallow redds data from Calfish.org or CDFW. Make sure that dates are clearly in "%Y-%m-%d" format.  
**Update** the kesFlow.csv file with any new flow scenarios of interest. Make sure that cell formatting for all flows is "General" (no commas).  
**Check** for an update on the YYYY_WR_INTERNET_CARCASS-REDDS_counts_as_of_M_dd_YY.xlsx file from calfish.org. Check the "Reporting" sheet for the "To date, unexpanded redd count" (cell B88 as of 2023).      
**Update** input lines for ./Real-time Estimates_Prelim_automated_v3.Rmd. All inputs that need to be adjusted are in the first chunk of code. Inputs to update:  
1. reddCount = To date, unexpanded redd count from YYYY_WR_INTERNET_CARCASS-REDDS_counts_as_of_M_dd_YY.xlsx  
2. countDate = date associated with "To date, unexpanded redd count from YYYY_WR_INTERNET_CARCASS-REDDS_counts_as_of_M_dd_YY.xlsx"  
3. updatedReddInfoDate = date of most recent shallow redds data from CDFW/PSMFC/calfish.org  
4. yr = year of analysis  
5. yr_exp #the analysis year's expected expansion number based on the linear relationship between yearly expansions vs recapture rate of tagged female salmon (see WR EXPANSIONS for Dewatered Redd Calculations 9-11-23.xlsx from CDFW)  
6. fallreddCount = a number of fall run redds to use in simulations. We recommend using the current value of 1000.  
7. scenarios = A list of scenarios under consideration   
8. kwk_webpage = cdec website for KWK gage daily flows  
9. kes_webpage  = cdec website for KES gage daily flows  

**Run** ./Real-time Estimates_Prelim_automated_v3.Rmd with Run/Run ALL to check that all code runs properly.  
**Knit** ./Real-time Estimates_Prelim_automated_v3.Rmd with Knit/Knit to Word  



