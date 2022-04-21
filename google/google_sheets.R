#Google Sheets 
library(googlesheets4)
library(googledrive)

#Authenticate
options(gargle_oauth_cache = ".secrets")
gs4_auth(cache = ".secrets", email = "nickjtarrant@gmail.com")


googlesheet_link <- "https://docs.google.com/spreadsheets/d/1GH_zvJCpBQhSX3RmFl95YxmX-MPCG-AdhCFLiL8g_Yw/edit#gid=0"


sheet_properties(googlesheet_link)
sheet_id <- as_sheets_id(googlesheet_link)

#Function to reload Google Sheets data
loadGoogleData <- function(sheet_name)
{loaded_data <- read_sheet(ss = sheet_id, sheet = sheet_name)
return(loaded_data)
}