# library(googlesheets)
# shiny_token <- gs_auth() # authenticate


library(rdrop2)
token <- drop_auth()
saveRDS(token, "App6/droptoken.rds")
dbfolder <- "shinyshop"
drop_create(dbfolder)


