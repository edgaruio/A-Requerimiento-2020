library(rdrop2)
token <- readRDS("droptoken.rds")
# Then pass the token to each drop_ function
drop_acc(dtoken = token)
db_folder <- "shinyshop"

# save_db <- function(dat) {
#   if(exists("mydata")) dat <- rbind(mydata$x, dat)
#   file_path <- file.path(tempdir(), "data.csv") # create temporary file
#   write.csv(dat, file_path, row.names = FALSE)
#   drop_upload(file_path, dest = db_folder)
# }

load_db <- function() {
  dd <- drop_read_csv(file.path(db_folder,"iris.csv"))
  return(dd)
}