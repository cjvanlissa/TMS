get_data <- function(){
websitedata <- read.csv("googlesheet.csv", stringsAsFactors = FALSE)
websitedata[c("date", "enddate", "removeby")] <- lapply(websitedata[c("date", "enddate", "removeby")], as.Date, format = "%d-%m-%Y")
removethese <- which(websitedata$removeby <= Sys.Date())
if(length(removethese) > 0){
  websitedata <- websitedata[-removethese, , drop = FALSE]
}
saveRDS(websitedata, "websitedata.RData")
}

get_data()
