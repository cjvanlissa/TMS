get_data <- function(){
  # https://www.obrien.page/blog/2023/03_10_google_and_github_actions/
  if(!Sys.info()["user"] == "vanlissa") googlesheets4::gs4_auth(path = Sys.getenv('GOOGLE_DRIVE'))
  websitedata <- googlesheets4::read_sheet('https://docs.google.com/spreadsheets/d/1ii_T5tXEp1CzUWivy4f0QFFkQcoKhefxqX2UmpuvELo/edit?usp=sharing')
  names(websitedata) <- c("Timestamp", "category", "title", "description", "date", "enddate", "removeby", "link", "picture")
  websitedata[c("date", "enddate", "removeby")] <- lapply(websitedata[c("date", "enddate", "removeby")], as.Date, format = "%d-%m-%Y")
  if(any(is.na(websitedata$date))){
    websitedata$date[which(is.na(websitedata$date))] <- as.Date(websitedata$Timestamp[which(is.na(websitedata$date))], format = "%d-%m-%Y")
  }

  # Remove date events
  if(any(is.na(websitedata$removeby) & websitedata$category == "Events")){
    these <- which(is.na(websitedata$removeby) & websitedata$category == "Events")
    websitedata$removeby[these] <- websitedata$Timestamp[these] + 86400
  }
  # End date events
  if(any(is.na(websitedata$enddate) & websitedata$category == "Events")){
    these <- which(is.na(websitedata$enddate) & websitedata$category == "Events")
    websitedata$enddate[these] <- websitedata$date[these]
  }

  removethese <- which(websitedata$removeby <= Sys.Date())
  if(length(removethese) > 0){
    websitedata <- websitedata[-removethese, , drop = FALSE]
  }


  saveRDS(websitedata, "websitedata.RData")
}

get_data()
