get_data <- function(){
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

  # Write People page
  ppl <- websitedata[websitedata$category == "People", , drop = FALSE]
  if(nrow(ppl) > 0){
    lnz <- c("---", "title: \"People\"", "---", "", '::: {layout="[20,-2,20]" layout-valign="center"}')
    lnz <- c(lnz,
             sapply(1:nrow(ppl), function(i){ paste0("### [", ppl$title[i], "](", ppl$link[i], ")\n\n", paste0("![](", ppl$picture[i], ")"), "\n\n", ppl$description[i], "\n\n")}),
             ":::", "")
    writeLines(lnz, "people.qmd")

  }

  # Calendar

  cal <- websitedata[websitedata$category == "Events", , drop = FALSE]
  if(nrow(cal) > 0){
    cal <- do.call(rbind, lapply(1:nrow(cal), function(i){
      calendar::ic_event(uid = cal$title[i], start_time = as.POSIXct(cal$date[i]), end_time = as.POSIXct(cal$enddate[i]), summary = cal$description[i])
    }))
    cal <- calendar::ical(cal) |>
    calendar::ic_write(file = "tms.ics")
  }

  saveRDS(websitedata, "websitedata.RData")
}

get_data()
