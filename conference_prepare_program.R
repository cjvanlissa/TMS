library(lubridate)
prog <- googlesheets4::read_sheet('https://docs.google.com/spreadsheets/d/1gu6VY_INm8sNfA3sBweHvqlioce7ocOosZbwDq6BN34/edit?usp=sharing')
names(prog) <- sapply(strsplit(names(prog), " "), `[[`, 1)

sht <- googlesheets4::read_sheet('https://docs.google.com/spreadsheets/d/16uHMLxyULCO0tBgE7iUqLIwhzpOClU66RCsWqd-i_v4', col_names = FALSE)
df <- sht
names(df) <- c("Date", "Start", "End", "Type", "Speaker", "Duration", "Title")
df$Start <- as.POSIXct(paste(df$Date, df$Start), format = "%d-%m-%Y %H:%M")
df$End <- as.POSIXct(paste(df$Date, df$End), format = "%d-%m-%Y %H:%M")
#df$Start <- lubridate::hm(df$Start)
#df$End <- lubridate::hm(df$End)
#df$Duration <- lubridate::minutes(df$Duration)
df$Duration <- as.difftime(df$Duration, units="mins")

if(!all((df$End - df$Start) == df$Duration, na.rm = TRUE)) stop()
df$Description <- prog$Abstract[match(df$Title, prog$Title)]

write.csv(df, "conference_program.csv", row.names = FALSE)
