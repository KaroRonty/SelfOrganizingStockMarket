library(httr) # downloading the xls(x) files
library(readxl) # reading xls(x) files
library(dplyr) # data formatting

# -------------------------------- Read Shiller data
GET("http://www.econ.yale.edu/~shiller/data/ie_data.xls", write_disk(temp <- tempfile(fileext = ".xls")))
shillerdata <- read_xls(temp, sheet = 3, skip = 7)

# Format the years and months correctly
current_year <- format(Sys.time(), "%Y")
corrected_dates <- expand.grid(1:12, 1871:current_year)
last_month <- length(grep(current_year, shillerdata$Date))
months_to_be_cut_off <- 12 - last_month
corrected_dates <- head(corrected_dates, nrow(corrected_dates) - months_to_be_cut_off)

# Add leading zeros
corrected_dates$Var1 <- sprintf("%02d", as.numeric(corrected_dates$Var1))
dates <- as.data.frame(paste(corrected_dates$Var2, corrected_dates$Var1, sep = "-"))
names(dates) <- "dates"

# Remove possible excess rows & add corrected dates back
shillerdata <- head(shillerdata, nrow(dates))
shillerdata <- cbind(dates, shillerdata)
shillerdata$Date <- NULL

# -------------------------------- Read Goyal data
GET("http://www.hec.unil.ch/agoyal/docs/PredictorData2017.xlsx", write_disk(temp <- tempfile(fileext = ".xls")))
goyaldata <- read_xlsx(temp, sheet = 1)
goyaldata <- select(goyaldata, c("yyyymm", "b/m"))

# Make dates into same format as above and prepare names for joining
goyaldata$yyyymm <- paste(substr(goyaldata$yyyymm, 1, 4), substr(goyaldata$yyyymm, 5, 6), sep = "-")
names(goyaldata) <- c("dates", "bm")

full_data <- full_join(shillerdata, goyaldata, by = "dates")

# --------------------------------  Replace written NAs with real NAs
full_data$bm[full_data$bm == "NaN"] <- NA
full_data$CAPE[full_data$CAPE == "NA"] <- NA
full_data$CAPE <- as.numeric(full_data$CAPE)

# Return only full data
rm(list = setdiff(ls(), "full_data"))