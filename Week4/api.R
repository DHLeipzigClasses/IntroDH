# libraries needed ####
# you probably will have to install almost all of them, e.g.:
# install.packages("httr")

library(tidyverse)
library(httr)
library(jsonlite)
library(lubridate)

# using environment to not disclose API Key in variables ####

apiComponents <- new.env()
# put your API key in the following line!! ###
apiComponents$key <- ""
apiComponents$url1 <- paste("http://api.digitalnz.org/v3/records.json?api_key=", apiComponents$key, sep = "")

query1 <- "&text=rugby&and[collection][]=New+Zealand+Cartoon+Archive&and[year][]=2011&fields=id&per_page=1&page=1"
query2 <- "&text=rugby&and[collection][]=New+Zealand+Cartoon+Archive&and[year][]=2011&fields=id,origin_url,title,description,collection,dc_identifier,large_thumbnail_url,authorities&per_page=100&page="

### How many API calls  ####
firstCall <- GET(paste(apiComponents$url1, query1, sep = "")) %>% content()
number_calls <- ceiling(firstCall$search$result_count / 100)
apiComponents$api_calls <- paste(apiComponents$url1, query2, 1:number_calls, sep = "")


### let's test  ####
data <- fromJSON(apiComponents$api_calls[1])
results <- data$search$results
ID <- results$id
Title <- results$title
Description <- results$description
TestData <- tibble(ID,Title,Description)
TestData

# GET DCDL identifiers  ####
results$dc_identifier[1]
Identifier <- results$dc_identifier
DCDL_IDs <- vector()

for(i in seq_along(Identifier)) {
  DCDL_IDs[i] <- Identifier[[i]][[grep("^.{0,1}DCDL", Identifier[[i]])]]
}

# Add it to the data.frame  ####
TestData$DCDL_IDs <- DCDL_IDs
TestData

# Let's extract the date from the title  ####
Date <- str_extract(Title, "\\d{1,2}\\s\\w*\\s\\d{4}")
TestData$Date <- dmy(Date)
TestData

# Let's extract the cartoonist from the title  ####
Cartoonist <- sub(",* [0-9]{4}.*", "", Title)
Cartoonist %>% table

Collection <- results$collection
Cartoonist2 <- vector()

for(i in seq_along(Collection)) {
  cartoonistInd <- grep("[0-9]{4}", Collection[[i]])
  if (length(cartoonistInd) != 0) {
    Cartoonist2[i] <- sub(",* [0-9]{4}.*", "", Collection[[i]][[cartoonistInd]])
  } else {
    Cartoonist2[i] <- NA
  }
}

Cartoonist2 %>% table

TestData$Cartoonist <- Cartoonist2
TestData

# Plot data via esquisse  ####
TestData %>% esquisse::esquisser()

# Or plot it directly with ggplot  ####

ggplot(TestData, aes(x = Date, fill = Cartoonist)) +
  geom_histogram(bins = 52L) +
  scale_fill_hue() +
  theme_minimal()

# Now that we know that everything works, we can actually write a big function that does it all for us:

### write all into a function  ####

TibbleFromCall <- function(x) {
  data <- fromJSON(x)
  results <- data$search$results
  ID <- results$id
  Title <- results$title
  Description <- results$description
  Identifier <- results$dc_identifier
  DCDL_IDs <- vector()
  for(i in 1:length(Identifier)) {
    DCDL_IDs[i] <- Identifier[[i]][[grep("^.{0,1}DCDL", Identifier[[i]])]]
  }
  Date <- str_extract(Title, "\\d{1,2}\\s\\w*\\s\\d{4}")
  Date <- dmy(Date)
  Collection <- results$collection
  Cartoonist <- vector()
  for(i in seq_along(Collection)) {
    cartoonistInd <- grep("[0-9]{4}", Collection[[i]])
    if (length(cartoonistInd) != 0) {
      Cartoonist[i] <- sub(",* [0-9]{4}.*", "", Collection[[i]][[cartoonistInd]])
    } else {
      Cartoonist[i] <- NA
    }
  }
  TestData <- tibble(ID,Cartoonist,Title,Description,DCDL_IDs,Date)
  return(TestData)
} 

### Run it all  ####

all.data <- tibble(ID=character(),
                   Cartoonist=character(),
                   Title=character(),
                   Description=character(),
                   DCDL_IDs=character(),
                   Date=character())

for (i in apiComponents$api_calls) {
  all.data <- rbind(all.data, TibbleFromCall(i))
}

is.na(all.data$Cartoonist) %>% table

# final plot  ####

all.data %>%
  filter(year(Date) >= 2011, !is.na(Cartoonist)) %>%
  ggplot(aes(x = Date, fill = Cartoonist)) +
  geom_histogram(bins = 52L) +
  scale_fill_hue() +
  theme_minimal()

