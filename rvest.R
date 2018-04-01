library(rvest)
library(RCurl)
doc <- read_html("https://data.gov.tw/dataset/40448")
url <- doc %>% html_nodes(".ff-icon-csv") %>% html_attr("href")
url
download.file(url,destfile="aqi.csv")