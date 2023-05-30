library(httr)
library(jsonlite)
library(rvest)
library(tidyverse)
library(plyr)
library(stringr)

# get meta data via Google API
url <- "https://factchecktools.googleapis.com/v1alpha1/claims:search"
## Note: pageSize depends on the time of scraping
source("Google_API_Key.R")
Rohdaten_DPA<-GET(url, query=list(reviewPublisherSiteFilter = "presseportal.de", key=key, pageSize=705, languageCode="de"))
data_DPA = fromJSON(rawToChar(Rohdaten_DPA$content), flatten=T)

Rohdaten_DPA<-GET(url, query=list(reviewPublisherSiteFilter = "dpa-factchecking.com", key=key, pageSize=2000, languageCode="de"))
data_DPA = fromJSON(rawToChar(Rohdaten_DPA$content), flatten=T)

#extract links to DPA sites
article_links_api_DPA <-c()
for (i in 1: length(data_DPA[["claims"]][["claimReview"]])){
  article_links_api_DPA <-c(article_links_api_DPA, data_DPA[["claims"]][["claimReview"]][[i]][["url"]])
  print(data_DPA[["claims"]][["claimReview"]][[i]][["url"]])
}

#Scrape Presseportal Texte

# create dataframe from DPA API list
df <- Map(as.data.frame, data_DPA$claims$claimReview)
df <- do.call(rbind.fill, df)

# get and drop observations with two values
dups <- which(sapply(data_DPA$claims$claimReview, nrow)>1)
df <- Reduce(function(d1, d2) rbind.fill(d1, d2),data_DPA$claims$claimReview)
if(!is.null(dups)) df<- df[-dups,]

df <- df[,1:4]
df2 <-data_DPA[["claims"]]
df2 <- df2[,-2]

DPA <- cbind.data.frame(df, df2)
colnames(DPA) <- c("url", "claimTitle","reviewDate", "textualRating","claim", "claimant", "claimDate")

daten = list(
  meta = data.frame(id = rep(NA, length(DPA$url)) , "claimURL" = rep(NA, length(DPA$url)), "ratingValue" = rep(NA, length(DPA$url)), "bestRating" = rep(NA, length(DPA$url)),"worstRating" = rep(NA, length(DPA$url))),
  text = list(),
  links = list()
)

daten$meta <- cbind.data.frame(daten$meta, DPA)

DPA_API <- daten

#scrape Presseportal Texte
for ( i in 1: length(DPA_API$meta$url)){
  article <- read_html(DPA_API$meta$url[i])
  
  text <- article %>% html_elements('article div.card') %>% html_elements('p') %>% html_text()
  text <- text[-c(1:3)]
  text <-head(text, -2)
  text <- paste(text, collapse = " ")
  DPA_API$text[i] <- text
  
  if(is.na(DPA_API$meta$reviewDate[i])){
    date <- article %>% html_element('.date') %>% html_text()
    date <-str_remove_all(date, "[:blank:]–[:blank:][:digit:]{2}:[:digit:]{2}")
    date <-as.character(as.Date(date, format="%d.%m.%Y"))
    DPA_API$meta$reviewDate[i] <-date
  }

  DPA_API$meta$reviewDate[i] <- str_remove_all(DPA_API$meta$reviewDate[i], "T.*")
  DPA_API$meta$claimDate[i] <- str_remove_all(DPA_API$meta$claimDate[i], "T.*")
  
  links <- article %>% html_elements('article div.card') %>% html_elements('p') %>% html_elements('a') %>% html_attr("href")
  links <- links[-c(1,2)]
  links <- head(links, -1)
  DPA_API$links[i] <- list(links)
  id <- paste0("DPA_API_", as.character(DPA_API$meta$reviewDate[i]),"-",str_sub(text, 7,8), str_sub(text, 11,13), nchar(DPA_API$text[i]))
  id <- str_replace_all(id, "[:blank:]", "§")
  id <- str_replace_all(id, '"', '_')
  id <- str_replace_all(id, "'", "'_")
  DPA_API$meta[i,1] <- id
  names(DPA_API$links)[i] <-id
  names(DPA_API$text)[i] <-id
}
DPA_API$meta$claimDate <- as.Date(DPA_API$meta$claimDate)
DPA_API$meta$reviewDate <-as.Date(DPA_API$meta$reviewDate)
saveRDS(DPA_API, "DPA_API.rds")

#Get DPA Fact Checking Links
article_links_search_DPA <-c()
search_result_page<- read_html("https://dpa-factchecking.com/germany/")
article_links_search_DPA <- search_result_page %>% html_elements(" main a ") %>%   html_attr("href")

article_links_search_DPA <- article_links_search_DPA[!duplicated(article_links_search_DPA)]
article_links_search_DPA <- gsub("\\./", "/",article_links_search_DPA)
url_l <- !grepl("/germany", article_links_search_DPA)
url <-  article_links_search_DPA
url[url_l] = paste0("https://dpa-factchecking.com/germany",article_links_search_DPA[url_l])
url[!url_l] = paste0("https://dpa-factchecking.com",article_links_search_DPA[!url_l])

meta <- data.frame(id = rep(NA, length(article_links_search_DPA)), claimURL = rep(NA, length(article_links_search_DPA)), ratingValue = rep(NA, length(article_links_search_DPA)),
                       bestRating = rep(NA, length(article_links_search_DPA)), worstRating = rep(NA, length(article_links_search_DPA)),
                      url = url, claimTitle =rep(NA, length(article_links_search_DPA)),
                       reviewDate = rep(as.Date(NA), length(article_links_search_DPA)), textualRating =rep(NA, length(article_links_search_DPA)),
                       claim = rep(NA, length(article_links_search_DPA)), claimant = rep(NA, length(article_links_search_DPA)), claimDate = rep(NA, length(article_links_search_DPA)))

DPA_Site = list(
  meta = meta,
  text = list(),
  links = list()
)

#Scrape DPA Site articles
for ( i in 1: length(DPA_Site$meta$url)){
  article <- read_html(DPA_Site$meta$url[i])
  date <- article %>% html_elements('p[class="tag is-white date"]')  %>% html_text()
  date <- str_remove_all(date, ",.*")
  date <-as.Date(date, format="%d.%m.%Y")
  
  DPA_Site$meta[i, ]
  text <- article %>% html_elements('div.content p')  %>% html_text()
  text <- paste(text, sep = "", collapse ="")
  if(!is.na(str_extract(text, ".*(?=---Links)"))) text <-str_extract(text, ".*(?=---Links)") 
  DPA_Site$text[i] <- text
  textualRating <- article %>% html_elements('h2, h1')  %>% html_text()
  if (length(textualRating) >1){
        textualRating<- textualRating[1:2] 
        textualRating <- str_flatten(textualRating, ": ")
  }
  DPA_Site$meta$textualRating[i] <- textualRating
  DPA_Site$meta$reviewDate[i] <- date
  id <- paste0("DPA_Site_", as.character(date),"-",str_sub(text, 7,8), str_sub(text, 11,13), nchar(DPA_Site$text[i]))
  id <- str_replace_all(id, "[:blank:]", "§")
  id <- str_replace_all(id, '"', '_')
  id <- str_replace_all(id, "'", "'_")
  DPA_Site$meta[i,1] <- id
  names(DPA_Site$text)[i] <-id
  links <- article %>% html_elements('div.content') %>% html_elements('p') %>% html_elements('a') %>% html_attr("href")
  DPA_Site$links[i] <- list(links)
  names(DPA_Site$links)[i] <-id
}

saveRDS(DPA_Site, "DPA_Site.rds")

#Clean the data
DPA_API$meta$reviewDate <- str_remove_all(DPA_API$meta$reviewDate, "T.*")
DPA_API$meta$claimDate <- str_remove_all(DPA_API$meta$claimDate, "T.*")
saveRDS(DPA_API, "DPA_API.rds")

DPA_Site$meta$reviewDate <- str_remove_all(DPA_Site$meta$reviewDate, ",.*")
saveRDS(DPA_Site, "DPA_Site.rds")


#Shorten the data to <=2023-01-31
date_log <- DPA_Site$meta$reviewDate <= "2023-01-31"

DPA_Site$meta <-DPA_Site$meta[date_log, ]
DPA_Site$text <- DPA_Site$text[date_log]
DPA_Site$links <- DPA_Site$links[date_log]

saveRDS(DPA_Site, "DPA_Site.rds")

# merge DPA_API and DPA_Site
meta <- rbind(DPA_API$meta, DPA_Site$meta)
text <- c(DPA_API$text, DPA_Site$text)
links <-c(DPA_API$links, DPA_Site$links)
DPA_all <- list(
  meta = meta,
  text = text,
  links = links
)

## change to common ids
DPA_all$meta$id <- str_replace_all(DPA_all$meta$id, "DPA_API", "DPA_")
DPA_all$meta$id <- str_replace_all(DPA_all$meta$id, "DPA_Site", "DPA_")
names(DPA_all$text) <- str_replace_all(names(DPA_all$text), "DPA_API", "DPA_")
names(DPA_all$text) <- str_replace_all(names(DPA_all$text), "DPA_Site", "DPA_")
names(DPA_all$links) <- str_replace_all(names(DPA_all$links), "DPA_API", "DPA_")
names(DPA_all$links) <- str_replace_all(names(DPA_all$links), "DPA_Site", "DPA_")
saveRDS(DPA_all, "DPA_all.rds")
