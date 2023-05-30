library(httr)
library(jsonlite)
library(rvest)
library(tidyverse)
library(xml2)
library(XML)

#-----functions
addNAs = function(dataframe, nrows=20){
  
  return(rbind(dataframe, matrix(NA,
                                 nrow=nrows,
                                 ncol=ncol(dataframe),
                                 dimnames=list(NULL,names(dataframe)))))
  
}
#-----

# get meta data via Google API
## Note: pageSize depends on the time of scraping
url <- "https://factchecktools.googleapis.com/v1alpha1/claims:search"
source("Google_API_Key.R")
Rohdaten_APA<-GET(url, query=list(reviewPublisherSiteFilter = "apa.at", key=key, pageSize=705, languageCode="de"))
data_APA = fromJSON(rawToChar(Rohdaten_APA$content), flatten=T)

#extract links to APA sites
article_links_api_APA <-c()
for (i in 1: length(data_APA[["claims"]][["claimReview"]])){
  article_links_api_APA <-c(article_links_api_APA, data_APA[["claims"]][["claimReview"]][[i]][["url"]])
  print(data_APA[["claims"]][["claimReview"]][[i]][["url"]])
}

# get APA websites
article_links_search_APA <-c()
search_result_page<- read_html("https://apa.at/faktencheck/ueberblick/")
article_links_search_APA <- search_result_page %>% html_elements(" p > a ") %>%   html_attr("href")

#compare article_links_search_APA to article_links_api_APA
article_links_api_APA %in% article_links_search_APA
Common_links_APA <-intersect(article_links_api_APA,article_links_search_APA)
API_exclusive_APA <- setdiff(article_links_api_APA, Common_links_APA)
Search_exclusive_APA <- setdiff(article_links_search_APA, Common_links_APA)

# create dataframe from APA API list
df <- data_APA$claims
df <- df[,-2]
df <- rbind.data.frame(df,data.frame(url=Search_exclusive_APA, title=rep(NA,length(Search_exclusive_APA)), reviewDate=rep(NA,length(Search_exclusive_APA)), textualRating=rep(NA,length(Search_exclusive_APA))) )

# get and drop observations with two values
dups <- which(sapply(data_APA$claims$claimReview, nrow)>1)+1

if(is.null(dups)) df<- df[-dups,]

df2 <-data_APA[["claims"]][["claimReview"]]
df2 <- do.call(rbind, df2)
df2 <- df2[,-c(5:7)]

if(is.null(dups))df2 <- df2[-dups,]

#Combine dataframes in one single frame
APA <- merge(df2, df, by="row.names", all=TRUE)[,-1]
APA<- rbind.data.frame(APA,data.frame(url=Search_exclusive_APA, title=rep(NA,length(Search_exclusive_APA)), reviewDate=rep(NA,length(Search_exclusive_APA)), 
                                      textualRating=rep(NA,length(Search_exclusive_APA)), text=rep(NA,length(Search_exclusive_APA)), claimant=rep(NA,length(Search_exclusive_APA)),claimDate=rep(NA,length(Search_exclusive_APA))) )
colnames(APA) <- c("url", "claimTitle","reviewDate", "textualRating","claim", "claimant", "claimDate")
daten = list(
  meta = data.frame(id = rep(NA, length(APA$url)) , "claimURL" = rep(NA, length(APA$url)), "ratingValue" = rep(NA, length(APA$url)), "bestRating" = rep(NA, length(APA$url)),"worstRating" = rep(NA, length(APA$url))),
  text = list(),
  links = list()
)
daten$meta <- cbind.data.frame(daten$meta, APA)
APA <- daten
APA$meta$reviewDate <- as.Date(APA$meta$reviewDate)
APA$meta$claimDate <- as.Date(APA$meta$claimDate)
urls_to_delete <- str_which(APA$meta$url, "liveblog")
APA$meta <- APA$meta[-urls_to_delete,]

for ( i in 1: length(APA$meta$url)){
  article <- read_html(APA$meta$url[i])
  
  if(is.na(APA$meta$reviewDate[i])){
    date <-article %>% html_element("span.text-grey-blue") %>% html_text()
    date <-str_extract(date, "[:digit:]{2}\\.[:digit:]{2}\\.[:digit:]{2}")
    date <- as.Date(date, format = "%d.%m.%y")
    APA$meta$reviewDate[i] <- date
  }
  
  if(is.na(APA$meta$claimTitle[i])){
    title <-article %>% html_element("h1") %>% html_text()
    title <- str_replace_all(title, "\n", " ")
    title <-str_trim(title)
    APA$meta$claimTitle[i] <- title
  }
  text <- article %>% html_elements("main") %>% html_elements("p") %>% html_text()
  if (is_empty(text)){
    text <- article %>% html_elements("div.row article div") %>% html_text()
  }
  text <- head(text,-2)
  text <- paste(text, collapse = " ")
  if (regexpr("\\w+",text) == -1){
    text <- article %>% html_elements('div.apa-container  div:not([class])')  %>% html_text()
  }
  text <- paste(text, collapse = " ")
  text <- str_replace_all(text, "\n", " ")
  text <- str_replace_all(text, '\\\\', '')
  text <- str_replace_all(text, "Wenn Sie zum Faktencheck-Team Kontakt aufnehmen.*", "")
  APA$text[i] <- text
  
  links <- article %>% html_elements(" main p") %>% html_elements('a') %>% html_attr("href")
  
  if (is_empty(links)){
    links <- article %>% html_elements("article div") %>% html_elements('a') %>% html_attr("href")
  }
  if (is_empty(links)){
    links <- article %>% html_elements('div.apa-container  div:not([class])') %>% html_elements('a') %>% html_attr("href")
  }
  APA$links[i] <- list(links)
  
  id <- paste0("APA_", as.character(APA$meta$reviewDate[i]),"-",str_sub(text, 7,8), str_sub(text, 11,13), nchar(APA$text[i]))
  id <- str_replace_all(id, "[:blank:]", "§")
  id <- str_replace_all(id, '"', '_')
  id <- str_replace_all(id, "'", "'_")
  APA$meta[i,1] <- id
  names(APA$links)[i] <-id
  names(APA$text)[i] <- id
  
}

saveRDS(APA,"APA.rds")

#---------Compare XML Files to scraped data
# Read XML file into a R object
xml_files <- list.files("APA_XML", ".xml")
xml_files <- paste0("APA_XML/", xml_files)
APA_XML <- data.frame(headline = rep(NA,length(xml_files)),date = rep(NA,length(xml_files)), text = rep(NA,length(xml_files)), links = rep(NA,length(xml_files)))

for (i in 1:length(xml_files)){
  xml_data <- read_xml(xml_files[i])
  APA_XML$text[i] <- xml_text(xml_find_all(xml_data, ".//text"))
  APA_XML$headline[i] <- xml_text(xml_find_all(xml_data, ".//titel"))
  APA_XML$date[i] <- xml_text(xml_find_all(xml_data, ".//datum"))
  APA_XML$links[i] <- str_extract_all( APA_XML$text[i], "https?://[A-Za-z0-9\\-.]+\\.[A-Za-z]{2,}/[A-Za-z0-9\\-.]+")
}

APA_XML$text <- str_replace_all(APA_XML$text, "\n", " ")
APA_XML$text <- str_replace_all(APA_XML$text, '\"', '"')
APA_XML$text <- str_replace_all(APA_XML$text, "Wenn Sie zum Faktencheck-Team Kontakt aufnehmen.*", "")
APA_XML$text <- str_replace(APA_XML$text, ".*\\(APA\\) - ", "")
APA_XML$date <- as.Date(APA_XML$date, format= "%d%m%Y")
APA_XML$headline <- str_replace(APA_XML$text, "APA-Faktencheck: ", "")
#check which XML data is new to the corpus
APA_text_start <- str_sub(APA_XML$text,start =1, end =120)
new_xml <-c()
for (i in 1:length(APA_text_start)) {
  #print(i)
  if(!any(grepl(APA_text_start[i], x= APA$text, fixed=T)))  new_xml <- c(new_xml,i)
}

APA$meta <-addNAs(APA$meta, length(new_xml))

#add new xml data to scraped and create IDs
for(i in new_xml){
  print(i)
  
  APA$meta$reviewDate[length(APA$text)+1] <- APA_XML$date[i]
  APA$meta$claimTitle[length(APA$text)+1] <- APA_XML$headline[i]
  id <- paste0("APA_", as.character(APA$meta$reviewDate[length(APA$text)+1]),"-",str_sub(APA_XML$text[i], 7,8), str_sub(APA_XML$text[i], 11,13), nchar(APA_XML$text[i]))
  id <- str_replace_all(id, "[:blank:]", "§")
  APA$meta$id[length(APA$text)+1] <-id
  APA$text[length(APA$text)+1] <- APA_XML$text[i]
  names(APA$text)[length(APA$text)] <-id
  APA$links[length(APA$links)+1] <- APA_XML$links[i]
  names(APA$links)[length(APA$links)] <-id
}

articles_to_delete <- which(duplicated(APA$meta$id))
APA$meta <-APA$meta[-articles_to_delete,]
APA$links <-APA$links[-articles_to_delete]
APA$text <-APA$text[-articles_to_delete]
saveRDS(APA,"APA.rds")
