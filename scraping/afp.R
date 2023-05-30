library(httr)
library(jsonlite)
library(rvest)
library(tidyverse)
library(stringr)

# get meta data via Google API
## Note: pageSize depends on the time of scraping
url <- "https://factchecktools.googleapis.com/v1alpha1/claims:search"
source("Google_API_Key.R")
Rohdaten_AFP<-GET(url, query=list(reviewPublisherSiteFilter = "faktencheck.afp.com", key=key, pageSize=705, languageCode="de"))
data_AFP = fromJSON(rawToChar(Rohdaten_AFP$content), flatten=T)


#extract links to afp sites
article_links_api_AFP <-c()
for (i in 1: length(data_AFP[["claims"]][["claimReview"]])){
  article_links_api_AFP <-c(article_links_api_AFP, data_AFP[["claims"]][["claimReview"]][[i]][["url"]])
  print(data_AFP[["claims"]][["claimReview"]][[i]][["url"]])
}

# get afp websites
search_results_AFP <- list()
## Note: number of pages depends on the time of scraping

for (i in 0:47){
  link <-paste0("https://faktencheck.afp.com/list?page=", i)
  search_results_AFP[[i+1]] <- link
}

article_links_search_AFP <-c()
for (i in 1:48){
  search_result_page<- read_html(search_results_AFP[[i]])
  article_links_search_AFP <- c(article_links_search_AFP, paste0("https://faktencheck.afp.com",search_result_page %>% html_elements(".card a") %>% html_attr("href")))
  
}

#compare article_links_search to article_links_api
article_links_api_AFP %in% article_links_search_AFP
Common_links_AFP <-intersect(article_links_api_AFP,article_links_search_AFP)
API_exclusive_AFP <- setdiff(article_links_api_AFP, Common_links_AFP)
Search_exclusive_AFP <- setdiff(article_links_search_AFP, Common_links_AFP)

# create dataframe from AFP API list
df <- Map(as.data.frame, data_AFP$claims$claimReview)
df <- do.call(rbind.data.frame, df)
df <- df[,1:4]
df <- rbind.data.frame(df,data.frame(url=Search_exclusive_AFP, title=rep(NA,length(Search_exclusive_AFP)), reviewDate=rep(NA,length(Search_exclusive_AFP)), textualRating=rep(NA,length(Search_exclusive_AFP))) )

# get and drop observations with two values
dups <- which(sapply(data_AFP$claims$claimReview, nrow)>1)+1

df2 <-data_AFP[["claims"]][,1:3]
for( i in 1:length(dups)){
  print(i)
  df2 <- rbind(df2[1:(dups[i]-1), ], df2[(dups[i]-1),], df2[(dups[i]):nrow(df2), ])
  
}
row.names(df2) <-NULL
AFP<- merge(df, df2, by="row.names", all=T)[,-1]
colnames(AFP) <- c("url", "claimReview","reviewDate", "textualRating","claim", "claimant", "claimDate")

# get data from AFP Website
AFP_all = list(
  meta = data.frame(id = rep(NA, length(AFP$url)) , "claimURL" = rep(NA, length(AFP$url)), "ratingValue" = rep(NA, length(AFP$url)), "bestRating" = rep(NA, length(AFP$url)),"worstRating" = rep(NA, length(AFP$url))),
  text = list(),
  links = list())
AFP_all$meta <- cbind(AFP_all$meta, AFP)

for ( i in 1: length(AFP_all$meta$url)){
  print(i)
  article <- read_html(AFP_all$meta$url[i])
  site_meta <- article %>% html_elements("script") %>% .[6] %>% html_text()
  raw_json_embed <- site_meta %>%
    str_remove_all("\\n|\\t") %>% 
    str_replace(".*(\\[\\{)", "\\1") %>% 
    str_replace("(\\}\\]).*", "\\1")
  
  ex_parsed_json <- jsonlite::parse_json(raw_json_embed)
  try(
    if(!is.null(ex_parsed_json[["@graph"]][[1]][["itemReviewed"]][["url"]])){
      AFP_all$meta$claimURL[i]<- ex_parsed_json[["@graph"]][[1]][["itemReviewed"]][["url"]]
    } 
  )
  try(AFP_all$meta$ratingValue[i] <- ex_parsed_json[["@graph"]][[1]][["reviewRating"]][["ratingValue"]])
  try(AFP_all$meta$bestRating[i] <- ex_parsed_json[["@graph"]][[1]][["reviewRating"]][["bestRating"]])
  try(AFP_all$meta$worstRating[i] <- ex_parsed_json[["@graph"]][[1]][["reviewRating"]][["worstRating"]])
  if(is.na(AFP_all$meta$claimReview[i])) try(AFP_all$meta$claimReview[i]<- ex_parsed_json[["@graph"]][[1]][["name"]])
  if(is.na(AFP_all$meta$reviewDate[i])) try(AFP_all$meta$reviewDate[i] <- ex_parsed_json[["@graph"]][[1]][["datePublished"]])
  if(is.na(AFP_all$meta$textualRating[i])) try(AFP_all$meta$textualRating[i]  <- ex_parsed_json[["@graph"]][[1]][["reviewRating"]][["alternateName"]])
  if(is.na(AFP_all$meta$claimant[i])) try(AFP_all$meta$claimant[i]   <- ex_parsed_json[["@graph"]][[1]][["itemReviewed"]][["author"]][["name"]])
  if(is.na(AFP_all$meta$claimDate[i])) try(AFP_all$meta$claimDate[i]  <- ex_parsed_json[["@graph"]][[1]][["itemReviewed"]][["datePublished"]])
  if(is.na(AFP_all$meta$claim[i])) try(AFP_all$meta$claim[i] <- ex_parsed_json[["@graph"]][[1]][["claimReviewed"]])
  
  text <- article %>% html_elements('div[class="article-entry clearfix"]') %>% html_elements('h3, p') %>% html_text()
  text <- paste(text, collapse = " ")
  AFP_all$text[i] <- text
  AFP_all$meta$reviewDate[i] <- str_remove_all(AFP_all$meta$reviewDate[i], "T.*")
  AFP_all$meta$claimDate[i] <- str_remove_all(AFP_all$meta$claimDate[i], "T.*")
  AFP_all$meta$reviewDate[i] <- str_remove_all(AFP_all$meta$reviewDate[i], "[:blank:][:digit:]{2}:[:digit:]{2}")
  AFP_all$meta$claimDate[i] <- str_remove_all(AFP_all$meta$claimDate[i], "T.*")
  AFP_all$meta$claimDate[i] <- str_remove_all(AFP_all$meta$claimDate[i], "[:blank:][:digit:]{2}:[:digit:]{2}")
  
  id <- paste0("AFP_", as.character(AFP_all$meta$reviewDate[i]),"-",str_sub(text, 7,8), str_sub(text, 11,13), nchar(AFP_all$text[i]))
  id <- str_replace_all(id, "[:blank:]", "§")
  id <- str_replace_all(id, '"', '_')
  id <- str_replace_all(id, "'", "'_")
  AFP_all$meta[i,1] <- id
  names(AFP_all$text)[i] <-id
  links <- article %>% html_elements('div[class="article-entry clearfix"]') %>% html_elements('h3, p') %>% html_elements('a') %>% html_attr("href")
  AFP_all$links[i] <- list(links)
  names(AFP_all$links)[i] <-id
}
AFP_all$meta$claimURL <- unlist(AFP_all$meta$claimURL)
saveRDS(AFP_all, "AFP.rds")

# Clean the data
AFP_all$meta$reviewDate <- as.Date(AFP_all$meta$reviewDate)
AFP_all$meta$claimDate <- as.Date(AFP_all$meta$claimDate)
dates_to_delete <- which(AFP_all$meta$reviewDate >"2023-01-31")
AFP_all$meta <- AFP_all$meta[-dates_to_delete,]
AFP_all$text <- AFP_all$text[-dates_to_delete]
AFP_all$links <- AFP_all$links[-dates_to_delete]
saveRDS(AFP_all, "AFP.rds")
