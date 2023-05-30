library(httr)
library(jsonlite)
library(rvest)
library(tidyverse)
library(plyr)
library(stringr)
library(xlsx)

# get meta data via Google API
## Note: pageSize depends on the time of scraping
url <- "https://factchecktools.googleapis.com/v1alpha1/claims:search"
source("Google_API_Key.R")
Rohdaten_Correctiv<-GET(url, query=list(reviewPublisherSiteFilter = "correctiv.org", key=key, pageSize=3000, languageCode="de"))
data_Correctiv = fromJSON(rawToChar(Rohdaten_Correctiv$content), flatten=T)

#extract links to Correctiv sites
article_links_api_Correctiv <-c()
for (i in 1: length(data_Correctiv[["claims"]][["claimReview"]])){
  article_links_api_Correctiv <-c(article_links_api_Correctiv, data_Correctiv[["claims"]][["claimReview"]][[i]][["url"]])
  print(data_Correctiv[["claims"]][["claimReview"]][[i]][["url"]])
}

# create dataframe from Correctiv API list
df <- Map(as.data.frame, data_Correctiv$claims$claimReview)
df <- do.call(rbind.fill, df)
df <- Reduce(function(d1, d2) rbind.fill(d1, d2),data_Correctiv$claims$claimReview)
df <- df[,1:4]
df2 <-data_Correctiv[["claims"]]
df2 <- df2[,-4]

# get and repeat observations with two or more values ## these observations share same meta data but refer to different articles, therefore the metadata is duplicated
dups <- which(sapply(data_Correctiv$claims$claimReview, nrow)>1)
dups_nums <- c(rep(2,7), 4) ## number of replications is based on number of rows of the corresponding data.frame, for the first seven duplicated observations this is two, for the last (1844) it is four
duptimes <- rep(1, nrow(df2)) 
duptimes[dups] <-dups_nums ## vector of number of reps 
index <- rep(1:nrow(df2), duptimes)
df2_rep <- df2[index,]

# create list with meta data, texts and urls on the website
Correctiv <- cbind.data.frame(df, df2_rep)
colnames(Correctiv) <- c("url", "claimTitle","reviewDate", "textualRating","claim", "claimant", "claimDate")
Correctiv <- Correctiv[!duplicated(Correctiv$url),]

# load and inspect Correctiv Excel Sheet (provided by Correctiv)
Correctiv_Excel <- read.xlsx("Faktenchecks_CORRECTIV seit 2020.xlsx", sheetIndex = 1)
Correctiv_Excel[,1] <- as.Date(as.integer(Correctiv_Excel[,1]), origin = "1899-12-30")
Correctiv_common <- intersect(Correctiv_Excel[,3],Correctiv$url)
Correctiv_Excel_excl <- setdiff(Correctiv_Excel[,3], Correctiv$url)
Correctiv_API_excl <- setdiff(Correctiv$url,Correctiv_Excel[,3] )
Correctiv_Excel_excl <- as.data.frame(Correctiv_Excel_excl)
colnames(Correctiv_Excel_excl) <- c("url")
Correctiv <-  rbind.fill(Correctiv,Correctiv_Excel_excl)

daten = list(
  meta = data.frame(id = rep(NA, length(Correctiv$url)) , "claimURL" = rep(NA, length(Correctiv$url)), "ratingValue" = rep(NA, length(Correctiv$url)), "bestRating" = rep(NA, length(Correctiv$url)),"worstRating" = rep(NA, length(Correctiv$url))),
  text = list(),
  links = list()
)

daten$meta <- cbind.data.frame(daten$meta, Correctiv)

Correctiv_API_Excel <- daten
#get all preview articles and hintergrund articles, which do not cover a specific claim

rows_to_delete <- grep("?preview|/hintergrund/|/thema/", Correctiv_API_Excel$meta$url)
Correctiv_API_Excel$meta <-Correctiv_API_Excel$meta[-rows_to_delete,]
Correctiv_API_Excel$meta <- Correctiv_API_Excel$meta[!is.na(Correctiv_API_Excel$meta$url),]
rownames(Correctiv_API_Excel$meta) <- NULL

error_ids <-c()
#scrape Correctiv texts
for ( i in 1: length(Correctiv_API_Excel$meta$url)){
  print(i)
  article <- read_html(Correctiv_API_Excel$meta$url[i])
  
  text <- article %>% html_elements('div.detail__wrapper') %>% html_elements('p, li') %>% html_text()
  text_log <- !str_detect(text, "\\(function\\(\\)|Unterstützen Sie unabhängigen Journalismus!|E-Mail-Adresse\\r\\n|Ihre Spende gegen Fake News|IFCN.*des US-amerikanischen Poynter Instituts|Faktenchecks per Mail|Verschwörungsmythen spalten die Gesellschaft|CORRECTIV ist spendenfinanziert|Unterstützen Sie uns dabei|Wir recherchieren zu Missständen in der Gesellschaft|Fördern auch Sie unsere Arbeit")  #finds and deletes funraising appeals and info boxes
  text <- text[text_log]
  text <- str_remove_all(text,"\\n|\\r")
  text <- paste(text, collapse = " ")
  text <- str_replace_all(text, "Redigatur:.*","")
  text <- str_squish(text)
  Correctiv_API_Excel$text[i] <- text
  links <- article %>% html_elements('div.detail__wrapper') %>% html_elements('p, li') %>% html_elements('a') %>% html_attr("href")
  links <- head(links, -5)
  Correctiv_API_Excel$links[i] <- list(links)
  
  claim <- article %>% html_elements("script")  %>% html_text() %>% str_subset("claimReview")
  if(is_empty(claim)) {
    error_ids <- c(error_ids,i)
    next
    }
  raw_json_embed <- claim %>%
    str_remove_all("\\n|\\t")
  try(tryCatch({ex_parsed_json <- jsonlite::parse_json(raw_json_embed)},
  error=function(e){ex_parsed_json <<- jsonlite::parse_json(raw_json_embed[2])}))
  try({tryCatch({Correctiv_API_Excel$meta$claimURL[i]<- ex_parsed_json[["itemReviewed"]][["url"]]},
               error=function(e) {Correctiv_API_Excel$meta$claimURL[i] <<-ex_parsed_json[[1]][["itemReviewed"]][["appearance"]][[1]][["url"]]})
    } 
  )
  try(tryCatch({Correctiv_API_Excel$meta$ratingValue[i] <- ex_parsed_json[[1]][["reviewRating"]][["ratingValue"]]},
           error=function(e) {Correctiv_API_Excel$meta$ratingValue[i] <<- ex_parsed_json[["reviewRating"]][["ratingValue"]]}))
  try(tryCatch({Correctiv_API_Excel$meta$bestRating[i] <- ex_parsed_json[[1]][["reviewRating"]][["bestRating"]]},
           error=function(e) {Correctiv_API_Excel$meta$bestRating[i] <<- ex_parsed_json[["reviewRating"]][["bestRating"]]}))
  try(tryCatch({Correctiv_API_Excel$meta$worstRating[i] <- ex_parsed_json[[1]][["reviewRating"]][["worstRating"]]},
           error=function(e) {Correctiv_API_Excel$meta$worstRating[i] <<- ex_parsed_json[["reviewRating"]][["worstRating"]]}))
  if(is.na(Correctiv_API_Excel$meta$reviewDate[i])){tryCatch({Correctiv_API_Excel$meta$reviewDate[i] <- ex_parsed_json[[1]][["datePublished"]]},
                                                                 error=function(e) {Correctiv_API_Excel$meta$reviewDate[i] <<- ex_parsed_json[["datePublished"]]})}
  Correctiv_API_Excel$meta$reviewDate[i] <- str_remove_all(Correctiv_API_Excel$meta$reviewDate[i], "(T.*)")
  Correctiv_API_Excel$meta$reviewDate[i] <- as.character(as.Date(Correctiv_API_Excel$meta$reviewDate[i]))
  
    
  if(is.na(Correctiv_API_Excel$meta$textualRating[i])){try(tryCatch({Correctiv_API_Excel$meta$textualRating[i] <- ex_parsed_json[[1]][["reviewRating"]][["alternateName"]]},
                                                             error=function(e) {Correctiv_API_Excel$meta$textualRating[i]<<- ex_parsed_json[["reviewRating"]][["alternateName"]]}))}  
  
  if(is.na(Correctiv_API_Excel$meta$claimant[i]))try({tryCatch({Correctiv_API_Excel$meta$claimant[i]   <- ex_parsed_json[[1]][["itemReviewed"]][["author"]][["name"]]},
                                                            error=function(e) {Correctiv_API_Excel$meta$claimant[i]   <<- ex_parsed_json[["itemReviewed"]][["author"]][["name"]]})})  
    
    
  if(is.na(Correctiv_API_Excel$meta$claimDate[i])){tryCatch({Correctiv_API_Excel$meta$claimDate[i]  <- ex_parsed_json[[1]][["datePublished"]]},
                                                            error=function(e) {Correctiv_API_Excel$meta$claimDate[i]  <<- ex_parsed_json[["datePublished"]]})}
  Correctiv_API_Excel$meta$claimDate[i] <- str_remove_all(Correctiv_API_Excel$meta$claimDate[i], "(T.*)")
  Correctiv_API_Excel$meta$claimDate[i] <- as.character(as.Date(Correctiv_API_Excel$meta$claimDate[i]))
  
  # get missing excel data values
  if(is.na(Correctiv_API_Excel$meta$claimTitle[i])) try(Correctiv_API_Excel$meta$claimTitle[i]  <- article %>% html_elements('h1') %>% html_text() )
  if(is.na(Correctiv_API_Excel$meta$claim[i])) {tryCatch({Correctiv_API_Excel$meta$claim[i]  <- ex_parsed_json[[1]][["claimReviewed"]]} ,
                                                         error=function(e) {Correctiv_API_Excel$meta$claim[i]  <<- ex_parsed_json[["claimReviewed"]]})}
  #define ids
  id <- paste0("Correctiv_", as.character(Correctiv_API_Excel$meta$reviewDate[i]),"-",str_sub(text, 7,8), str_sub(text, 11,13), nchar(Correctiv_API_Excel$text[i]))
  id <- str_replace_all(id, "[:blank:]", "§")
  id <- str_replace_all(id, '"', '_')
  id <- str_replace_all(id, "'", "'_")
  Correctiv_API_Excel$meta[i,1] <- id
  names(Correctiv_API_Excel$text)[i] <-id
  names(Correctiv_API_Excel$links)[i] <-id

}

## fill the missing data for the error ids (problem: the ClaimReview json is called "claim" not "claimreview")
Correctiv_API_Excel$text[error_ids]
for ( i in error_ids){
  print(i)
  article <- read_html(Correctiv_API_Excel$meta$url[i])
  
  text <- article %>% html_elements('div.detail__wrapper') %>% html_elements('p, li') %>% html_text()
  text_log <- !str_detect(text, "\\(function\\(\\)|Unterstützen Sie unabhängigen Journalismus!|E-Mail-Adresse\\r\\n|Ihre Spende gegen Fake News|IFCN.*des US-amerikanischen Poynter Instituts|Faktenchecks per Mail|Verschwörungsmythen spalten die Gesellschaft|CORRECTIV ist spendenfinanziert|Unterstützen Sie uns dabei|Wir recherchieren zu Missständen in der Gesellschaft|Fördern auch Sie unsere Arbeit")   #finds and deletes funraising appeals and info boxes
  text <- text[text_log]
  text <- str_remove_all(text,"\\n|\\r")
  text <- paste(text, collapse = " ")
  text <- str_replace_all(text, "Redigatur:.*","")
  text <- str_squish(text)
  Correctiv_API_Excel$text[i] <- text
  links <- article %>% html_elements('div.detail__wrapper') %>% html_elements('p, li') %>% html_elements('a') %>% html_attr("href")
  links <- head(links, -5)
  Correctiv_API_Excel$links[i] <- list(links)
  
  claim <- article %>% html_elements("script")  %>% html_text() %>% str_subset("claim")
  if(is_empty(claim)) {
    
  }
  raw_json_embed <- claim %>%
    str_remove_all("\\n|\\t")
  try(tryCatch({ex_parsed_json <- jsonlite::parse_json(raw_json_embed)},
               error=function(e){ex_parsed_json <<- jsonlite::parse_json(raw_json_embed[2])}))
  try({tryCatch({Correctiv_API_Excel$meta$claimURL[i]<- ex_parsed_json[["itemReviewed"]][["url"]]},
                error=function(e) {Correctiv_API_Excel$meta$claimURL[i] <<-ex_parsed_json[[1]][["itemReviewed"]][["appearance"]][[1]][["url"]]})
  } 
  )
  try(tryCatch({Correctiv_API_Excel$meta$ratingValue[i] <- ex_parsed_json[[1]][["reviewRating"]][["ratingValue"]]},
               error=function(e) {Correctiv_API_Excel$meta$ratingValue[i] <<- ex_parsed_json[["reviewRating"]][["ratingValue"]]}))
  try(tryCatch({Correctiv_API_Excel$meta$bestRating[i] <- ex_parsed_json[[1]][["reviewRating"]][["bestRating"]]},
               error=function(e) {Correctiv_API_Excel$meta$bestRating[i] <<- ex_parsed_json[["reviewRating"]][["bestRating"]]}))
  try(tryCatch({Correctiv_API_Excel$meta$worstRating[i] <- ex_parsed_json[[1]][["reviewRating"]][["worstRating"]]},
               error=function(e) {Correctiv_API_Excel$meta$worstRating[i] <<- ex_parsed_json[["reviewRating"]][["worstRating"]]}))
  if(is.na(Correctiv_API_Excel$meta$reviewDate[i])){tryCatch({Correctiv_API_Excel$meta$reviewDate[i] <- ex_parsed_json[[1]][["datePublished"]]},
                                                             error=function(e) {Correctiv_API_Excel$meta$reviewDate[i] <<- ex_parsed_json[["datePublished"]]})}
  Correctiv_API_Excel$meta$reviewDate[i] <- str_remove_all(Correctiv_API_Excel$meta$reviewDate[i], "(T.*)")
  Correctiv_API_Excel$meta$reviewDate[i] <- as.character(as.Date(Correctiv_API_Excel$meta$reviewDate[i]))
  
  
  if(is.na(Correctiv_API_Excel$meta$textualRating[i])){try(tryCatch({Correctiv_API_Excel$meta$textualRating[i] <- ex_parsed_json[[1]][["reviewRating"]][["alternateName"]]},
                                                                    error=function(e) {Correctiv_API_Excel$meta$textualRating[i]<<- ex_parsed_json[["reviewRating"]][["alternateName"]]}))}  
  
  if(is.na(Correctiv_API_Excel$meta$claimant[i]))try({tryCatch({Correctiv_API_Excel$meta$claimant[i]   <- ex_parsed_json[[1]][["itemReviewed"]][["author"]][["name"]]},
                                                               error=function(e) {Correctiv_API_Excel$meta$claimant[i]   <<- ex_parsed_json[["itemReviewed"]][["author"]][["name"]]})})  
  
  
  if(is.na(Correctiv_API_Excel$meta$claimDate[i])){tryCatch({Correctiv_API_Excel$meta$claimDate[i]  <- ex_parsed_json[[1]][["datePublished"]]},
                                                            error=function(e) {Correctiv_API_Excel$meta$claimDate[i]  <<- ex_parsed_json[["datePublished"]]})}
  Correctiv_API_Excel$meta$claimDate[i] <- str_remove_all(Correctiv_API_Excel$meta$claimDate[i], "(T.*)")
  Correctiv_API_Excel$meta$claimDate[i] <- as.character(as.Date(Correctiv_API_Excel$meta$claimDate[i]))
  
  # get missing excel data values
  if(is.na(Correctiv_API_Excel$meta$claimTitle[i])) try(Correctiv_API_Excel$meta$claimTitle[i]  <- article %>% html_elements('h1') %>% html_text() )
  if(is.na(Correctiv_API_Excel$meta$claim[i])) {tryCatch({Correctiv_API_Excel$meta$claim[i]  <- ex_parsed_json[[1]][["claimReviewed"]]} ,
                                                         error=function(e) {Correctiv_API_Excel$meta$claim[i]  <<- ex_parsed_json[["claimReviewed"]]})}
  #define ids
  id <- paste0("Correctiv_", as.character(Correctiv_API_Excel$meta$reviewDate[i]),"-",str_sub(text, 7,8), str_sub(text, 11,13), nchar(Correctiv_API_Excel$text[i]))
  id <- str_replace_all(id, "[:blank:]", "§")
  id <- str_replace_all(id, '"', '_')
  id <- str_replace_all(id, "'", "'_")
  Correctiv_API_Excel$meta[i,1] <- id
  names(Correctiv_API_Excel$text)[i] <-id
  names(Correctiv_API_Excel$links)[i] <-id
  
}

saveRDS(Correctiv_API_Excel, "Correctiv_API_Excel.rds")

## Clean the data
Correctiv_API_Excel$meta$reviewDate <- as.Date(Correctiv_API_Excel$meta$reviewDate)
Correctiv_API_Excel$meta$claimDate <- as.Date(Correctiv_API_Excel$meta$claimDate)
duplicated_text <- which(duplicated(Correctiv_API_Excel$text))
Correctiv_API_Excel$meta <-Correctiv_API_Excel$meta[-duplicated_text,]
Correctiv_API_Excel$text <-Correctiv_API_Excel$text[-duplicated_text]
Correctiv_API_Excel$links <-Correctiv_API_Excel$links[-duplicated_text]

##
dates_to_delete <- which(Correctiv_API_Excel$meta$reviewDate > "2023-01-31")
Correctiv_API_Excel$meta <-Correctiv_API_Excel$meta[-dates_to_delete,]
Correctiv_API_Excel$text <-Correctiv_API_Excel$text[-dates_to_delete]
Correctiv_API_Excel$links <-Correctiv_API_Excel$links[-dates_to_delete]

saveRDS(Correctiv_API_Excel, "Correctiv_API_Excel.rds")
