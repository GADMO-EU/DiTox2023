library(ldaPrototype)
library(tosca)
library(data.table)
library(tm)

l = list()
for(medium in c("AFP", "APA", "Correctiv_API_Excel", "DPA_all")){
  obj = readRDS(file.path("Daten_2023-01-31", paste0(medium, ".rds")))
  obj$meta$title = NA_character_
  obj$meta$date = obj$meta$reviewDate
  obj$meta$resource = rep(gsub("_(.?)*", "", medium), nrow(obj$meta))
  l[[medium]] = textmeta(text = obj$text, meta = obj$meta)
}
obj = mergeTextmeta(l)

saveRDS(obj, file.path("data", "obj.rds"))

texts = removeUmlauts(obj$text)
sw = c(stopwords("german"), "dass", "fuer",  "koennen", "koennte", "ueber",
       "waehrend", "wuerde", "wuerden")
texts = sapply(texts, function(x) gsub("\u00AD", " ", x))
texts = sapply(texts, function(x) gsub("\u00A0", " ", x))
texts = sapply(texts, function(x) gsub("\u200B", " ", x))

texts = trimws(stripWhitespace(
  removeWords(
    removeNumbers(
      removePunctuation(
        removePunctuation(
          removeWords(
            tolower(texts), sw)), ucp = TRUE)), sw)))

texts = sapply(texts, function(x) strsplit(x, "\\s")[1])
texts = sapply(texts, function(x) x[nchar(x) > 1])

vocab = makeWordlist(texts)
vocab = vocab$words[vocab$wordtable > 4]
docs = LDAprep(texts, vocab)

saveRDS(docs, file.path("data", "docs.rds"))
saveRDS(vocab, file.path("data", "vocab.rds"))
