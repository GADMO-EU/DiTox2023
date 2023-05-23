library(ldaPrototype)
library(tosca)
library(lubridate)
library(data.table)
library(ggplot2)
library(ggdendro)
library(grid)
library(zoo)

obj = readRDS(file.path("data", "obj.rds"))
docs = readRDS(file.path("data", "docs.rds"))
vocab = readRDS(file.path("data", "vocab.rds"))

dir.create("analysis")

for(K in c(12)){
  dir.create(file.path("analysis", K))
  proto = readRDS(file.path("proto", paste0(K, ".rds")))
  K = getK(getLDA(proto))
  labels = paste0("Topic", 1:K)
  labels = fread("Labels_K12_22_01_31.csv")$"Deutsches Label"
  labels = fread("Labels_K12_22_01_31.csv")$"Englisches Label"
  label_order = c(12,1,9,8,2,10,5,4,11,3,6,7)
  col_order = c(1,2,3,8,5,6,12,4,9,10,11,7)
  
  # helper
  dates = obj$meta$date[match(names(docs), obj$meta$id)]
  daily = seq(as.Date("2018-01-01"), as.Date("2023-01-31"), "day")
  medium = obj$meta$resource[match(names(docs), obj$meta$id)]
  score = obj$meta$ratingValue[match(names(docs), obj$meta$id)]
  score[is.na(score)] = "NA"
  score[score == ""] = "..."
  months = floor_date(dates, "month")
  xmonth = sort(unique(months))
  
  #topWords global
  topwords = topWords(getTopics(getLDA(proto)), 200)
  prop = round(rowSums(getTopics(getLDA(proto))) / sum(getTopics(getLDA(proto))) * 100, 4)
  out = rbind(prop, topwords)
  colnames(out) = labels
  row.names(out) = c("Proportion (%)", 1:200)
  write.csv(out, file = file.path("analysis", K, "topwords.csv"), fileEncoding = "UTF-8")
  
  #topwords per year
  topics = lapply(xmonth, function(x){
    tmp = table(factor(unlist(getAssignments(getLDA(proto))[months == x])+1, levels = 1:getK(getLDA(proto))), 
                factor(unlist(lapply(docs[months == x], function(y) y[1,]))+1,
                       levels = seq_len(length(vocab))))
    tmp = matrix(as.integer(tmp), nrow = getK(getLDA(proto)))
    colnames(tmp) = vocab
    tmp
  })
  topics_abs = do.call(rbind, lapply(topics, rowSums))
  topics_rel = topics_abs/rowSums(topics_abs)
  topwords = lapply(topics, topWords, numWords = 100)
  topwords = lapply(1:K, function(x) sapply(seq_along(topwords), function(t) topwords[[t]][,x]))
  dir.create(file.path("analysis", K, "topWordsPerYear"))
  for(i in 1:K){
    out = rbind(round(topics_rel[, i] * 100, 4),
                topwords[[i]])
    colnames(out) = as.character(xmonth)
    row.names(out) = c("Share (in %) on corpus", 1:100)
    write.csv(out,
              file = file.path(file.path("analysis", K, "topWordsPerYear"), paste0(labels[i], ".csv")),
              fileEncoding = "UTF-8")
  }
  colnames(topics_abs) = labels
  rownames(topics_abs) = as.character(xmonth)
  write.csv(topics_abs, file = file.path("analysis", K, "topics_abs_month.csv"), fileEncoding = "UTF-8")
  colnames(topics_rel) = labels
  rownames(topics_rel) = as.character(xmonth)
  write.csv(topics_rel, file = file.path("analysis", K, "topics_rel_month.csv"), fileEncoding = "UTF-8")
  
  # corpus viz
  freq = rbindlist(lapply(daily, function(x){
    rbindlist(lapply(unique(medium), function(y){
      ind = dates == x & medium == y
      docs = sum(ind)
      words = sum(lengths(getAssignments(getLDA(proto))[ind]))
      data.table(date = x, medium = y, docs = docs, words = words)
    }))}))
  
  topics = rbindlist(lapply(daily, function(x){
    rbindlist(lapply(unique(medium), function(y){
      value = tabulate(unlist(getAssignments(getLDA(proto))[dates == x & medium == y])+1, nbins = K)
      data.table(date = x, medium = y, topic = 1:K, value = value)
    }))}))
  
  freq[, docs_90 := rollsum(docs, 90, fill = NA), by = medium]
  freq[, words_90 := rollsum(words, 90, fill = NA), by = medium]
  freq[, rel_90 := words_90/docs_90]
  freq[is.nan(rel_90), rel_90 := 0]
  
  corpus_composition = ggplot(freq) + aes(x = date, y = words_90, fill = medium) +
    geom_bar(position = "fill", stat = "identity") +
    xlab("") + ylab("Word Share per Medium (per Quarter)") +
    ggtitle("Fact Check Corpus Composition") + labs(fill = "Medium")
  
  number_docs = ggplot(freq) + aes(x = date, y = docs_90) + geom_line() +
    facet_wrap(~medium) + xlab("") + ylab("Number of Fact Checks (per Quarter)") +
    ggtitle("Fact Checks per Organization")
  number_words = ggplot(freq) + aes(x = date, y = words_90) + geom_line() +
    facet_wrap(~medium) + xlab("") + ylab("Number of Words (per Quarter)") +
    ggtitle("Words in Fact Checks per Organization")
  docs_length = ggplot(freq) + aes(x = date, y = rel_90) + geom_line() +
    facet_wrap(~medium) + xlab("") + ylab("Number of Words per Fact Check (per Quarter)") +
    ggtitle("Fact Check Lenghts per Organization")
  
  pdf(file.path("analysis", K, "Corpus.pdf"), width = 12, height = 8)
  corpus_composition
  number_docs
  number_words
  docs_length
  dev.off()
  
  # topic viz
  topics[, value_90 := rollsum(value, 90, fill = NA), by = c("topic", "medium")]
  topics[, label := factor(labels[topic], levels = labels[label_order])]
  
  topics_ges = ggplot(topics) + aes(x = date, y = value_90, fill = label) +
    geom_bar(position = "fill", stat = "identity") +
    xlab("") + ylab("Topic Share (per Quarter)") +
    ggtitle("Fact Check Topics") + labs(fill = "Topic") +
    scale_fill_hue(limits = labels[col_order],
                   labels = labels[label_order],
                   breaks = labels[label_order])
  
  topics_medium = ggplot(topics) + aes(x = date, y = value_90, fill = label) +
    geom_bar(position = "fill", stat = "identity") +
    xlab("") + ylab("Topic Share (per Quarter)") + 
    ggtitle("Fact Check Topics per Organization") + labs(fill = "Topic") +
    facet_wrap(~medium) +
    scale_fill_hue(limits = labels[col_order],
                   labels = labels[label_order],
                   breaks = labels[label_order])
  
  pdf(file.path("analysis", K, "Topics.pdf"), width = 12, height = 8)
  topics_ges
  topics_medium
  dev.off()
  
  # co-occurrence analysis
  docs_sub = getDocument_sums(getLDA(proto))[, dates >= "2018-01-01" & dates < "2023-02-01"]
  doc_ids_sub = names(docs)[dates >= "2018-01-01" & dates < "2023-02-01"]
  medium_sub = medium[dates >= "2018-01-01" & dates < "2023-02-01"]
  tmp = apply(docs_sub, 2, function(x) sort(x, decreasing = TRUE)/sum(x))
  topics_ranks = rbindlist(lapply(seq_len(nrow(tmp)), function(i)
    data.table(Rank = i, Share = tmp[i,])))
  topics_ranks[, Rank := as.factor(Rank)]
  topic_rank_shares = ggplot(topics_ranks) + aes(x = Rank, y = Share) +
    geom_violin(scale = "width", fill = "grey") + ggtitle("Share of Top Topics")
  
  docs_count = rbindlist(lapply(seq_along(doc_ids_sub), function(i)
    data.table(id = doc_ids_sub[i], medium = medium_sub[i],
               topic = seq_len(K), count = docs_sub[,i])))
  docs_count[, label := factor(labels[topic], levels = labels[label_order])]
  docs_count[, rel := count/sum(count), by = id]
  docs_count[, dom := rel > 0.5]
  docs_count[, dom_topic := topic[dom], by = id]
  docs_count[, dom_label := factor(labels[dom_topic], levels = labels[label_order])]
  docs_count[, coocc := count/max(count), by = id]
  
  docs_count_dom = docs_count[, .(count = sum(count)), by = c("dom", "label")]
  docs_count_dom[, rel := count/sum(count), by = label]
  top_topic_abs = ggplot(docs_count_dom) + aes(x = label, y = count, fill = dom) +
    geom_bar(stat = "identity", position = "dodge") + 
    xlab("Topic") + ylab("Frequency") + labs(fill = "Top Topic") +
    ggtitle("Frequency of Topic Assignments as Top Topic") +
    theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust = 1))
  top_topic_rel = ggplot(docs_count_dom) + aes(x = label, y = rel, fill = dom) +
    geom_bar(stat = "identity") + 
    xlab("Topic") + ylab("Share") + labs(fill = "Top Topic") +
    ggtitle("Share of Topic Assignments as Top Topic") +
    theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust = 1))
  
  cooccurrence_ges = docs_count[, .(coocc = mean(coocc)), by = c("label", "dom_label")]
  coocc_matrix = ggplot(cooccurrence_ges[label != dom_label | is.na(dom_label)]) +
    aes(x = dom_label, y = label, fill = coocc) +
    geom_tile() + scale_fill_viridis_c() +
    xlab("Dominant Topic") + ylab("Co-occurring Topic") + labs(fill = "Mean Share") +
    ggtitle("Share of Co-occurring Topics (Doument-level based)") +
    theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust = 1))
  cooccurrence_medium = docs_count[, .(coocc = mean(coocc)), by = c("label", "dom_label", "medium")]
  coocc_matrix_medium = ggplot(cooccurrence_medium[label != dom_label | is.na(dom_label)]) +
    aes(x = dom_label, y = label, fill = coocc) +
    geom_tile() + facet_wrap(~medium) + scale_fill_viridis_c() +
    xlab("Dominant Topic") + ylab("Co-occurring Topic") + labs(fill = "Mean Share") +
    ggtitle("Share of Co-occurring Topics per Organization (Doument-level based)") +
    theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust = 1))
  
  cooccurrence_assignments = docs_count[, .(count = sum(count)), by = c("label", "dom_label")]
  cooccurrence_assignments_wo_self = cooccurrence_assignments[dom_label != label | is.na(dom_label)]
  cooccurrence_assignments_wo_self[, rel := count/sum(count), by = "dom_label"]
  cooccurrence_assignments[, rel := count/sum(count), by = "dom_label"]
  cooccurrence_assignments[label == dom_label, label := NA]
  coocc_share = ggplot(cooccurrence_assignments) +
    aes(x = dom_label, fill = label, y = rel) +
    geom_bar(stat = "identity") +
    xlab("Dominant Topic") + ylab("Share") + labs(fill = "Co-occuring Topic") +
    ggtitle("Share of Co-occurring Topics (Assignment-level based, NA = self)") +
    scale_fill_hue(limits = labels[col_order],
                   labels = labels[label_order],
                   breaks = labels[label_order]) +
    theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust = 1))
  coocc_share_wo_self = ggplot(cooccurrence_assignments_wo_self) +
    aes(x = dom_label, fill = label, y = rel) +
    geom_bar(stat = "identity") +
    xlab("Dominant Topic") + ylab("Share") + labs(fill = "Co-occuring Topic") +
    ggtitle("Share of Co-occurring Topics (Assignment-level based, w/o self)") +
    scale_fill_hue(limits = labels[col_order],
                   labels = labels[label_order],
                   breaks = labels[label_order]) +
    theme(axis.text.x = element_text(angle = 30, vjust = 1, hjust = 1))
  
  pdf(file.path("analysis", K, "Cooccurrence.pdf"), width = 12, height = 8)
  topic_rank_shares
  top_topic_abs
  top_topic_rel
  coocc_matrix
  coocc_matrix_medium
  coocc_share
  coocc_share_wo_self
  dev.off()
  
  # score analysis
  topics = rbindlist(lapply(daily, function(x){
    rbindlist(lapply(c("AFP", "Correctiv"), function(y){
      rbindlist(lapply(unique(score), function(s){
        value = tabulate(unlist(getAssignments(getLDA(proto))[dates == x & medium == y & score == s])+1, nbins = K)
        data.table(date = x, medium = y, topic = 1:K, score = s, value = value)
      }))}))}))
  topics[, value_90 := rollsum(value, 90, fill = NA), by = c("topic", "medium", "score")]
  topics[, label := factor(labels[topic], levels = labels[label_order])]
  
  scores_correctiv = ggplot(topics[medium == "Correctiv"]) +
    aes(x = date, y = value_90, fill = score) +
    geom_bar(position = "fill", stat = "identity") +
    xlab("") + ylab("Share") + labs(fill = "Rating") +
    ggtitle("Correctiv Ratings")
  scores_topic_correctiv = ggplot(topics[medium == "Correctiv"]) +
    aes(x = date, y = value_90, fill = score) +
    geom_bar(position = "fill", stat = "identity") +
    facet_wrap(~label) + xlab("") + ylab("Share") + labs(fill = "Rating") +
    ggtitle("Correctiv Ratings per Topic")
  scores_afp = ggplot(topics[medium == "AFP" & score %in% c(1:3, "NA")]) +
    aes(x = date, y = value_90, fill = score) +
    geom_bar(position = "fill", stat = "identity") +
    xlab("") + ylab("Share") + labs(fill = "Rating") +
    ggtitle("AFP Ratings")
  scores_topic_afp = ggplot(topics[medium == "AFP" & score %in% c(1:3, "NA")]) +
    aes(x = date, y = value_90, fill = score) +
    geom_bar(position = "fill", stat = "identity") +
    facet_wrap(~label) + xlab("") + ylab("Share") + labs(fill = "Rating") +
    ggtitle("AFP Ratings per Topic")
  
  pdf(file.path("analysis", K, "Scores.pdf"), width = 12, height = 8)
  scores_correctiv
  scores_topic_correctiv
  scores_afp
  scores_topic_afp
  dev.off()
}
