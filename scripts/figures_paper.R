library(ldaPrototype)
library(tosca)
library(lubridate)
library(data.table)
library(ggplot2)
library(grid)
library(zoo)
library(xtable)

obj = readRDS(file.path("data", "obj.rds"))
source(file.path("scripts", "correctiv_score.R"))
source(file.path("scripts", "afp_score.R"))
docs = readRDS(file.path("data", "docs.rds"))
vocab = readRDS(file.path("data", "vocab.rds"))

K = 12

proto = readRDS(file.path("proto", paste0(K, ".rds")))
K = getK(getLDA(proto))
labels = paste0("Topic", 1:K)
labels = fread("Labels_K12_22_01_31.csv")$"Deutsches Label"
labels = fread("Labels_K12_22_01_31.csv")$"Englisches Label"
label_order = c(12,1,9,8,2,10,5,4,11,3,6,7)
col_order = c(1,2,3,8,6,5,12,4,9,10,11,7)

# helper
dates = obj$meta$date[match(names(docs), obj$meta$id)]
daily = seq(as.Date("2018-01-01"), as.Date("2023-01-31"), "day")
domains = obj$meta$BaseClaim[match(names(docs), obj$meta$id)]
medium = obj$meta$resource[match(names(docs), obj$meta$id)]
medium[medium == "Correctiv"] = "CORRECTIV"
medium[medium == "DPA"] = "dpa"
score = obj$meta$ratingValue_korrigiert[match(names(docs), obj$meta$id)]
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

#topwords per year
topics = lapply(xmonth, function(x){
  tmp = table(factor(unlist(getAssignments(getLDA(proto))[months == x])+1, levels = 1:getK(getLDA(proto))), 
              factor(unlist(lapply(docs[months == x], function(y) y[1,]))+1,
                     levels = seq_len(length(vocab))))
  tmp = matrix(as.integer(tmp), nrow = getK(getLDA(proto)))
  colnames(tmp) = vocab
  tmp
})

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

# table of docs and words after preproc
freq[,halfyear := lubridate::floor_date(date, unit = "halfyear")]
freq_tab = freq[, .(docs = sum(docs), words = sum(words), mean_length = sum(words)/sum(docs)),
                by = c("medium", "halfyear")]
freq_tab = freq_tab[halfyear > "2019-06-01"]
freq_tab_2 = merge(merge(dcast(freq_tab, halfyear ~ medium, value.var = "docs"),
                         dcast(freq_tab, halfyear ~ medium, value.var = "words"),
                         suffixes = c(".docs", ".words")),
                   dcast(freq_tab, halfyear ~ medium, value.var = "mean_length"))
freq_tab_2[, halfyear := as.character(halfyear)]
print(xtable(freq_tab_2[, c(1,2,6,10,3,7,11,4,8,12,5,9,13)], digits = 0), include.rownames = FALSE)

tmp = freq[, .(docs = sum(docs), words = sum(words), mean_length = sum(words)/sum(docs)), by = medium]

# global count of docs and mean length 
sum(tmp$docs) # -> 5229
sum(tmp$words)/sum(tmp$docs) # -> 281
# add share info to text
round(tmp$docs/sum(tmp$docs)*100, 2)
# 14.53  4.59 38.90 41.98
round(tmp$words/sum(tmp$words)*100, 2)
# 24.65  5.57 41.28 28.50

tmp2 = data.table(Organization = c("AFP", "APA", "CORRECTIV", "dpa", "AFP", "APA", "CORRECTIV", "dpa"),
                  Type = rep(c("Documents", "Words"), each = 4),
                  Share = c(tmp$docs/sum(tmp$docs), tmp$words/sum(tmp$words)))
ggplot(tmp2) + aes(x = Type, y = Share, fill = Organization, label = paste0(round(Share*100,2),"%")) +
  geom_bar(stat = "identity") + geom_text(position = position_stack(vjust =  0.5)) + xlab("")
ggsave(file.path("paper", "share.eps"), width = 9, height = 6, units = "cm")


# topic:
# cumulated sum of the number of assigned words to a topic
# using centered rolling window of 90 days
ggplot(topics[date >= "2019-06-01"]) + aes(x = date, y = label, fill = value_90) +
  geom_tile() + scale_fill_viridis_c(option = "turbo") +
  xlab("") + ylab("Topic") + labs(fill = "# 90 days") + scale_y_discrete(limits=rev)
ggsave(file.path("paper", "heat.eps"), width = 20, height = 7, units = "cm")

topics[, value_90_stand := value_90/max(value_90, na.rm = TRUE), by = medium]
ggplot(topics[date >= "2019-06-01"]) + aes(x = date, y = label, fill = value_90_stand) +
  geom_tile() + scale_fill_viridis_c(option = "turbo") +
  xlab("") + ylab("Topic") + labs(fill = "# 90 days") + scale_y_discrete(limits=rev) +
  facet_wrap(~medium) + theme(legend.position = "n") +
  theme(axis.text.y=element_text(size=rel(0.9)))
ggsave(file.path("paper", "heat_org.eps"), width = 20, height = 10, units = "cm")

# cooc topic
docs_sub = getDocument_sums(getLDA(proto))[, dates >= "2019-06-01" & dates < "2023-02-01"]
doc_ids_sub = names(docs)[dates >= "2019-06-01" & dates < "2023-02-01"]
medium_sub = medium[dates >= "2019-06-01" & dates < "2023-02-01"]
docs_count = rbindlist(lapply(seq_along(doc_ids_sub), function(i)
  data.table(id = doc_ids_sub[i], medium = medium_sub[i],
             topic = seq_len(K), count = docs_sub[,i])))
docs_count[, label := factor(labels[topic], levels = labels[label_order])]
docs_count[, rel := count/sum(count), by = id]
docs_count[, dom := rel > 0.5]
docs_count[, dom_topic := topic[dom], by = id]
docs_count[, dom_label := factor(labels[dom_topic], levels = labels[label_order])]
docs_count[, coocc := count/max(count), by = id]
cooccurrence_assignments = docs_count[, .(count = sum(count)), by = c("label", "dom_label")]
cooccurrence_assignments_wo_self = cooccurrence_assignments[dom_label != label | is.na(dom_label)]
cooccurrence_assignments_wo_self[, rel := count/sum(count), by = "dom_label"]

tab1 = docs_count[dom == TRUE]
tab1[, medium := "Total"]
tab1 = rbind(tab1, docs_count[dom == TRUE])
tab1 = tab1[, .N, by = c("label", "medium")]
tab1[, Share := N/sum(N), by = medium]

ggplot(tab1) + aes(x = medium, y = Share, fill = label) +
  geom_bar(stat = "identity", position = position_fill(reverse = TRUE)) +
  coord_flip() + scale_x_discrete(limits=rev) +
  xlab("Organization") + ylab("Share") + theme(legend.position = "n") +
  scale_fill_manual(values= RColorBrewer::brewer.pal(12, "Set3"))
#  scale_fill_hue(limits = labels[col_order],
#                 labels = labels[label_order],
#                 breaks = labels[label_order])
ggsave(file.path("paper", "topic_share.eps"), width = 9.6, height = 3.4, units = "cm")

tab2 = copy(docs_count)
tab2[, medium := "Total"]
tab2 = rbind(tab2, docs_count)
tab2 = tab2[, .(N = sum(count)), by = c("label", "medium")]
tab2[, Share := N/sum(N), by = medium]

ggplot(tab2) + aes(x = medium, y = Share, fill = label) +
  geom_bar(stat = "identity", position = position_fill(reverse = TRUE)) +
  coord_flip() + scale_x_discrete(limits=rev) +
  xlab("Organization") + ylab("Share") + theme(legend.position = "n") +
  scale_fill_manual(values= RColorBrewer::brewer.pal(12, "Set3"))
#  scale_fill_hue(limits = labels[col_order],
#                 labels = labels[label_order],
#                 breaks = labels[label_order])
ggsave(file.path("paper", "topic_share.eps"), width = 9.6, height = 3.4, units = "cm")

# Share of Co-occurring Topics (Assignment-level based, w/o self)
# based on fact checks from June 2019 until January 2023
ggplot(cooccurrence_assignments_wo_self) +
  aes(x = dom_label, fill = label, y = rel) +
  geom_bar(stat = "identity") +
  xlab("Dominant Topic") + ylab("Share") + labs(fill = "Co-occuring Topic") +
  scale_fill_manual(values= RColorBrewer::brewer.pal(12, "Set3")) +
#  scale_fill_hue(limits = labels[col_order],
#                 labels = labels[label_order],
#                 breaks = labels[label_order]) +
  theme(axis.text.x = element_text(angle=30, vjust=1, hjust=1, size=rel(0.8)),
        legend.justification="top")
ggsave(file.path("paper", "cooc.eps"), width = 20, height = 8.5, units = "cm")


# score analysis
topics = rbindlist(lapply(daily, function(x){
  rbindlist(lapply(c("AFP", "CORRECTIV"), function(y){
    rbindlist(lapply(unique(score), function(s){
      value = tabulate(unlist(getAssignments(getLDA(proto))[dates == x & medium == y & score == s])+1, nbins = K)
      data.table(date = x, medium = y, topic = 1:K, rating = s, value = value)
    }))}))}))
topics[, value_90 := rollsum(value, 90, fill = NA), by = c("topic", "medium", "rating")]
topics[, label := factor(labels[topic], levels = labels[label_order])]

tmp = topics[, .(N = sum(value)), by = c("medium", "label", "rating")]
tmp[, rel := N/sum(N), by = c("medium", "label")]
ggplot(tmp[medium == "AFP" & N > 0]) +
  aes(x = label, y = rel, fill = rating) +
  geom_bar(stat = "identity", position = position_fill(reverse = TRUE)) + 
  xlab("Topic") + ylab("Share") + labs(fill = "Rating") + coord_flip() +
  scale_x_discrete(limits=rev) +
  theme(axis.text.y=element_text(size=rel(0.9)), legend.position="top",
        legend.justification="right") +
  scale_fill_manual(values=c("red", "orange", "#fbe870", "grey50"))
ggsave(file.path("paper", "rating_afp.eps"), width = 9.6, height = 6, units = "cm")

ggplot(topics[medium == "CORRECTIV"]) +
  aes(x = date, y = value_90, fill = rating, width = 1) +
  geom_bar(position = "fill", stat = "identity") +
  facet_wrap(~label) + xlab("") + ylab("Share") + labs(fill = "Rating") +
  theme(legend.position="top",
        strip.text = element_text(size=rel(0.7))) +
  guides(fill = guide_legend(nrow = 1)) +
  scale_fill_manual(values=c("#a52019", "red", "orange", "#fbe870",
                             "#86ceec", "#027b76", "#68b300" ,"grey50"))
ggsave(file.path("paper", "rating_correctiv.eps"), width = 20, height = 12, units = "cm")


### domain

a = table(obj$meta$BaseClaim, obj$meta$resource)
top10_domains = names(head(sort(table(obj$meta$BaseClaim), decreasing = TRUE), 11))
top5_domains = names(head(sort(table(obj$meta$BaseClaim), decreasing = TRUE), 5))
a[top10_domains,]
colSums(a[!rownames(a) %in% top10_domains,])

tab = rbindlist(lapply(xmonth, function(x){
  rbindlist(lapply(unique(medium), function(y){
    ind = months == x & medium == y
    value = sapply(top5_domains, function(z) sum(domains[ind] == z, na.rm = TRUE))
    data.table(month = x, medium = y,
               domain = c(top5_domains, "Other", NA),
               value = c(value,
                         sum(!domains[ind] %in% top5_domains, na.rm = TRUE),
                         sum(is.na(domains[ind]))))
  }))}))


ggplot(tab[month >= "2019-06-01",]) +
  aes(x = month, y = value, fill = domain) +
  geom_bar(stat = "identity") +
  facet_wrap(~medium, scales = "free") + xlab("") + ylab("Share") + labs(fill = "Domain")

#facebook: seit Anfang 2019 mit DPA, seit 2020 mit AFP, seit ?? mit Correctiv
ggplot(tab[month >= "2018-01-01",]) +
  aes(x = month, y = value, fill = domain) +
  geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) +
  facet_wrap(~medium, scales = "free") + xlab("") +
  ylab("Number of Fact-Checks") + labs(fill = "Domain") +
  theme(legend.position="top") +
  guides(fill = guide_legend(nrow = 1))
ggsave(file.path("paper", "domain.eps"), width = 20, height = 10, units = "cm")

###

domains2 = domains
domains2[!domains %in% top5_domains] = "Other"
domains2[is.na(domains)] = "NA"
domains3 = domains
domains3[!domains %in% top10_domains] = "Other"
domains3[is.na(domains)] = "NA"

tab = rbindlist(lapply(unique(domains2), function(d){
  data.table(domain = d, topic = 1:12, 
             value = rowSums(getDocument_sums(getLDA(proto))[, domains2 == d, drop=FALSE]))
}))
tab[, label := factor(labels[topic], levels = labels[label_order])]
tab[domain == "NA", domain := NA]

tab10 = rbindlist(lapply(unique(domains3), function(d){
  data.table(domain = d, topic = 1:12, 
             value = rowSums(getDocument_sums(getLDA(proto))[, domains3 == d, drop=FALSE]))
}))
tab10[, label := factor(labels[topic], levels = labels[label_order])]
tab10[domain == "NA", domain := NA]

ggplot(tab) + aes(x = label, y = value, fill = domain) + 
  geom_bar(stat = "identity", position = position_fill(reverse = TRUE))+
  theme(axis.text.y=element_text(size=rel(0.9)), legend.position="none",
        legend.justification="right") +
  xlab("Topic") + ylab("Share") + labs(fill = "") + coord_flip() +
  scale_x_discrete(limits=rev)
ggsave(file.path("paper", "topic_domain.eps"), width = 9.6, height = 5, units = "cm")

ggplot(tab10) + aes(x = factor(domain, levels = c(top10_domains, "Other")), y = value, fill = label) + 
  geom_bar(stat = "identity", position = position_fill(reverse = TRUE))+
  theme(axis.text.y=element_text(size=rel(0.9)), legend.position="none",
        legend.justification="right") +
  xlab("Domain") + ylab("Share") + labs(fill = "") + coord_flip() +
  scale_x_discrete(limits=rev) +
  scale_fill_manual(values= RColorBrewer::brewer.pal(12, "Set3"))
#  scale_fill_hue(limits = labels[col_order],
#                 labels = labels[label_order],
#                 breaks = labels[label_order])
ggsave(file.path("paper", "topic_domain_flip.eps"), width = 9.6, height = 5.3, units = "cm")

### score vs. domain only for Correctiv
tab = rbindlist(lapply(unique(score), function(x){
  ind = score == x & medium == "CORRECTIV"
  value = sapply(top10_domains, function(z) sum(domains[ind] == z, na.rm = TRUE))
  data.table(rating = x,
             domain = c(top10_domains, "Other", NA),
             value = c(value,
                       sum(!domains[ind] %in% top10_domains, na.rm = TRUE),
                       sum(is.na(domains[ind]))))
}))

ggplot(tab) + aes(x = factor(domain, levels = c(top10_domains, "Other")), y = value, fill = rating) +
  geom_bar(position = position_fill(reverse = TRUE), stat = "identity") +
  xlab("Domain") + ylab("Share") + labs(fill = "Rating") +
  theme(axis.text.y=element_text(size=rel(0.9)), legend.position="none",
        legend.justification="right") + guides(fill = guide_legend(nrow = 1)) +
  coord_flip() + scale_x_discrete(limits=rev) +
  scale_fill_manual(values=c("#a52019", "red", "orange", "#fbe870",
                             "#86ceec", "#027b76", "#68b300" ,"grey50"))
ggsave(file.path("paper", "domain_rating.eps"), width = 9.6, height = 5.3, units = "cm")
