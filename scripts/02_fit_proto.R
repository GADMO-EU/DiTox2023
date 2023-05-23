library(ldaPrototype)

docs = readRDS(file.path("data", "docs.rds"))
vocab = readRDS(file.path("data", "vocab.rds"))

dir.create("proto")

for(K in 5:25){
  lda = LDAPrototype(docs, vocab, K = K, pm.backend = "socket", ncpus = 4)
  saveRDS(lda, file.path("proto", paste0(K, ".rds")))
}
