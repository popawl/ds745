library(LDAvis) 
library(dplyr) 
library(mallet)
library(servr)
library(stringi) 
library(tm) 
library(topicmodels)
library(wordcloud)


#------------------------------------------------------------------------------
# Read data and clean it up
#------------------------------------------------------------------------------
df <-
  read.csv(
    "data/convos.csv",
    header = FALSE,
    col.names = c("email", "text"),
    stringsAsFactors = FALSE,
    sep = "|"
  )
df$id <- as.character(seq_along(df$email))

# For some reason my text has a capital a with circumflex. Need to remove
df$text <- gsub("Â", "", df$text)
# Need to elimanate "away" messages from the chat bot
df <- df[df$email != 'operator+s7wboyhg@intercom.io',]
# I am removing responses from the counselor
df <- df[grep("plusrelocation", df$email, invert=TRUE),]
# df$text <- tolower(df$text)
# df$words <- df$words[which(df$words != "")]

additional_stop_words = c(
  "thank",
  "thanks",
  "hi",
  "hey",
  "can",
  "please",
  "can",
  "will",
  "plus",
  "relocation",
  "justin",
  "annika",
  "chris",
  "pam"
)
corpus <- VCorpus(VectorSource(df$text))
corpus = tm_map(corpus, content_transformer(tolower))
corpus = tm_map(corpus, removeNumbers)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords())
corpus = tm_map(corpus, removeWords, additional_stop_words)
corpus = tm_map(corpus, stemDocument)
corpus = tm_map(corpus, stripWhitespace)
df$clean_text <- unlist(sapply(corpus, '[', "content"))
df$words <- strsplit(df$clean_text, "\\W")


#------------------------------------------------------------------------------
# Create wordcloud from term matrix
#------------------------------------------------------------------------------
dtm = DocumentTermMatrix(corpus)
dtm = removeSparseTerms(dtm, 0.999)

#This is another way to create term matrix
# words <- unlist(df$words)
# word_freq <- table(words)
# sorted_word_freq <- sort(word_freq, decreasing = TRUE)
# sorted_word_rel_freq <-
#   100 * (sorted_word_freq / sum(sorted_word_freq))


#------------------------------------------------------------------------------
# LDA with mallet package
#------------------------------------------------------------------------------
mallet_instances <-
  mallet.import(df$id,
                df$clean_text,
                "data/stoplist.csv",
                FALSE,
                token.regexp = "[\\p{L}']+")
topic_model <- MalletLDA(num.topics = 5)
topic_model$loadDocuments(mallet_instances)
vocabulary <- topic_model$getVocabulary()
topic_model_word_freq <- mallet.word.freqs(topic_model)
topic_model$train(400)
topic_model_words <-
  mallet.topic.words(topic_model, smoothed = TRUE, normalized = TRUE)
colnames(topic_model_words) <- vocabulary
keywords <- c("bank", "reimburs")
r
imp_row <- which(rowSums(topic_model_words[, keywords]) == max(rowSums(topic_model_words[, keywords])))
topic_top_words <- mallet.top.words(topic_model, topic_model_words[imp_row,], 10)

#------------------------------------------------------------------------------
# LDA with topicmodels
#------------------------------------------------------------------------------
burnin <- 1000
iter <- 1000
thin <- 100
seed <- list(1,1,1,1,1)
nstart <- 5
best <- TRUE
k <- 5
dtm_sum <- apply(dtm, 1, FUN=sum)
dtm <- dtm[dtm_sum != 0,]
topic_model2 <-
  LDA(
    dtm,
    k,
    method = "Gibbs",
    control = list(
      nstart = nstart,
      seed = seed,
      best = best,
      burnin = burnin,
      iter = iter
    )
  )
topic_model2_topic <- as.matrix(topics(topic_model2))
topic_model2_term <- as.matrix(terms(topic_model2, 10))
topic_model2_prob <- as.matrix((topic_model2@gamma))

#------------------------------------------------------------------------------
# Visualizations
#------------------------------------------------------------------------------
freq <- colSums(as.matrix(dtm))
ord <- order(freq, decreasing=TRUE)

plot(freq[ord])
axis(1, at=1:length(freq[ord]), labels=names(freq[ord]))

png("wordcloud_all.png", width=500, height=500)
wordcloud(
  names(freq[ord]),
  freq[ord],
  scale=c(3,0.2),
  rot.per=0,
  random.order=FALSE,
  max.words=100,
)
dev.off()
png("wordcloud_keyword.png", width=500, height=500)
wordcloud(
  topic_top_words$words,
  topic_top_words$weights,
  scale=c(3, 0.2),
  rot.per = 0,
  random.order = FALSE
)
dev.off()

topicmodels_json_ldavis <- function(fitted, corpus, doc_term) {
  # Find required quantities
  phi <- posterior(fitted)$terms %>% as.matrix
  theta <- posterior(fitted)$topics %>% as.matrix
  vocab <- colnames(phi)
  doc_length <- vector()
  for (i in 1:length(corpus)) {
    temp <- paste(corpus[[i]]$content, collapse = ' ')
    doc_length <- c(doc_length, stri_count(temp, regex = '\\S+'))
  }
  temp_frequency <- as.matrix(doc_term)
  freq_matrix <-
    data.frame(ST = colnames(temp_frequency),
               Freq = colSums(temp_frequency))
  rm(temp_frequency)
  # Convert to json
  json_lda <-
    LDAvis::createJSON(
      phi = phi,
      theta = theta,
      vocab = vocab,
      doc.length = doc_length,
      term.frequency = freq_matrix$Freq
    )
  # freq_matrix$Freq
  return(json_lda)
}
# I found this condensed function at https://gist.github.com/trinker/477d7ae65ff6ca73cace
topicmodels2LDAvis <- function(x, ...){
  post <- topicmodels::posterior(x)
  if (ncol(post[["topics"]]) < 3) stop("The model must contain > 2 topics")
  mat <- x@wordassignments
  LDAvis::createJSON(
    phi = post[["terms"]], 
    theta = post[["topics"]],
    vocab = colnames(post[["terms"]]),
    doc.length = slam::row_sums(mat, na.rm = TRUE),
    term.frequency = slam::col_sums(mat, na.rm = TRUE)
  )
}
serVis(topicmodels2LDAvis(topic_model2))
json1 <- topicmodels_json_ldavis(topic_model2, corpus, dtm)
serVis(json1, out.dir = 'vis2', open.browser = FALSE)
