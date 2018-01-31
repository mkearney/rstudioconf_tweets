## load rtweet
library(rtweet)

## load ggplot2
library(ggplot2)

## Read in the streamed data
d <- parse_stream("data/stream.json")

## function for cleaning text and creating word freq table
clean_text_table <- function(data) {
  txt <- tolower(plain_tweets(data$text))
  txt <- gsub("&amp;", "", txt)
  txt <- gsub("#nca17", "", txt, ignore.case = TRUE)
  txt <- unlist(strsplit(txt, " "))
  txt <- gsub("^[[:punct:]]{1,}|[[:punct:]]{1,}$", "", txt)
  txt <- trimws(txt)
  txt <- txt[txt != ""]
  swds <- stopwordslangs$word[stopwordslangs$lang == "en" & stopwordslangs$p > .99]
  txt <- txt[!txt %in% swds]
  sort(table(txt), decreasing = TRUE)
}

## create frequency table of popular words
wds <- clean_text_table(nca)

## calc min freq for word cloud
minfreq <- quantile(as.double(wds), .75)

## save word cloud
png("../nca17-wc.png", height = 8, width = 8, "in", res = 300)
par(bg = "black")
wordcloud::wordcloud(
  names(wds),
  as.integer(wds),
  min.freq = minfreq,
  random.color = FALSE,
  random.order = FALSE,
  colors = gg_cols(6)
)
dev.off()
