## load rtweet
library(rtweet)

## load ggplot2
library(ggplot2)

## Read in the streamed data
d <- parse_stream("data/stream.json")

## function to create freq table
tab_sort <- function (x, n = 10, mentions = FALSE) {
  sumrow <- data.frame(
    "screen_name" = paste(length(unique(x)), "users"),
    "n_tweets" = length(x),
    "prop_tweets" = 1.000,
    stringsAsFactors = FALSE
  )
  x <- sort(table(x), decreasing = TRUE)
  x <- data.frame(
    "screen_name" = names(x),
    "n_tweets" = as.integer(x),
    stringsAsFactors = FALSE
  )
  x$prop_tweets <- x$n_tweets / sum(x$n_tweets, na.rm = TRUE)
  x$prop_tweets <- round(x$prop_tweets, 3)
  x <- head(x, n)
  x <- rbind(x, sumrow)
  row.names(x) <- c(seq_len(nrow(x) - 1L), "total")
  if (mentions) {
    names(x)[2:3] <- c("n_mentions", "prop_mentions")
  }
  x
}

## most frequent tweeters table
usrs <- tab_sort(nca$screen_name)

## save most freq tweeters table
png("../nca17-usrs.png", height = 3.1, width = 4.25, "in", res = 300)
par(bg = "white")
gridExtra::grid.table(usrs, theme = gridExtra::ttheme_default(base_size = 9))
dev.off()

## most frequent mentions table
naomit <- function(x) x[!is.na(x)]
usrs <- tab_sort(naomit(unlist(nca$mentions_screen_name)), mentions = TRUE)

## save most freq mentions table
png("../nca17-ats.png", height = 3.1, width = 4.25, "in", res = 300)
par(bg = "white")
gridExtra::grid.table(usrs, theme = gridExtra::ttheme_default(base_size = 9))
dev.off()
