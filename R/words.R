str_split <- function(x, sep, sub = NA_character_, ...) {
  x <- strsplit(x, sep, ...)
  x[lengths(x) == 0L] <- sub
  x
}
split_words <- function(x) {
  str_split(x, "\\s+")
}
omit_num <- function(x) {
  omit_num_ <- function(x) {
    x <- grep("^[[:digit:][:punct:]]+$", x, value = TRUE, invert = TRUE)
    if (length(x) == 0L) return(NA_character_)
    x
  }
  lapply(x, omit_num_)
}
trim_punct <- function(x) {
  trim_punct_ <- function(x) {
    x <- gsub("^[[:punct:]]+|[[:punct:]]+$", "", x)
    x <- x[x != ""]
    if (length(x) == 0L) return(NA_character_)
    x
  }
  lapply(x, trim_punct_)
}
remove_stopwords <- function(x, stopwords = NULL) {
  if (is.null(stopwords)) {
    data("stopwordslangs", package = "rtweet")
    stopwords <- dplyr::filter(stopwordslangs, lang == "en", p > .9999) %>% pull(word)
  }
  wordbreakor <- function(x) {
    x <- paste(x, collapse = "\\s{0,1}\\b|\\b\\s{0,1}")
    paste0("\\b", x, "\\b")
  }
  stopwords <- wordbreakor(stopwords)
  x <- gsub(stopwords, " ", x, perl = TRUE, ignore.case = TRUE)
  trim_ws(x)
}
tbl_table <- function(.data, ...) {
  vars <- quos(...)
  if (length(vars) == 0L) vars <- names(.data)
  if (is.data.frame(.data)) {
    .data <- dplyr::select(.data, !!!vars)
    tb <- do.call("table", as.list(.data))
  } else {
    tb <- table(var = .data)
  }
  tibble::as_tibble(tb)
}

suppressPackageStartupMessages(library(rlang))

rt$text2 <- gsub(
  "^RT:?\\s{0,}|#|@\\S+|https?[[:graph:]]+", "", rt$text)
## convert to lower case
rt$text2 <- tolower(rt$text2)
## trim extra white space
rt$text2 <- gsub("^\\s{1,}|\\s{1,}$", "", rt$text2)
rt$text2 <- gsub("\\s{2,}", " ", rt$text2)
rt %>%
  mutate(
    tidyverse = str_detect(
      text2, "dplyr|tidyeval|tidyverse|rlang|map|purrr|readr|tibble"),
    shiny = str_detect(text2, "shiny|dashboard|interactiv"),
    wds = remove_stopwords(text2),
    wds = split_words(wds),
    wds = trim_punct(wds),
    wds = omit_num(wds)) %>%
  select(created_at, tidyverse:shiny, wds) %>%
  gather(pkg, mention, -created_at, -wds) %>%
  mutate(pkg = factor(pkg, labels = c("Shiny", "Tidyverse"))) %>%
  filter(mention) %>%
  group_by(pkg) %>%
  summarise(
    wds = list(arrange(tbl_table(unlist(wds)), -n))) -> rtw
shiny <- rtw$wds[[1]]
shiny <- filter(shiny, !var %in% c("rstudioconf", "shiny"))
tidyverse <- rtw$wds[[2]]
tidyverse <- filter(tidyverse, !var %in% c("rstudioconf", "tidyverse", "tidy"))


gg_cols <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}



