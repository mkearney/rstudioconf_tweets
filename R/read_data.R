## NCA17 tweets

## install and load rtweet
if (!requireNamespace("devtools", quietly = TRUE)) install.packages("devtools")
devtools::install_github("mkearney/rtweet")
library(rtweet)

## create data folder is it doesn't already exist
if (!dir.exists(file.path("..", "data"))) dir.create(file.path("..", "data"))

## download stream data, save it to data folder
download.file(
  "https://www.dropbox.com/s/t0sefc0lzqbwd32/stream-1.json?dl=1",
  file.path("..", "data", "nca17.json")
)

## read in stream data, converting it to data frame
nca <- parse_stream(file.path("..", "data", "nca17.json"))

## preview data (should be N = 3332)
nca
