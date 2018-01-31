## load rtweet
library(rtweet)

## load ggplot2
library(ggplot2)

## Read in the streamed data
d <- parse_stream("data/stream-1.json")

## plot the time series of #NCA17 activity
ts_plot(nca, "hours") +
  theme_minimal(base_family = "sans") +
  theme(plot.title = element_text(face = "bold")) +
  labs(x = NULL, y = NULL, title = "Time series of #NCA17 Twitter statuses",
       subtitle = "Twitter statuses aggregated by hour",
       caption = "\nData collected from Twitter's stream (filter) API using rtweet") +
  ggsave("../nca17-ts.png", width = 8, height = 6, units = "in")
