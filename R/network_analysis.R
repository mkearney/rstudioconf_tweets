## load rtweet
library(rtweet)

## load igraph
library(igraph)

## Read in the streamed data
d <- parse_stream("data/stream.json")

## function to filter out missing and non-unique IDs
uq_naomit <- function(x) unique(x[!is.na(x)])

## function to create connections data frames
connections_df <- function(user, var, interaction = NULL) {
  connections_df_ <- function(user, var) {
    data.frame(
      screen_name = user,
      connection = unlist(var, use.names = FALSE),
      row.names = NULL,
      check.rows = FALSE,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
  }
  d <- Map("connections_df_", user, var)
  d <- do.call("rbind", d)
  d <- d[!is.na(d$connection), ]
  if (!is.null(interaction)) {
      d$interaction <- interaction
  }
  tibble::as_tibble(d, validate = FALSE)
}

##----------------------------------------------------------------------------##
##                                retweet users                               ##
##----------------------------------------------------------------------------##

## lookup retweets
rts <- lookup_tweets(uq_naomit(d$retweet_status_id))

## select and rename columns
rts <- dplyr::select(
  rts, retweet_status_id = status_id, retweet_screen_name = screen_name
)

## left join with data
d <- dplyr::left_join(d, rts, by = "retweet_status_id")


##----------------------------------------------------------------------------##
##                                 quote users                                ##
##----------------------------------------------------------------------------##

## lookup quotes
qts <- lookup_tweets(uq_naomit(d$quoted_status_id))

## select and rename columns
qts <- dplyr::select(
  qts, quoted_status_id = status_id, quoted_screen_name = screen_name
)

## left join with data
d <- dplyr::left_join(d, qts, by = "quoted_status_id")


##----------------------------------------------------------------------------##
##                          semantic connections data                         ##
##----------------------------------------------------------------------------##

## mentions data
md <- connections_df(d$screen_name, d$mentions_screen_name, "mention")

## replies data
td <- connections_df(d$screen_name, d$reply_to_screen_name, "reply")

## retweets data
rd <- connections_df(d$screen_name, d$retweet_screen_name, "retweet")

## quotes data
qd <- connections_df(d$screen_name, d$quoted_screen_name, "quote")

## combine connections data
snd <- do.call("rbind", list(md, td, rd, qd))

## rename
names(snd) <- c("from", "to", "interaction")

## list of all users
all_users <- c(snd$from, snd$to)

## list of user screen names with at least 5 connections
kp_users <- table(all_users)
kp_users <- names(kp_users[kp_users > 4L])

## lookup users data
nodes <- lookup_users(kp_users)

## filter kp_usres and count interactions
links <- snd %>%
  dplyr::filter(from %in% nodes$screen_name & to %in% nodes$screen_name & from != to) %>%
  dplyr::group_by(from, to) %>%
  dplyr::summarise(n = n()) %>%
  dplyr::ungroup()

## filter only nodes found in links
nodes <- dplyr::filter(nodes, screen_name %in% c(links$to, links$from))

## size vector
size1 <- links %>%
  dplyr::group_by(from) %>%
  dplyr::summarise(n = sum(n)) %>%
  dplyr::select(screen_name = from, n)

size2 <- links %>%
  dplyr::group_by(to) %>%
  dplyr::summarise(n = sum(n)) %>%
  dplyr::select(screen_name = to, n)

sizes <- rbind(size1, size2) %>%
  dplyr::group_by(screen_name) %>%
  dplyr::summarise(n = sum(n))

## network graph
net <- graph_from_data_frame(
  d = links,
  vertices = nodes[, c("screen_name", "statuses_count")],
  directed = TRUE
)

## save plot
png("../nca17-network.png", 20, 18, "in", res = 300)
par(mar = c(0, 0, 0, 0), bg = "#000044")
plot(net, edge.size = .25,
     margin = c(-.05, -.05, -.05, -.05),
     edge.arrow.size = 0,
     edge.arrow.width = 0,
     vertex.color = "#ff00ff55",
     vertex.frame.color = "transparent",
     vertex.label.color = "greenyellow",
     vertex.label.cex = .35,
     vertex.label.family = "sans",
     vertex.size = sqrt(sizes$n) / 1.8,
     edge.color = "#ff00ff55",
     edge.width = .25)
dev.off()
