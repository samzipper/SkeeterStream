## LoadTweets.R
#' This script is intended to load a data frame of tweets from an
#' SQLite database generated with the script SearchAndStoreTweets.R

rm(list=ls())

# path to git directory
git.dir <- "C:/Users/Sam/WorkGits/SkeeterStream/"

# load packages
require(rtweet)
require(lubridate)
require(ggmap)
require(stringr)
require(maptools)
require(DBI)
require(ROAuth)
require(dplyr)

# save plots?
save.plots <- F

# output directory: this is where the SQLite database is
out.dir <- "C:/Users/Sam/Dropbox/Work/Twitter/SkeeterStream/"
#out.dir <- "D:/Dropbox/Work/Twitter/AgroStream/"

# path to database
path.out <- paste0(out.dir, "rTweetsOut.sqlite")

# connect to database
db <- dbConnect(RSQLite::SQLite(), path.out)

# read in table
df <- dbReadTable(db, "tweets")

# trim to unique and rewrite
df <- unique(df)
dbWriteTable(db, "tweets", df, overwrite=T)

# when you're done, disconnect from database (this is when the data will be written)
dbDisconnect(db)

# plot of tweets by day
df$created_at <- ymd_hms(df$created_at)
df$DOY <- yday(df$created_at)
df$Date <- as.Date(df$created_at, tz=Sys.timezone(location = TRUE))
df.d <- summarize(group_by(df, Date),
                  tweets = sum(is.finite(lat.location)))

# list of missing days
missing <- seq(df.d$Date[1], Sys.Date()-1, by="day")[!(seq(df.d$Date[1], Sys.Date()-1, by="day") %in% df.d$Date)]
print(missing)
#print(yday(missing))
#print(week(missing))

# print most recent tweet
print(paste0("Last tweet: ", df$created_at[which.max(df$status_id)]))

# bar plot: tweets by day
p.bar.tweets.DOY <-
  ggplot(df.d, aes(x=Date, y=tweets)) +
  geom_bar(stat="identity") +
  geom_hline(yintercept=0, color="gray65") +
  labs(title="Number of Mosquito Tweets", 
       subtitle="Search Term: (mosquito OR mosquitos OR mosquitoes)") +
  scale_y_continuous(name="# Tweets") +
  theme_bw() +
  theme(panel.grid=element_blank())
p.bar.tweets.DOY

if (save.plots){
  ggsave(paste0(out.dir, "LoadTweets_p.bar.tweets.DOY.png"),
         p.bar.tweets.DOY, width=8, height=8, units="in")
  
  # map of tweet locations, based on user profile location
  mapWorld <- borders("world", colour="gray50", fill="white")
  df.loc <- summarize(group_by(df, lon.location, lat.location),
                      n.tweets = sum(is.finite(lon.location)))
  p.map.location <-
    ggplot(df.loc, aes(x=lon.location, y=lat.location, size=log10(n.tweets))) + 
    mapWorld + 
    geom_point(shape=21, color="red") + 
    scale_x_continuous(name="Longitude", limits=c(-180,180), expand=c(0,0)) +
    scale_y_continuous(name="Latitude", limits=c(-90,90), expand=c(0,0)) +
    scale_size_continuous(name="log(# Tweets)") +
    labs(title=paste0("Map of ", sum(df.loc$n.tweets), " Geocoded Mosquito Tweets"), 
         subtitle="Location geocoded from user profile location") +
    coord_equal() +
    theme_bw() +
    theme(panel.grid=element_blank())
  ggsave(paste0(out.dir, "LoadTweets_p.map.location.png"),
         p.map.location, width=16, height=9, units="in")
  
  # date of first mosquito bite by latitude
  i.first <- which(str_detect(str_to_lower(df$text), "first"))
  i.bite <- which(str_detect(str_to_lower(df$text), "bite"))
  i.first.bite <- i.bite[i.bite %in% i.first]
  p.first.bite.lat <-
    ggplot(df[i.first.bite, ], aes(y=Date, x=lat.location)) +
    geom_point() +
    stat_smooth(method="lm") +
    scale_x_continuous(name="Latitude") +
    scale_y_date(name="Date of 'First Bite' Tweet") +
    labs(title="Date of First Bite as a function of Latitude", 
         subtitle="Location geocoded from user profile location") +
    theme_bw() +
    theme(panel.grid=element_blank())
  ggsave(paste0(out.dir, "LoadTweets_p.first.bite.lat.png"),
         p.first.bite.lat, width=8, height=8, units="in")
  
}
