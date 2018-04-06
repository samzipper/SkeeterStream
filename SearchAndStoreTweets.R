## SearchAndStoreTweets.R
#' This script is intended to:
#'  (1) search Twitter for a keyword or set of keywords
#'  (2) download all matching Tweets
#'  (3) extract the location of the tweeter via Google Maps
#'  (4) save the output as a CSV file

rm(list=ls())

# path to git directory
git.dir <- "C:/Users/Sam/WorkGits/SkeeterStream/"

# load packages
require(rtweet)
require(lubridate)
require(ggmap)
require(stringr)
require(stringi)
require(maptools)
require(DBI)
require(ROAuth)
require(dplyr)
require(httpuv)

# get today/yesterday dates
date_today <- as.Date(Sys.time(), tz=Sys.timezone(location = TRUE))
date_yesterday <- date_today-days(1)

# search string: what will you search twitter for?
search.str <- paste0("(mosquito OR mosquitos OR mosquitoes) since:", as.character(date_yesterday), " until:", as.character(date_today), " lang:en")

# output directory: save to Dropbox, not git repository, so it's automatically backed up
# this is also where authentication info is stored
out.dir <- "C:/Users/Sam/Dropbox/Work/Twitter/SkeeterStream/"

# path to save output data
path.out <- paste0(out.dir, "rTweetsOut.sqlite")

# path to save the screen output
path.sink <- paste0(out.dir, "rTweetsOut_Screen_", format(Sys.time(), "%Y%m%d-%H%M"), ".txt")

## launch sink file, which will store screen output 
# this is useful when automating, so it can be double-checked later
# to make sure nothing weird happened
s <- file(path.sink, open="wt")
sink(s, type="message")

# status update
print(paste0("starting, from ", date_yesterday, " to ", date_today))

# load existing tweet SQLite database
db <- dbConnect(RSQLite::SQLite(), path.out)
df.in <- dbReadTable(db, "tweets")

# read in token which was created with script rtweet_SetUpToken.R
r.token <- readRDS(file.path(path.expand("~/"), "twitter_token_skeeter.Rds"))

# search twitter!
tweets <- search_tweets2(search.str,
                         n=10000, 
                         type="recent",
                         include_rts=F,
                         retryOnRateLimit=T,
                         token=r.token)

# subset to yesterday only, just in case...
df <- subset(tweets, created_at >= date_yesterday & created_at < date_today)

# get rid of duplicates just in case
df <- unique(df)

# tweets status
print(paste0(dim(df)[1], " tweets"))

## using Google Maps API, get estimated geographic coordinates based on user location
# limit of 2500/day! so, get clean location as much as possible first to minimize calls to API

# get user location
users.unique <- unique(df$screen_name)
print(paste0(length(users.unique), " screen names"))
df.users <- lookup_users(users.unique, 
                         token=r.token)

# users status
print(paste0(dim(df.users)[1], " users"))

# trim to only users with location info
df.users <- df.users[df.users$location != "",]

# replace % and # in user location with blank so geocode doesn't get messed up
df.users$location <- gsub("%", " ",df.users$location)
df.users$location <- gsub("#", " ",df.users$location)

# deal with emojis and other weird characters
df.users$location <- iconv(df.users$location, "UTF-8", "ASCII", sub="")

# trim leading/trailing white space
df.users$location <- trimws(df.users$location)

# get unique locations
locations <- unique(df.users$location)

# get rid of any locations that are empty
locations <- trimws(locations)
locations <- locations[!is.na(locations)]
locations <- locations[!is.null(locations)]
locations <- locations[!stri_isempty(locations)]

# figure out which locations have already been geocoded
locations.exist <- locations[str_to_lower(locations) %in% str_to_lower(df.in$location)]
df.locations.exist <- 
  data.frame(location = locations.exist,
             lat.location = df.in$lat.location[match(str_to_lower(locations.exist), str_to_lower(df.in$location))],
             lon.location = df.in$lon.location[match(str_to_lower(locations.exist), str_to_lower(df.in$location))])
locations <- locations[!(locations %in% locations.exist)]

# status update
print(paste0(length(locations), " locations to geocode"))

# call geocode
geo.out <- geocode(locations, source="google", output="all")

## filter output
# status check: did geocode find a location?
check.status <- sapply(geo.out, function(x) x["status"]=="OK" & length(x["status"])>0)
check.status[is.na(check.status)] <- F
geo.out <- geo.out[check.status]
locations <- locations[check.status]

# status check: is location ambiguous?
check.ambig <- sapply(lapply(geo.out, lapply, length), function(x) x["results"]=="1")
geo.out <- geo.out[check.ambig]
locations <- locations[check.ambig]

## make final locations data frame
df.locations <- rbind(df.locations.exist,
                      data.frame(
                        location = locations,
                        lat.location = sapply(geo.out, function(x) x["results"]$results[[1]]$geometry$location$lat),
                        lon.location = sapply(geo.out, function(x) x["results"]$results[[1]]$geometry$location$lng)
                      ))

# status update
print(paste0(length(locations), " locations successfully geocoded"))

# add location info back to user data frame
df.users <- left_join(df.users[c("location", "description", "screen_name")], df.locations, by="location", all.x=T)

# make output data frame including tweet, user, location, etc.
df.out <- left_join(df, df.users, by="screen_name", all.x=T)

# put in order
df.out <- df.out[order(df.out$status_id), ]

# convert dates to character string for database
df.out$created_at <- as.character(df.out$created_at)

## convert columns that are lists to text strings separated by _<>_
# find list columns
cols.list <- which(lapply(df.out, class) == "list")

for (col in cols.list){
  df.out[,col] <- apply(df.out[,col], 1, function(x) as.character(paste(x, collapse="_<>_")))
}

## put into database
# add data frame to database (if it doesn't exist, it will be created)
dbWriteTable(db, "tweets", df.out, append=T)

# if you want to read in a data frame from your db to check...
#df.test <- dbReadTable(db, "tweets")
#dbWriteTable(db, "tweets", df.test, overwrite=T)

# when you're done, disconnect from database (this is when the data will be written)
dbDisconnect(db)

# print status update
print(paste0(dim(df.out)[1], " tweets added to database"))

# close sink
close(s)