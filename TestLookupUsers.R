## TestLookupUsers.R
# Testing lookup users command.

require(rtweet)
require(lubridate)
require(ROAuth)

# get today/yesterday dates
date_today <- as.Date(Sys.time(), tz=Sys.timezone(location = TRUE))
date_yesterday <- date_today-days(1)

# search string: what will you search twitter for?
search.str <- paste0("(mosquito OR mosquitos OR mosquitoes) since:", as.character(date_yesterday), " until:", as.character(date_today), " lang:en")

# read in token which was created with script rtweet_SetUpToken.R
r.token <- readRDS(file.path(path.expand("~/"), "twitter_token_skeeter.Rds"))

# search twitter!
tweets <- search_tweets2(search.str,
                         n=25, 
                         type="recent",
                         include_rts=F,
                         retryOnRateLimit=T,
                         token=r.token)

# get rid of duplicates just in case
df <- unique(tweets)

# get user location
users.unique <- unique(df$screen_name)
df.users <- lookup_users(users.unique,
                         token=r.token)

# print some diagnostics
print(paste0(dim(df)[1], " tweets"))
print(paste0(length(users.unique), " screen names"))
head(users.unique)
print(paste0(dim(df.users)[1], " users successfully looked up"))
