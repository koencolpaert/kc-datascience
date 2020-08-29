library(twitteR)
library(RCurl)

# Twitter Authentication
api_key <- "ZLS8PzJG0oNEvuJSBfnNzdSFW" # your api_key
api_secret <- "XOu6TRQ5zZmZtfydcsugURXWLYf6LebEmDc6GOkhNg6TXahRZ2" # your api_secret
access_token <- "889345782-x3o8LzJnYuBYerXOVOgkeRIjSwZRQFikTPF6VZUE" # your access_token
access_token_secret <- "7r3IXsdvPJNyGFE7N0d5Mgpj4KqEsYL7rD1FJbMhzxQ7L" # your access_token_secret
setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)

# get 3000 tweets with #chaand tag
tag.antand <- searchTwitter('#AntAnd', n="3000")
tag.andkvo <- searchTwitter('#ANDKVO', n="3000")

tag.andant <- searchTwitter('#ANDANT', n="3000")
tag.andgnt <- searchTwitter('#ANDGNT', n="3000")
tag.chaand <- searchTwitter('#CHAAND', n="3000")

# convert to data frame
df.antand <- do.call("rbind", lapply(tag.antand, as.data.frame))

df.andant <- do.call("rbind", lapply(tag.andant, as.data.frame))
df.andgnt <- do.call("rbind", lapply(tag.andgnt, as.data.frame)) 
df.chaand <- do.call("rbind", lapply(tag.chaand, as.data.frame)) 


# get column names to see structure of the data
names(df.andgnt) 
# look at the first three rows to check content
head(df.andgnt,3)

# Merge all dataframes
alltags <- rbind(df.chaand,df.andgnt)
write.csv(alltags, file="./data/alltags.csv")