rm(list=ls())
library(readr)
spotify <- read_csv("Most Streamed Spotify Songs 2024.csv", 
                    col_types = cols(`Release Date` = col_date(format = "%m/%d/%Y"), 
                                     `TIDAL Popularity` = col_skip(), 
                                     `Explicit Track` = col_logical()))
