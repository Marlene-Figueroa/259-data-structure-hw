#PSYC 259 Homework 3 - Data Structure
#For full credit, provide answers for at least 8/11 questions

#List names of students collaborating with: 

### SETUP: RUN THIS BEFORE STARTING ----------

install.packages("rvest")

#Load packages
library(tidyverse)
library(lubridate)
library(rvest)

# Scrape the data for the new rolling stone top 500 list
url <- "https://stuarte.co/2021/2021-full-list-rolling-stones-top-500-songs-of-all-time-updated/"
rs_new <- url %>% read_html() %>% html_nodes(xpath='//*[@id="post-14376"]/div[2]/div[2]/table') %>% html_table() %>% pluck(1)

# Scrape the data for the old rolling stone top 500 list
url_old <- "https://www.cs.ubc.ca/~davet/music/list/Best9.html"
rs_old <- url_old %>% read_html() %>% html_nodes(xpath='/html/body/table[2]') %>% html_table() %>% pluck(1) %>% 
  select(1, 4, 3, 7) %>% rename(Rank = X1, Artist = X3, Song = X4, Year = X7) %>% filter(Year != "YEAR") 

# If there's a security error, add:
#url %>% httr::GET(config = httr::config(ssl_verifypeer = FALSE)) %>% read_html()

#OR
load("rs_data.RData")

### Question 1 ---------- 

# Use "full_join" to merge the old and new datasets, rs_new and rs_old,
# by Artist AND Song. Save the results to a dataset called rs_joined_orig
# If the merged worked, each song-artist combination that appears in both
# datasets should now be in a single row with the old/new ranks
# Use the nrow() function to see how many rows of data there are
# In the viewer, take a look at the merge...what kinds of problems are there?
# Why did some of the artist-song fail to match up?

#ANSWER

rs_joined_orig <- full_join(rs_new, rs_old, by = c("Artist", "Song"))
nrow(rs_joined_orig)
View(rs_joined_orig)
# Answer to your questions: There are 860 rows. In the merge, the problems I see are that there is a lot of missing data (lots of NAs) and that is probably because the name of the columns in the joined dataset do not match the column names in the rs_new and the rs_old data set. Perhaps matching the names of the columns would give us more data in the year column. 


### Question 2 ---------- 

# To clean up the datasets, it would be more efficient to put them into a single data set
# Add a new variable to each dataset called "Source" with value "New" for rs_new and
# "Old" for rs_old. Then use bind_rows to join the two datasets into a single one called rs_all
# You will run into a problem because the old dataset has rank/year as characters instead of integers
# Make Rank and Year into integer variables for rs_old before binding them into rs_all

#ANSWER

rs_new <- rs_new %>% mutate(Source = "New")
rs_old <- rs_old %>% mutate(Source = "Old")

rs_old <- rs_old %>%
  mutate(Rank = as.integer(Rank),
         Year = as.integer(Year))
rs_all <- bind_rows(rs_new, rs_old)
str(rs_all)
View(rs_all)


### Question 3 ----------

# The join in Q1 resulted in duplicates because of differences in how the songs and artists names were written
# Use string_remove_all to remove the word "The" from every artist/song (e.g., Beach Boys should match The Beach Boys)
# Use string_replace_all to replace the "&" with the full word "and" from every artist/song
# Then use string_remove_all to remove all punctuation from artists/songs
# Finally, read the documentation for the functions str_to_lower and str_trim
# Use both functions to make all artists/song lowercase and remove any extra spaces

#ANSWER

cleaned_data <- function(text) {
  text %>%
    str_remove_all("\\bThe\\b") %>%  
    str_replace_all("&", "and") %>%  
    str_remove_all("[[:punct:]]") %>% 
    str_to_lower() %>%               
    str_trim()                        
}
rs_new <- rs_new %>%
  mutate(Artist = cleaned_data(Artist),
         Song = cleaned_data(Song))
rs_old <- rs_old %>%
  mutate(Artist = cleaned_data(Artist),
         Song = cleaned_data(Song))
rs_joined_clean <- full_join(rs_new, rs_old, by = c("Artist", "Song"))
nrow(rs_joined_clean)
View(rs_joined_clean)


### Question 4 ----------

# Now that the data have been cleaned, split rs_all into two datasets, one for old and one for new
# Each dataset should have 500 observations and 5 variables
# Use full_join again to merge the old and new datasets by artist and song, and save it to rs_joined
# Read about the "suffix" argument in full_join, and use it to append _Old and _New to year and rank
# rather than the default (x and y)
# Did the string cleaning improve matches? If so, there should be fewer rows of data (fewer duplicates)
# in the new rs_joined compared to the original. Use nrow to check (there should be 799 rows)

#ANSWER
rs_old_clean <- rs_all %>% filter(Source == "Old") %>% select(-Source) %>% head(500)
rs_new_clean <- rs_all %>% filter(Source == "New") %>% select(-Source) %>% head(500)
rs_joined <- full_join(rs_old_clean, rs_new_clean, by = c("Artist", "Song"), suffix = c("_Old", "_New"))
nrow(rs_joined) 
View(rs_joined)


### Question 5 ----------

# Let's clean up rs_joined with the following steps:
  # remove the variable "Source"
  # remove any rows where Rank_New or Rank_Old is NA (so we have only the songs that appeared in both lists)
  # calculate a new variable called "Rank_Change" that subtracts new rank from old rank
  # sort by rank change
# Save those changes to rs_joined
# You should now be able to see how each song moved up/down in rankings between the two lists

#ANSWER
rs_joined <- rs_joined %>%
  select(-Source) %>%                   
  filter(!is.na(Rank_New) & !is.na(Rank_Old)) %>%  
  mutate(Rank_Change = Rank_Old - Rank_New) %>%   
  arrange(desc(Rank_Change))
rs_joined
View(rs_joined)

#second try because I am getting an error on the first one

rs_joined <- rs_joined %>%
  filter(!is.na(Rank_New) & !is.na(Rank_Old)) %>%  
  mutate(Rank_Change = Rank_Old - Rank_New) %>%    
  arrange(desc(Rank_Change))                       
View(rs_joined)

#yes! That last code did work!!! 


### Question 6 ----------

# Add a new variable to rs_joined that takes the year and turns it into a decade with "s" at the end
# The new variable should be a factor
# 1971 should be 1970s, 1985 should be 1980s, etc.
# Group by decade and summarize the mean rank_change for songs released in each decade (you don't have to save it)
# Which decade improved the most?

#ANSWER
rs_joined <- rs_joined %>%
  mutate(Decade = factor(floor(Year_Old / 10) * 10, levels = seq(min(floor(Year_Old / 10) * 10), max(floor(Year_Old / 10) * 10), 10))) %>%
  mutate(Decade = paste0(Decade, "s"))  
rs_joined %>%
  group_by(Decade) %>%
  summarize(mean_rank_change = mean(Rank_Change, na.rm = TRUE)) %>%
  arrange(desc(mean_rank_change)) 

### Question 7 ----------

# Use fct_count to see the number of songs within each decade
# Then use fct_lump to limit decade to 3 levels (plus other), and
# Do fct_count on the lumped factor with the prop argument to see the 
# proportion of songs in each of the top three decades (vs. all the rest)

#ANSWER
rs_joined <- rs_joined %>%
  mutate(Decade = factor(Decade))
rs_joined %>%
  pull(Decade) %>%             
  fct_count() %>%
  arrange(desc(n))  
rs_joined <- rs_joined %>%
  mutate(Decade = fct_lump(Decade, n = 3))  
rs_joined %>%
  pull(Decade) %>%          
  fct_count(prop = TRUE) %>%
  arrange(desc(n))

### Question 8 ---------- 

# Read the file "top_20.csv" into a tibble called top20
# Release_Date isn't read in correctly as a date
# Use parse_date_time to fix it

#ANSWER
library(dplyr)
library(readr)
library(lubridate)

top_20 <- read_csv("top_20.csv")
top20 <- top_20 %>%
  mutate(Release = parse_date_time(Release, orders = c("dmy", "mdy", "ymd")))
head(top20)

### Question 9 --------

# top20's Style and Value are mixing two different variables into one column
# use pivot_wider to fix the issue so that bpm and key are columns
# overwrite top20 with the pivoted data (there should now be 20 rows!)

#ANSWER

top20 <- top20 %>%
  pivot_wider(names_from = Style, values_from = Value)
head(top20)


### Question 10 ---------

# Merge in the data from rs_joined to top20 using left_join by artist and song
# The results should be top20 (20 rows of data) with columns added from rs_joined
# Use the "month" function from lubridate to get the release month from the release date
# and add that as a new variable to top 20. 
# It should be a factor - if you get a number back read the help for ?month to see how to get a factor
# Create a new factor called "season" that collapses each set of 3 months into a season "Winter", "Spring", etc.
# Count the number of songs that were released in each season

#ANSWER
library(dplyr)
library(lubridate)

top20 <- top20 %>%
  left_join(rs_joined, by = c("Artist" = "Artist", "Song" = "Song"))

top20 <- top20 %>%
  mutate(
    Release_Month = factor(month(Release, label = TRUE, abbr = FALSE), 
                           levels = month.name)
  )

top20 <- top20 %>%
  mutate(
    season = case_when(
      Release_Month %in% c("December", "January", "February") ~ "Winter",
      Release_Month %in% c("March", "April", "May") ~ "Spring",
      Release_Month %in% c("June", "July", "August") ~ "Summer",
      Release_Month %in% c("September", "October", "November") ~ "Fall",
      TRUE ~ NA_character_  
    )
  )

season_count <- top20 %>%
  count(season)
season_count




### Question 11 ---------

# How many songs in the top 20 were major vs. minor? 
# Create a new factor called "Quality" that is either Major or Minor
# Minor keys contain the lowercase letter "m". If there's no "m", it's Major
# Figure out which is the top-ranked song (from Rank_New) that used a minor key

#ANSWER
top_minor_song <- top20 %>%
  filter(Quality == "Minor") %>%
  arrange(Rank_New) %>%  
  slice(1) 
quality_count
top_minor_song



