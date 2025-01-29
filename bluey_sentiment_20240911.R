library(tidyverse)
library(rvest)
library(janitor)


# Specify the URL
url <- "https://blueypedia.fandom.com/wiki/Episode_List"

# Read the HTML content of the webpage
webpage <- read_html(url)

# Extract the tables from the webpage
tables <- html_table(webpage) 




# Since html_table returns a list of data frames, you might need to access the first element
# Assuming you want to combine all tables from the page, you can use bind_rows
combined_table <- bind_rows(filtered_tables) %>% 
  janitor::clean_names() %>% 
  filter(!grepl("Part \\d+", number)) %>% 
  remove_empty(which = "cols") %>% 
  mutate(viewers = gsub("\\[\\d+\\]", "",viewers_for_first_airing)) %>% 
  mutate(viewers = as.numeric(gsub("\\D", "", viewers))) %>% 
  mutate(number = as.numeric(number)) %>% 
  tail(-1) %>% 
  filter(!is.na(number)) %>% 
  mutate(season = cumsum(number < lag(number , default = first(number))) + 1) %>% 
  mutate(title_under = gsub(" ", "_", title)) 
  
# write_csv(combined_table, "combined_table_new.csv")

combined_table <- read_csv("tidy_episodes.csv")

# list of episodes

episode_names <- combined_table %>% 
  select(title) %>% 
  mutate(title = gsub(" ", "_", title)) %>% 
  as.list() %>% 
  unname()

episode_names <- episode_names[[1]]

addtional_episodes <-  c("Calypso_(Episode)", "Bingo_(Episode)")

episode_names <-  c(episode_names, addtional_episodes)

script_df <- data.frame(Episode = character(), Character = character(), Dialogue = character())

# Iterate over each episode name
for (episode_name in episode_names) {
  # Construct the URL for the episode
  url <- paste0("https://blueypedia.fandom.com/wiki/", episode_name, "/Script")
  
  tryCatch({
    # Read the HTML content of the webpage
    webpage <- read_html(url)
    
    # Extract the episode name from the URL
    episode_name <- gsub(".*/(.*)/Script", "\\1", url)
    
    # Extract the script text
    script_text <- html_text(html_nodes(webpage, "div.mw-parser-output"))
    
    # Split the script text into lines
    script_lines <- strsplit(script_text, "\n")[[1]]
    
    # Initialize variables to store character and dialogue
    characters <- character()
    dialogues <- character()
    
    # Extract characters and dialogues
    for (line in script_lines) {
      if (grepl(":", line)) { # If the line contains a colon (indicating a character's dialogue)
        parts <- strsplit(line, ":", fixed = TRUE)[[1]]
        character <- trimws(parts[1])
        dialogue <- trimws(parts[2])
        
        # Append character and dialogue to the vectors
        characters <- c(characters, character)
        dialogues <- c(dialogues, dialogue)
      }
    }
    
    # Create a data frame for the current episode
    episode_df <- data.frame(
      Episode = rep(episode_name, length(characters)),
      Character = characters,
      Dialogue = dialogues
    )
    
    # Remove rows with empty dialogues
    # episode_df <- episode_df[episode_df$Dialogue != "", ]
    
    # Append the current episode's data to the script data frame
    script_df <- bind_rows(script_df, episode_df)
  }, error = function(e) {
    cat("Error processing URL:", url, "\n")
    cat("Error message:", conditionMessage(e), "\n")
  })
}

# View the resulting data frame
print(head(script_df))


### time to manipulate our data to get a word for each row

script_df <- as_tibble(script_df)

library(tidytext)

tidy_episodes <- script_df %>% 
  unnest_tokens(word, Dialogue) %>% 
  anti_join(stop_words) %>% 
  left_join(combined_table %>% select(season, title_under)
            , by = c('Episode'= 'title_under')) %>% 
  relocate(season) 

check <- tidy_episodes %>% 
  group_by(Character) %>% 
  tally(sort= T)

tidy_episodes %>% 
  count(word, Character , sort = TRUE) %>%
  filter(n > 50) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL) +
  facet_wrap(vars(Character )) 
  

# write_csv(tidy_episodes, "tidy_episodes.csv")

# Visualizations
library(syuzhet)

count <- tidy_episodes %>% 
  count(word, Character , sort = TRUE) %>%
  filter(n > 50) %>%
  mutate(word = reorder(word, n))

ggplot(count, aes(x = word, y = n, size = n)) +
  geom_point(alpha = 0.7) +
  theme_minimal() +
  labs(title = "Word Frequency Bubble Chart", 
       x = "Words", 
       y = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Tokenize and count word frequencies by character and season
word_freq_by_season <- tidy_episodes %>%
  count(Character, season, word, sort = TRUE)

# Sentiment analysis on words spoken by each character per season
library(syuzhet)  

# Positive Sentiment: Values above 0 represent positive emotions like happiness or excitement.
# Negative Sentiment: Values below 0 represent negative emotions like sadness or anger.

sentiments <- get_sentiment(script_df$Dialogue, method = "syuzhet")

script_df$sentiment <- sentiments

sentiment_df <- script_df %>% 
  unnest_tokens(word, Dialogue) %>% 
  anti_join(stop_words) %>% 
  left_join(combined_table %>% select(season, title_under)
            , by = c('Episode'= 'title_under')) %>% 
  relocate(season) %>% 
  mutate(Character = gsub("^\\(.*?\\)\\s*", "", Character),
         Character = str_replace_all(Character, "[^A-Za-z0-9\\s]", ""))

name_mapping <- c("Chili" = "Chilli", "Bluye" = "Bluey")

correct_names <- function(names_vector, mapping) {
  corrected_names <-
    ifelse(names_vector %in%
             names(mapping),
           mapping[names_vector], names_vector)
  return(corrected_names)
}

sentiment_df$Character <- correct_names(sentiment_df$Character, name_mapping)

sentiment_summary <- sentiment_df %>%
  mutate(Character = tolower(Character)) %>% 
  group_by(Character, season) %>%
  summarize(avg_sentiment = mean(sentiment),
            word_count = n_distinct(word)) %>% 
  filter(grepl("^\\w+$", Character)) %>% 
  filter(Character %in% c("bluey", "bingo","bandit","chilli","muffin"))


ggplot(sentiment_summary, aes(x = season, y = avg_sentiment, color = Character, group = Character)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  theme_minimal() +
  labs(title = "Character Sentiment by Season",
       x = "Season", 
       y = "Average Sentiment",
       color = "Character") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(sentiment_summary, aes(x = factor(season), y = Character, fill = avg_sentiment)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(low = "red", high = "green", mid = "white", midpoint = 0, 
                       limit = c(min(sentiment_summary$avg_sentiment), max(sentiment_summary$avg_sentiment)),
                       space = "Lab", name="Sentiment") +
  theme_minimal() +
  labs(title = "Character Sentiment Heatmap by Season", 
       x = "Season", 
       y = "Character")

# Specify the URL
url2 <- "https://www.imdb.com/title/tt7678620/ratings/?ref_=tt_ov_rt"

# Read the HTML content of the webpage
webpage2 <- read_html(url2)

# Extract the tables from the webpage
tables2 <- webpage2 %>%
  html_table(fill = TRUE)


# Filter out the first two tables
filtered_tables2 <- tables2 %>%
  discard(~ identical(., tables2[[1]])) %>% 
  # map_dfr(~{
  #   colnames(.x) <- c("#", "Title", "Airdate", "Premier Viewers", "viewers_for_first_airing")
  #   .x
  # }) %>%
  # clean_names()
