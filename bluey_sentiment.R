library(tidyverse)
library(rvest)
library(janitor)


# Specify the URL
url <- "https://blueypedia.fandom.com/wiki/Episode_List"

# Read the HTML content of the webpage
webpage <- read_html(url)

# Extract the tables from the webpage
tables <- html_table(webpage)

# Filter out the first two tables
filtered_tables <- tables %>%
  discard(~ identical(., tables[[1]]) || identical(., tables[[2]]))


# Since html_table returns a list of data frames, you might need to access the first element
# Assuming you want to combine all tables from the page, you can use bind_rows
combined_table <- bind_rows(filtered_tables) %>% 
  janitor::clean_names() %>% 
  filter(!grepl("Part \\d+", number)) %>% 
  remove_empty(which = "cols") %>% 
  mutate(viewers = gsub("\\[\\d+\\]", "",viewers_for_first_airing)) %>% 
  mutate(viewers = as.numeric(gsub("\\D", "", viewers))) %>% 
  mutate(number = as.numeric(number)) %>% 
  mutate(season = cumsum(number  < lag(number , default = first(number))) + 1) %>% 
  mutate(title_under = gsub(" ", "_", title)) 
  

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


tidy_episodes %>% 
  count(word, season, sort = TRUE) %>%
  filter(n > 50) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(n, word)) +
  geom_col() +
  labs(y = NULL) +
  facet_wrap(vars(season))



