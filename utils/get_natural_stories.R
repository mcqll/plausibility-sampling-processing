library(dplyr)
library(readr)
library(magrittr)
library(tidyr)

naturalstories_rootdir <- "https://raw.githubusercontent.com/languageMIT/naturalstories/master/"

all_stories <- read_tsv(
  paste0(naturalstories_rootdir, "naturalstories_RTS/all_stories.tok")
  ) %>% rename(word_num_in_story = zone, story_num = item)

write_tsv(all_stories, "data/natural_stories_words.tsv")


# Get natural stories grouped by sentence
amaze_naturalstories_rootdir <- "https://raw.githubusercontent.com/vboyce/amaze-natural-stories/master/"
natural_stories_sentences <- read_delim(
      paste0(amaze_naturalstories_rootdir,"Materials/natural_stories_sentences.tsv"),
      delim="\t") %>% rename(
        story_num = Story_Num,
        sentence_num = Sentence_Num, sentence = Sentence)
# Write out as tsv and as csv for convenience
write_tsv(natural_stories_sentences, "data/natural_stories_sentences.tsv")
write_csv(natural_stories_sentences, "data/natural_stories_sentences.csv")

natural_stories_words_and_sentences <- natural_stories_sentences %>%
    rename(word = sentence) %>% 
    separate_rows(word, sep = " ") %>% 
    group_by(story_num,sentence_num) %>% 
    mutate(word_num_in_sentence=1:n()) %>% 
    ungroup(sentence_num) %>% 
    mutate(word_num_in_story=1:n()) %>% ungroup()

write_tsv(natural_stories_words_and_sentences, "data/natural_stories_words_and_sentences.tsv")
