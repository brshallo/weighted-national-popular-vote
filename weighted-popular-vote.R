library(tidyverse)
library(politicaldata)

library(pins)
board_register_kaggle(token = "kaggle.json")

electoral_votes <- read_csv(pins::pin_get("daithibhard/us-electoral-college-votes-per-state-17882020", board = "kaggle")) %>% 
  filter(Year >= 1976) %>% 
  janitor::clean_names() %>% 
  rename(state_name = state, electoral_votes = votes)

states_translate <- tibble(state_name = c(state.name, "D.C."), 
                           state_abb = c(state.abb, "DC"))

# downloaded from:
# https://dataverse.harvard.edu/file.xhtml?persistentId=doi:10.7910/DVN/42MVDX/MFU99O&version=5.0
vote_raw <- read_csv("1976-2016-president.csv")

votes_clean <- vote_raw %>% 
  mutate(party = ifelse(is.na(party), "", party)) %>% 
  mutate(party = case_when(party == "democrat" ~ "dem",
                           party == "republican" ~ "rep",
                           TRUE ~ "other")) %>% 
  # filter(!writein) %>% # include if not wanting to include write-ins
  select(-state) %>% 
  rename(state = state_po) %>% 
  group_by(year, state, party) %>% 
  summarise(votes = sum(candidatevotes)) %>% 
  ungroup() %>% 
  spread(party, votes) %>% 
  relocate(other, .after = last_col()) %>% 
  mutate(across(c(dem, rep, other), ~ ifelse(is.na(.), 0, .))) %>% 
  mutate(total_votes = dem + rep + other) %>% 
  mutate(across(c(dem, rep, other), ~ . / total_votes))

vote_totals <- votes_clean %>% 
  as_tibble() %>% 
  rename(state_abb = state) %>% 
  left_join(states_translate) %>% 
  left_join(electoral_votes)

# weighted popular vote
vote_totals %>% 
  mutate(other = 1 - (dem + rep)) %>%
  mutate(across(c(dem, rep, other), ~.*electoral_votes)) %>% 
  group_by(year) %>% 
  summarise(across(c(dem, rep, other), sum, na.rm = TRUE)) %>% 
  mutate(across(c(dem, rep, other), ~./538))

# raw popular votes
vote_totals %>% 
  mutate(other = 1 - (dem + rep)) %>%
  mutate(across(c(dem, rep, other), ~.*total_votes)) %>% 
  group_by(year) %>% 
  summarise(across(c(dem, rep, other), sum, na.rm = TRUE)) %>% 
  mutate(total_votes = dem + rep + other) %>% 
  mutate(across(c(dem, rep, other), ~./total_votes)) 
