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

vote_clean <- vote_raw %>% 
  mutate(party = ifelse(is.na(party), "", party)) %>% 
  mutate(party = case_when(party == "democratic-farmer-labor" ~ "democrat",
                           party == "democrat/republican" ~ "other",
                           TRUE ~ party)) %>% 
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

vote_totals <- vote_clean %>% 
  as_tibble() %>% 
  rename(state_abb = state) %>% 
  left_join(states_translate) %>% 
  left_join(electoral_votes)

# weighted popular vote (kable)
vote_totals %>% 
  mutate(other = 1 - (dem + rep)) %>%
  mutate(across(c(dem, rep, other), ~.*electoral_votes)) %>% 
  group_by(year) %>% 
  summarise(across(c(dem, rep, other), sum, na.rm = TRUE)) %>%
  print() %>% 
  mutate(across(c(dem, rep, other), ~round(./538, 3))) %>% 
  knitr::kable()

# # raw popular votes
# vote_totals %>% 
#   mutate(other = 1 - (dem + rep)) %>%
#   mutate(across(c(dem, rep, other), ~.*total_votes)) %>% 
#   group_by(year) %>% 
#   summarise(across(c(dem, rep, other), sum, na.rm = TRUE)) %>% 
#   mutate(total_votes = dem + rep + other) %>% 
#   mutate(across(c(dem, rep, other), ~./total_votes)) 

# Vote segmentation by state
vote_totals %>% 
  mutate(other = 1 - (dem + rep)) %>%
  mutate(across(c(dem, rep, other), ~.*electoral_votes)) %>% 
  filter(year == 2012) %>% 
  select(year, state_abb, dem, rep, other) %>% 
  pivot_longer(c(dem, rep, other), 
               names_to = "party", 
               values_to = "electoral_votes") %>% 
  mutate(state_abb = forcats::fct_reorder(state_abb, electoral_votes, sum),
         party = factor(party, c("dem", "rep", "other"))) %>%
  ggplot(aes(x = state_abb, y = electoral_votes, fill = party))+
  geom_bar(position = "stack", stat = "identity")+
  scale_fill_manual(values = c("blue", "red", "grey"))+
  labs(title = "State electoral votes allocated by proportion of state vote",
       subtitle = "2012 election", 
       x = "State", 
       y = "Electoral Votes",
       caption = "Electoral votes may be divided fractionally.")+
  coord_flip()+
  theme_bw()

# ballot influence by state
vote_totals %>% 
  select(year, state_abb, state_name, total_votes, electoral_votes) %>% 
  group_by(year) %>% 
  mutate(gen_votes = sum(total_votes)) %>% 
  ungroup() %>% 
  mutate(votes_per_electoral = (total_votes / electoral_votes),
         votes_per_electoral_gen = gen_votes / 538) %>%
  mutate(vote_weight = votes_per_electoral_gen / votes_per_electoral) %>% 
  filter(year == 2012) %>% 
  arrange(desc(vote_weight)) %>% 
  select(year, state_abb, state_name, vote_weight, total_votes) %>% 
  mutate(state_abb = fct_reorder(state_abb, vote_weight)) %>% 
  ggplot(aes(x = state_abb, y = vote_weight))+
  geom_col()+
  coord_flip()+
  theme_bw()+
  labs(x = "State",
       y = "Number of votes a ballot is worth",
       title = "Value of a ballot when weighting by the electoral college",
       subtitle = "2012 election")
