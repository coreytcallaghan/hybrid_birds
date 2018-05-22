## A script to calculate hybrids
library(readr)
library(dplyr)
library(RSQLite)
library(ggplot2)
library(forcats)

eBird_all_db <- src_sqlite("D:/eBird_November2017/eBird_November2017.sqlite", create=FALSE)
eBird_all <- tbl(eBird_all_db, "usa-2017")


df <- eBird_all %>%
  select(SAMPLING_EVENT_IDENTIFIER, COMMON_NAME, SCIENTIFIC_NAME, OBSERVATION_COUNT, STATE_PROVINCE,
         LOCALITY_ID, LOCALITY_TYPE, LATITUDE, LONGITUDE, OBSERVATION_DATE, CATEGORY,
         TIME_OBSERVATIONS_STARTED, OBSERVER_ID, PROTOCOL_TYPE, DURATION_MINUTES, 
         EFFORT_DISTANCE_KM, EFFORT_AREA_HA, NUMBER_OBSERVERS, ALL_SPECIES_REPORTED, GROUP_IDENTIFIER) %>%
  filter(CATEGORY == "hybrid") %>%
  collect(n=Inf)


length(unique(df$COMMON_NAME))

summary <- df %>%
  group_by(COMMON_NAME) %>%
  summarise(obs=n()) %>%
  arrange(obs)

ggplot(summary, aes(x=fct_inorder(COMMON_NAME), y=obs)) +
  geom_bar(stat="identity")+
  theme_classic()+
  coord_flip()
