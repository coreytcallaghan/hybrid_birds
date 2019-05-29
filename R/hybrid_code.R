## A script to calculate hybrids
library(readr)
library(dplyr)
library(RSQLite)
library(ggplot2)
library(forcats)
library(stringr)
library(tidyr)
library(RMariaDB)
library(dbplyr)

## establish connection to MaraDB
## function
get_con  <- function () {
  dbConnect(RMariaDB::MariaDB(), host = 'KESTREL', dbname='ebird',user = Sys.getenv('userid'), password = Sys.getenv('pwd'))
} 

# make connection
eBird_all <- get_con()


# list tables in MariaDb
dbListTables(eBird_all)

# sites
sites <- tbl(eBird_all, 'sites')

# checklists
lists <- tbl(eBird_all, 'checklists')

# samples
obs <- tbl(eBird_all, 'samples')

# species
species <- tbl(eBird_all, 'species')

# filter sites for US sites only
# filter to between 2010 to 2018
# this provides us with how many observations
# over the entire time period
# and the area of interest
df <- sites %>%
  dplyr::filter(Country == "United States") %>%
  left_join(., lists, by="LOCALITY_ID") %>% 
  dplyr::filter(OBSERVATION_DATE > "2010-01-01") %>%
  left_join(., obs, by="SAMPLING_EVENT_IDENTIFIER") %>%
  left_join(., species, by="TAXONOMIC_ORDER") %>%
  dplyr::select(SAMPLING_EVENT_IDENTIFIER, COMMON_NAME, SCIENTIFIC_NAME, OBSERVATION_COUNT, STATE_PROVINCE,
                LOCALITY_ID, LOCALITY_TYPE, LATITUDE, LONGITUDE, OBSERVATION_DATE, CATEGORY,
                TIME_OBSERVATIONS_STARTED, OBSERVER_ID, PROTOCOL_TYPE, DURATION_MINUTES, 
                EFFORT_DISTANCE_KM, EFFORT_AREA_HA, NUMBER_OBSERVERS, ALL_SPECIES_REPORTED, GROUP_IDENTIFIER) %>%
  group_by(COMMON_NAME, CATEGORY) %>%
  summarise(N=n()) %>%
  collect(n=Inf)

saveRDS(df, file = "Data/all_observations.RDS")

# now remove all the categories not interested in
# then resummarise the data so its just Common Name and
# the number of records


# this will read in the above dataframe instead of
# querying the MariaDB
# df <- readRDS("Data/all_observations.RDS")
unique(df$CATEGORY)

df2 <- df %>%
  filter(CATEGORY %in% c("species", "issf", "hybrid", "intergrade", "form", "domestic")) %>%
  group_by(COMMON_NAME) %>%
  summarise(N=sum(N))


## Total number of observations
sum(df2$N)



## Now get only the hybrids and create a dataframe
df_hybrid <- sites %>%
  dplyr::filter(Country == "United States") %>%
  left_join(., lists, by="LOCALITY_ID") %>% 
  dplyr::filter(OBSERVATION_DATE > "2010-01-01") %>%
  left_join(., obs, by="SAMPLING_EVENT_IDENTIFIER") %>%
  left_join(., species, by="TAXONOMIC_ORDER") %>%
  dplyr::select(SAMPLING_EVENT_IDENTIFIER, COMMON_NAME, SCIENTIFIC_NAME, OBSERVATION_COUNT, STATE_PROVINCE,
                LOCALITY_ID, LOCALITY_TYPE, LATITUDE, LONGITUDE, OBSERVATION_DATE, CATEGORY,
                TIME_OBSERVATIONS_STARTED, OBSERVER_ID, PROTOCOL_TYPE, DURATION_MINUTES, 
                EFFORT_DISTANCE_KM, EFFORT_AREA_HA, NUMBER_OBSERVERS, ALL_SPECIES_REPORTED, GROUP_IDENTIFIER) %>%
  filter(CATEGORY == "hybrid") %>%
  group_by(COMMON_NAME, SCIENTIFIC_NAME) %>%
  summarise(obs=n()) %>%
  collect(n=Inf)

# save this as an RDS so I don't have to query the database again!
saveRDS(df_hybrid, file = "Data/hybrid_summary_df.RDS")

# read RDS to save time
# df_hybrid <- readRDS("Data/hybrid_summary_df.RDS")

# change integer into something else
df_hybrid <- df_hybrid %>%
  ungroup() %>%
  mutate(obs=as.integer(.$obs)) %>%
  arrange(obs)

# summarise this df_hybrid
length(unique(df_hybrid$COMMON_NAME))

# make a barplot
ggplot(df_hybrid, aes(x=fct_inorder(COMMON_NAME), y=obs)) +
  geom_bar(stat="identity")+
  theme_classic()+
  coord_flip()

## Get a list of all species belong to a hybrid pair
all_species <- df_hybrid %>%
  dplyr::select(SCIENTIFIC_NAME) %>%
  distinct()

# first, easily extract the first species in the pair
first_species <- data.frame(species=word(all_species$SCIENTIFIC_NAME, 1,2, sep=" "))

# Now, get the second species in the pair
second_temp <- data.frame(second_species=word(all_species$SCIENTIFIC_NAME, 4, 5, sep=" ")) %>%
  mutate(ID=1:nrow(.))

second_temp1 <- data.frame(first=word(all_species$SCIENTIFIC_NAME, 1, sep=" "),
                           second=word(all_species$SCIENTIFIC_NAME, 4, sep=" ")) %>%
  unite(second_species, first, second, sep=" ") %>%
  mutate(ID=1:nrow(.))

# combine the above
second_species <- second_temp %>%
  left_join(., second_temp1, by="ID") %>%
  mutate(species=ifelse(is.na(.$second_species.x), .$second_species.y, as.character.factor(.$second_species.x))) %>%
  dplyr::select(species)

# combine the first species and second species with the original scientific name
all_species.2 <- cbind(all_species, first_species, second_species)

species_list <- rbind(first_species, second_species) %>%
  distinct() %>%
  filter(species != "Anser sp.") %>%
  filter(species != "cyanoptera (F1") %>%
  filter(species != "cyanoptera (F2") %>%
  filter(species != "type) x") %>%
  filter(species != "cygnoides (Domestic") %>%
  filter(species != "Selasphorus sp.")

# now get the number of observations from the eBird database for each of these species
df2 <- sites %>%
  dplyr::filter(Country == "United States") %>%
  left_join(., lists, by="LOCALITY_ID") %>% 
  dplyr::filter(OBSERVATION_DATE > "2010-01-01") %>%
  left_join(., obs, by="SAMPLING_EVENT_IDENTIFIER") %>%
  left_join(., species, by="TAXONOMIC_ORDER") %>%
  dplyr::select(SAMPLING_EVENT_IDENTIFIER, COMMON_NAME, SCIENTIFIC_NAME, OBSERVATION_COUNT, STATE_PROVINCE,
         LOCALITY_ID, LOCALITY_TYPE, LATITUDE, LONGITUDE, OBSERVATION_DATE, CATEGORY,
         TIME_OBSERVATIONS_STARTED, OBSERVER_ID, PROTOCOL_TYPE, DURATION_MINUTES, 
         EFFORT_DISTANCE_KM, EFFORT_AREA_HA, NUMBER_OBSERVERS, ALL_SPECIES_REPORTED, GROUP_IDENTIFIER) %>%
  filter(SCIENTIFIC_NAME %in% species_list$species) %>%
  group_by(COMMON_NAME, SCIENTIFIC_NAME) %>%
  summarise(N=n()) %>%
  collect(n=Inf)

# save this as an RDS so I don't have to query the database again!
saveRDS(df2, file = "Data/all_observations.RDS")

# read RDS to save time
# all_observations <- readRDS("Data/all_observations.RDS")


# write out some files
# summary file
write_csv(df_hybrid, "Summaries/summary.csv")

# species key for hybrid and associated scientific name
write_csv(all_species.2, "Summaries/species_key.csv")

# number of total observations for each species
write_csv(df2, "Summaries/number_obs_species.csv")



######################################
######## Have a quick look at family information
######### see if I can get a count for number of families of each species
# number of observations for each species in the eBird database
df3 <- sites %>%
  dplyr::filter(Country == "United States") %>%
  left_join(., lists, by="LOCALITY_ID") %>% 
  dplyr::filter(OBSERVATION_DATE > "2010-01-01") %>%
  left_join(., obs, by="SAMPLING_EVENT_IDENTIFIER") %>%
  left_join(., species, by="TAXONOMIC_ORDER") %>%
  dplyr::select(SAMPLING_EVENT_IDENTIFIER, COMMON_NAME, SCIENTIFIC_NAME, OBSERVATION_COUNT, STATE_PROVINCE,
         LOCALITY_ID, LOCALITY_TYPE, LATITUDE, LONGITUDE, OBSERVATION_DATE, CATEGORY,
         TIME_OBSERVATIONS_STARTED, OBSERVER_ID, PROTOCOL_TYPE, DURATION_MINUTES, 
         EFFORT_DISTANCE_KM, EFFORT_AREA_HA, NUMBER_OBSERVERS, ALL_SPECIES_REPORTED, GROUP_IDENTIFIER) %>%
  group_by(COMMON_NAME, SCIENTIFIC_NAME) %>%
  summarise(N=n()) %>%
  collect(n=Inf)

# save this as an RDS so I don't have to query the database again!
saveRDS(df3, file = "Data/family_summary_df.RDS")

# read RDS to save time
# df_families <- readRDS("Data/family_summary_df.RDS")


# read in clements dataset
clements_list <- read_csv("Data/Clements-Checklist-v2018-August-2018.csv") %>%
  rename(SCIENTIFIC_NAME = `scientific name`)


# join the clements list with the list of all species (df3) from above
joined_species <- df_families %>%
  inner_join(., clements_list, by="SCIENTIFIC_NAME")

# A few things to look at, but still a decent start
# let's get a summary of observations per family
family_summary <- joined_species %>%
  group_by(family) %>%
  summarise(total_obs=sum(N))

# write out this dataframe of family summary
write_csv(family_summary, "Summaries/family_summary.csv")













############################################ OLD CODE #######################################
---------------------------------------------------------------------------------------------

## First find out how many observations exist for the entire database
## Let's do it between 2010-2017
## bit of a hack to do it
df <- eBird_all %>%
  dplyr::select(SAMPLING_EVENT_IDENTIFIER, COMMON_NAME, SCIENTIFIC_NAME, OBSERVATION_COUNT, STATE_PROVINCE,
         LOCALITY_ID, LOCALITY_TYPE, LATITUDE, LONGITUDE, OBSERVATION_DATE, CATEGORY,
         TIME_OBSERVATIONS_STARTED, OBSERVER_ID, PROTOCOL_TYPE, DURATION_MINUTES, 
         EFFORT_DISTANCE_KM, EFFORT_AREA_HA, NUMBER_OBSERVERS, ALL_SPECIES_REPORTED, GROUP_IDENTIFIER) %>%
  group_by(COMMON_NAME, CATEGORY) %>%
  summarise(N=n()) %>%
  collect(n=Inf)









eBird_all_db <- src_sqlite("D:/eBird_November2017/eBird_November2017.sqlite", create=FALSE)
eBird_all <- tbl(eBird_all_db, "usa-2017")


## First find out how many observations exist for the entire database
## Let's do it between 2010-2017
## bit of a hack to do it
df <- eBird_all %>%
  select(SAMPLING_EVENT_IDENTIFIER, COMMON_NAME, SCIENTIFIC_NAME, OBSERVATION_COUNT, STATE_PROVINCE,
         LOCALITY_ID, LOCALITY_TYPE, LATITUDE, LONGITUDE, OBSERVATION_DATE, CATEGORY,
         TIME_OBSERVATIONS_STARTED, OBSERVER_ID, PROTOCOL_TYPE, DURATION_MINUTES, 
         EFFORT_DISTANCE_KM, EFFORT_AREA_HA, NUMBER_OBSERVERS, ALL_SPECIES_REPORTED, GROUP_IDENTIFIER) %>%
  group_by(COMMON_NAME, CATEGORY) %>%
  summarise(N=n()) %>%
  collect(n=Inf)

# now remove all the categories not interested in
# then resummarise the data so its just Common Name and
# the number of records
unique(df$CATEGORY)

df2 <- df %>%
  filter(CATEGORY %in% c("species", "issf", "hybrid", "intergrade", "form", "domestic")) %>%
  group_by(COMMON_NAME) %>%
  summarise(N=sum(N))


## Total number of observations
sum(df2$N)



df_hybrid <- eBird_all %>%
  select(SAMPLING_EVENT_IDENTIFIER, COMMON_NAME, SCIENTIFIC_NAME, OBSERVATION_COUNT, STATE_PROVINCE,
         LOCALITY_ID, LOCALITY_TYPE, LATITUDE, LONGITUDE, OBSERVATION_DATE, CATEGORY,
         TIME_OBSERVATIONS_STARTED, OBSERVER_ID, PROTOCOL_TYPE, DURATION_MINUTES, 
         EFFORT_DISTANCE_KM, EFFORT_AREA_HA, NUMBER_OBSERVERS, ALL_SPECIES_REPORTED, GROUP_IDENTIFIER) %>%
  filter(CATEGORY == "hybrid") %>%
  group_by(COMMON_NAME, SCIENTIFIC_NAME) %>%
  summarise(obs=n()) %>%
  collect(n=Inf)


length(unique(df$COMMON_NAME))

summary <- df %>%
  group_by(COMMON_NAME, SCIENTIFIC_NAME) %>%
  summarise(obs=n()) %>%
  arrange(obs)

ggplot(summary, aes(x=fct_inorder(COMMON_NAME), y=obs)) +
  geom_bar(stat="identity")+
  theme_classic()+
  coord_flip()

## Get a list of all species belong to a hybrid pair
all_species <- df_hybrid %>%
  dplyr::select(SCIENTIFIC_NAME) %>%
  distinct()

# first, easily extract the first species in the pair
first_species <- data.frame(species=word(all_species$SCIENTIFIC_NAME, 1,2, sep=" "))

# Now, get the second species in the pair
second_temp <- data.frame(second_species=word(all_species$SCIENTIFIC_NAME, 4, 5, sep=" ")) %>%
  mutate(ID=1:nrow(.))

second_temp1 <- data.frame(first=word(all_species$SCIENTIFIC_NAME, 1, sep=" "),
                           second=word(all_species$SCIENTIFIC_NAME, 4, sep=" ")) %>%
  unite(second_species, first, second, sep=" ") %>%
  mutate(ID=1:nrow(.))

# combine the above
second_species <- second_temp %>%
  left_join(., second_temp1, by="ID") %>%
  mutate(species=ifelse(is.na(.$second_species.x), .$second_species.y, as.character.factor(.$second_species.x))) %>%
  dplyr::select(species)


# combine the first species and second species with the original scientific name
all_species.2 <- cbind(all_species, first_species, second_species)

species_list <- rbind(first_species, second_species) %>%
  distinct() %>%
  filter(species != "Anser sp.") %>%
  filter(species != "cyanoptera (F1") %>%
  filter(species != "cyanoptera (F2") %>%
  filter(species != "type) x") %>%
  filter(species != "cygnoides (Domestic") %>%
  filter(species != "Selasphorus sp.")


# now get the number of observations from the eBird database for each of these species
df2 <- eBird_all %>%
  select(SAMPLING_EVENT_IDENTIFIER, COMMON_NAME, SCIENTIFIC_NAME, OBSERVATION_COUNT, STATE_PROVINCE,
         LOCALITY_ID, LOCALITY_TYPE, LATITUDE, LONGITUDE, OBSERVATION_DATE, CATEGORY,
         TIME_OBSERVATIONS_STARTED, OBSERVER_ID, PROTOCOL_TYPE, DURATION_MINUTES, 
         EFFORT_DISTANCE_KM, EFFORT_AREA_HA, NUMBER_OBSERVERS, ALL_SPECIES_REPORTED, GROUP_IDENTIFIER) %>%
  filter(SCIENTIFIC_NAME %in% species_list$species) %>%
  group_by(COMMON_NAME, SCIENTIFIC_NAME) %>%
  summarise(N=n()) %>%
  collect(n=Inf)



# write out some files
# summary file
write_csv(summary, "C:/Users/CTC/Desktop/summary.csv")

# species key for hybrid and associated scientific name
write_csv(all_species.2, "C:/Users/CTC/Desktop/species_key.csv")

# number of total observations for each species
write_csv(df2, "C:/Users/CTC/Desktop/number_obs_species.csv")
           


######################################
######## Have a quick look at family information
######### see if I can get a count for number of families of each species
# number of observations for each species in the eBird database
df3 <- eBird_all %>%
  select(SAMPLING_EVENT_IDENTIFIER, COMMON_NAME, SCIENTIFIC_NAME, OBSERVATION_COUNT, STATE_PROVINCE,
         LOCALITY_ID, LOCALITY_TYPE, LATITUDE, LONGITUDE, OBSERVATION_DATE, CATEGORY,
         TIME_OBSERVATIONS_STARTED, OBSERVER_ID, PROTOCOL_TYPE, DURATION_MINUTES, 
         EFFORT_DISTANCE_KM, EFFORT_AREA_HA, NUMBER_OBSERVERS, ALL_SPECIES_REPORTED, GROUP_IDENTIFIER) %>%
  group_by(COMMON_NAME, SCIENTIFIC_NAME) %>%
  summarise(N=n()) %>%
  collect(n=Inf)

# read in clements dataset
clements_list <- read_csv("C:/Users/CTC/Desktop/Clements-Checklist-v2017-August-2017_2.csv") %>%
  rename(SCIENTIFIC_NAME = `scientific name`)


# join the clements list with the list of all species (df3) from above
joined_species <- df3 %>%
  inner_join(., clements_list, by="SCIENTIFIC_NAME")

# A few things to look at, but still a decent start
# let's get a summary of observations per family
family_summary <- joined_species %>%
  group_by(family) %>%
  summarise(total_obs=sum(N))

# write out this dataframe of family summary
write_csv(family_summary, "C:/Users/CTC/Desktop/family_summary.csv")


