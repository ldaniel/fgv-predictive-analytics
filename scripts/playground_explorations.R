# loading required libraries --------------------------------------------------
library(dplyr)
library(ggplot2)
library(tidyr)
library(lubridate)
library(ggalluvial)
library(stringr)
library(VIM)
library(psych)
library(ggthemes)
library(feather)
library(plotly)
library(leaflet)
library(geojsonio)

inst# loading other scripts do be used here ---------------------------------------
source("./scripts/step_00_config_environment.R")
source("./scripts/step_01_create_functions.R")
source("./scripts/step_02_data_ingestion.R")
source("./scripts/step_03_data_cleaning.R")
source("./scripts/step_04_label_translation.R")
source("./scripts/step_05_data_enhancement.R")

# gender distribution of clients in the bank ----------------------------------
p <- ggplot(data = client) +
  aes(x = gender, fill = gender) +
  geom_bar() +
  labs(title = "Gender distribution of clients in the bank",
       subtitle = "A well balanced bank",
       x = "Gender",
       y = "Total clients") +
  theme_economist()

ggplotly(p)

# gender distribution of clients in the bank over the decades -----------------
clientGenderOverDecades <- client %>% 
  group_by(decade = as.integer(substr(client$birth_number, 1,1)) * 10, gender = client$gender) %>% 
  count()

p <- ggplot(data = clientGenderOverDecades) +
  aes(x = decade, fill = gender, weight = n) +
  scale_x_continuous(breaks = c(0, 10, 20, 30, 40, 50, 60, 70, 80)) +
  geom_bar() +
  geom_line(aes(y = n, color = gender)) +
  labs(title = "Gender distribution of clients in the bank over the decades",
       subtitle = "Equality at its finest",
       x = "Decades",
       y = "Total clients") +
  theme_economist() +
  facet_wrap(vars(gender))

ggplotly(p)

# alluvial diagram representation of gender, age group and region -------------
clientGenderAgeGroupByRegion <- client %>% 
  mutate(age_group = ifelse(age < 21, "young", ifelse(age >= 21 & age <= 60, "adult", "senior"))) %>% 
  inner_join(district, by = "district_id") %>% 
  group_by(age_group, gender, region) %>% 
  count()
  
ggplot(data = clientGenderAgeGroupByRegion, aes(axis1 = region, axis2 = age_group, y = n)) +
  scale_x_discrete(limits = c("region", "age group"), expand = c(.1, .1)) +
  xlab("Demographic") +
  geom_alluvium(aes(fill = gender), knot.pos = 0) +
  geom_stratum() + 
  geom_text(stat = "stratum", label.strata = TRUE) +
  theme_economist() +
  ggtitle("Region and age group by gender", "Equality is everywhere") 

# Loan Analisys - Delinquency Rate by Region ----------------------------------
left_join(loan, disposition, by = 'account_id') %>% 
  left_join(client, by = 'client_id') %>% 
  left_join(district, by = 'district_id') %>% 
  group_by(region, contract_status, defaulter) %>% 
  summarise(count = n(),
            amount = sum(amount)) %>% 
  group_by(region, contract_status) %>% 
  mutate(count_contract_status = sum(count),
         amount_contract_status = sum(amount)) %>% 
  group_by(region) %>% 
  mutate(count_region = sum(count),
         amount_region = sum(amount)) %>% 
  ggplot(aes(x = defaulter, y = contract_status, fill = count / count_region)) +
  geom_bin2d(stat = 'identity') +
  geom_text(aes(label = paste(round(count / count_region * 100, 2), '%')), color = 'white') +
  facet_wrap(~region) +
  theme_economist() +
  theme(legend.position = 'none', panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  labs(x = 'Defaulter', y = 'Contract Status', title = 'Loan Contract Status by Region Heatmap')

# Loan Analisys - Delinquency Rate by Age Group -------------------------------
left_join(loan, disposition, by = 'account_id') %>%
  left_join(client, by = 'client_id') %>% 
  left_join(district, by = 'district_id') %>% 
  group_by(age_bin, contract_status, defaulter) %>% 
  summarise(count = n(),
            amount = sum(amount)) %>% 
  group_by(age_bin, contract_status) %>% 
  mutate(count_contract_status = sum(count),
         amount_contract_status = sum(amount)) %>% 
  group_by(age_bin) %>% 
  mutate(count_age_bin = sum(count),
         amount_age_bin = sum(amount)) %>% 
  ggplot(aes(x = defaulter, y = contract_status, fill = count / count_age_bin)) +
    geom_bin2d(stat = 'identity') +
    geom_text(aes(label = paste(round(count / count_age_bin * 100, 2), '%')), color = 'white') +
    facet_wrap(~age_bin) +
    theme_economist() +
    theme(legend.position = 'none', panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    labs(x = 'Defaulter',
         y = 'Contract Status',
         title = 'Loan Contract Status by Age Group Heatmap')

# Loan Analisys - Delinquency Rate by Gender ----------------------------------
left_join(loan, disposition, by = 'account_id') %>%
  left_join(client, by = 'client_id') %>% 
  left_join(district, by = 'district_id') %>% 
  group_by(gender, contract_status, defaulter) %>% 
  summarise(count = n(),
            amount = sum(amount)) %>% 
  group_by(gender, contract_status) %>% 
  mutate(count_contract_status = sum(count),
         amount_contract_status = sum(amount)) %>% 
  group_by(gender) %>% 
  mutate(count_gender = sum(count),
         amount_gender = sum(amount)) %>% 
  ggplot(aes(x = defaulter, y = contract_status, fill = count / count_gender)) +
    geom_bin2d(stat = 'identity') +
    geom_text(aes(label = paste(round(count / count_gender * 100, 2), '%')), color = 'white') +
    facet_wrap(~gender) +
    theme_economist() +
    theme(legend.position = 'none', panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
    labs(x = 'Defaulter',
         y = 'Contract Status',
         title = 'Loan Contract Status by Gender Heatmap')

# Account Balance Analisys ----------------------------------------------------

left_join(account_balance, disposition, by = 'account_id') %>%
  left_join(client, by = 'client_id') %>% 
  left_join(district, by = 'district_id') %>% 
  filter(type == 'Owner') %>% 
  ggplot(aes(avg_balance)) +
    geom_density(alpha = 0.5, aes(fill = gender)) +
    scale_x_continuous(labels = scales::comma) +
    labs(title = 'Average Account Balance Distribution by Gender and Region') +
    theme_economist() +
    facet_wrap(~region)

# Ploting map with loadn district analysis ------------------------------------------

loan_amount_by_region <- select(loan, account_id, amount, defaulter, contract_status) %>% 
  filter(defaulter == TRUE) %>% 
  inner_join(account) %>% 
  inner_join(district) %>% 
  group_by(region) %>% 
  summarise(transaction_count = n(),
            amount = sum(amount)) %>% 
  inner_join(czech_regions_coords)

jsonMapFile <- "./map/czech-republic-regions.json"
czech_regions <- as.json(geojson_read(jsonMapFile))

leaflet(loan_amount_by_region) %>% 
  addTiles() %>% 
  setView(lng = 15.3, lat = 49.8, zoom = 7) %>%
  addGeoJSON(czech_regions, fillColor = "red", stroke = "#555555") %>% 
  addCircles(lng = ~long, 	
             lat = ~lat, 	
             weight = 2, 	
             radius = ~sqrt(amount) * 30, 	
             popup = ~region)

