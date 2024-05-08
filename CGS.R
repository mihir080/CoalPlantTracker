##This is a script to analyse the impact of coal power plants and their 
#CO2 emissions

## Package Installation ##

##_____________________________________________________##

#Installing the required packages for a brief analysis
packages <- c('readr', 'tidyverse', 'ggplot2', 'ggtext', 'tidytext', 
              'ggthemes', 'broom', 'knitr', 'plotly', 'sf', 
              "rnaturalearthdata", 'rnaturalearth', 'readxl', 'xts', 
              'reshape2', "writexl")

#install all packages that are not already installed
install.packages(setdiff(packages, rownames(installed.packages())))

# Loading all packages as required
for (pkg in packages){
  library(pkg, character.only = TRUE)
}

##_____________________________________________________##

## Data Reading ##

##_____________________________________________________##

#Reading in the datasets
#2017 Coal plants data set
coalplants_2017 <- read_excel("Global-Coal-Plant-Tracker-Jan-2017.xlsx", 
                              sheet = "Coal Projects")
#2023 Coal plants data set
coalplants_2023 <- read_excel("Global-Coal-Plant-Tracker-July-2023.xlsx", 
                              sheet = "Coal Projects")

#Checking the structure and head of dataset
#2017
str(coalplants_2017)
head(coalplants_2017)
#2023 
str(coalplants_2023)
head(coalplants_2023)

##_____________________________________________________##

## Data Cleaning and Wrangling ##

##_____________________________________________________##

#Subsetting the data _ Keeping the variables that are currently 
#required for the analysis

coal2017 <- coalplants_2017 %>% 
  select('Tracker ID', 'Plant', 'Parent', 'Capacity (MW)', 'Status',
         'Year', 'Year Retire', 'Location', 'Country', 'Latitude', 'Longitude',
         'Heat rate', 'Emission factor', 'Capacity factor', 
         'Annual CO2 (mt/year)') %>% 
  rename(
    Tracker_ID = 'Tracker ID' ## Creating a, index variable
  )

coal2023 <- coalplants_2023 %>% 
  select('GEM unit/phase ID', 'Country', 'Plant name', 'Parent', 
         'Capacity (MW)', 
         'Status', 'Retired year', 'Location', 'Latitude', 'Longitude',
         'Heat rate (Btu per kWh)', 'Emission factor (kg of CO2 per TJ)', 
         'Capacity factor', 'Annual CO2 (million tonnes / annum)', 
         'Remaining plant lifetime (years)') %>% 
  rename(
    Tracker_ID = 'GEM unit/phase ID' ## Creating a, index variable
  )

# Creating a combined dataset for the tracker 
dataset <- merge(coal2017, coal2023, by.x = 'Tracker_ID', by.y = 'Tracker_ID', 
                 all = TRUE) %>% 
  
  #Removing duplicate variables
  select(-'Country.x', -'Parent.x', -'Location.x', -'Longitude.x', 
         -'Latitude.x',-'Capacity factor.x', -'Capacity factor.y', 
         -'Location.y') %>% 
  
  # Renaming all the variables according to year
  rename(
    PlantNames_2017 = 'Plant', 
    PlantNames_2023 = 'Plant name',
    Country = 'Country.y',
    Status_2017 = 'Status.x',
    Status_2023 = 'Status.y',
    Capacity_2017 = 'Capacity (MW).x',
    Capacity_2023 = 'Capacity (MW).y',
    HeatRate_2017 = 'Heat rate',
    Emission_2017 = 'Emission factor',
    AnnualCO2_2017 = 'Annual CO2 (mt/year)',
    HeatRate_2023 = 'Heat rate (Btu per kWh)',
    Emission_2023 = 'Emission factor (kg of CO2 per TJ)',
    AnnualCO2_2023 = 'Annual CO2 (million tonnes / annum)',
    RemainingLife_2023 = 'Remaining plant lifetime (years)',
    ParentCo = 'Parent.y',
    Longitude = 'Longitude.y',
    Latitude = 'Latitude.y'
  )

#Changing the datatype of Years and combining the 'Year Retired' Columns
dataset$`Year Retire` = as.character(dataset$`Year Retire`)
dataset$`Retired year` = as.character(dataset$`Retired year`)
dataset$YearRetired <- dataset$`Year Retire`  
dataset$YearRetired[!is.na(dataset$`Retired year`)] = 
  dataset$`Retired year`[!is.na(dataset$`Retired year`)]

# Removing the extra variables
dataset <- dataset %>% 
  select(-'Year Retire', -'Retired year')

# Reordering the dataset for ease of use
colnames(dataset)
order <- c("Tracker_ID", "PlantNames_2017", "PlantNames_2023", "ParentCo", 
           "Country", "Year", 
           "YearRetired", "Status_2017", "Status_2023", "Capacity_2017", 
           "Capacity_2023", "Emission_2017", "Emission_2023", "AnnualCO2_2017", 
           "AnnualCO2_2023", "HeatRate_2017", "HeatRate_2023", "Longitude", 
           "Latitude", "RemainingLife_2023")
dataset <- dataset[, order] %>% 
  #Removing values without a country for country level analysis
  filter(!is.na(Country)) 

#Checking country names to maintain uniformity and consistency
unique_country <- unique(dataset$Country)
print(unique_country)
dataset$Country <- gsub("Türkiye", "Turkey", dataset$Country)

#Standardising Power Plant Operating Status
#2017
unique_status17 <- unique(dataset$Status_2017)
print(unique_status17)
dataset$Status_2017 <- gsub("Pre", "Pre-Permit", dataset$Status_2017)
dataset$Status_2017 <- gsub("retired", "Retired", dataset$Status_2017)
dataset$Status_2017 <- gsub("permitted", "Permitted", dataset$Status_2017)
dataset$Status_2017 <- gsub("cancelled", "Cancelled", dataset$Status_2017)
dataset$Status_2017 <- gsub("shelved", "Shelved", dataset$Status_2017)
dataset$Status_2017 <- gsub("Pre-Permit-Permit", "Pre-Permit", 
                            dataset$Status_2017)

#2023
unique_status23 <- unique(dataset$Status_2023)
print(unique_status23)
dataset$Status_2023 <- gsub("operating", "Operating", dataset$Status_2023)
dataset$Status_2023 <- gsub("retired", "Retired", dataset$Status_2023)
dataset$Status_2023 <- gsub("cancelled", "Cancelled", dataset$Status_2023)
dataset$Status_2023 <- gsub("mothballed", "Deactivated", dataset$Status_2023)
dataset$Status_2023 <- gsub("pre-permit", "Pre-Permit", dataset$Status_2023)
dataset$Status_2023 <- gsub("permitted", "Permitted", dataset$Status_2023)
dataset$Status_2023 <- gsub("construction", "Construction", dataset$Status_2023)
dataset$Status_2023 <- gsub("announced", "Announced", dataset$Status_2023)
dataset$Status_2023 <- gsub("shelved", "Shelved", dataset$Status_2023)

#Creating a consolidated dataset grouping by country for a global analysis
dataset_country <- dataset %>% 
  select(-'Year', -'YearRetired', -'ParentCo', - 'Longitude', -'Latitude', 
         -'RemainingLife_2023') %>%
  group_by(Country) %>% 
  summarise(
    PowerPlants_2017 = n_distinct(PlantNames_2017, na.rm = TRUE),
    PowerPlants_2023 = n_distinct(PlantNames_2023, na.rm = TRUE),
    Capacity_2017 = sum(Capacity_2017, na.rm = TRUE),
    Capacity_2023 = sum(Capacity_2023, na.rm = TRUE),
    Emission_2017 = sum(Emission_2017, na.rm = TRUE),
    Emission_2023 = sum(Emission_2023, na.rm = TRUE),
    AnnualCO2_2017 = sum(AnnualCO2_2017, na.rm = TRUE),
    AnnualCO2_2023 = sum(AnnualCO2_2023, na.rm = TRUE),
    Operating_2017 = sum(Status_2017 == "Operating", na.rm = TRUE),
    Shelved_2017 = sum(Status_2017 == "Shelved", na.rm = TRUE),
    Retired_2017 = sum(Status_2017 == "Retired", na.rm = TRUE),
    Cancelled_2017 = sum(Status_2017 == "Cancelled", na.rm = TRUE),
    Permitted_2017 = sum(Status_2017 == "Permitted", na.rm = TRUE),
    PrePermit_2017 = sum(Status_2017 == "Pre-Permit", na.rm = TRUE),
    Announced_2017 = sum(Status_2017 == "Announced", na.rm = TRUE),
    Construction_2017 = sum(Status_2017 == "Construction", na.rm = TRUE),
    Operating_2023 = sum(Status_2023 == "Operating", na.rm = TRUE),
    Retired_2023 = sum(Status_2023 == "Retired", na.rm = TRUE),
    Cancelled_2023 = sum(Status_2023 == "Cancelled", na.rm = TRUE),
    Deactivated_2023 = sum(Status_2023 == "Deactivated", na.rm = TRUE),
    Shelved_2023 = sum(Status_2023 == "Shelved", na.rm = TRUE),
    PrePermit_2023 = sum(Status_2023 == "Pre-Permit", na.rm = TRUE),
    Permitted_2023 = sum(Status_2023 == "Permitted", na.rm = TRUE),
    Construction_2023 = sum(Status_2023 == "Construction", na.rm = TRUE),
    Announced_2023 = sum(Status_2023 == "Announced", na.rm = TRUE)
  )

# Exporting the final dataset

write_xlsx(dataset_country, "FinalDataset.xlsx")

##_____________________________________________________##

## Data Visualization ##

##_____________________________________________________##

## Cleaning global and local and importing cleaned data

rm(list = ls())

dataset_country <- read_excel("FinalDataset.xlsx")

view(dataset_country)

## Plotting change in total global capacity 

## Creating a dataset with total capacity and total number of power plants 
# to see the aggregate change

total_capacity <- dataset_country %>% 
  
  #Selecting relevatn columns
  select(Country, Capacity_2017, Capacity_2023, PowerPlants_2017, 
         PowerPlants_2023) %>%
  
  #Summarising to identify totals
  summarise(Capacity_2017 = sum(Capacity_2017, na.rm = TRUE), 
            Capacity_2023 = sum(Capacity_2023, na.rm = TRUE), 
            PowerPlants_2017 = sum(PowerPlants_2017, na.rm = TRUE),
            PowerPlants_2023 = sum(PowerPlants_2023, na.rm = TRUE))

#Creating a dataset for plotting
plot_total <- total_capacity %>% 
  pivot_longer(cols = starts_with("Capacity"), 
               names_to = "Year", 
               values_to = "Total_Capacity")

#Creating a preliminary plot to identify overall global trend
ggplot(plot_total, aes(x = Year, y = Total_Capacity)) +
  geom_bar(stat = "identity", width = 0.25) +
  labs(title = "Total Global Capacity (2017 vs. 2023)",
       x = "Year", y = "Total Capacity (MW)",
       fill = "Year") +
  theme_minimal() +
  scale_y_continuous(breaks = seq(0, max(plot_total$Total_Capacity), 
                                  by = 1000000), labels = scales::comma)
  theme(legend.position = "none")
  

## Changes in Operating Capacity

# Creating a dataset with major identifiable changes by country
majorchanges <- dataset_country %>% 
  
  # Removing countries with no change in capacity (Can increase capacity in 
  #existing plants, hence sorted by capacity)
  filter(Capacity_2017 != Capacity_2023) %>%
  
  #Adding a variable to identify addition or retirement by capacity
  mutate(CapacityChg = (((Capacity_2023 - Capacity_2017)
                         /Capacity_2017)*100)) %>%
  
  #Adding a variable to identify addition or retirement by number of 
  #power plants
  mutate(PlantsChg = (PowerPlants_2023 - PowerPlants_2017)) %>% 
  arrange(CapacityChg)

#Creating plot datas
plot_capacitychg <- majorchanges %>% 
  #Reverifying the change variable
  filter(CapacityChg != 0) %>%
  #Keeping change more than 10% to identify major contributors
  filter(CapacityChg > 10 | CapacityChg < 0)

plot_plantchg <- majorchanges %>% 
  #Reverifying the change variable
  filter(PlantsChg != 0)

plot_plantchg_noout <- majorchanges %>% 
  #Reverifying the change variable
  filter(PlantsChg != 0) %>% 
  filter(Country != "China")

#Plotting the change in capacity
ggplot(plot_capacitychg, aes(x = CapacityChg, y = Country)) +
  geom_bar(stat = "identity") +
  labs(title = "Capacity Change (2017 vs. 2023) by Country",
       x = "Country", y = "Capacity Change in % (More than 10%)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 

#Plotting the change in number of plants
ggplot(plot_plantchg, aes(x = PlantsChg, y = Country)) +
  geom_bar(stat = "identity") +
  labs(title = "Number of Power Plants Change (2017 vs. 2023) by Country",
       x = "Country", y = "Change in Number of Power Plants") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 

#Plotting the change in number of plants
ggplot(plot_plantchg_noout, aes(x = PlantsChg, y = Country)) +
  geom_bar(stat = "identity") +
  labs(title = "Number of Power Plants Change (2017 vs. 2023) by Country",
       x = "Country", y = "Change in Number of Power Plants") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 

# Power Plants under development
# Creating a dataset to idetnify the total number of power plants 
# under development
majorchanges <- majorchanges %>% 
  
  #Creating a combined variable _ for under development in 2017
  mutate(UnderDev_17 = Permitted_2017 + PrePermit_2017 + 
           Announced_2017 + Construction_2017) %>% 
  
  #Creating a combined variable _ for under development in 2023
  mutate(UnderDev_23 = Permitted_2023 + PrePermit_2023 + 
           Announced_2023 + Construction_2023) %>% 
  
  #Creating a combined variable _ for deactivated / closed in 2017
  mutate(TotalCancelled_17 = Shelved_2017 + Retired_2017 + Cancelled_2017) %>% 
  
  #Creating a combined variable _ for deactivated / closed in 2023
  mutate(TotalCancelled_23 = Shelved_2023 + Retired_2023 + Cancelled_2023 +
           Deactivated_2023)

#Creating plot data for under development and cancelled plants
plot_data_uc <- majorchanges %>%
  select(Country, UnderDev_17, UnderDev_23, TotalCancelled_17, 
         TotalCancelled_23) %>% 
  filter(UnderDev_17 != 0 | UnderDev_23 != 0) %>%
  pivot_longer(cols = starts_with("Under"), 
               names_to = "UnderDev", 
               values_to = "Count") %>% 
  pivot_longer(cols = starts_with("Total"), 
               names_to = "Cancelled", 
               values_to = "Count_Can")

#Plotting the under development plants (as a total percentage)  
ggplot(plot_data_uc, aes(x = Country, y = Count, fill = UnderDev)) +
  geom_bar(position = "fill", stat = "identity") +
  labs(title = "Plants Under Development by Country",
       x = "Country", y = "Plants Under Development") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 

#Plotting the cancelled plants (as a total percentage)
ggplot(plot_data_uc, aes(x = Country, y = Count_Can, fill = Cancelled)) +
  geom_bar(position = "fill", stat = "identity") +
  labs(title = "Plants Cancelled by Country",
       x = "Country", y = "Plants Cancelled") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) 

# Plotting the Annual CO2 Emissions

#Creating a world map dataset
world_map <- ne_countries(scale = "medium", returnclass = "sf")

#Creating a dataset for Annual Co2 emissions
CO2_Country <- dataset_country %>% 
  select(Country, AnnualCO2_2017, AnnualCO2_2023)

#Creating the plot data
plot_data_Co2 <- merge(world_map, CO2_Country, by.x = "name", by.y = 
                         "Country", all.x = TRUE)

#Plotting for 2017
ggplot() +
  geom_sf(data = plot_data_Co2, aes(fill = AnnualCO2_2017)) +
  scale_fill_gradient(low = "lightblue", high = "darkred", name = 
                        "CO2 Emissions") +
  labs(title = "CO2 Emissions by Country") +
  theme_void()

#Plotting for 2023
ggplot() +
  geom_sf(data = plot_data_Co2, aes(fill = AnnualCO2_2023)) +
  scale_fill_gradient(low = "lightblue", high = "darkred", name = 
                        "CO2 Emissions") +
  labs(title = "CO2 Emissions by Country") +
  theme_void()

#Creating a dataset for Annual Co2 emissions _ without outliers
CO2_Country_out <- dataset_country %>% 
  select(Country, AnnualCO2_2017, AnnualCO2_2023) %>% 
  filter(AnnualCO2_2017 < 1000)

#Creating a plot dataset for Annual Co2 emissions _ without outliers
plot_data_Co3 <- merge(world_map, CO2_Country_out, by.x = "name", 
                       by.y = "Country", all.x = TRUE)

#Plotting for 2017 _ without outliers
ggplot() +
  geom_sf(data = plot_data_Co3, aes(fill = AnnualCO2_2017)) +
  scale_fill_gradient(low = "lightblue", high = "darkred", name = 
                        "CO2 Emissions") +
  labs(title = "CO2 Emissions by Country") +
  theme_void()

#Plotting for 2023 _ without outliers
ggplot() +
  geom_sf(data = plot_data_Co3, aes(fill = AnnualCO2_2023)) +
  scale_fill_gradient(low = "lightblue", high = "darkred", name = 
                        "CO2 Emissions") +
  labs(title = "CO2 Emissions by Country (Without Outliers)") +
  theme_void() 

##_____________________________________________________##