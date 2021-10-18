# HEADER ----

# Term Paper in Advanced Environmental and Resource Economics (WiSe 2020/21)
# Topic: Industry Presence and Urban Climate Action: Evidence from a Set of Dutch Cities
# Student: Marc Blauert (ID: 605555)

# Purpose of the script: Load and combine data from different sources, establish data matrix for csQCA.


# Setup ----

rm(list = ls())
pacman::p_load(tidyverse, stringr, RColorBrewer, hrbrthemes, corrplot, QCA, SetMethods, xtable)

# Load data ----

city_corop_selection <- "data/city_corop_selection.csv" # Selection of Dutch municipalities included in the Reckien et al. (2018) data and matching to COROP regions, export from QGIS
city_corop_selection <- read.csv2(city_corop_selection, header = TRUE, stringsAsFactors = FALSE)

population_cities_cbs <- "data/population_cities_cbs.csv" # City population data from CBS as of Jan. 2016
population_cities_cbs <- read.csv2(population_cities_cbs, header = TRUE, stringsAsFactors = FALSE)

population_corop_cbs <- "data/population_corop_cbs.csv" # COROP population data from CBS as of Jan. 2016
population_corop_cbs <- read.csv2(population_corop_cbs, header = TRUE, stringsAsFactors = FALSE)

industry_corop_cbs <- "data/industry_corop_cbs.csv" # Industry data per COROP region from CBS as of Jan. 2016
industry_corop_cbs <- read.csv2(industry_corop_cbs, header = TRUE, stringsAsFactors = FALSE, dec = ".")

reckien <- "data/reckien_mitigation.csv" # Reckien et al. (2018) dataset on climate action planning, selection of mitigation variables
reckien <- read.csv2(reckien, header = TRUE, stringsAsFactors = FALSE)

flood_risk <- "data/luisa_floodrisk.csv" # LOUISA flood risk data
flood_risk <- read.csv2(flood_risk, header = TRUE, stringsAsFactors = FALSE)

gdp_capita <- read_delim("data/gdp_capita.csv", ";", escape_double = FALSE,
                         col_types = cols(gpcX2012 = col_number(),
                                          gpcX2013 = col_number(),
                                          gpcX2014 = col_number(),
                                          gpcX2015 = col_number(),
                                          gpcX2016 = col_number()),
                         trim_ws = TRUE) # GDP per capita data


# Data matching & wrangling (Part 1) ----

# City population data
city_corop_selection <- merge(city_corop_selection, population_cities_cbs,
                              by.x = "city_code_cbs", by.y = "Regions")
city_corop_selection <- subset(city_corop_selection, select = -c(ID:Periods))
names(city_corop_selection)[names(city_corop_selection)=="PopulationOn1January_1"] <- "pop_city_jan_2016"

# COROP population data
population_corop_cbs$Regions <- str_replace_all(population_corop_cbs$Regions, fixed(" "), "") # Space in import data
city_corop_selection <- merge(city_corop_selection, population_corop_cbs,
                              by.x = "corop_code_cbs", by.y = "Regions")
city_corop_selection <- subset(city_corop_selection, select = -c(ID:Periods))
names(city_corop_selection)[names(city_corop_selection)=="PopulationOn01January_2016"] <- "pop_corop_jan_2016"
city_corop_selection$pop_share_city_of_corop <- city_corop_selection$pop_city_jan_2016/city_corop_selection$pop_corop_jan_2016

# COROP industry data
industry_corop_cbs$Regions <- str_replace_all(industry_corop_cbs$Regions, fixed(" "), "") # Space in import data
city_corop_selection <- merge(city_corop_selection, industry_corop_cbs,
                              by.x = "corop_code_cbs", by.y = "Regions")
city_corop_selection <- subset(city_corop_selection, select = -c(ID:Periods))

# Create per capita values for matched COROP industry data
city_corop_selection$GVA_millionEUR_percapita <- (city_corop_selection$GVA_millionEUR/city_corop_selection$pop_corop_jan_2016)*1000000 # From mil. euro to euro

city_corop_selection$EmployedPersons_1000fte_percapita <- (city_corop_selection$EmployedPersons_1000fte/city_corop_selection$pop_corop_jan_2016)*1000 # From 1000fte to relationship between 1fte in manufacturing per capita
# Note: Not all person within a COROP region are available for the labor market (children, retired etc.) and also not all employments are full-time equivalents; therefore share lower than national industry employment rates

# Sample selection ----

# Cumulative condition for selection: A city must be included in the Rekien et al. (2018) data AND be the most populous city in its respective COROP region.

list_largest_cities_corop <- city_corop_selection %>% 
  group_by(corop_code_cbs) %>% 
  summarise(pop_city_jan_2016 = max(pop_city_jan_2016, na.rm = TRUE))

city_corop_selection <- merge(city_corop_selection, list_largest_cities_corop,
                              by = "pop_city_jan_2016", all.x = TRUE)

names(city_corop_selection)[names(city_corop_selection)=="corop_code_cbs.y"] <- "largest_city_corop"
city_corop_selection$largest_city_corop <- ifelse(is.na(city_corop_selection$largest_city_corop) == TRUE, 0, 1)

city_corop_selection$sample <- ifelse(
  ((city_corop_selection$selection == 1) &
     (city_corop_selection$largest_city_corop == 1)), 1, 0) # Apply the cumulative conditions

# Data matching & wrangling (Part 2) ----

# Subset Reckien et al. (2018) dataset and merge with CBS data

reckien_dutch <- subset.data.frame(reckien, country_name == "Netherlands")

reckien_dutch$city_name[reckien_dutch$city_name == "s-Gravenhage"] <- "'s-Gravenhage"
reckien_dutch$city_name[reckien_dutch$city_name == "s-Hertogenbosch"] <- "'s-Hertogenbosch"
remove(reckien)

reckien_dutch <- merge(reckien_dutch, city_corop_selection,
                       by = "city_name", all.x = TRUE, all.y = TRUE)

reckien_dutch <- reckien_dutch[!is.na(reckien_dutch$geoname_id), ]
reckien_dutch <- reckien_dutch[!is.na(reckien_dutch$corop_name), ]


# Merge subset with provided gdp per capita and flood risk data

gdp_capita$gpc_mean12_15 <- rowMeans(gdp_capita[, c("gpcX2012", "gpcX2013", "gpcX2014", "gpcX2015")], na.rm = TRUE)
gdp_capita$gpc_growth <- ((gdp_capita$gpcX2015 - gdp_capita$gpcX2012)/gdp_capita$gpcX2012)/3
reckien_dutch <- merge(reckien_dutch, gdp_capita[, c("geoname_id", "gpc_mean12_15", "gpc_growth")],
                       by = "geoname_id", all.x = TRUE)


flood_risk <- subset.data.frame(flood_risk, year_observation == 2050 & country_code == "NL") # use future values for 2050
reckien_dutch <- merge(reckien_dutch, flood_risk[, c("city_name", "floodrisk_city")],
                       by = "city_name", all.x = TRUE)

# Eliminate incomplete observations
reckien_dutch <- subset.data.frame(reckien_dutch, sample == 1)
reckien_dutch <- reckien_dutch %>% filter(!is.na(floodrisk_city))
names(reckien_dutch)


# Export of set ----

sample_QCA <- select(reckien_dutch, geoname_id, city_code_cbs, corop_code_cbs.x, city_name, corop_name, mitigation_plan, pop_city_jan_2016,
                     pop_corop_jan_2016, pop_share_city_of_corop, GVA_millionEUR, EmployedPersons_1000fte, GVA_millionEUR_percapita, EmployedPersons_1000fte_percapita,
                     gpc_mean12_15, gpc_growth, floodrisk_city)

write.csv2(sample_QCA,"output/tables/sample_QCA.csv", row.names = FALSE)

QCA_matrix_real <- select(reckien_dutch, city_name, gpc_mean12_15, floodrisk_city, EmployedPersons_1000fte_percapita, mitigation_plan)
QCA_matrix_real$EmployedPersons_1000fte_percapita <- QCA_matrix_real$EmployedPersons_1000fte_percapita*100
write.csv2(QCA_matrix_real,"output/tables/QCA_matrix_real.csv", row.names = FALSE)


QCA_matrix_box_data <- select(reckien_dutch, geoname_id, City_Code, city_code_cbs, corop_code_cbs.x, city_name, corop_name, pop_corop_jan_2016, GVA_millionEUR, EmployedPersons_1000fte)
names(QCA_matrix_box_data)[names(QCA_matrix_box_data)=="GVA_millionEUR"] <- "GVA_millionEUR_COROP"
names(QCA_matrix_box_data)[names(QCA_matrix_box_data)=="EmployedPersons_1000fte"] <- "EmployedPersons_1000fte_COROP"

write.csv2(QCA_matrix_box_data,"output/tables/cbs_industry_data_corop_to_cities.csv", row.names = FALSE)


rm(list=ls()[! ls() %in% c("sample_QCA")])

# QCA matrix ----

#hist(sample_QCA$gpc_mean12_15/1000)
#summary(sample_QCA$gpc_mean12_15)
CAP <- sample_QCA$gpc_mean12_15 > 46437 # (CONDITION) Mean value, presence of high gdp --> capacity to mitigate
CAP <- as.numeric(CAP)

#hist(sample_QCA$gpc_growth)
#CAP <- sample_QCA$gpc_growth > 0.01 # (CONDITION) No decline of GDP per, presence of high gdp --> capacity to mitigate
#CAP <- as.numeric(C)

#hist(sample_QCA$floodrisk_city)
#summary(sample_QCA$floodrisk_city)
VUL <- sample_QCA$floodrisk_city > 3 # (CONDITION) Taken from Roggero's QCA example, presence of high flood risk --> if vulnerable to climate change then more willing to mitigate
VUL <- as.numeric(VUL)

#hist(sample_QCA$EmployedPersons_1000fte_percapita)
#summary(sample_QCA$EmployedPersons_1000fte_percapita)
IND <- sample_QCA$EmployedPersons_1000fte_percapita > 0.045 # (CONDITION) TBD,  employment in manufacturing --> if highly dependent on carbon-intensive processes then less willing to mitigate
IND <- as.numeric(IND)

#I <- sample_QCA$GVA_millionEUR_percapita < 5500 # value subject to discussion
#I <- as.numeric(I)

#hist(sample_QCA$mitigation_plan)
MIT <- sample_QCA$mitigation_plan %in% c("A1", "A3") # (OUTCOME) Taken from Roggero's QCA example, if A-plans present then more active in climate change mitigation activities
MIT <- as.numeric(MIT)

QCA_matrix <- data.frame(IND, CAP, VUL, MIT)
rownames(QCA_matrix) <- sample_QCA$city_name
dim(QCA_matrix)
colSums(QCA_matrix) / nrow(QCA_matrix)

write.csv2(QCA_matrix,"output/tables/QCA_matrix.csv", row.names = TRUE)
