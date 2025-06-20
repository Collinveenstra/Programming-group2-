
###################################################################
####importing the data of gemeentes and unemployment in Rstudio####
###################################################################
library(readr)

#import of mental data gemeentes 2020
Aatillgoereemental2020 = read.csv("data/Aatillgoereemental2020.csv")
goestillnissewaardmental2020 = read.csv("data/goestillnissewaardmental2020.csv")
noardtillvlielandmental2020 = read.csv("data/noardtillvlielandmental2020.csv")
vlissingentillzwollemental2020 = read.csv("data/vlissingentillzwollemental2020.csv")

#import of mental data gemeentes 2022
Aatillgoereemental2022 = read.csv("data/Aatillgoereemental.csv")
goestillnissewaardmental2022 = read.csv("data/goestillnissewaardmental2022.csv")
noardtillvlielandmental2022 = read.csv("data/noardtillvlielandmental2022.csv")
vlissingentillzwollemental2022 = read.csv("data/vlissingentillzwollemental2022.csv")

#import of unemployment data 2020-2023
gem_unemployment = read.csv("standard data/gem_unemployment.csv")

#################################################################
####combining the multiple pieces of gemeentes into 1 dataset####
#################################################################

library(dplyr)
library(tidyverse)

#make 1 large mental data set of all the small partial mental datasets for 2020 and 2022
gemeentesmentaldata2020 = bind_rows(Aatillgoereemental2020,goestillnissewaardmental2020,noardtillvlielandmental2020,vlissingentillzwollemental2020)
gemeentesmentaldata2022 = bind_rows(Aatillgoereemental2022,goestillnissewaardmental2022,noardtillvlielandmental2022,vlissingentillzwollemental2022)

write.csv(gemeentesmentaldata2020,"data/gemeentesmentaldata2020.csv")
write.csv(gemeentesmentaldata2022,"data/gemeentesmentaldata2022.csv")

##########################################################################
####data cleaning, get rid of the useless variables in mental data set####
##########################################################################

#filtering useless categories like smoking, alcohol consumption and other health statistics, so that we only have mental data left
filtered_gemeentesmentaldata2020 = gemeentesmentaldata2020 %>%
  select(Leeftijd,Marges,Perioden,Wijken.en.buurten,Regioaanduiding.Soort.regio..omschrijving.,Psychische.klachten....,Hoog.risico.op.angst.of.depressie....)

write.csv(filtered_gemeentesmentaldata2020,"important data/filtered_gemeentesmentaldata2020.csv")

#the same for the data of 2022
filtered_gemeentesmentaldata2022 = gemeentesmentaldata2022 %>%
  select(Leeftijd,Marges,Perioden,Wijken.en.buurten,Regioaanduiding.Soort.regio..omschrijving.,Psychische.klachten....,Hoog.risico.op.angst.of.depressie....)

write.csv(filtered_gemeentesmentaldata2022,"important data/filtered_gemeentesmentaldata2022.csv")

################################################################################
####data cleaning, get rid of the useless variables in unemployment data set####
################################################################################

#to calculate unemployment% we need total population and the unemployed population. Therefore we can filter out the employed population
fltrd_unemployment = gem_unemployment %>%
  filter(Arbeidskenmerken %in% c("Totaal ", "Arb.pos.: niet-werkzame bevolking"))

#this category was not useful for our analysis so to make the data set less complex we filtered this variable out 
fltrd_unemployment = fltrd_unemployment %>%
  filter(Geregistreerd.werkzoekende.bij.UWV %in% "Totaal")

write.csv(fltrd_unemployment,"important data/fltrd_unemployment")

############################################################################################
####prepare mental and unemployment set so that we can merge them together in 1 data set####
############################################################################################

# unemployment data set arranged at Period
fltrd_unemployment = fltrd_unemployment %>%
  arrange(Perioden)

# variable name changed from regio.s to gemeente
fltrd_unemployment = fltrd_unemployment %>%
  rename(Gemeente = Regio.s) 

#Period made as character so that we could combine the data sets 
fltrd_unemployment$Perioden = as.character(fltrd_unemployment$Perioden)

#instead of no value, we made all missing values of 2020 NA
filtered_gemeentesmentaldata2020$Psychische.klachten.... = NA_real_

#combined the 2020 and 2022 mental data set
Mental2020_2022 = bind_rows(filtered_gemeentesmentaldata2020,filtered_gemeentesmentaldata2022)

#variable name changed from wijken en buurten to gemeente
Mental2020_2022 = Mental2020_2022 %>%
  rename(Gemeente = Wijken.en.buurten)

#Period made as character so thaat we could combine the data sets
Mental2020_2022$Perioden = as.character(Mental2020_2022$Perioden)

#########################
####
#########################

totaal_unemp <- fltrd_unemployment %>% filter(Arbeidskenmerken == "Totaal ")
totaal_unemp$aantal_jongeren <- totaal_unemp$Jongeren..15.tot.27.jaar...aantal.
totaal_unemp$Jongeren..15.tot.27.jaar...aantal. <- NULL
totaal_unemp$X <- NULL
totaal_unemp$Arbeidskenmerken <- NULL
nietwerkzaam <- fltrd_unemployment %>% filter(Arbeidskenmerken != "Totaal ")
nietwerkzaam$nietwerkzame_jongeren <- nietwerkzaam$Jongeren..15.tot.27.jaar...aantal.
nietwerkzaam$Jongeren..15.tot.27.jaar...aantal. <- NULL
nietwerkzaam$X <- NULL
nietwerkzaam$Arbeidskenmerken <- NULL

unemployment <- full_join(totaal_unemp, nietwerkzaam, by = c("Onderwijsvolgend", "Uitkering", "Geregistreerd.werkzoekende.bij.UWV", "Perioden", "Gemeente"))

write.csv(unemployment,"important data/unemployment.csv")

########################################
###now merge the 2 data sets together###
########################################

#merge the mental and unemployment data set
Combined_data = full_join(unemployment,Mental2020_2022, by = c("Gemeente","Perioden"))

write.csv(Combined_data,"important data/Combined_data.csv")

##################################################################################
####clean the combined data set so that all useless variables are filtered out####
##################################################################################

#clean up0 all useless variables so that the data set becomes more clear
Data_Cleancombined = Combined_data %>%
  select(-Onderwijsvolgend, -Uitkering, -Geregistreerd.werkzoekende.bij.UWV, -Marges, -Leeftijd)

#change the variable names to easier names
names(Data_Cleancombined) = c(
  "Periode",
  "Gemeente",
  "Aantal_jongeren",
  "Niet_werkzame_jongeren",
  "Regio_type",
  "Psychische_klachten",
  "Hoog_risico_angst_depressie"
)

write.csv(Data_Cleancombined,"important data/Data_Cleancombined.csv")

#####################################
####unemployment percentage#########
#####################################

#making an extra variable unemployment in percentage
Data_Cleancombined$unemployment_percentage = (Data_Cleancombined$Niet_werkzame_jongeren/Data_Cleancombined$Aantal_jongeren)*100
Data_Cleancombined$unemployment_percentage = round(Data_Cleancombined$unemployment_percentage,2)

##########################################################
#### Mental data higher or lower than national average####
##########################################################

#national mental averages of 2020 and 2022
avgmental_2020 = Data_Cleancombined$Hoog_risico_angst_depressie[Data_Cleancombined$Periode == 2020 & Data_Cleancombined$Gemeente == "Nederland"]
avgmental_2022 = Data_Cleancombined$Hoog_risico_angst_depressie[Data_Cleancombined$Periode == 2022 & Data_Cleancombined$Gemeente == "Nederland"]

#compare the gemeentes to national average which gives a value of Above or Below average
Data_Cleancombined$Above_or_Below_National_Risk <- ifelse(
  is.na(Data_Cleancombined$Hoog_risico_angst_depressie), NA,
  ifelse(Data_Cleancombined$Periode == 2020 & Data_Cleancombined$Hoog_risico_angst_depressie > avgmental_2020, "Above",
         ifelse(Data_Cleancombined$Periode == 2020 & Data_Cleancombined$Hoog_risico_angst_depressie < avgmental_2020, "Below",
                ifelse(Data_Cleancombined$Periode == 2022 & Data_Cleancombined$Hoog_risico_angst_depressie > avgmental_2022, "Above",
                       ifelse(Data_Cleancombined$Periode == 2022 & Data_Cleancombined$Hoog_risico_angst_depressie < avgmental_2022, "Below", "Equal")))))


##########################################################
#### unemployment higher or lower than national average####
##########################################################

#in the dataset 2023 was stated as 2023*, so we had to correct this
Data_Cleancombined$Periode <- gsub("\\*", "", Data_Cleancombined$Periode)

#national unemployment averages of 2020-2023
avgunemp_2020 = Data_Cleancombined$unemployment_percentage[Data_Cleancombined$Periode == 2020 & Data_Cleancombined$Gemeente == "Nederland"]
avgunemp_2021 = Data_Cleancombined$unemployment_percentage[Data_Cleancombined$Periode == 2021 & Data_Cleancombined$Gemeente == "Nederland"]
avgunemp_2022 = Data_Cleancombined$unemployment_percentage[Data_Cleancombined$Periode == 2022 & Data_Cleancombined$Gemeente == "Nederland"]
avgunemp_2023 = Data_Cleancombined$unemployment_percentage[Data_Cleancombined$Periode == 2023 & Data_Cleancombined$Gemeente == "Nederland"]

#compare the gemeentes to national unemployment average which gives a value of Above or Below average
Data_Cleancombined$Above_or_Below_National_Unemployment <- ifelse(
  is.na(Data_Cleancombined$unemployment_percentage), NA,
  ifelse(Data_Cleancombined$Periode == 2020 & Data_Cleancombined$unemployment_percentage > avgunemp_2020, "Above",
         ifelse(Data_Cleancombined$Periode == 2020 & Data_Cleancombined$unemployment_percentage < avgunemp_2020, "Below",
                ifelse(Data_Cleancombined$Periode == 2021 & Data_Cleancombined$unemployment_percentage > avgunemp_2021, "Above",
                       ifelse(Data_Cleancombined$Periode == 2021 & Data_Cleancombined$unemployment_percentage < avgunemp_2021, "Below",
                              ifelse(Data_Cleancombined$Periode == 2022 & Data_Cleancombined$unemployment_percentage > avgunemp_2022, "Above",
                                     ifelse(Data_Cleancombined$Periode == 2022 & Data_Cleancombined$unemployment_percentage < avgunemp_2022, "Below",
                                            ifelse(Data_Cleancombined$Periode == 2023 & Data_Cleancombined$unemployment_percentage > avgunemp_2023, "Above",
                                                   ifelse(Data_Cleancombined$Periode == 2023 & Data_Cleancombined$unemployment_percentage < avgunemp_2023, "Below","Equal")))))))))

##########################################################################
####Top 10 gemeentes with highest and lowest fear and depression risks####
##########################################################################

top10_highrisk = Data_Cleancombined %>%
  filter(Periode %in% c(2020,2022)) %>%
  group_by(Periode) %>%
  arrange(desc(Hoog_risico_angst_depressie)) %>%
  slice(1:10)

top10_lowrisk = Data_Cleancombined %>%
  filter(Periode %in% c(2020,2022)) %>%
  group_by(Periode) %>%
  arrange(Hoog_risico_angst_depressie) %>%
  slice(1:10)

write.csv(top10_highrisk, "important data/top10_highrisk.csv")
write.csv(top10_lowrisk, "important data/top10_lowrisk.csv")

##########################################################################
####Top 10 gemeenten with highest and lowest unemployment####
##########################################################################

top10_highunemployment = Data_Cleancombined %>%
  group_by(Periode) %>%
  arrange(desc(unemployment_percentage)) %>%
  slice(1:10)

top10_lowunemployment = Data_Cleancombined %>%
  group_by(Periode) %>%
  arrange(unemployment_percentage) %>%
  slice(1:10)

write.csv(top10_highunemployment,"important data/top10_highunemployment.csv")
write.csv(top10_lowunemployment, "important data/top10_lowunemployment.csv")

##############################
####temporal visualization####
##############################

library(ggplot2)
library(dplyr)

#####
landelijkunem = Data_Cleancombined %>%
  filter(Gemeente == "Nederland")

#top 10 gemeentes with the highest unemployment from 2020
library(dplyr)

top10_gemeentes_2020 <- Data_Cleancombined %>%
  filter(Periode == 2020) %>%                        # Keep only rows from the year 2020
  arrange(desc(unemployment_percentage)) %>%         # Sort by unemployment percentage, highest first
  slice_head(n = 10)                                 # Take the top 10 rows

top10_gemeentes_2020
#new data set with data solely from the gemeentes with the highest unemployment 2020-2023
library(dplyr)

selected_gemeentes <- c(
  "Vaals", "Wassenaar", "Wageningen", "Maastricht",
  "Blaricum", "Bloemendaal", "Laren (NH.)", 
  "Rozendaal", "Vlieland", "Delft"
)

TOP10HIGH2020 <- Data_Cleancombined %>%
  filter(Periode >= 2020 & Periode <= 2023) %>%       # Keep only years 2020–2023
  filter(Gemeente %in% selected_gemeentes)      # Keep only the specified Gemeentes

write.csv(TOP10HIGH2020, "important data/TOP10HIGH2020.csv")

# View the new dataset
View(TOP10HIGH2020)

write.csv(TOP10HIGH2020,"important data/TOP10HIGH2020.csv")


#top 10 gemeentes with the lowest unemployment from 2020
library(dplyr)

lowest10_gemeentes_2020 <- Data_Cleancombined %>%
  filter(Periode == 2020) %>%                         # Only rows from the year 2020
  arrange(unemployment_percentage) %>%               # Sort by unemployment percentage (lowest first)
  slice_head(n = 10)                                  # Take the top 10 rows

lowest10_gemeentes_2020
library(dplyr)

selected_gemeentes <- c(
  "Schiermonnikoog", "Urk", "Zwartewaterland", "Opmeer",
  "Bladel", "Staphorst", "Nederweert", "Neder-Betuwe",
  "Boekel", "Bunschoten"
)

TOP10LOW2020 <- Data_Cleancombined %>%
  filter(Periode >= 2020 & Periode <= 2023) %>%        # Filter years 2020 to 2023
  filter(Gemeente %in% selected_gemeentes)             # Filter to selected Gemeentes
write.csv(TOP10LOW2020,"important data/TOP10LOW2020.csv")
# View the new dataset
View(TOP10LOW2020)
library(dplyr)
Subgroupunemp = bind_rows(TOP10HIGH2020, TOP10LOW2020)
#Vlieland is an island with 300 people, so there was no unemployment one year and half the population was unemployed the next

write.csv(Subgroupunemp,"important data/Subgroupemp.csv")

Data_Cleancombined$Periode = as.numeric(Data_Cleancombined$Periode)

###unemployment percentage change
Data_Cleancombined <- Data_Cleancombined %>%
  arrange(Gemeente, Periode) %>%
  group_by(Gemeente) %>%
  mutate(
    unemployment_change = if_else(
      Periode - lag(Periode) == 1 & !is.na(lag(Niet_werkzame_jongeren)),
      (Niet_werkzame_jongeren - lag(Niet_werkzame_jongeren)) / lag(Niet_werkzame_jongeren) * 100,
      NA_real_
    )
  ) %>%
  ungroup()
# Round to 2 decimals
library(ggplot2)
library(dplyr)

library(ggplot2)
library(dplyr)

# Remove Vlieland and Schiermonnikoog (those are islands with little population, and therefore zero unemployment in some years, so it gives a wrong visual)
plot_data <- Subgroupunemp %>%
  filter(!Gemeente %in% c("Vlieland", "Schiermonnikoog"))

# Define groups without Vlieland
high_gemeentes <- c("Vaals", "Wassenaar", "Wageningen", "Maastricht",
                    "Blaricum", "Bloemendaal", "Laren (NH.)", 
                    "Rozendaal", "Delft")  # Vlieland removed

low_gemeentes <- c("Urk", "Zwartewaterland", "Opmeer",
                   "Bladel", "Staphorst", "Nederweert", "Neder-Betuwe",
                   "Boekel", "Bunschoten")

plot_data <- plot_data %>%
  mutate(group = case_when(
    Gemeente %in% high_gemeentes ~ "High Unemployment",
    Gemeente %in% low_gemeentes  ~ "Low Unemployment",
    TRUE ~ "Other"
  ))

###boxplot
ggplot(plot_data, aes(x = group, y = unemployment_percentage, fill = group)) +
  geom_boxplot() +
  scale_fill_manual(values = c("High Unemployment" = "red", "Low Unemployment" = "blue")) +
  labs(
    title = "Unemployment Percentage \nby Subgroup (2020-2023)",
    x = NULL,
    y = "Unemployment Percentage (%)\n",
    fill = "group"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),    # Remove x-axis labels
    axis.ticks.x = element_blank(),   # Remove x-axis ticks
    legend.position = "right"
  ) +
  scale_y_continuous(breaks = (0:6)*10, lim = c(0,70))

### visual temporal
landelijkunem$Periode = as.numeric(landelijkunem$Periode)

ggplot(landelijkunem, aes(x = Periode, y = unemployment_percentage)) +
  geom_line(color = "blue", linewidth = 1.2) +
  geom_point(color = "blue", size = 3) +
  labs(title = "Unemployment in the Netherlands (2020-2023)",
       x = "Year",
       y = "Unemployment (%)") +
  scale_y_continuous(breaks = (0:4)*10, lim = c(0,40)) +
  theme_minimal()

###
library(giscoR)

gemeenten_nl = gisco_get_lau(country = "NL", year = 2020) %>%
  arrange(LAU_NAME)
View(gemeenten_nl)

library(sf)


gemeenten_nl = gemeenten_nl %>%
  rename(Periode = YEAR, Gemeente = LAU_NAME, Gemeentegrenzen = `_ogr_geometry_`) 

gemeenten_nl = gemeenten_nl %>%
  select(Gemeente, Periode, Gemeentegrenzen)

write.csv(gemeenten_nl,"important data/gemeenten_nl.csv")

geo_data = Data_Cleancombined %>%
  inner_join(gemeenten_nl, by = c("Periode", "Gemeente"))

library(sf)
geo_data = geo_data %>%
  filter(!is.na(unemployment_percentage)) %>%
  st_sf()

write.csv(geo_data,"important data/geo_data.csv")

ggplot(geo_data) +
  geom_sf(aes(fill = unemployment_percentage), color = "black") +
  scale_fill_viridis_c(option = "inferno", name = "Unemployment (%)") +
  theme_minimal() +
  labs(
    title = "Youth unemployment per gemeente (2020)",
  )  

############ change the name of the dutch variables
Data_Cleancombined <- Data_Cleancombined %>%
  rename(High_Risk_Anxiety_Depression = Hoog_risico_angst_depressie, MHI_5 = Psychische_klachten, Total_Youth = Aantal_jongeren, Total_Unemployed_Youth = Niet_werkzame_jongeren)

names(Data_Cleancombined) = c(
  "Period",
  "Gemeente",
  "Total_Youth",
  "Total_Unemployed_Youth",
  "Region_Type",
  "MHI_5",
  "High_Risk_Anxiety_Depression",
  "Unemployment(%)",
  "Above_or_Below_National_Risk",
  "Above_or_Below_National_Unemployment",
  "Unemployment_Change_per_Year"
)


library(ggplot2)
library(dplyr)

# Assuming your grouped data is in `plot_data` with columns:
# - unemployment_percentage (numeric)
# - group ("High Unemployment" or "Low Unemployment")

landelijkunem$Periode = as.numeric(landelijkunem$Periode)
### event analysis
ggplot(landelijkunem, aes(x = Periode, y = unemployment_percentage)) +
  geom_line(color = "blue", linewidth = 1.2) +
  geom_point(color = "blue", size = 3) +
  labs(title = "Unemployment in the Netherlands (2020-2023)",
       x = "Year",
       y = "Unemployment (%)") +
  scale_y_continuous(breaks = (0:8)*5, lim = c(0,40)) +
  theme_minimal() +
  geom_vline(xintercept = 2022, linetype = "solid", color = "black", linewidth = 1) +
  annotate("text", x = 2021, y = 20, size = 3.5, label = "End of Covid-19 -->")

####Above or Below national unemployment visual 
dfunem = Data_Cleancombined %>%
  filter(Above_or_Below_National_Unemployment != "Equal")

df_percent <- dfunem %>%
  group_by(Period, Above_or_Below_National_Unemployment) %>%
  summarise(amount = n(), .groups = "drop") %>%
  group_by(Period) %>%
  mutate(
    percentage = 100 * amount / sum(amount) 
  )

ggplot(df_percent, aes(x = Period, y = percentage, fill = Above_or_Below_National_Unemployment)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("Above" = "red", "Below" = "steelblue")) +
  labs(
    title = "Percentage Gemeenten Above/Below \nNational Average per Year",
    x = "Year",
    y = "Percentage of Gemeentes",
    fill = "Above/Below"
  ) +
  theme_minimal()

####Above or Below national unemployment visual

dfmental = Data_Cleancombined %>%
  filter(Above_or_Below_National_Risk != "Equal")

df_percentmental <- dfmental %>%
  group_by(Period, Above_or_Below_National_Risk) %>%
  summarise(amount = n(), .groups = "drop") %>%
  group_by(Period) %>%
  mutate(
    percentage = 100 * amount / sum(amount) 
  )

ggplot(df_percentmental, aes(x = Period, y = percentage, fill = Above_or_Below_National_Risk)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("Above" = "red", "Below" = "steelblue")) +
  labs(
    title = "Percentage Gemeenten \nAbove or Below National \nMental Average per Year",
    x = "Year",
    y = "Percentage of Gemeentes",
    fill = "Above/Below"
  ) +
  theme_minimal()

