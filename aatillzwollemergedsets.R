
###################################################################
####importing the data of gemeentes and unemployment in Rstudio####
###################################################################

library(readr)

Aatillgoereemental2020 = read.csv("data/Aatillgoereemental2020.csv")
goestillnissewaardmental2020 = read.csv("data/goestillnissewaardmental2020.csv")
noardtillvlielandmental2020 = read.csv("data/noardtillvlielandmental2020.csv")
vlissingentillzwollemental2020 = read.csv("data/vlissingentillzwollemental2020.csv")

Aatillgoereemental2022 = read.csv("data/Aatillgoereemental.csv")
goestillnissewaardmental2022 = read.csv("data/goestillnissewaardmental2022.csv")
noardtillvlielandmental2022 = read.csv("data/noardtillvlielandmental2022.csv")
vlissingentillzwollemental2022 = read.csv("data/vlissingentillzwollemental2022.csv")

unemploymentgemeentes = read.csv("data/unemployment.csv")

#################################################################
####combining the multiple pieces of gemeentes into 1 dataset####
#################################################################

library(dplyr)
library(tidyverse)

gemeentesmentaldata2020 = bind_rows(Aatillgoereemental2020,goestillnissewaardmental2020,noardtillvlielandmental2020,vlissingentillzwollemental2020)
gemeentesmentaldata2022 = bind_rows(Aatillgoereemental2022,goestillnissewaardmental2022,noardtillvlielandmental2022,vlissingentillzwollemental2022)

write.csv(gemeentesmentaldata2020,"data/gemeentesmentaldata2020.csv")
write.csv(gemeentesmentaldata2022,"data/gemeentesmentaldata2022.csv")

####################################################
####putting important data into different folder####
####################################################

write.csv(gemeentesmentaldata2020,"important data/gemeentesmentaldata2020.csv")
write.csv(gemeentesmentaldata2022,"important data/gemeentesmentaldata2022.csv")
write.csv(unemploymentgemeentes,"important data/unemploymentgemeentes.csv")

##########################################################################
####data cleaning, get rid of the useless variables in mental data set####
##########################################################################

filtered_gemeentesmentaldata2020 = gemeentesmentaldata2020 %>%
  select(Leeftijd,Marges,Perioden,Wijken.en.buurten,Regioaanduiding.Soort.regio..omschrijving.,Psychische.klachten....,Hoog.risico.op.angst.of.depressie....)

write.csv(filtered_gemeentesmentaldata2020,"important data/filtered_gemeentesmentaldata2020.csv")

filtered_gemeentesmentaldata2022 = gemeentesmentaldata2022 %>%
  select(Leeftijd,Marges,Perioden,Wijken.en.buurten,Regioaanduiding.Soort.regio..omschrijving.,Psychische.klachten....,Hoog.risico.op.angst.of.depressie....)

write.csv(filtered_gemeentesmentaldata2022,"important data/filtered_gemeentesmentaldata2022.csv")

################################################################################
####data cleaning, get rid of the useless variables in unemployment data set####
################################################################################

filtered_unemploymentgemeentes = unemploymentgemeentes %>%
  filter(str_detect(Onderwijsvolgend, "Totaal") &
           str_detect(Arbeidskenmerken, "Totaal") &
           str_detect(Uitkering, "Totaal") &
           str_detect(Geregistreerd.werkzoekende.bij.UWV, "Totaal"))


write.csv(filtered_unemploymentgemeentes,"important data/filtered_unemploymentgemeentes.csv")

############################################################################################
####prepare mental and unemployment set so that we can merge them together in 1 data set####
############################################################################################

unemploymentGem = filtered_unemploymentgemeentes %>%
#  filter(Perioden %in% c(2020,2022)) %>%
  arrange(Perioden)


unemploymentGem = unemploymentGem %>%
  rename(Gemeente = Regio.s) 
unemploymentGem$Perioden = as.character(unemploymentGem$Perioden)


filtered_gemeentesmentaldata2020$Psychische.klachten.... = NA_real_

filtered_comparisonmental2020_2022 = bind_rows(filtered_gemeentesmentaldata2020,filtered_gemeentesmentaldata2022)
Mental2020_2022 <- filtered_comparisonmental2020_2022

Mental2020_2022 = Mental2020_2022 %>%
  rename(Gemeente = Wijken.en.buurten)
Mental2020_2022$Perioden = as.character(Mental2020_2022$Perioden)

########################################
###now merge the 2 data sets together###
########################################

combined_data = full_join(
  unemploymentGem,
  Mental2020_2022,
  by = c("Gemeente","Perioden")
)

write.csv(combined_data,"important data/combined_data.csv")

##################################################################################
####clean the combined data set so that all useless variables are filtered out####
##################################################################################

data_cleancombined = combined_data %>%
  select(-Onderwijsvolgend, -Arbeidskenmerken, -Uitkering, -Geregistreerd.werkzoekende.bij.UWV, -Marges)

names(data_cleancombined) = c(
  "Periode",
  "Gemeente",
  "Aantal_werklozen_15_27",
  "Leeftijd",
  "Regio_type",
  "Psychische_klachten",
  "Hoog_risico_angst_depressie"
)

write.csv(data_cleancombined,"important data/data_cleancombined.csv")

######################################
####unemployment percentage change####
######################################

f_unplmntchng <- filtered_unemploymentgemeentes %>%
  arrange(Regio.s, Perioden) %>%
  group_by(Regio.s) %>%
  mutate(
    unemployment_pct_change = 
      (`Jongeren..15.tot.27.jaar...aantal.` - lag(`Jongeren..15.tot.27.jaar...aantal.`)) / 
      lag(`Jongeren..15.tot.27.jaar...aantal.`) * 100
  ) %>%
  ungroup()

f_unplmntchng$unemployment_pct_change = round(f_unplmntchng$unemployment_pct_change, 3)

write.csv(f_unplmntchng,"important data/f_unplmntchng.csv")

#################################
####geografische data inladen####
#################################

library(giscoR)

gemeenten_nl = gisco_get_lau(country = "NL", year = 2020) %>%
  arrange(LAU_NAME)
View(gemeenten_nl)

install.packages(c("sf","dplyr","ggplot2","tmap","rmapshaper","readr"))
library(sf)

ggplot(data = gemeenten_nl) + 
  geom_sf(fill = "white", color = "black") +
  theme_minimal() +
  labs(title = "Gemeentegrenzen van Nederland")

write.csv(gemeenten_nl,"important data/gemeenten_nl.csv")

#########################################################
####geografische data toevoegen aan combined data set####
#########################################################






