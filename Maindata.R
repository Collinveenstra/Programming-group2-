
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

Data_Cleancombined = Combined_data %>%
  select(-Onderwijsvolgend, -Uitkering, -Geregistreerd.werkzoekende.bij.UWV, -Marges, -Leeftijd)

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

Data_Cleancombined$werkloosheidspercentage = (Data_Cleancombined$Niet_werkzame_jongeren/Data_Cleancombined$Aantal_jongeren)*100
Data_Cleancombined$werkloosheidspercentage = round(Data_Cleancombined$werkloosheidspercentage,2)

##########################################################
#### Mental data higher or lower than national average####
##########################################################

gem_2020 = Data_Cleancombined$Hoog_risico_angst_depressie[Data_Cleancombined$Periode == 2020 & Data_Cleancombined$Gemeente == "Nederland"]

gem_2022 = Data_Cleancombined$Hoog_risico_angst_depressie[Data_Cleancombined$Periode == 2022 & Data_Cleancombined$Gemeente == "Nederland"]

Data_Cleancombined$Boven_of_onder_gemiddeld_risico <- ifelse(
  is.na(Data_Cleancombined$Hoog_risico_angst_depressie), NA,
  ifelse(Data_Cleancombined$Periode == 2020 & Data_Cleancombined$Hoog_risico_angst_depressie > gem_2020, "Boven",
         ifelse(Data_Cleancombined$Periode == 2020 & Data_Cleancombined$Hoog_risico_angst_depressie < gem_2020, "Onder",
                ifelse(Data_Cleancombined$Periode == 2022 & Data_Cleancombined$Hoog_risico_angst_depressie > gem_2022, "Boven",
                       ifelse(Data_Cleancombined$Periode == 2022 & Data_Cleancombined$Hoog_risico_angst_depressie < gem_2022, "Onder", "Gelijk"))))
)


##########################################################
#### Mental data higher or lower than national average####
##########################################################

Data_Cleancombined$Periode <- gsub("\\*", "", Data_Cleancombined$Periode)

gemunemployment_2020 = Data_Cleancombined$werkloosheidspercentage[Data_Cleancombined$Periode == 2020 & Data_Cleancombined$Gemeente == "Nederland"]
gemunempoyment_2021 = Data_Cleancombined$werkloosheidspercentage[Data_Cleancombined$Periode == 2021 & Data_Cleancombined$Gemeente == "Nederland"]
gemunemployment_2022 = Data_Cleancombined$werkloosheidspercentage[Data_Cleancombined$Periode == 2022 & Data_Cleancombined$Gemeente == "Nederland"]
gemunemployment_2023 = Data_Cleancombined$werkloosheidspercentage[Data_Cleancombined$Periode == 2023 & Data_Cleancombined$Gemeente == "Nederland"]

Data_Cleancombined$Boven_of_onder_gemiddelde_werkloosheid <- ifelse(
  is.na(Data_Cleancombined$werkloosheidspercentage), NA,
  ifelse(Data_Cleancombined$Periode == 2020 & Data_Cleancombined$werkloosheidspercentage > gemunemployment_2020, "Boven",
         ifelse(Data_Cleancombined$Periode == 2020 & Data_Cleancombined$werkloosheidspercentage < gemunemployment_2020, "Onder",
                ifelse(Data_Cleancombined$Periode == 2021 & Data_Cleancombined$werkloosheidspercentage > gemunempoyment_2021, "Boven",
                       ifelse(Data_Cleancombined$Periode == 2021 & Data_Cleancombined$werkloosheidspercentage < gemunempoyment_2021, "Onder",
                              ifelse(Data_Cleancombined$Periode == 2022 & Data_Cleancombined$werkloosheidspercentage > gemunemployment_2022, "Boven",
                                     ifelse(Data_Cleancombined$Periode == 2022 & Data_Cleancombined$werkloosheidspercentage < gemunemployment_2022, "Onder",
                                            ifelse(Data_Cleancombined$Periode == 2023 & Data_Cleancombined$werkloosheidspercentage > gemunemployment_2023, "Boven",
                                                   ifelse(Data_Cleancombined$Periode == 2023 & Data_Cleancombined$werkloosheidspercentage < gemunemployment_2023, "Onder","Gelijk")))))))))

##########################################################################
####Top 10 gemeenten with highest and lowest fear and depression risks####
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
  arrange(desc(werkloosheidspercentage)) %>%
  slice(1:10)

top10_lowunemployment = Data_Cleancombined %>%
  group_by(Periode) %>%
  arrange(werkloosheidspercentage) %>%
  slice(1:10)

write.csv(top10_highunemployment,"important data/top10_highunemployment.csv")
write.csv(top10_lowunemployment, "important data/top10_lowunemployment.csv")

##############################
####temporal visualization####
##############################

library(ggplot2)
library(dplyr)

#het idee hier is goed alleen door de top 10 over die jaren te nemen krijg je waardes van maar 1 of 2 jaar in de grafiek
# daarom moeten we de top 10 hoogste en laagste in 2020 pakken en de trend van die gemeentes verder bekijken

Data_Cleancombined$Periode = as.numeric(Data_Cleancombined$Periode)
top10_highunemployment$Periode = as.numeric(top10_highunemployment$Periode)
top10_lowunemployment$Periode = as.numeric(top10_lowunemployment$Periode)

top10HLunem = bind_rows(top10_highunemployment, top10_lowunemployment)

landelijkunem = Data_Cleancombined %>%
  filter(Gemeente == "Nederland")

ggplot(top10HLunem, aes(x = Periode, y = werkloosheidspercentage, color = Gemeente)) +
  geom_line(linewidth = 1) +
  geom_point() +
  labs(
    title = "Ontwikkeling werkloosheidspercentage (2020–2023)",
    subtitle = "Top 10 hoogste en laagste gemeenten + landelijk gemiddelde",
    x = "Jaar",
    y = "Werkloosheidspercentage",
    color = "Gemeente"
  ) +
  geom_line(data = landelijkunem, aes(x = Periode, y = werkloosheidspercentage),
            color = "black", linewidth = 1.2, linetype = "dashed") +
  scale_x_continuous(breaks = 2020:2023) +
  theme_minimal()







