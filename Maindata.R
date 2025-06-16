
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

gem_unemployment = read.csv("standard data/gem_unemployment.csv")


#################################################################
####combining the multiple pieces of gemeentes into 1 dataset####
#################################################################

library(dplyr)
library(tidyverse)

gemeentesmentaldata2020 = bind_rows(Aatillgoereemental2020,goestillnissewaardmental2020,noardtillvlielandmental2020,vlissingentillzwollemental2020)
gemeentesmentaldata2022 = bind_rows(Aatillgoereemental2022,goestillnissewaardmental2022,noardtillvlielandmental2022,vlissingentillzwollemental2022)

write.csv(gemeentesmentaldata2020,"data/gemeentesmentaldata2020.csv")
write.csv(gemeentesmentaldata2022,"data/gemeentesmentaldata2022.csv")

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

fltrd_unemployment = gem_unemployment %>%
  filter(Arbeidskenmerken %in% c("Totaal ", "Arb.pos.: niet-werkzame bevolking"))

fltrd_unemployment = fltrd_unemployment %>%
  filter(Geregistreerd.werkzoekende.bij.UWV %in% "Totaal")

write.csv(fltrd_unemployment,"important data/fltrd_unemployment")

############################################################################################
####prepare mental and unemployment set so that we can merge them together in 1 data set####
############################################################################################

fltrd_unemployment = fltrd_unemployment %>%
  arrange(Perioden)

fltrd_unemployment = fltrd_unemployment %>%
  rename(Gemeente = Regio.s) 
fltrd_unemployment$Perioden = as.character(fltrd_unemployment$Perioden)

filtered_gemeentesmentaldata2020$Psychische.klachten.... = NA_real_

Mental2020_2022 = bind_rows(filtered_gemeentesmentaldata2020,filtered_gemeentesmentaldata2022)

Mental2020_2022 = Mental2020_2022 %>%
  rename(Gemeente = Wijken.en.buurten)
Mental2020_2022$Perioden = as.character(Mental2020_2022$Perioden)

#######################
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

########################################
###now merge the 2 data sets together###
########################################

Combined_data = full_join(gem_unemployment,Mental2020_2022, by = c("Gemeente","Perioden"))

combined_data = full_join(gem_unemployment,Mental2020_2022,by = c("Gemeente,"Perioden"))


write.csv(combined_data,"important data/combined_data.csv")