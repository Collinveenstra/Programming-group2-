
##################################################
####importing the data of gemeentes and unemployment in Rstudio####
##################################################

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
