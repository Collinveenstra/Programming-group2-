UnemploymentRegion <- read.csv("~/Downloads/Arbeidsmarktsituatie_jongeren__regio_10062025_142603.csv", sep=";")
UnemploymentRegion$Geregistreerd.werkzoekende.bij.UWV <- as.factor(UnemploymentRegion$Geregistreerd.werkzoekende.bij.UWV)
summary(UnemploymentRegion$Geregistreerd.werkzoekende.bij.UWV)