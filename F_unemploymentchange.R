library(dplyr)
filtered_unemploymentgemeentes <- read_csv("important data/filtered_unemploymentgemeentes.csv")
filtered_unemploymentchangegemeentes <- 
  filtered_unemploymentgemeentes %>%
  arrange(Perioden) %>%  # ensure data is ordered correctly
  mutate(unemployment_change = Jongeren..15.tot.27.jaar...aantal. - 
                                    lag(Jongeren..15.tot.27.jaar...aantal.))
     
filtered_unemploymentchangegemeentes

filtered_unemploymentchangegemeentes <- filtered_unemploymentgemeentes %>%
  arrange(Regio.s, Perioden) %>%  # ensure data is ordered correctly
  group_by(Regio.s) %>%
  Jongeren..15.tot.27.jaar...aantal.           ungroup()
names(filtered_unemploymentgemeentes)
filtered_unemploymentgemeentes

df <- df %>%
  arrange(Regio.s, Perioden) %>%
  group_by(Regio.s) %>%
  mutate(
    unemployment_pct_change = 
      (`Jongeren..15.tot.27.jaar...aantal.` - lag(`Jongeren..15.tot.27.jaar...aantal.`)) / 
      lag(`Jongeren..15.tot.27.jaar...aantal.`) * 100
  ) %>%
  dplyr::ungroup()
class(filtered_unemploymentgemeentes)
print(class(filtered_unemploymentgemeentes))

arrange
