fsave <- function(x, file, location = "./data/processed/", ...) {
  if (!dir.exists(location))
    dir.create(location)
  datename <- substr(gsub("[:-]", "", Sys.time()), 1, 8)
  totalname <- paste(location, datename, file, sep = "")
  print(paste("SAVED: ", totalname, sep = ""))
  save(x, file = totalname)
}

fpackage.check <- function(packages) {
  lapply(packages, FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
    }
  })
}

colorize <- function(x, color) {
  sprintf("<span style='color: %s;'>%s</span>", color, x)
}

packages = c("rjson", "tidyverse", "ggplot2", "leaflet", "sf")

fpackage.check(packages)

elections <- rjson::fromJSON(file = "./data/tk2021.geo.json")

# this is a bit messy. tidyverse would would be welcome

# retrieve the data
test <- lapply(elections[["features"]], `[[`, 2)
js_df <- sapply(test, function(x) as.data.frame(x))
library(plyr)
js_df <- ldply(js_df, data.frame)
detach("package:plyr", unload = TRUE)

test <- lapply(elections[["features"]], `[[`, 3)
test <- lapply(test, "[[", 2)
js_df$long <- sapply(test, function(x) x[1])
js_df$lat <- sapply(test, function(x) x[2])

names(js_df) <- c("Stembureau", "Adres", "Locatie", "description", "Geldige.stemmen", "Opgeroepen", "Ongeldig",
                  "Blanco", "Geldige.stempassen", "Geldige.volmachtbewijzen", "Geldige.kiezerspassen", "Toegelaten.kiezers",
                  "Meer.getelde.stembiljetten", "Minder.getelde.stembiljetten", "Meegenomen.stembiljetten", "Te.weinig.uitgereikte.stembiljetten",
                  "Te.veel.uitgereikte.stembiljetten", "Geen.verklaring", "Andere.verklaring", "Te.veel.briefstembiljetten",
                  "Geen.briefstembiljetten", "VVD", "PVV", "CDA", "D66", "GL", "SP", "PvdA", "CU", "PvdD", "PLUS50",
                  "SGP", "DENK", "FvD", "BIJ1", "JA21", "CODE.ORANJE", "Volt", "NIDA", "Piratenpartij", "LP..Libertaire.Partij.",
                  "JONG", "Splinter", "BBB", "NLBeter", "Lijst.Henk.Krol", "OPRECHT", "JEZUS.LEEFT", "Trots.op.Nederland..TROTS.",
                  "U.Buntu.Connected.Front", "Blanco..Zeven..A.J.L.B..", "Partij.van.de.Eenheid", "Wij.zijn.Nederland",
                  "gmcode", "election", "electionName", "DE.FEESTPARTIJ..DFP.", "Modern.Nederland", "Vrij.en.Sociaal.Nederland",
                  "De.Groenen", "Partij.voor.de.Republiek", "long", "lat")

# data cleaning

total_phases <-  6
vector_n_phases <-  rep(NA, total_phases)

# remove polling stations without coordinates (briefstembureaus mostly)

phase <- 1
js_df %>%
  filter(!lat == 0) -> js_df2

loses <- nrow(js_df) - nrow(js_df2)

vector_n_phases[phase] <- loses

message(paste("phase:", phase,"-> current loses", loses))

# aggregate polling stations with same location

phase <- 2

js_df2 %>%
  group_by(long, lat) %>%
  dplyr::summarise(Adres = first(Adres), Stembureau = first(Stembureau), description = first(description),
                   gmcode = first(gmcode), across(Geldige.stemmen:Wij.zijn.Nederland, ~sum(.x, na.rm = TRUE))) ->
  js_df2

loses <- nrow(js_df) - nrow(js_df2)

vector_n_phases[phase] <- loses

message(paste("phase:", phase,"-> current loses", loses))

# remove polling station for which no voters were invited

phase <- 3

print("polling stations without invited voters")
table(js_df2$Opgeroepen == 0, useNA = "always")
# lets throw these away
js_df2 %>%
  filter(!Opgeroepen == 0) -> js_df2

loses <- nrow(js_df) - nrow(js_df2)

vector_n_phases[phase] <- loses

message(paste("phase:", phase,"-> current loses", loses))

# remove polling stations where a lot more voters voted than were invited.

phase <- 4

print("polling stations that are very popular (more than 110% votes)")
table((js_df2$Geldige.stemmen/js_df2$Opgeroepen) > 1.1, useNA = "always")

# lets throw these away
js_df2 %>%
  filter(!((Geldige.stemmen/Opgeroepen) > 1.1)) -> js_df2

loses <- nrow(js_df) - nrow(js_df2)

vector_n_phases[phase] <- loses

message(paste("phase:", phase,"-> current loses", loses))

# still need to remove polling stations where it is unlikely that voters are from their catching
# area. (e.g. NS stations)

phase <- 5

sel <- which(grepl("Brief", js_df2$description))
js_df2 <- js_df2[-sel, ]
sel <- which(grepl("station", js_df2$description))
js_df2 <- js_df2[-sel, ]

loses <- nrow(js_df) - nrow(js_df2)

vector_n_phases[phase] <- loses

message(paste("phase:", phase,"-> current loses", loses))

# remove polling stations without valid votes

phase <- 6

js_df2 %>%
  filter(!(Geldige.stemmen == 0)) -> js_df2

loses <- nrow(js_df) - nrow(js_df2)

vector_n_phases[phase] <- loses

message(paste("phase:", phase,"-> current loses", loses))

# final

#differences of the original dataset v/s the cleaned one
 
tibble(phase=1:6, n=vector_n_phases) %>%
  mutate(losses=lead(n)-n) %>%
  pull(losses)

# how many losses in different regions

library(purrr)
library(stringr)
library(janitor)
library(readr)
library(tidyr)

# postcodes https://github.com/wesdegroot/PHP/blob/5673a5bb5fbd9ae0fd4870755a3dab428cf5cbf3/datasets/postcode_NL.csv.zip

postcodes <- read_delim("data/postcode_NL.csv", 
                        delim = ";",
                        col_select = c(2,6,7,9,10,14) ,
                        col_names = FALSE) %>% 
  rename(postalcode=X2, from=X6, to=X7, address=X9, city=X10, province=X14)

js_df_postalcode <- left_join(js_df, postcodes, by=c("Adres"="postalcode"))
js_df2_postalcode <- left_join(js_df2, postcodes, by=c("Adres"="postalcode"))


is.na(js_df_postalcode %>% pull(city)) %>% tabyl()

# warning: lots of NA

js_df_postalcode %>%
  group_by(city) %>%
  count(name = "n_initial") %>% 
  arrange(desc(n_initial)) %>% 
  full_join(js_df2_postalcode %>%
              group_by(city) %>%
              count(name = "n_final") %>% 
              arrange(desc(n_final))) %>% 
  mutate(final_coverage = (n_final/n_initial)*100) %>% 
  arrange(final_coverage) %>% 
  filter(final_coverage<50) %>% 
  print(n=1000)

#how voted jochem

js_df_postalcode %>% 
  filter(city=="Berg en Dal") %>% 
  select(Locatie,VVD:Wij.zijn.Nederland) %>% 
  gather(key="party", value="votes", -Locatie) %>% 
  group_by(party) %>% 
  summarise(total_votes=sum(votes, na.rm = TRUE)) %>% 
  mutate(prc=(total_votes/sum(total_votes, na.rm = TRUE))*100) %>% 
  arrange(desc(prc))
  
#https://en.wikipedia.org/wiki/Democrats_66







