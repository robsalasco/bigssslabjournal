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

packages <- c("haven", "tidyverse")
fpackage.check(packages)



kieskom <- read.csv("./data/kieskompas_df.csv")

names(kieskom)[-c(1, 2)] <- labs <- c("build1", "build2", "build3", "democracy1", "democracy2", "democracy3",
                                      "democracy4", "social1", "social2", "social3", "social4", "climate1", "climate2", "climate3", "climate4",
                                      "educ_care1", "educ_care2", "educ_care3", "educ_care4", "educ_care5", "immigration1", "immigration2",
                                      "immigration3", "foreign_policy1", "foreign_policy2", "foreign_policy3", "foreign_policy4", "justice1",
                                      "justice2", "justice3")
kieskom <- kieskom[-1]  # exclude the indicator column
names(kieskom)[-1] <- paste0("kieskom_", names(kieskom)[-1])  # add a kieskom label to the column names
names(kieskom)[1] <- "party"

pop <- read_dta("./data/expert_data_stata.dta")
# subset Dutch parties (country id=19)
pop <- pop[which(pop$country_id == 19), ]

# subset party and dimensions names(pop)
pop <- pop[, c(4, 6:21)]
# add poppa label to variables names(pop)[-c(1)] <- paste0('pop_',names(pop)[-c(1)])

ches <- read_dta("./data/CHES2019_experts.dta")
ches <- as.data.frame(ches)
dutch <- 1001:1051  #Dutch party ids
ches <- ches[ches$party_id %in% dutch, ]  #subset Dutch parties
ches <- ches[, c(3, 5:49)]

df <- pop %>%
  group_by(party) %>%
  summarize(median = median(lroverall, na.rm = T))  # strip missings before aggregation!

kieskom$party <- tolower(kieskom$party)
df$party <- tolower(df$party)

# merge by party;

merged <- merge(df, kieskom, by = "party", all = T)

# MY WORKKKKKK

colnames(merged)
nrow(merged)

# check distribution of lroverall

hist(df$median)


# Back again to the full code

dpes <- read_spss("./data/DPES2021 v1.0.sav")

dpes %>% 
  select(S152, N76) %>% 
  group_by(N76) %>% 
  summarize(mean = mean(S152, na.rm = T)) %>% 
  arrange(desc(mean)) %>% 
  mutate(dif=lag(mean)-mean) %>% 
  print(n=100)

df_similar_mean <- dpes %>% 
  select(S152, N76) %>% 
  filter(N76 %in% c(22, 7))

df_similar_mean %>% 
  ggplot(aes(x=S152))+
  geom_histogram(color="black", fill="white")+
  facet_wrap(~N76, scales = "free_y", nrow = 2)

df <- dpes %>% 
  select(S152, N76) %>% 
  group_by(N76) %>% 
  summarize(median = median(S152, na.rm = T)) %>% 
  arrange(desc(median))

