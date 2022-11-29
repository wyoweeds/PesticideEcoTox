library(tidyverse)
library(readxl)
library(readODS)
library(cowplot)

ecotox.dat <- 
  read_excel("ECOTOX-Terrestrial-Export_20211207_132145 Honey Bee Data.xlsx",
             sheet = "Terrestrial-Export") %>%
  rename(cas.number = "CAS Number", chemName = "Chemical Name") %>%
  select(cas.number, 
         ConcType = `Conc 1 Type (Author)`,
         Effect, 
         LifeStage = `Organism Lifestage`,
         ExposureType = `Exposure Type`, 
         Endpoint, 
         ObservedDuration.d = `Observed Duration (Days)`,
         ObsRespMean = `Observed Response Mean`,
         ToxUnit = `Observed Response Units`) %>%
  filter(Endpoint %in% c("LD50", "NOEL"))
glimpse(ecotox.dat)  


cclist <- read_ods("CornSoy_aiConcordanceList.ods", 
                   sheet = "AgroTrakCornSoy_aiList") %>%
  mutate(cas.number = as.numeric(str_remove_all(cas, "-")),
         .keep = "unused") %>%
  mutate(ecoTox = cas.number %in% unique(ecotox.dat$cas.number))
glimpse(cclist)
tail(cclist)


