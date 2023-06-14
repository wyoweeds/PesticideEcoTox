library("tidyverse")
library("readxl")

### Load Kynnetec AgroTrak pesticide data (proprietary):
AgroTrak.CornSoy.dat <- read_excel("DATA/AgroTrak_CornSoy_v1.xlsx",
                                   skip = 5) %>%
  select(Year, Crop,
         Type = "Pesticide Type",
         Active.Ingredient = "Active Ingredient",
         Volume.lbs = "AI volume\r\n(lb)",
         Base.Area.Treated = "Base Area Treated\r\n(acres)") %>%
  mutate(ai = str_replace_all(Active.Ingredient, " \\(.\\)", "")) 
glimpse(AgroTrak.CornSoy.dat)
## Get full pesticide list from AgroTrak:
AgroTrak.aiList <- AgroTrak.CornSoy.dat %>%
  select(ai) %>%
  distinct()

### Load ALL ecotox with the preferred selection chain
### then compare to Farruggia to see what is different.

### load Farruggia tox data:
farruggia.s1.all <- read_excel("DATA/Farruggia_2022_S1 data table.xlsx",
                          sheet = "Supporting Table S1", 
                          skip = 7, 
                          range = cell_cols("A:F"),
                          col_names = c("Pesticide", "PCCODE", 
                                        "Type", "Class", "MOA", 
                                        "AAC.LD50")) %>%
  select(Pesticide:AAC.LD50) %>%
  mutate(ineq = ifelse(str_detect(AAC.LD50, ">"), ">", NA),
         AAC.LD50 = as.numeric(str_remove(AAC.LD50, ">")),
         ai = str_to_upper(Pesticide)) %>%
  mutate(source = "Farruggia et al. 2022") %>%
  select(ai, ineq, AAC.LD50, source) %>%
  filter(!is.na(AAC.LD50)) %>%
  right_join(AgroTrak.aiList)

### Load concordance file:
srcTab <- read_csv("DATA/cclistInEcotox.csv")
glimpse(srcTab)

### Load honey bee toxicity values from Ecotox export
ecotox.dat0 <- 
  read_excel("DATA/ECOTOX-Terrestrial-Export_20211207.xlsx",
             sheet = "Terrestrial-Export") %>%
  rename(cas.number = "CAS Number", chemName = "Chemical Name") %>%
  filter(`Species Scientific Name` == "Apis mellifera") %>%
  select(source, cas.number, chemName,
         Species = `Species Scientific Name`, 
         Lifestage = `Organism Lifestage`,
         ExposureType = `Exposure Type`, 
         concType = `Conc 1 Type (Author)`,
         Effect, Endpoint, ineq = `Observed Response Mean Op`,
         ObsRespMean = `Observed Response Mean`,
         ToxUnit = `Observed Response Units`,
         ObservedDuration.d = `Observed Duration (Days)`)
glimpse(ecotox.dat0)

### Join with the concordance list:
ecotox.dat1 <- ecotox.dat0 %>%
  filter(Lifestage == "Adult" &
           Endpoint == "LD50" & 
           ExposureType == "Dermal") %>%
  right_join(srcTab) %>%
  select(-inEcoTox, -inManualAdd, -ModeOfAction)
glimpse(ecotox.dat1) 

## SELECTION LOGIC for exported Ecotox data
## For ais with multiple adult acute dermal LD50 values in Ecotox:
## 1) if there is only one exact estimate use it (exclude > values)
## 2) if there are multiple exact estimates, use the lowest value
## 3) if there are no exact estimates, use the lowest > value (questionable...)
ecotox.select1 <- ecotox.dat1 %>%
  group_by(ai) %>%
  mutate(n.est = sum(is.na(ineq))) %>%
  filter(case_when(
    n.est == 1 ~ is.na(ineq), ## (1)
    n.est > 1 ~ is.na(ineq) & ObsRespMean == min(ObsRespMean), ## (2)
    n.est == 0 ~ ObsRespMean == min(ObsRespMean) ## (3)
  )) %>%
  select(ai, eco.ineq = ineq, ObsRespMean, source)
glimpse(ecotox.select1)

ecotox.dat2 <- left_join(ecotox.select1 %>% select(-source), 
                         farruggia.s1.all %>% select(-source)) %>%
  rename(ecotox.ineq = eco.ineq,
         ecotox.LD50 = ObsRespMean,
         farruggia.ineq = ineq,
         farruggia.LD50 = AAC.LD50)
glimpse(ecotox.dat2)

missingSearch.dat <- read_excel("DATA/MissingToxDataSearch.xlsx",
                                sheet = "MissingToxDataSearch",
                                range = cell_cols("A:F")) %>%
  select(ai, manual.ineq = ineq, manual.LD50 = AAC.LD50)
glimpse(missingSearch.dat)
ecotox.dat3 <- left_join(ecotox.dat2, missingSearch.dat)
write_csv(ecotox.dat3, "AllToxSelection.csv")


ecotox.dat2 %>%
  mutate(same.LD50 = ecotox.LD50 == farruggia.LD50,
         lower.LD50 = case_when(
           ecotox.LD50 - farruggia.LD50 < 0 ~ "Ecotox",
           ecotox.LD50 - farruggia.LD50 > 0 ~ "Farruggia",
           same.LD50 == TRUE ~ "Same")) %>%
  arrange(lower.LD50, ecotox.ineq) %>%
  write_csv("ToxSelectionMethodCompare.csv")



## This kind of suggests we can ignore Farruggia and just go with ecotox?

### Test loading new AgroTrak data:

AgroTrak.PestTiming.dat <- read_excel(
  "DATA/US AgroTrak Chemical Use Timing and Pests.xlsx",
  skip = 5) %>%
  select(Year, Crop,
         Timing0 = `Application Timing`,
         Type = `Pesticide Type`,
         Active.Ingredient = `Active Ingredient`,
         Pest = Pests,
         Volume.lbs = `AI volume\r\n(lb)`,
         Base.Area.Treated = `Base Area Treated\r\n(acres)`,
         Total.Area.Treated = `Total Area Treated\r\n(acres)`) %>%
  mutate(ai = str_replace_all(Active.Ingredient, " \\(.\\)", ""),
         Timing = case_when(
           Timing0 %in% c("At Planting", "Before Crop Emergence",
                          "Last Fall", "Prior to Planting") ~ "PRE",
           Timing0 %in% c("Early Post (Herbicides)", "Late Post (Herbicides)",
                          "Postemergence (Non-Herbicides)",
                          "Foliar atTasseling (Fungicides)", 
                          "Foliar before Tasseling (Fungicides)") ~ "POST")) 
glimpse(AgroTrak.PestTiming.dat)

