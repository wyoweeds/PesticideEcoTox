library(tidyverse)
library(readxl)
library(readODS)
library(cowplot)

### Load Kynnetec AgroTrak pesticide data:
AgroTrak.CornSoy.dat <- read_excel("AgroTrak_CornSoy_v1.xlsx",
                          skip = 5) %>%
  select(Year, Crop,
         Type = "Pesticide Type",
         Active.Ingredient = "Active Ingredient",
         Volume.lbs = "AI volume\r\n(lb)",
         Base.Area.Treated = "Base Area Treated\r\n(acres)")
glimpse(AgroTrak.CornSoy.dat)

### Create a complete list of active ingredients from AgroTrak:
CornSoy.aiList <- AgroTrak.CornSoy.dat %>%
  select(Active.Ingredient, Type) %>%
  mutate(ai = str_replace_all(Active.Ingredient, " \\(.\\)", "")) %>%
  distinct() %>%
  arrange(Type, ai)
## Active.Ingredient = as listed in AgroTrak data
## Type = pesticide type as listed in AgroTrak data
## ai = same as 'Active.Ingredient' but with (X) removed for searches
glimpse(CornSoy.aiList)
## Write the AI list to a csv file:
#write.csv(CornSoy.aiList, "AgroTrakCornSoy_aiList.csv")

### Use AgroTrak list to search EPA's comptox by ai
# https://comptox.epa.gov/dashboard/batch-search
# Website returns the exported file 'CCD-Batch-Search*.csv'
# Load the exported comptox file:
compTox1 <- read.csv("CCD-Batch-Search_2022-09-20_08_11_20.csv") %>%
  mutate(ai = INPUT) %>%
  mutate(cas.number  = as.numeric(str_remove_all(CASRN, "-")))
# manual search for missing cas numbers, search compTox, returned:
compTox2 <- read.csv("CCD-Batch-Search_2022-11-15_04_13_54.csv") %>%
  mutate(cas.number = as.numeric(str_remove_all(CASRN, "-")))
compTox.agrotrak <- compTox1

glimpse(compTox.agrotrak)

### Load honey bee toxicity values from ecotox export:
ecotox.dat0 <- 
  read_excel("ECOTOX-Terrestrial-Export_20211207_132145 Honey Bee Data.xlsx",
             sheet = "Terrestrial-Export") %>%
  rename(cas.number = "CAS Number", chemName = "Chemical Name") 
glimpse(ecotox.dat0)
### Create a complete list of active ingredients from the Ecotox data:
ecotox.chemList <- ecotox.dat0 %>%
  select(cas.number, chemName) %>%
  unique()
glimpse(ecotox.chemList)
### Write the AI list to a CSV file:
#write.csv(ecotox.chemList, "EcotoxChemList.csv", row.names = FALSE)

### load a manually created list of names and cas:
#cclist <- read_ods("CornSoy_aiConcordanceList.ods", 
#         sheet = "AgroTrakCornSoy_aiList")
#glimpse(cclist)

### Merge comptox names with AgroTrak names:
CornSoy.nameList <- CornSoy.aiList %>%
  select(AgroTrakName = Active.Ingredient, Type) %>%
  left_join(cclist) %>%
  mutate(cas.number = as.numeric(str_remove_all(cas, "-")),
         .keep = "unused")
glimpse(CornSoy.nameList)

CornSoy.aiList.joined <- left_join(CornSoy.nameList, compTox.agrotrak) %>%
  select(AgroTrakName, Type, ai, PREFERRED_NAME = CompTox.PREFERRED_NAME,
         cas.number,
         Koc = SOIL_ADSORPTION_COEFFICIENT_KOC_L.KG_OPERA_PRED,
         logKow = OCTANOL_WATER_PARTITION_LOGP_OPERA_PRED,
         h2o = WATER_SOLUBILITY_MOL.L_OPERA_PRED) %>%
  mutate(Koc = as.numeric(Koc),
         logKow = as.numeric(logKow),
         h2o = as.numeric(h2o))
glimpse(CornSoy.aiList.joined)

# CornSoy.aiList.joined <- left_join(CornSoy.aiList, compTox.agrotrak) %>%
#   select(Active.Ingredient, Type, ai, PREFERRED_NAME, IUPAC_NAME,
#          CASRN, cas.number, 
#          Koc = SOIL_ADSORPTION_COEFFICIENT_KOC_L.KG_OPERA_PRED,
#          logKow = OCTANOL_WATER_PARTITION_LOGP_OPERA_PRED,
#          h2o = WATER_SOLUBILITY_MOL.L_OPERA_PRED) %>%
#   mutate(Koc = as.numeric(Koc),
#          logKow = as.numeric(logKow),
#          h2o = as.numeric(h2o))
# glimpse(CornSoy.aiList.joined)

### Pare down ecotox values to those used in the model:
ecotox.dat <- ecotox.dat0 %>%
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

### Merge AgroTrak with Ecotox values:
CornSoy.aiToxList <- left_join(CornSoy.aiList.joined, ecotox.dat) #%>%
glimpse(CornSoy.aiToxList)
### There has GOT to be a more efficient way of doing this <crying emoji>
CornSoyTox.1 <- CornSoy.aiToxList %>%
  filter(
    LifeStage == "Adult" 
    & ExposureType == "Dermal" 
    & Endpoint == "LD50") %>%
  select(Active.Ingredient, ai, cas.number,
         ObsRespMean, ToxUnit) %>%
  mutate(ToxDataInput = "AdultContact.LD50")
CornSoyTox.2 <- CornSoy.aiToxList %>%
  filter(
    LifeStage == "Adult" 
    & ExposureType == "Food" 
    & Endpoint == "LD50") %>%
  select(Active.Ingredient, ai, cas.number,
         ObsRespMean, ToxUnit) %>%
  mutate(ToxDataInput = "AdultOral.LD50")
CornSoyTox.3 <- CornSoy.aiToxList %>%
  filter(
    LifeStage == "Adult" 
    & ExposureType == "Food" 
    & Endpoint == "NOEL") %>%
  select(Active.Ingredient, ai, cas.number,
         ObsRespMean, ToxUnit) %>%
  mutate(ToxDataInput = "AdultOral.NOEL")
CornSoyTox.4 <- CornSoy.aiToxList %>%
  filter(
    LifeStage == "Larva" 
    & ExposureType == "Food" 
    & Endpoint == "LD50") %>%
  select(Active.Ingredient, ai, cas.number,
         ObsRespMean, ToxUnit) %>%
  mutate(ToxDataInput = "LarvaOral.LD50")
CornSoyTox.5 <- CornSoy.aiToxList %>%
  filter(
    LifeStage == "Larva" 
    & ExposureType == "Food" 
    & Endpoint == "NOEL") %>%
  select(Active.Ingredient, ai, cas.number,
         ObsRespMean, ToxUnit) %>%
  mutate(ToxDataInput = "LarvaOral.NOEL")
CornSoyTox.00 <- bind_rows(
  CornSoyTox.1,
  CornSoyTox.2,
  CornSoyTox.3,
  CornSoyTox.4,
  CornSoyTox.5)

CornSoy.tox <- left_join(CornSoy.aiList.joined, CornSoyTox.00)
write_csv(CornSoy.tox %>%
            select(Type, ai, cas.number,
                   Koc, logKow, ToxDataInput,
                   ObsRespMean, ToxUnit),
          "CornSoy_aiToxData-modNew.csv")
### List of all ai:
write_csv(CornSoy.tox %>% distinct(cas.number), "CASListSearchTox.csv")
### List of pesticides with no Tox data:
nodatalist <- data.frame(CornSoy.tox %>%
  filter(is.na(ToxDataInput)) %>%
  distinct(ai))
### List of pesticides with complete Tox data:
CornSoy.tox %>%
  group_by(ai) %>%
  summarize(toxN = length(unique(ToxDataInput))) %>%
  filter(toxN == 5)

  
CornSoy.tox %>%
  select(Active.Ingredient, cas.number, Koc, logKow, h2o, 
         ObsRespMean, ToxUnit, ToxDataInput) %>%
  unite(Mean_ToxUnit, c(ObsRespMean, ToxUnit)) %>%
  filter(!is.na(ToxDataInput)) %>%
  pivot_wider(id_cols = c(Active.Ingredient, cas.number, Koc, logKow, h2o),
              names_from = ToxDataInput,
              values_from = Mean_ToxUnit) %>%
  #mutate_if(is.list, ~ length(.) == 0) %>%
  glimpse()
  

CornSoy.totals <- AgroTrak.CornSoy.dat %>%
  mutate(ai = str_replace_all(Active.Ingredient, " \\(.\\)", "")) %>%
  group_by(ai) %>%
  summarize(TotalVol = sum(Volume.lbs)) %>%
  #filter(ai %in% nodatalist$ai) %>%
  arrange(desc(TotalVol))
CornSoy.totals

### FOR TESTING::: This didn't change anything...
# newTox <- read_csv("TerrestrialReport_20200927.csv")
# glimpse(newTox)
# 
# ecotox.dat <- newTox %>%
#   select(cas.number = `CAS Number`, 
#          ConcType = `Conc 1 Type (Author)`,
#          Effect, 
#          LifeStage = `Organism Lifestage`,
#          ExposureType = `Exposure Type`, 
#          Endpoint, 
#          ObservedDuration.d = `Observed Duration (Days)`,
#          ObsRespMean = `Observed Response Mean`,
#          ToxUnit = `Observed Response Units`) %>%
#   filter(Endpoint %in% c("LD50", "NOEL") &
#            LifeStage %in% c("Adult", "Larva") &
#            str_detect(ObsRespMean, "/", negate = TRUE)) %>%
#   arrange(cas.number, LifeStage, Effect)
# glimpse(ecotox.dat)  
# 
