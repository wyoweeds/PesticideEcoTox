library(tidyverse)
library(readxl)
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

### Create a complete list of active ingredients from the AgroTrak data:
CornSoy.aiList <- AgroTrak.CornSoy.dat %>%
  select(Active.Ingredient, Type) %>%
  mutate(ai = str_replace_all(Active.Ingredient, " \\(.\\)", "")) %>%
  distinct()
## Active.Ingredient = as listed in AgroTrak data
## Type = pesticide type as listed in AgroTrak data
## ai = same as 'Active.Ingredient' but with (X) removed for searches
glimpse(CornSoy.aiList)
## Write the AI list to a csv file:
#write.csv(CornSoy.aiList, "AgroTrakCornSoy_aiList.csv")

### Load honey bee toxicity values from ecotox:
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

### Use AgroTrak list to search EPA's comptox by ai
# https://comptox.epa.gov/dashboard/batch-search
# then load the exported comptox files:
compTox.agrotrak <- read.csv("CCD-Batch-Search_2022-09-20_08_11_20.csv") %>%
  mutate(ai = INPUT) %>%
  mutate(cas.number  = as.numeric(str_remove_all(CASRN, "-")))
glimpse(compTox.agrotrak)

### Merge comptox names with AgroTrak names:
CornSoy.aiList.joined <- left_join(CornSoy.aiList, compTox.agrotrak) %>%
  select(Active.Ingredient, Type, ai, PREFERRED_NAME, IUPAC_NAME,
         CASRN, cas.number, 
         Koc = SOIL_ADSORPTION_COEFFICIENT_KOC_L.KG_OPERA_PRED,
         logKow = OCTANOL_WATER_PARTITION_LOGP_OPERA_PRED,
         h2o = WATER_SOLUBILITY_MOL.L_OPERA_PRED) %>%
  mutate(Koc = as.numeric(Koc),
         logKow = as.numeric(logKow),
         h2o = as.numeric(h2o))
glimpse(CornSoy.aiList.joined)

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
         ToxUnit = `Observed Response Units`)
glimpse(ecotox.dat)  


### Merge AgroTrak with Ecotox values:
##### THIS IS FUCKING HARD...........
### Need to figure out how to get the tox data in a format that is useful.
CornSoy.aiToxList <- left_join(CornSoy.aiList.joined, ecotox.dat) #%>%
#### Maybe create a standard tox value?? Probably has to be by life stage...
#  mutate(stdToxValue.ugX = case_when(
#    ToxUnit == "AI mg/kg food" ~ ObsRespMean * 1,
#    ToxUnit == "AI ug/kg fd" ~ ObsRespMean * 1000,
#  ))
glimpse(CornSoy.aiToxList)


### Need to discuss this table among some EPA-informed folks:
table(CornSoy.aiToxList$ToxUnit, CornSoy.aiToxList$Effect, 
      CornSoy.aiToxList$LifeStage)

