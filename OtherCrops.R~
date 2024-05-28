library("tidyverse")
library("readxl")

### Cotton

cottonRegions <- data.frame(
    State = c("VIRGINIA", "NORTH CAROLINA", "SOUTH CAROLINA", "GEORGIA", "FLORIDA", "ALABAMA",
              "MISSOURI", "TENNESSEE", "ARKANSAS", "MISSISSIPPI", "LOUISIANA",
              "TEXAS", "OKLAHOMA", "KANSAS",
              "CALIFORNIA", "ARIZONA", "NEW MEXICO"),
    Region = c(rep("Southeastern", 6),
               rep("Mid South", 5),
               rep("Central", 3),
               rep("Western", 3)))
cottonRegions              

    
### Load Kynnetec FarmTrak pesticide data (proprietary):
FarmTrak.Cotton.dat <- read_excel(
  "DATA/NewCropsRJ/Cotton_US_AgroTrak_Chemical_Use_USDA-State.xlsx",
  skip = 5) %>%
    left_join(cottonRegions) %>%
  select(Year, Region, State, Crop,
         Timing0 = `Application Timing`,
         Type = `Pesticide Type`,
         Active.Ingredient = `Active Ingredient`,
         Volume.lbs = `AI volume\r\n(lb)`,
         Base.Area.Treated = `Base Area Treated\r\n(acres)`,
         Total.Area.Treated = `Total Area Treated\r\n(acres)`) %>%
  mutate(ai = str_replace_all(Active.Ingredient, " \\(.\\)", ""),
         Timing = case_when(
           Timing0 %in% c("At Planting", "Before Crop Emergence",
                          "Last Fall", "Prior to Planting") ~ "PRE",
           Timing0 %in% c("Early Post (Herbicides)", 
                          "Emergence to Pinhead Squaring",
                          "Peak Bloom to Harvest",
                          "Pinhead Squaring to Peak Bloom",
                          "Late Post (Herbicides)",
                          "Postemergence (Non-Herbicides)",
                          "Foliar atTasseling (Fungicides)", 
                          "Foliar before Tasseling (Fungicides)") ~ "POST"),
         Timing = factor(Timing, ordered = "TRUE",
                         levels = c("PRE", "POST")),
         BloomTime = case_when(
             Timing0 %in% c("Pinhead Squaring to Peak Bloom",
                            "Peak Bloom to Harvest") ~ "Pinhead squaring to harvest",
             .default = "All other times")) %>%
  group_by(Year, Region, Crop, BloomTime, Type, Active.Ingredient, ai) %>%
  summarize(Volume.lbs = round(sum(Volume.lbs)),
            Base.Area.Treated = round(sum(Base.Area.Treated)),
            Total.Area.Treated = round(sum(Total.Area.Treated))) %>%
  mutate(Type = factor(Type,
                       levels = c("Insecticide", "Herbicide", "Fungicide",
                                  "Nematicide", "Growth Regulator")))
glimpse(FarmTrak.Cotton.dat)


### Other Crops

### Load Kynnetec FarmTrak pesticide data (proprietary):
FarmTrak.Other.dat <- read_excel(
  "DATA/OtherCrops_US_AgroTrak_Chemical_Use_USDA.xlsx",
  skip = 4) %>%
  select(Year, Crop,
         Timing0 = `Application Timing`,
         Type = `Pesticide Type`,
         Active.Ingredient = `Active Ingredient`,
         Volume.lbs = `AI volume\r\n(lb)`,
         Total.Area.Treated = `Total Area Treated\r\n(acres)`) %>%
  mutate(ai = str_replace_all(Active.Ingredient, " \\(.\\)", ""),
         Timing = case_when(
           Timing0 %in% c("At Planting", "Before Crop Emergence",
                          "Last Fall", "Prior to Planting") ~ "PRE",
           Timing0 %in% c("Early Post (Herbicides)", 
                          "Late Post (Herbicides)",
                          "Postemergence (Non-Herbicides)",
                          "This Year (Pasture, Fallow)",
                          "Post Dormant Alfalfa <= 1 Yr", 
                          "Post Dormant Alfalfa > 1 Yr", 
                          "Post Growing Alfalfa <= 1 Yr",
                          "Post Growing Alfalfa > 1 Yr",
                          "Post Kill Alfalfa <= 1 Yr",
                          "Post Kill Alfalfa > 1 Yr") ~ "POST"),
         Timing = factor(Timing, ordered = "TRUE",
                         levels = c("PRE", "POST"))) %>%
  group_by(Year, Crop, Timing, Type, Active.Ingredient, ai) %>%
  summarize(Volume.lbs = round(sum(Volume.lbs)),
            Total.Area.Treated = round(sum(Total.Area.Treated))) %>%
  mutate(Type = factor(Type,
                       levels = c("Insecticide", "Herbicide", "Fungicide")),
         Year = as.numeric(Year))
glimpse(FarmTrak.Other.dat)

### Load new honey bee toxicity values from Ecotox export
ecotox.dat0 <- 
  read_excel("DATA/NewCropsRJ/ECOTOX-Terrestrial-Export_20240507_115351-NewAIs_rj.xlsx",
             sheet = "Terrestrial-Export") %>%
  rename(cas.number = "CAS Number", chemName = "Preferred name") %>%
  filter(`Species Scientific Name` == "Apis mellifera") %>%
  select(ai, cas.number, chemName,
         Species = `Species Scientific Name`, 
         Lifestage = `Organism Lifestage`,
         ExposureType = `Exposure Type`, 
         concType = `Conc 1 Type (Author)`,
         Effect, Endpoint, ineq = `Observed Response Mean Op`,
         ObsRespMean = `Observed Response Mean`,
         ToxUnit = `Observed Response Units`,
         ObservedDuration.d = `Observed Duration (Days)`)
glimpse(ecotox.dat0)

## SELECTION LOGIC for exported Ecotox data
## For ais with multiple adult acute dermal LD50 values in Ecotox:
## 1) if there is only one exact estimate use it (exclude > values)
## 2) if there are multiple exact estimates, use the lowest value
## 3) if there are no exact estimates, use the lowest > value (questionable...)
ecotox.select1 <- ecotox.dat0 %>%
  group_by(ai) %>%
  mutate(n.est = sum(is.na(ineq))) %>%
  filter(ObsRespMean > 0) %>%
  filter(case_when(
    n.est == 1 ~ is.na(ineq), ## (1)
    n.est > 1 ~ is.na(ineq) & ObsRespMean == min(ObsRespMean), ## (2)
    n.est == 0 ~ ObsRespMean == min(ObsRespMean) ## (3)
  )) %>%
  select(ai, ineq, AAC.LD50 = ObsRespMean) %>%
  summarize(AAC.LD50 = unique(AAC.LD50)) %>%
  mutate(source = "Ecotox Export (new)")
glimpse(ecotox.select1)

### Read in final tox values from corn & soy analysis
cstox.dat <- read.csv("DATA/NewCropsRJ/CornSoyToxData.csv")
### Merge new AI ecotox data with corn & soy values
ecotox.dat <- bind_rows(ecotox.select1, cstox.dat) %>%
  select(ai, ineq, AAC.LD50, source) %>%
  arrange(ai)
glimpse(ecotox.dat)
write_csv(ecotox.dat,
          "DATA/NewCropsRJ/MergedEcotoxDRAFT.csv")

cropArea.dat <- read_excel("DATA/NewCropsRJ/CropAcres.xlsx",
                           skip = 2) %>%
  rename(acres = `Crop Area Grown\r\n(Acres)`) %>%
  mutate(Year = as.numeric(Year),
         hectares = acres / 2.47)
glimpse(cropArea.dat)

cottonArea.dat <- read_excel("DATA/NewCropsRJ/CottonAcres.xlsx",
                             skip = 2) %>%
    left_join(cottonRegions) %>%
  rename(acres = `Crop Area Grown\r\n(Acres)`) %>%
  mutate(Year = as.numeric(Year),
         hectares = acres / 2.47) %>%
    group_by(Year, Region) %>%
    summarize(hectares = sum(hectares, na.rm = TRUE))
glimpse(cottonArea.dat)


## Merge Ecotox with usage data
Cotton.ecotox <-
  FarmTrak.Cotton.dat %>%
  left_join(ecotox.dat) %>%
  mutate(Year = as.numeric(Year),
         Volume.kg = round(Volume.lbs * 0.45359)) %>%
  left_join(cottonArea.dat) %>%
  mutate(ToxIndex.vol = Volume.kg / AAC.LD50,
         ToxIndex.area = (Volume.kg / hectares) / AAC.LD50)
glimpse(Cotton.ecotox)

Other.ecotox <-
  FarmTrak.Other.dat %>%
  left_join(ecotox.dat) %>%
  left_join(cropArea.dat) %>%
  mutate(Year = as.numeric(Year),
         Volume.kg = round(Volume.lbs * 0.45359),
         Rate.kgha = Volume.kg / hectares) %>%
  mutate(BeeRex.EEC = (Volume.kg / hectares) * 2.4 / AAC.LD50,
         ToxIndex.vol = Volume.kg / AAC.LD50,
         ToxIndex.area = (Volume.kg / hectares) / AAC.LD50)
glimpse(Other.ecotox)


### Figure out what's missing!
Cotton.ecotox %>%
  group_by(Type, ai) %>%
  mutate(toxMissing = is.na(AAC.LD50)) %>%
  summarize(totalVol = sum(Volume.kg, na.rm = TRUE),
            toxMissing = unique(toxMissing)) %>%
  arrange(-totalVol) %>%
  filter(Type == "Insecticide" & toxMissing == TRUE)

Other.ecotox %>%
  group_by(Crop, Type, ai) %>%
  mutate(toxMissing = is.na(AAC.LD50)) %>%
  summarize(totalVol = sum(Volume.kg, na.rm = TRUE),
            toxMissing = unique(toxMissing)) %>%
  arrange(-totalVol) %>%
  filter(Type == "Insecticide" & toxMissing == TRUE)




###########################
### Cotton Plot
glimpse(Cotton.ecotox)
CottonToxIndex.summary.yr <- Cotton.ecotox %>%
  filter(Type == "Insecticide") %>%
  group_by(Year, Region, Crop, BloomTime) %>%
  summarize(ToxIndex.vol = sum(ToxIndex.vol, na.rm = TRUE),
            ToxIndex.area = sum(ToxIndex.area, na.rm = TRUE))

glimpse(CottonToxIndex.summary.yr)
options(scipen = 10)
CottonTI.gg <- ggplot(CottonToxIndex.summary.yr,
       aes(x = Year, y = ToxIndex.area)) +
  geom_point() + geom_smooth(se = FALSE) +
  facet_grid(BloomTime ~ Region) +#,
#             scales = "free_y") +
  ggtitle("Cotton Insecticide") +
  ylab("Toxicity Index") +
  xlab(element_blank()) 
ggsave("DATA/NewCropsRJ/CottonTIplot.png", CottonTI.gg,
       width = 9, height = 5, units = "in", dpi = 300)

### Other Crops Plot
glimpse(Other.ecotox)
OtherToxIndex.summary.yr <- Other.ecotox %>%
  filter(Type == "Insecticide") %>%
  group_by(Year, Crop, Timing) %>%
  summarize(ToxIndex.vol = sum(ToxIndex.vol, na.rm = TRUE),
            ToxIndex.area = sum(ToxIndex.area, na.rm = TRUE))
glimpse(OtherToxIndex.summary.yr)

OtherToxIndex.summary.yr %>%
  group_by(Crop, Timing) %>%
  summarize(toxIndex.vol = sum(ToxIndex.vol, na.rm = TRUE),
            toxIndex.area = sum(ToxIndex.area, na.rm = TRUE))

OtherTI.gg <- ggplot(OtherToxIndex.summary.yr %>% 
                       filter(Year >= 2000 &
#                                Timing == "POST" &
                                Crop %in% c("Alfalfa",
                                           "Dry Beans/Peas")),
       aes(x = Year, y = ToxIndex.area)) +
  geom_point() + geom_smooth() +
  facet_grid(Timing ~ Crop,
             scales = "free_y") +
  ggtitle("Insecticides") +
  ylab("Toxicity Index") +
  xlab(element_blank())
ggsave("DATA/NewCropsRJ/OtherTIplot.png", OtherTI.gg,
       width = 7, height = 5, units = "in", dpi = 300)

CanolaTI.gg <- ggplot(OtherToxIndex.summary.yr %>% 
                        filter(Timing == "POST" &
                                 Crop %in% c("Canola (oilseed rape)")),
                      aes(x = Year, y = ToxIndex.area)) +
  geom_point() + geom_smooth() +
  facet_grid(Timing ~ Crop,
             scales = "free_y") +
  ggtitle("Insecticides") +
  ylab("Toxicity Index") +
  xlab(element_blank())
CanolaTI.gg
ggsave("DATA/NewCropsRJ/CanolaTIplot.png", CanolaTI.gg,
       width = 4, height = 3, units = "in", dpi = 300)
