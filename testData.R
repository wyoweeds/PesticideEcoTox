library("tidyverse")
library("readxl")

### Test loading new AgroTrak data with pest data:


pestList <- read_excel("DATA/PestLists.xlsx",
                       sheet = "PestList") %>%
  select(Crop, Pest, Order)
glimpse(pestList)

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
  mutate(Year = as.numeric(Year),
         ai = str_replace_all(Active.Ingredient, " \\(.\\)", ""),
         Timing = case_when(
           Timing0 %in% c("At Planting", "Before Crop Emergence",
                          "Last Fall", "Prior to Planting") ~ "PRE",
           Timing0 %in% c("Early Post (Herbicides)", "Late Post (Herbicides)",
                          "Postemergence (Non-Herbicides)",
                          "Foliar atTasseling (Fungicides)", 
                          "Foliar before Tasseling (Fungicides)") ~ "POST"),
         Volume.kg = Volume.lbs * 454,
         Treated.ha = Base.Area.Treated / 2.47,
         AppRate.kgha = Volume.kg / Treated.ha) %>%
  left_join(pestList)
glimpse(AgroTrak.PestTiming.dat)


groupRQs <- AgroTrak.ecotox %>%
  group_by(Year, Crop, Timing) %>%
  mutate(RQ.groupSum = sum(RQ, na.rm = TRUE),
         RQ.groupProp = RQ / RQ.groupSum) %>%
  select(Year, Crop, Timing, ai, 
         RQ.groupProp)
AgroTrak.PestRQ <- AgroTrak.PestTiming.dat %>%
  left_join(groupRQs) %>% 
  select(-Timing0) %>%
  group_by(Year, Crop, Timing, ai) %>%
  mutate(ai.pestVol = Volume.kg / sum(Volume.kg)) %>%
  select(Year, Crop, Type, ai, Timing, Pest, Order, 
         RQ.groupProp, ai.pestVol) %>%
  ungroup() %>%
  mutate(pestRQ = RQ.groupProp * ai.pestVol)

glimpse(AgroTrak.PestRQ)

AgroTrak.PestRQ %>%
  filter(Type == "Insecticide" & 
           Timing == "POST" &
           Crop == "Corn") %>%
  group_by(Crop, Year, Order) %>%
  summarize(pestRQ.sum = sum(pestRQ, na.rm = TRUE)) %>%
  group_by(Crop, Order) %>%
  mutate(orderKeep = length(!is.na(pestRQ.sum)) > 4) %>%
  filter(orderKeep == TRUE) %>%
  ggplot(aes(x = Year, y = pestRQ.sum)) +
  geom_point() + stat_smooth(method = "lm", se = FALSE) +
  facet_wrap(~Order) +
  ggtitle("Corn POST Insecticide Targets")


AgroTrak.PestRQ %>%
  filter(Type == "Insecticide" & 
           Timing == "POST" &
           Crop == "Soybeans") %>%
  group_by(Crop, Year, Order) %>%
  summarize(pestRQ.sum = sum(pestRQ, na.rm = TRUE)) %>%
  group_by(Crop, Order) %>%
  mutate(orderKeep = length(!is.na(pestRQ.sum)) > 4) %>%
  filter(orderKeep == TRUE) %>%
  ggplot(aes(x = Year, y = pestRQ.sum)) +
  geom_point() + stat_smooth(method = "lm", se = FALSE) +
  facet_wrap(~Order) +
  ggtitle("Soybean POST Insecticide Targets")
