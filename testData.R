library("tidyverse")
library("readxl")



AgroTrak.ecotox %>%
  filter(Type == "Insecticide" &
           Timing == "POST" &
           Crop == "Soybeans") %>%
  group_by(ai) %>%
  summarize(ToxIndex.areaSum = sum(ToxIndex.area, na.rm = TRUE)) %>%
  arrange(-ToxIndex.areaSum) %>% 
  top_n(7) %>%
  select(ai)


cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", 
               "#0072B2", "#D55E00", "#CC79A7")

glimpse(AgroTrak.ecotox)
AgroTrak.ecotox %>%
  filter(Type == "Insecticide" &
           Timing == "POST" &
           Crop == "Corn") %>%
  mutate(aiplot = case_when(
    ai %in% c("BIFENTHRIN", "CHLORPYRIFOS", "CYHALOTHRIN-LAMBDA",
              "PERMETHRIN", "DELTAMETHRIN", "ZETA-CYPERMETHRIN") ~ ai,
    TRUE ~ "ALL OTHER INSECTICIDES"),
    aiplot = factor(aiplot, levels = c(
      "BIFENTHRIN", "CHLORPYRIFOS", "CYHALOTHRIN-LAMBDA",
      "PERMETHRIN", "DELTAMETHRIN", "ZETA-CYPERMETHRIN",
      "ALL OTHER INSECTICIDES"))) %>%
  group_by(Year, aiplot) %>%
  summarize(ToxIndex.area = sum(ToxIndex.area, na.rm = TRUE)) %>%
  ungroup() %>%
  complete(aiplot, Year, fill = list(ToxIndex.area = 0)) %>%
  ggplot(aes(x = Year, y = ToxIndex.area)) +
  geom_area(aes(fill = aiplot)) +
  scale_fill_manual(values = cbPalette) +
  ggtitle("Corn POST") +
  xlim(c(1998, 2020))
  
AgroTrak.ecotox %>%
  filter(Type == "Insecticide" &
           Timing == "POST" &
           Crop == "Soybeans") %>%
  mutate(aiplot = case_when(
    ai %in% c("BIFENTHRIN", "CHLORPYRIFOS", "CYHALOTHRIN-LAMBDA",
              "DELTAMETHRIN", "THIAMETHOXAM", "CYFLUTHRIN", "ACEPHATE") ~ ai,
    TRUE ~ "ALL OTHER INSECTICIDES"),
    aiplot = factor(aiplot, 
                    levels = c("BIFENTHRIN", "CHLORPYRIFOS", 
                               "CYHALOTHRIN-LAMBDA", "DELTAMETHRIN", 
                               "THIAMETHOXAM", "CYFLUTHRIN", "ACEPHATE",
                               "ALL OTHER INSECTICIDES"))) %>%
  group_by(Year, aiplot) %>%
  summarize(ToxIndex.area = sum(ToxIndex.area, na.rm = TRUE)) %>%
  ungroup() %>%
  complete(aiplot, Year, fill = list(ToxIndex.area = 0)) %>%
  ggplot(aes(x = Year, y = ToxIndex.area)) +
  geom_area(aes(fill = aiplot)) +
  scale_fill_manual(values = cbPalette) +
  ggtitle("Soybean POST") +
  xlim(c(1998, 2020))

AgroTrak.PestToxIndex %>%
  filter(Type == "Insecticide" & 
           Timing == "POST" &
           Crop == "Corn" &
           TaxonGroup %in% c("All Insects", "Unknown")) %>%
  group_by(Year, ai) %>%
  summarize(mean.pestToxIndex = mean(pestToxIndex)) %>%
  ggplot(aes(x = Year, y = mean.pestToxIndex)) +
  geom_point() + geom_line() +
  facet_wrap(~ ai)

AgroTrak.PestToxIndex %>%
  filter(Type == "Insecticide" & 
           Timing == "POST" &
           Crop == "Corn" &
           ai == "BIFENTHRIN") %>%
  group_by(Year, TaxonGroup) %>%
  summarize(mean.pestToxIndex = mean(pestToxIndex)) %>%
  ggplot(aes(x = Year, y = mean.pestToxIndex)) +
  geom_point() + geom_line() +
  facet_wrap(~ TaxonGroup)


AgroTrak.PestToxIndex %>%
  filter(Type == "Insecticide" & 
           Timing == "POST" &
           Crop == "Soybeans" &
           TaxonGroup %in% c("All Insects", "Unknown")) %>%
  group_by(Year, ai) %>%
  summarize(mean.pestToxIndex = mean(pestToxIndex)) %>%
  ggplot(aes(x = Year, y = mean.pestToxIndex)) +
  geom_point() + geom_line() +
  facet_wrap(~ ai)

AgroTrak.PestToxIndex %>%
  filter(Type == "Insecticide" & 
           Timing == "POST" &
           Crop == "Soybeans" &
           ai == "BIFENTHRIN") %>%
  group_by(Year, TaxonGroup) %>%
  summarize(mean.pestToxIndex = mean(pestToxIndex)) %>%
  ggplot(aes(x = Year, y = mean.pestToxIndex)) +
  geom_point() + geom_line() +
  facet_wrap(~ TaxonGroup)

glimpse(AgroTrak.ecotox) %>%
  filter(ai == "BIFENTHRIN" &
           Crop == "Corn" &
           Timing == "POST") %>%
  ggplot(aes(x = Year, y = Volume.kg)) +
  geom_line(size = 2)
  

####

AgroTrak.PestToxIndex %>%
  filter(Type == "Insecticide" & 
           Timing == "POST" &
           Crop == "Corn" &
           TaxonGroup == "Unknown" & !is.na(TaxonGroup)) %>%
  group_by(Crop, Year, TaxonGroup) %>%
  summarize(pestToxIndex.sum = sum(pestToxIndex, na.rm = TRUE)) %>%
  group_by(Crop, TaxonGroup) %>%
  mutate(taxonKeep = length(!is.na(pestToxIndex.sum)) > 4) %>%
  filter(taxonKeep == TRUE) %>%
  ggplot(aes(x = Year, y = pestToxIndex.sum)) +
  geom_point() + stat_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ TaxonGroup) +
  ggtitle("Corn POST Insecticide Targets") +
  ylab("Total Pest Toxicity Index")





plot_grid(
  ToxIndex.summary.yr %>% 
    filter(Crop == "Corn" & Timing == "POST") %>%
    ggplot(aes(y = EcoEff, x = Year)) +
    facet_wrap(~ Type, scales = "free_y",
               ncol = 1) +
    geom_point() + 
    stat_smooth(se = FALSE, span = 0.9) + 
    ylab("Honeybee Eco-Efficiency Index") +
    theme_gray(base_size = 18) +
    scale_y_continuous(limits = c(0, NA), expand = c(0, 0)) +
    ggtitle("Corn POST"),
ToxIndex.summary.yr %>% 
  filter(Crop == "Soybeans" & Timing == "POST") %>%
  ggplot(aes(y = EcoEff, x = Year)) +
  facet_wrap(~ Type, scales = "free_y",
             ncol = 1) +
  geom_point() + 
  stat_smooth(se = FALSE, span = 0.9) + 
  ylab("Honeybee Eco-Efficiency Index") +
  theme_gray(base_size = 18) +
  scale_y_continuous(limits = c(0, NA), expand = c(0, 0)) +
  ggtitle("Soybeans POST"),
nrow = 1)


plantedAcres.dat <- read_csv("DATA/USDA-PlantedAcresProduction_1990-2022.csv") %>%
  select(Year, State, Commodity, Data.Item = `Data Item`, Value) %>%
  mutate(Data.Item = 
           str_remove(Data.Item, 
                      c("CORN - |CORN, GRAIN - |SOYBEANS - "))) %>%
  pivot_wider(id_cols = c(Year, State, Commodity),
              values_from = Value,
              names_from = Data.Item) %>%
  rename(Acres = `ACRES PLANTED`,
         productionBu = `PRODUCTION, MEASURED IN BU`) %>%
  mutate(Crop = str_to_title(Commodity),
         PlantedHectares = Acres / 2.47,
         ProductionTonnes = case_when(
           Crop == "Corn" ~ round(productionBu * 56 * 0.45359 /1000),
           Crop == "Soybeans" ~ round(productionBu * 60 * 0.45359 /1000)),
         Yield.tha = round(ProductionTonnes / PlantedHectares, 2)) %>%
  select(Year, Crop, PlantedHectares, ProductionTonnes, Yield.tha)
glimpse(plantedAcres.dat)


insectToxIndex.b2006 <- AgroTrak.PestToxIndex %>%
  filter(Type == "Insecticide" & Year < 2006 & 
           Timing == "POST") %>%
  group_by(Crop, TaxonGroup) %>%
  summarize(pestToxIndex.sum = sum(pestToxIndex, na.rm = TRUE) / 
              length(unique(Year))) %>%
  ungroup() %>%
  mutate(era = "1998 to 2005")
insectToxIndex.a2015 <- AgroTrak.PestToxIndex %>%
  filter(Type == "Insecticide" & Year >= 2015 & 
           Timing == "POST") %>%
  group_by(Crop, TaxonGroup) %>%
  summarize(pestToxIndex.sum = sum(pestToxIndex, na.rm = TRUE) / 
              length(unique(Year))) %>%
  ungroup() %>%
  mutate(era = "2015 to 2020")

insectToxIndex.post <- bind_rows(insectToxIndex.b2006, insectToxIndex.a2015)

plotKeep.taxon <- insectToxIndex.post %>%
  filter(!is.na(TaxonGroup) &
           !TaxonGroup %in% c("Unknown", "All Insects")) %>%
  group_by(Crop, era) %>%
  slice_max(pestToxIndex.sum, n = 16) %>%
  ungroup() %>%
  distinct(TaxonGroup)

insectToxIndex.post %>%
  filter(Crop == "Soybeans" &
           TaxonGroup %in% plotKeep.taxon$TaxonGroup) %>%
  ggplot(aes(x = pestToxIndex.sum,
             y = reorder(TaxonGroup, pestToxIndex.sum))) +
  geom_line(alpha = .5, size = 1.1) +
  geom_point(aes(shape = era, color = era), size = 4, alpha = 0.8) +
  scale_shape_manual(values=c(20, 18)) +
  scale_color_manual(values = c("#ff6347", "#1dc460")) +
  theme(legend.position = "right",
        legend.title = element_blank()) +
  ggtitle("Soybean POST - Insecticide targets") +
  ylab(element_blank()) +
  xlab("Annual Pest Toxicity Index")

insectToxIndex.post %>%
  filter(Crop == "Corn" &
           TaxonGroup %in% plotKeep.taxon$TaxonGroup) %>%
  ggplot(aes(x = pestToxIndex.sum,
             y = reorder(TaxonGroup, pestToxIndex.sum))) +
  geom_line(alpha = .5, size = 1.1) +
  geom_point(aes(shape = era, color = era), size = 4, alpha = .8) +
  scale_shape_manual(values=c(20, 18)) +
  scale_color_manual(values = c("#ff6347", "#1dc460")) +
  theme(legend.position = "right",
        legend.title = element_blank()) +
  ggtitle("Corn POST - Insecticide targets") +
  ylab(element_blank()) +
  xlab("Annual Pest Toxicity Index")


weedToxIndex.post %>%
  filter(Crop == "Corn" &
           Genus %in% plotKeep.genus$Genus) %>%
  ggplot(aes(x = pestToxIndex.sum,
             y = reorder(Genus, pestToxIndex.sum))) +
  geom_line(alpha = .5, size = 1.1) +
  geom_point(aes(shape = era, color = era), size = 4) +
  scale_shape_manual(values=c(20, 18)) +
  scale_color_manual(values = c("#ff6347", "#1dc460")) +
  theme(legend.position = "right",
        legend.title = element_blank()) +
  ggtitle("Corn POST - Herbicide targets") +
  ylab(element_blank()) +
  xlab("Annual Pest Toxicity Index")




PestToxGroups <- AgroTrak.PestToxIndex %>%
  mutate(PestType = case_when(
    Type == "Herbicide" ~ "Weeds",
    Type == "Insecticide" ~ "Insects",
    Type == "Fungicide" ~ "Diseases"),
    TaxonGroup = ifelse(PestType == "Diseases", "Diseases", TaxonGroup)) %>%
  filter(!grepl("All |Annual |Perennial |Unknown", TaxonGroup) &
           !is.na(TaxonGroup) &
           Year > 2004 & Year < 2011) %>%
  group_by(Crop, PestType, TaxonGroup) %>%
  summarize(pestToxIndex.sum = sum(pestToxIndex, na.rm = TRUE)) %>%
  ungroup()

PestToxGroups %>%
  filter(Crop == "Corn") %>%
  slice_max(pestToxIndex.sum, n = 20) %>%
  ggplot(aes(x = pestToxIndex.sum,
           y = reorder(TaxonGroup, pestToxIndex.sum))) +
  geom_bar(stat = "identity",
           aes(fill = PestType)) +
  theme(legend.position = "right",
        legend.title = element_blank()) +
  ggtitle("Corn Pesticide Targets, 2015-2020") +
  ylab(element_blank()) +
  xlab("Summed Pest Toxicity Index")

PestToxGroups %>%
  filter(Crop == "Soybeans") %>%
  slice_max(pestToxIndex.sum, n = 20) %>%
  ggplot(aes(x = pestToxIndex.sum,
             y = reorder(TaxonGroup, pestToxIndex.sum))) +
  geom_bar(stat = "identity",
           aes(fill = PestType)) +
  theme(legend.position = "right",
        legend.title = element_blank()) +
  ggtitle("Soybean Pesticide Targets, 2015-2020") +
  ylab(element_blank()) +
  xlab("Summed Pest Toxicity Index")





AgroTrak.PestToxIndex %>%
  filter(Type == "Herbicide" & 
           Timing == "POST" &
           Crop == "Soybeans" &
           TaxonGroup %in% c(
             "Amaranthaceae", "Asteraceae", "Poaceae", "Malvaceae", 
             "Convolvulaceae", "Polygonaceae", "Solanaceae", "Brassicaceae", 
             "Rubiaceae", "Cucurbitaceae", "Fabaceae")) %>%
  group_by(Crop, Year, TaxonGroup) %>%
  summarize(pestToxIndex.sum = sum(pestToxIndex, na.rm = TRUE)) %>%
  group_by(Crop, TaxonGroup) %>%
  mutate(taxonKeep = length(!is.na(pestToxIndex.sum)) > 4) %>%
  filter(taxonKeep == TRUE) %>%
  ggplot(aes(x = Year, y = pestToxIndex.sum)) +
  geom_point() + geom_line(alpha = .3) +
  #stat_smooth(method = "loess", se = FALSE) +
  facet_wrap(~ TaxonGroup) +
  ggtitle("Soybean POST Herbicide Targets") +
  ylab("Total Pest Toxicity Index")

AgroTrak.PestToxIndex %>%
  filter(Type == "Herbicide" & 
           Timing == "POST" &
           Crop == "Corn" &
           TaxonGroup %in% c(
             "Amaranthaceae", "Asteraceae", "Poaceae", "Malvaceae", 
             "Convolvulaceae", "Polygonaceae", "Solanaceae", "Brassicaceae", 
             "Rubiaceae", "Cucurbitaceae", "Fabaceae")) %>%
  group_by(Crop, Year, TaxonGroup) %>%
  summarize(pestToxIndex.sum = sum(pestToxIndex, na.rm = TRUE)) %>%
  group_by(Crop, TaxonGroup) %>%
  mutate(taxonKeep = length(!is.na(pestToxIndex.sum)) > 4) %>%
  filter(taxonKeep == TRUE) %>%
  ggplot(aes(x = Year, y = pestToxIndex.sum)) +
  geom_point() + geom_line(alpha = .3) +
  stat_smooth(method = "loess", se = FALSE) +
  facet_wrap(~ TaxonGroup) +
  ggtitle("Corn POST Herbicide Targets") +
  ylab("Total Pest Toxicity Index")





WeedTotals <- AgroTrak.PestTiming.dat %>%
  filter(Timing == "POST" & Type == "Herbicide") %>%
  group_by(Crop, Pest, ai) %>%
  summarize(Volume.kg = sum(Volume.kg)) %>%
  pivot_wider(id_cols = c(Crop, Pest),
              names_from = ai,
              values_from = Volume.kg) %>%
  replace(is.na(.), 0) %>%
  ungroup()

WeedTotals.corn <- data.frame(WeedTotals %>% filter(Crop == "Corn"))
WeedTotals.soy <- data.frame(WeedTotals %>% filter(Crop == "Soybeans"))

rownames(WeedTotals.corn) <- WeedTotals.corn$Pest
rownames(WeedTotals.soy) <- WeedTotals.soy$Pest
glimpse(WeedTotals.corn)

pr0 <- prcomp(WeedTotals.corn %>% select(-Crop, -Pest))
biplot(pr0)

pr0 <- prcomp(WeedTotals.soy %>% select(-Crop, -Pest))
biplot(pr0)


ggplot(AgroTrak.ecotox, 
       aes(x = ToxIndex.vol, y = ToxIndex.rate)) +
  geom_point()
ggplot(AgroTrak.ecotox, 
       aes(x = ToxIndex.vol, y = ToxIndex.area)) +
  geom_point()
ggplot(AgroTrak.ecotox, 
       aes(x = ToxIndex.rate, y = ToxIndex.area)) +
  geom_point()

```{r include = FALSE}
weedToxIndex.post <- AgroTrak.PestToxIndex %>%
  filter(Type == "Herbicide" & Year < 2006 & 
           Timing == "POST") %>%
  group_by(Crop, TaxonGroup, Genus) %>%
  summarize(pestToxIndex.sum = sum(pestToxIndex, na.rm = TRUE)) %>%
  ungroup()
glimpse(weedToxIndex.post)
```

```{r, echo = FALSE, warning = FALSE, message = FALSE, fig.width = 8, fig.height = 6, fig.cap = "**Figure 8. Contribution of weeds to the honey bee adult acute contact toxicity index; corn herbicides applied *after* crop emergence, 1998-2020.**"}
weedToxIndex.post %>%
  filter(Crop == "Corn" &
           !is.na(Genus) &
           TaxonGroup != "All Dicots") %>%
  slice_max(pestToxIndex.sum, n = 30) %>%
  ggplot(aes(x = pestToxIndex.sum,
             y = reorder(Genus, pestToxIndex.sum))) +
  geom_bar(stat = "identity",
           aes(fill = TaxonGroup)) +
  scale_fill_brewer(palette = "Paired") +
  theme(legend.position = "right",
        legend.title = element_blank()) +
  ggtitle("Corn POST - Herbicide targets") +
  ylab(element_blank()) +
  xlab("Summed Pest Toxicity Index")
```

```{r, echo = FALSE, warning = FALSE, message = FALSE, fig.width = 8, fig.height = 6, fig.cap = "**Figure 9. Contribution of weeds to the honey bee adult acute contact toxicity index; soybean herbicides applied *after* crop emergence, 1998-2020.**"}
weedToxIndex.post %>%
  filter(Crop == "Soybeans" &
           !is.na(Genus) &
           TaxonGroup != "All Dicots") %>%
  slice_max(pestToxIndex.sum, n = 30) %>%
  ggplot(aes(x = pestToxIndex.sum,
             y = reorder(Genus, pestToxIndex.sum))) +
  geom_bar(stat = "identity",
           aes(fill = TaxonGroup)) +
  scale_fill_brewer(palette = "Paired") +
  theme(legend.position = "right",
        legend.title = element_blank()) +
  ggtitle("Soybean POST - Herbicide targets") +
  ylab(element_blank()) +
  xlab("Summed Pest Toxicity Index")
```


## Active ingredient contributions (2016-2020)

```{r, include = FALSE}
ToxIndex.post2015 <- AgroTrak.ecotox %>%
  filter(Year > 2015) %>%
  group_by(Crop, Timing, Type, ai) %>%
  summarize(ToxIndex.area = sum(ToxIndex.area, na.rm = TRUE)) %>%
  ungroup()
```


```{r, echo = FALSE, warning = FALSE, message = FALSE, fig.width = 8, fig.height = 6, fig.cap = "**Figure 6. Contribution of individual pesticides to the honey bee adult acute contact risk quotient; corn pesticides applied *after* crop emergence, 2016-2020.**"}
ToxIndex.post2015 %>%
  filter(Crop == "Corn" & Timing == "POST") %>%
  arrange(-ToxIndex.area) %>% 
  top_n(25) %>%
  ggplot(aes(x = ToxIndex.area, y = reorder(ai, ToxIndex.area))) +
  geom_point(aes(color = Type, shape = Type)) +
  xlab("Honey bee acute toxicity index") +
  ylab(element_blank()) +
  ggtitle("Corn POST - 2016-2020") +
  theme_gray(base_size = 18)
```

```{r, echo = FALSE, warning = FALSE, message = FALSE, fig.width = 8, fig.height = 6, fig.cap = "**Figure 7. Contribution of individual pesticides to the honey bee adult acute contact risk quotient; soybean pesticides applied *after* crop emergence, 2016-2020.**"}
ToxIndex.post2015 %>%
  filter(Crop == "Soybeans", Timing == "POST") %>%
  arrange(-ToxIndex.area) %>% 
  top_n(25) %>%
  ggplot(aes(x = ToxIndex.area, y = reorder(ai, ToxIndex.area))) +
  geom_point(aes(color = Type, shape = Type)) +
  xlab("Honey bee acute toxicity index") +
  ylab(element_blank()) +
  ggtitle("Soybean POST - 2016-2020") +
  theme_gray(base_size = 18)
```
```{r include = FALSE}
insectToxIndex.b2006 <- AgroTrak.PestToxIndex %>%
  filter(Type == "Insecticide" & Year < 2006 & 
           Timing == "POST") %>%
  group_by(Crop, TaxonGroup) %>%
  summarize(pestToxIndex.sum = sum(pestToxIndex, na.rm = TRUE) / 
              length(unique(Year))) %>%
  ungroup() %>%
  mutate(era = "1998 to 2005")
insectToxIndex.a2015 <- AgroTrak.PestToxIndex %>%
  filter(Type == "Insecticide" & Year > 2015 & 
           Timing == "POST") %>%
  group_by(Crop, TaxonGroup) %>%
  summarize(pestToxIndex.sum = sum(pestToxIndex, na.rm = TRUE) / 
              length(unique(Year))) %>%
  ungroup() %>%
  mutate(era = "2016 to 2020")

insectToxIndex.post <- bind_rows(insectToxIndex.b2006, insectToxIndex.a2015)

plotKeep.taxon <- insectToxIndex.post %>%
  filter(!is.na(TaxonGroup) &
           !TaxonGroup %in% c("Unknown", "All Insects")) %>%
  group_by(Crop, era) %>%
  slice_max(pestToxIndex.sum, n = 16) %>%
  ungroup() %>%
  distinct(TaxonGroup)
```

```{r, echo = FALSE, warning = FALSE, message = FALSE, fig.width = 8, fig.height = 3, fig.cap = "**Figure 10. Contribution of insect pest taxa to the honey bee adult acute contact toxicity index; soybean pesticides applied *after* crop emergence, 1998-2005 compared to 2016-2020.**"}
insectToxIndex.post %>%
  filter(Crop == "Corn" &
           TaxonGroup %in% plotKeep.taxon$TaxonGroup) %>%
  ggplot(aes(x = pestToxIndex.sum,
             y = reorder(TaxonGroup, pestToxIndex.sum))) +
  geom_line(alpha = .5, size = 1.1) +
  geom_point(aes(shape = era, color = era), size = 4, alpha = .8) +
  scale_shape_manual(values=c(20, 18)) +
  scale_color_manual(values = c("#ff6347", "#1dc460")) +
  theme(legend.position = "right",
        legend.title = element_blank()) +
  ggtitle("Corn POST - Insecticide targets") +
  ylab(element_blank()) +
  xlab("Annual Pest Toxicity Index")
```

```{r, echo = FALSE, warning = FALSE, message = FALSE, fig.width = 8, fig.height = 2.5, fig.cap = "**Figure 11. Contribution of insect pest taxa to the honey bee adult acute contact toxicity index; soybean pesticides applied *after* crop emergence, 1998-2005 compared to 2016-2020.**"}
insectToxIndex.post %>%
  filter(Crop == "Soybeans" &
           TaxonGroup %in% plotKeep.taxon$TaxonGroup) %>%
  ggplot(aes(x = pestToxIndex.sum,
             y = reorder(TaxonGroup, pestToxIndex.sum))) +
  geom_line(alpha = .5, size = 1.1) +
  geom_point(aes(shape = era, color = era), size = 4, alpha = 0.8) +
  scale_shape_manual(values=c(20, 18)) +
  scale_color_manual(values = c("#ff6347", "#1dc460")) +
  theme(legend.position = "right",
        legend.title = element_blank()) +
  ggtitle("Soybean POST - Insecticide targets") +
  ylab(element_blank()) +
  xlab("Annual Pest Toxicity Index")
```
