library("tidyverse")
library("readxl")

aiRanks <- FarmTrak.ecotox %>%
  group_by(Crop, Type, ai) %>%
  summarize(TotalVolume.kg = sum(Volume.kg)) %>%
  arrange(Crop, Type, desc(TotalVolume.kg)) %>%
  mutate(rank = dense_rank(desc(TotalVolume.kg)))
aiRanks.big2 %>% filter(ai %in% 
                     c("ETHALFLURALIN", "TEBUPIRIMPHOS"))

## Clayton Grub question
cornPrePestTox.summary <- AgroTrak.PestToxIndex %>%
  filter(Type == "Insecticide" & 
           Timing == "PRE" &
           Crop == "Corn" &!is.na(TaxonGroup)) %>%
  group_by(Crop, Year, TaxonGroup) %>%
  summarize(pestToxIndex.sum = sum(pestToxIndex, na.rm = TRUE)) %>%
  group_by(Crop, TaxonGroup) %>%
  mutate(taxonKeep = length(!is.na(pestToxIndex.sum)) > 4) %>%
  filter(taxonKeep == TRUE)

cornPre.taxa.lm <- lm(pestToxIndex.sum ~ TaxonGroup/Year + 0,
                       data = cornPrePestTox.summary)
cornPre.lm.summary <- broom::tidy(summary(cornPre.taxa.lm)) %>%
  filter(grepl(":Year", term)) %>%
  mutate(TaxonGroup = str_remove_all(term, "TaxonGroup|:Year"),
         pval.display = case_when(
           round(p.value, 3) == 0 ~ "< 0.001",
           round(p.value, 3) >  0 ~ paste0("= ",round(p.value, 3))),
         Crop = "Corn") %>%
  arrange(estimate) %>%
  select(Crop, TaxonGroup, estimate, std.error, pval.display)
ggplot(cornPrePestTox.summary, aes(x = Year, y = pestToxIndex.sum)) +
  geom_point() + stat_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ TaxonGroup, ncol = 3) +
  ggtitle("Corn PRE Insecticide Targets") +
  ylab("Total Pest Toxicity Index") +
  theme_minimal_grid() + 
  theme(panel.background = element_rect(fill = "#F5F5F5"),
        axis.text.x = element_text(size = 9)) +
  geom_text(data = cornPre.lm.summary,
            aes(x = Inf, y = 0.5, label = paste("P", pval.display)),
            hjust = 1, vjust = 1, color = "blue")



This is some data I pulled from USDA - it is not specific to a crop, but estimates chemical expenses and all expenses for crop production operations. I'm unsure whether non-pesticides are included in "chemicals" here (fertilizer seems to not be included). I'm also unsure whether it is useful in any way... Panel B, chemicals as a proportion of total expenses seems to show the same (inverse) pattern that one might expect given the efficiency pattern in corn; but I'm not convinced those things are related very closely. Might be best to ignore this. 

```{r, echo = FALSE, warning = FALSE, message = FALSE, fig.width = 7, fig.height = 2.5, fig.cap = "**Figure X. Chemical expenses in billion USD (A) and as a proportion of total crop production operation expenses (B).**"}

usda.exp <- read_csv("DATA/USDA-Expenses_1998-2020.csv") %>%
  select(Program, Year, Period, Geo.Level = `Geo Level`, State,
         Commodity, Data.Item = `Data Item`, 
         Domain, Category = `Domain Category`,
         Value) %>%
  filter(Category == "TYPE OF OPERATION: (CROP)")

usda.cropExp <- usda.exp %>%
  select(Year, Data.Item, Value) %>%
  pivot_wider(id_cols = Year,
              names_from = Data.Item,
              values_from = Value) %>%
  rename(ChemicalTotal = `CHEMICAL TOTALS - EXPENSE, MEASURED IN $`,
         ExpenseTotals = `EXPENSE TOTALS, PRODUCTION - EXPENSE, MEASURED IN $`,
         FertilizerTotal = 
           `FERTILIZER TOTALS, INCL LIME & SOIL CONDITIONERS - EXPENSE, MEASURED IN $`) %>%
  mutate(chemPct = ChemicalTotal / ExpenseTotals,
         fertPct = FertilizerTotal / ExpenseTotals)
  #right_join(plantedAcres.dat) %>%
  #mutate(costEff = ProductionBUSD / (ChemicalTotal/(1*10^9)))
#glimpse(usda.cropExp)

plot_grid(
ggplot(usda.cropExp, aes(y = ChemicalTotal / (10^9), x = Year)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  ylab("Total chemical expenses \n(Billion USD)") +
  xlab(element_blank()) +
  ylim(c(0, NA)),
ggplot(usda.cropExp, aes(y = chemPct, x = Year)) +
  geom_point() +
  geom_smooth(se = FALSE) +
  ylab("Chemical proportion of \ntotal expenses") +
  xlab(element_blank()) +
  ylim(c(0, NA)),
labels = LETTERS)

```

ggplot(AgroTrak.ecotox %>% filter(ai == "TEBUPIRIMPHOS"),
       aes(x = Year, y = Volume.lbs)) +
  #geom_point(aes(color = Crop, shape = Crop)) +
  facet_wrap(~Timing) +
  geom_smooth(se = FALSE)


ToxIndex.summary.early <- AgroTrak.ecotox %>%
  filter(Crop == "Corn" & Year > 1999 & Year < 2006 |
           Crop == "Soybeans" & Year > 2004 & Year < 2011) %>%
  group_by(Crop, Timing, Type) %>%
  summarize(ToxIndex.area = sum(ToxIndex.area, na.rm = TRUE)/6) %>%
  ungroup() %>% group_by(Crop) %>% 
  mutate(cropToxIndex.area = sum(ToxIndex.area),
         Period = "Early")
ToxIndex.summary.late <- AgroTrak.ecotox %>%
  filter(Year > 2014) %>%
  group_by(Crop, Timing, Type) %>%
  summarize(ToxIndex.area = sum(ToxIndex.area, na.rm = TRUE)/6) %>%
  ungroup() %>% group_by(Crop) %>% 
  mutate(cropToxIndex.area = sum(ToxIndex.area),
         Period = "Late")
ToxIndex.evl <- bind_rows(ToxIndex.summary.early, ToxIndex.summary.late)
evl.SummaryTab <- ToxIndex.evl %>% 
  filter(Type == "Insecticide") %>%
  mutate(ToxIndex.area = signif(ToxIndex.area, 2)) %>%
  select(Crop, Period, Timing, Type, ToxIndex.area) %>%
  arrange(Crop, Type, Timing, Period) %>%
  pivot_wider(id_cols = c(Crop, Timing, Type),
              names_from = Period,
              values_from = ToxIndex.area) %>%
  ungroup() 

knitr::kable(evl.SummaryTab) %>%
  kableExtra::add_footnote("Footnote 1", notation="alphabet")


sampleSizes <- read_excel("DATA/AgroTrak_CornSoy-SampleSizes.xlsx", 
                          skip = 4) %>%
  select(Crop, 
         Type = `Pesticide Type`, 
         Active.Ingredient = `Active Ingredient`, 
         Year, 
         N = `Sample Farms\r\n(Number)`) %>%
  group_by(Crop, Active.Ingredient) %>%
  summarize(maxYearN = max(N),
            totalN = sum(N))
sampleSizes %>% filter(maxYearN <= 3 & totalN > 5)

ggplot(missDat, aes(x = totalVol/1000000, y = reorder(ai, totalVol))) +
  geom_bar(stat = "identity", aes(fill = Type)) +
  ylab(element_blank()) +
  xlab("Volume applied (million kg) 1998 - 2020") +
  theme_minimal_grid() +
  theme(legend.position = "top", 
        legend.justification = "left",
        legend.title = element_blank())

length(goodDat$ai)
length(missDat$ai)

AgroTrak.ecotox %>%
  ungroup() %>%
  distinct(Active.Ingredient)
AgroTrak.ecotox %>%
  ungroup() %>%
  distinct(ai)

goodDat <- AgroTrak.ecotox %>%
  group_by(Type, ai) %>%
  summarize(AAC.LD50 = unique(AAC.LD50)) %>%
  filter(!is.na(AAC.LD50)) %>%
  select(ai, Type)
goodDat

notgoodDat <- AgroTrak.ecotox %>%
  group_by(Type, ai) %>%
  summarize(AAC.LD50 = unique(AAC.LD50)) %>%
  filter(is.na(AAC.LD50)) %>%
  select(ai, Type)
notgoodDat

glimpse(AgroTrak.ecotox)# %>%
  group_by(Type, ai) %>%
  summarize(AAC.LD50 = unique(AAC.LD50)) %>%
  filter(!is.na(AAC.LD50)) %>%
  select(ai, Type)


AgroTrak.ecotox %>%
  filter(Type == "Insecticide" &
           Timing == "PRE" &
           Crop == "Corn") %>%
  mutate(aiplot = case_when(
    ai %in% cornPre.90list$ai ~ ai,
    TRUE ~ "ALL OTHER INSECTICIDES"),
    aiplot = factor(aiplot, levels = c(cornPre.90list$ai,
                                       "ALL OTHER INSECTICIDES"))) %>%
  group_by(Year, aiplot) %>%
  summarize(ToxIndex.area = sum(ToxIndex.area, na.rm = TRUE)) %>%
  ungroup() %>%
  complete(aiplot, Year, fill = list(ToxIndex.area = 0)) %>%
  ggplot(aes(x = Year, y = ToxIndex.area)) +
  #geom_area(aes(fill = aiplot)) +
  geom_line(aes(color = aiplot)) +
  geom_point(aes(color = aiplot, shape = aiplot)) +
  scale_fill_manual(values = cbPalette.cornPre) +
  ggtitle("Corn Insecticides Applied PRE") +
  xlim(c(1998, 2020)) +
  scale_y_continuous(expand = c(0,0)) +
  ylab(aiplot.ylab) + xlab(element_blank()) +
  theme_minimal_grid() +
  theme(legend.title = element_blank()) #+
  #annotate("rect", xmin = 2007, xmax = Inf, 
  #         ymin = 0.8, ymax = Inf, fill = "white")


cornPre.inset <- cornPre.aigg +
  coord_cartesian(ylim = c(0, 0.35)) +
  ylab(element_blank()) +
  scale_x_continuous(limits = c(2012, 2020),
                     breaks = seq(2012, 2020, 4)) +
  scale_y_continuous(expand = c(0,0)) +
  #theme_minimal_grid() +
  theme(legend.position = "none",
        panel.background = element_rect(fill = "#F0F0F0")) +
  ggtitle(element_blank())
cornPre.2panel <- ggdraw(cornPre.aigg) + 
  draw_plot(cornPre.inset, .36, .25, .29, .66)


AgroTrak.ecotox %>%
  filter(Type == "Insecticide" &
           Timing == "POST" &
           Crop == "Corn") %>%
  mutate(aiplot = case_when(
    ai %in% cornPost.90list$ai ~ ai,
    TRUE ~ "ALL OTHER INSECTICIDES"),
    aiplot = factor(aiplot, levels = c(cornPost.90list$ai,
                                       "ALL OTHER INSECTICIDES"))) %>%
  group_by(Year, aiplot) %>%
  summarize(ToxIndex.area = sum(ToxIndex.area, na.rm = TRUE)) %>%
  ungroup() %>%
  complete(aiplot, Year, fill = list(ToxIndex.area = 0)) %>%
  ggplot(aes(x = Year, y = ToxIndex.area)) +
  #geom_area(aes(fill = aiplot)) +
  geom_line(aes(color = aiplot)) +
  geom_point(aes(color = aiplot, shape = aiplot)) +
  scale_fill_manual(values = cbPalette.corn) +
  ggtitle("Corn Insecticides Applied POST") +
  xlim(c(1998, 2020)) +
  scale_y_continuous(expand = c(0,0)) +
  ylab(aiplot.ylab) + xlab(element_blank()) +
  theme_minimal_grid() +
  theme(legend.title = element_blank())

soy.aigg <- AgroTrak.ecotox %>%
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
  scale_fill_manual(values = cbPalette.soy) +
  ggtitle("Soybean Insecticides Applied POST") +
  xlim(c(1998, 2020)) +
  scale_y_continuous(expand = c(0,0)) +
  ylab(aiplot.ylab) + xlab(element_blank()) +
  theme_minimal_grid() +
  theme(legend.title = element_blank())

ggdraw(plot_grid(cornPre.aigg + 
                   annotate("text", x = 2008, y = 3, label = "B", 
                            size = 5, fontface = "bold"), 
                 corn.aigg, soy.aigg,
                 ncol = 1, align = "v",
                 labels = LETTERS[c(1,3,4)])) + 
  draw_plot(cornPre.inset, .36, .75, .29, .2)




yield.gg <- ggplot(plantedAcres.dat, aes(x = Year, y = Yield.tha)) +
  geom_point() +
  facet_wrap(~ Crop, scales = "free_y", nrow = 2) +
  geom_smooth(method = "lm", se = FALSE) +
  xlim(c(1998, 2020)) +
  ylim(c(0, NA)) +
  ylab("Yield (tonnes/hectare)")

b <- plot_grid(Corn.EcoY.gg, Corn.EcoV.gg, 
          Soy.EcoY.gg,  Soy.EcoV.gg,
          nrow = 2, ncol = 2, align = "hv", labels = LETTERS)
plot_grid(yield.gg, b, nrow = 1, rel_widths = c(.3, .7))

bifenthrin.Pests <- AgroTrak.PestToxIndex %>%
  filter(ai == "BIFENTHRIN" &
           Timing == "POST" &
           Year > 2014) %>%
  group_by(Crop, Timing, TaxonGroup, Pest) %>%
  summarize(pestToxIndex = sum(pestToxIndex) / length(unique(Year))) %>%
  arrange(Crop, -pestToxIndex)
bifenthrin.Pests

glimpse(AgroTrak.PestToxIndex)

AgroTrak.PestToxIndex %>%
  filter(TaxonGroup %in% c("Coleoptera", "Hemiptera", "Lepidoptera", 
                           "Unknown")) %>%
  group_by(Crop, TaxonGroup, Pest) %>%
  summarize(pestToxIndex = sum(pestToxIndex, na.rm = TRUE)) %>%
  ungroup() %>% group_by(Crop, TaxonGroup) %>%
  mutate(TaxonGroup.ToxTotal = sum(pestToxIndex, na.rm = TRUE),
         PestProp = round(100 * (pestToxIndex / TaxonGroup.ToxTotal)),
         Pest = str_to_lower(Pest)) %>%
  select(-TaxonGroup.ToxTotal) %>%
  arrange(Crop, TaxonGroup, -pestToxIndex) %>%
  filter(PestProp >= 5 ) %>%
  data.frame()



usda.exp <- read_csv("DATA/USDA-Expenses_1998-2020.csv") %>%
  select(Program, Year, Period, Geo.Level = `Geo Level`, State,
         Commodity, Data.Item = `Data Item`, 
         Domain, Category = `Domain Category`,
         Value) %>%
  filter(Category == "TYPE OF OPERATION: (CROP)")
usda.cropExp <- usda.exp %>%
  select(Year, Data.Item, Value) %>%
  pivot_wider(id_cols = Year,
              names_from = Data.Item,
              values_from = Value) %>%
  rename(ChemicalTotal = `CHEMICAL TOTALS - EXPENSE, MEASURED IN $`,
         ExpenseTotals = `EXPENSE TOTALS, PRODUCTION - EXPENSE, MEASURED IN $`,
         FertilizerTotal = 
           `FERTILIZER TOTALS, INCL LIME & SOIL CONDITIONERS - EXPENSE, MEASURED IN $`) %>%
  mutate(chemPct = ChemicalTotal / ExpenseTotals,
         fertPct = FertilizerTotal / ExpenseTotals)
glimpse(usda.cropExp)

ggplot(usda.cropExp, aes(y = chemPct, x = Year)) +
  geom_point() +
  geom_smooth(se = FALSE)


ToxIndex.summary.yr %>% 
  filter(Timing == "POST" & Type == "Insecticide") %>%
  ggplot(aes(y = EcoEff.Y, x = Year)) +
  facet_wrap(~ Crop, scales = "free_y",
             nrow = 1) +
  geom_point() + 
  stat_smooth(se = FALSE, span = 0.9) + 
  ylab("Yield Eco-Efficiency") +
  xlab(element_blank()) +
  theme_gray(base_size = 18) +
  scale_y_continuous(limits = c(1, NA)) + #, expand = c(0, 0)) +
  scale_x_continuous(limits = c(1998, 2020))
ToxIndex.summary.yr %>% 
  filter(Timing == "POST" & Type == "Insecticide") %>%
  ggplot(aes(y = EcoEff.V, x = Year)) +
  facet_wrap(~ Crop, scales = "free_y",
             nrow = 1) +
  geom_point() + 
  stat_smooth(se = FALSE, span = 0.9) + 
  ylab("Value Eco-Efficiency") +
  xlab(element_blank()) +
  theme_gray(base_size = 18) +
  scale_y_continuous(limits = c(1, NA)) + #, expand = c(0, 0)) +
  scale_x_continuous(limits = c(1998, 2020))





glimpse(AgroTrak.PestTiming.dat)

ggplot(data = AgroTrak.PestToxIndex %>%
         filter(TaxonGroup == "Unknown" &
                  Type == "Insecticide" &
                  ai %in% c("BIFENTHRIN", "CHLORPYRIFOS", 
                            "CYHALOTHRIN-LAMBDA", "DELTAMETHRIN", 
                            "THIAMETHOXAM", "CYFLUTHRIN", "ACEPHATE",
                            "ALL OTHER INSECTICIDES")),
       aes(y = pestToxIndex, x = ai.pestVol)) +
  geom_point() + facet_wrap(~ai)

paste0("**Figure 4. Contribution of insecticide active ingredients to honey bee adult acute contact toxicity index applied to corn or soybean *after* crop emergence. 'All Other Insecticides' category includes ", 100-cornPre.top7prop, "% of the total corn toxicity index applied PRE, ", 100-corn.top7prop, "% of the total corn toxicity index applied POST, and ", 100-soy.top7prop, "% of the total soybean toxicity index. **")


```{r, echo = FALSE, warning = FALSE, message = FALSE}
ToxIndex.plot <- function(
    data = ToxIndex.summary.yr, 
    crop, timing, 
    type = "Insecticide") {
  data %>% filter(Crop == crop & Timing == timing & Type == type) %>%
    ggplot(aes(y = ToxIndex.area, x = Year)) +
    facet_wrap(~ Type, scales = "free_y",
               ncol = 1) +
    geom_point() + 
    stat_smooth(se = FALSE, span = 0.9) +
    ylab("Honeybee acute \ntoxicity index") +
    #theme_minimal_hgrid() +
    theme_gray(base_size = 18) +
    scale_y_continuous(limits = c(0, NA), expand = c(0, 0)) +
    ggtitle(paste(crop, timing))
}
```
plot_grid(ToxIndex.plot(crop = "Corn", 
                        timing = "PRE", 
                        type = "Insecticide"),
          ToxIndex.plot(crop = "Soybeans", 
                        timing = "PRE", 
                        type = "Insecticide"),
          nrow = 1)
```{r, echo = FALSE, warning = FALSE, message = FALSE, fig.width = 8, fig.height = 4, fig.cap = "**Figure 3. Honey bee adult acute contact toxicity index for insecticides applied to corn or soybean *after* crop emergence.**"}
plot_grid(ToxIndex.plot(crop = "Corn", 
                        timing = "POST", 
                        type = "Insecticide"),
          ToxIndex.plot(crop = "Soybeans", 
                        timing = "POST", 
                        type = "Insecticide"),
          nrow = 1)
```



ToxIndex.summary.early <- AgroTrak.ecotox %>%
  filter(Year > 1999 & Year < 2006) %>%
  group_by(Crop, Timing, Type) %>%
  summarize(ToxIndex.area = sum(ToxIndex.area, na.rm = TRUE)) %>%
  ungroup() %>% group_by(Crop) %>% 
  mutate(cropToxIndex.area = sum(ToxIndex.area),
         cropToxIndex.areaPct = signif(ToxIndex.area / 
                                         cropToxIndex.area*100, 2),
         Period = "2000 - 2005")
ToxIndex.summary.late <- AgroTrak.ecotox %>%
  filter(Year > 2014) %>%
  group_by(Crop, Timing, Type) %>%
  summarize(ToxIndex.area = sum(ToxIndex.area, na.rm = TRUE)) %>%
  ungroup() %>% group_by(Crop) %>% 
  mutate(cropToxIndex.area = sum(ToxIndex.area),
         cropToxIndex.areaPct = signif(ToxIndex.area / 
                                         cropToxIndex.area*100, 2),
         Period = "2015 - 2020")
ToxIndex.evl <- bind_rows(ToxIndex.summary.early, ToxIndex.summary.late)
knitr::kable(ToxIndex.evl %>% 
               filter(Type != "Fungicide") %>%
               mutate(ToxIndex.area = signif(ToxIndex.area, 2)) %>%
               select(Crop, Period, Timing, Type, ToxIndex.area) %>%
               arrange(Crop, Type, Timing, Period) %>%
               pivot_wider(id_cols = c(Crop, Timing, Type),
                           names_from = Period,
                           values_from = ToxIndex.area))

corn.evl.gg <- ggplot(ToxIndex.evl %>% filter(Crop == "Corn"), 
       aes(x = ToxIndex.area, y = Type)) +
  facet_grid(Timing ~ Period) +
  geom_bar(stat = "identity",
           aes(fill = Type)) +
  ylab(element_blank()) +
  xlab("Area-adjusted honey bee acute toxicity index") +
  theme_minimal_grid() +
  theme(legend.position = "none",
        plot.title.position = "plot") +
  ggtitle("Corn")
soy.evl.gg <- ggplot(ToxIndex.evl %>% filter(Crop == "Soybeans"), 
       aes(x = ToxIndex.area, y = Type)) +
  facet_grid(Timing ~ Period) +
  geom_bar(stat = "identity",
           aes(fill = Type)) +
  ylab(element_blank()) +
  xlab("Area-adjusted honey bee acute toxicity index") +
  theme_minimal_grid() +
  theme(legend.position = "none",
        plot.title.position = "plot") +
  ggtitle("Soybean")



egg::ggarrange(corn.evl.gg + xlab(element_blank()), 
               soy.evl.gg,
               ncol = 1,
               labels = c("Corn", "Soybean"))

tisumEarly.area.gg <- ggplot(ToxIndex.summary.early, 
                        aes(x = ToxIndex.area, y = Type)) +
  facet_grid(Crop ~ Timing) + 
  geom_bar(stat = "identity",
           aes(fill = Crop)) +
  ylab(element_blank()) +
  xlab("Area-adjusted honey bee acute toxicity index") +
  theme_minimal_grid() +
  theme(legend.position = "none",
        plot.title.position = "plot") +
  ggtitle("2000 - 2005")


tisumLate.area.gg <- ggplot(ToxIndex.summary.late, 
                             aes(x = ToxIndex.area, y = Type)) +
  facet_grid(Crop ~ Timing) + 
  geom_bar(stat = "identity",
           aes(fill = Crop)) +
  ylab(element_blank()) +
  xlab("Area-adjusted honey bee acute toxicity index") +
  theme_minimal_grid() +
  theme(legend.position = "none",
        plot.title.position = "plot") +
  ggtitle("2015 - 2020")

plot_grid(tisumEarly.area.gg, tisumLate.area.gg,
          nrow = 2, align = "v")

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

```{r, include = FALSE}
### These bullet points from below moved here to avoid printing. 
### Can move them back if desired.
#* The summed honey bee $TI_{rate}$ over all years was `r round(ToxIndex.cropTotals$cropToxIndex.rate[1]/ToxIndex.cropTotals$cropToxIndex.rate[2], 1)`-times greater in corn compared to soybean.

```


Application rate-based toxicity index $TI_{rate}$ was calculated by dividing $P_{vol}$ by the area treated with the pesticide in hectares ($A_{trt}$) to get the average application rate, then dividing that application rate by the adult acute contact $LD_{50}$.

$$TI_{rate} = \sum_{i=1}^{N_{CYT}} (\frac{P_{vol} / A_{trt}}{LD_{50}})$$
