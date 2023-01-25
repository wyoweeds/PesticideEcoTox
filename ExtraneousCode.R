## Adult Oral LD50

```{r, warning = FALSE, message = FALSE, results = 'hide'}
AdultOralLD50 <- ecotox.dat %>%
  filter(LifeStage == "Adult" &
           ExposureType == "Food" & 
           Endpoint == "LD50") %>%
  group_by(ai) %>%
  mutate(checkAI = any(grepl("AI|ai|ae", ToxUnit)),
         hasAI = grepl("AI|ai|ae", ToxUnit)) %>%
  group_by(ai, hasAI) %>%
  mutate(min.d.grp = min(abs(2 - ObservedDuration.d) + 2, 
                         na.rm = TRUE)) %>%
  filter(case_when(checkAI == TRUE ~ 
                     hasAI == TRUE & ObservedDuration.d == min.d.grp,
                   checkAI == FALSE ~
                     ObservedDuration.d == min.d.grp)) %>%
  ungroup() %>%
  select(-checkAI, -hasAI, -min.d.grp) %>%
  group_by(ai) %>%
  slice(which.min(ObsRespMean))
glimpse(AdultOralLD50)
unique(AdultOralLD50$ConcType)
unique(AdultOralLD50$ToxUnit) 
### ^^ There are some 'ug/bee without AI in this one.
AgroTrak.AdultOralLD50.Rank <- left_join(AgroTrak.Ranked, AdultOralLD50)
#write.xlsx(AgroTrak.AdultOralLD50.Rank,
#          file = "Draft_AdultOralLD50.xlsx", asTable = TRUE)

AdultOralLD50.summary <- AgroTrak.AdultOralLD50.Rank %>%
  group_by(Type) %>%
  mutate(totalVol = sum(Volume.lbs),
         pctVol = Volume.lbs / totalVol * 100,
         toxVal = !is.na(ObsRespMean)) %>%
  #select(Type, ai, Volume.lbs, totalVol, pctVol, toxVal)
  group_by(Type, toxVal) %>%
  summarize(toxPct = sum(pctVol)) %>%
  pivot_wider(names_from = toxVal, 
              values_from = toxPct) %>%
  rename("missing Tox value" = `FALSE`,
         "Tox value present" = `TRUE`)
```
```{r, warning = FALSE, message = FALSE}
AdultOralLD50.summary
```

## Adult Oral NOEL

```{r, warning = FALSE, message = FALSE, results = 'hide'}
AdultOralNOEL <- ecotox.dat %>%
  filter(LifeStage == "Adult" &
           ExposureType == "Food" & 
           Endpoint == "NOEL") %>%
  group_by(ai) %>%
  mutate(checkAI = any(grepl("AI|ai|ae", ToxUnit)),
         hasAI = grepl("AI|ai|ae", ToxUnit)) %>%
  group_by(ai, hasAI) %>%
  mutate(min.d.grp = min(abs(10 - ObservedDuration.d) + 10, 
                         na.rm = TRUE)) %>%
  filter(case_when(checkAI == TRUE ~ 
                     hasAI == TRUE & ObservedDuration.d == min.d.grp,
                   checkAI == FALSE ~
                     ObservedDuration.d == min.d.grp)) %>%
  ungroup() %>%
  select(-checkAI, -hasAI, -min.d.grp) %>%
  group_by(ai) %>%
  slice(which.min(ObsRespMean))
glimpse(AdultOralNOEL)
unique(AdultOralNOEL$ConcType)
unique(AdultOralNOEL$ToxUnit)
AgroTrak.AdultOralNOEL.Rank <- left_join(AgroTrak.Ranked, AdultOralNOEL)
#write.xlsx(AgroTrak.AdultOralNOEL.Rank,
#          file = "Draft_AdultOralNOEL.xlsx", asTable = TRUE)

AdultOralNOEL.summary <- AgroTrak.AdultOralNOEL.Rank %>%
  group_by(Type) %>%
  mutate(totalVol = sum(Volume.lbs),
         pctVol = Volume.lbs / totalVol * 100,
         toxVal = !is.na(ObsRespMean)) %>%
  #select(Type, ai, Volume.lbs, totalVol, pctVol, toxVal)
  group_by(Type, toxVal) %>%
  summarize(toxPct = sum(pctVol)) %>%
  pivot_wider(names_from = toxVal, 
              values_from = toxPct) %>%
  rename("missing Tox value" = `FALSE`,
         "Tox value present" = `TRUE`)
```
```{r, warning = FALSE, message = FALSE}
AdultOralNOEL.summary
```

## Larval Oral LD50

```{r, warning = FALSE, message = FALSE, results = 'hide'}
LarvaOralLD50 <- ecotox.dat %>%
  filter(LifeStage == "Larva" &
           ExposureType == "Food" & 
           Endpoint == "LD50") %>%
  group_by(ai) %>%
  mutate(checkAI = any(grepl("AI|ai|ae", ToxUnit)),
         hasAI = grepl("AI|ai|ae", ToxUnit)) %>%
  group_by(ai, hasAI) %>%
  mutate(min.d.grp = min(abs(3 - ObservedDuration.d) + 3, 
                         na.rm = TRUE)) %>%
  filter(case_when(checkAI == TRUE ~ 
                     hasAI == TRUE & ObservedDuration.d == min.d.grp,
                   checkAI == FALSE ~
                     ObservedDuration.d == min.d.grp)) %>%
  ungroup() %>%
  select(-checkAI, -hasAI, -min.d.grp) %>%
  group_by(ai) %>%
  slice(which.min(ObsRespMean))
glimpse(LarvaOralLD50)
unique(LarvaOralLD50$ConcType)
unique(LarvaOralLD50$ToxUnit)
AgroTrak.LarvaOralLD50.Rank <- left_join(AgroTrak.Ranked, LarvaOralLD50)
#write.xlsx(AgroTrak.LarvaOralLD50.Rank,
#          file = "Draft_LarvaOralLD50.xlsx", asTable = TRUE)

LarvaOralLD50.summary <- AgroTrak.LarvaOralLD50.Rank %>%
  group_by(Type) %>%
  mutate(totalVol = sum(Volume.lbs),
         pctVol = Volume.lbs / totalVol * 100,
         toxVal = !is.na(ObsRespMean)) %>%
  #select(Type, ai, Volume.lbs, totalVol, pctVol, toxVal)
  group_by(Type, toxVal) %>%
  summarize(toxPct = sum(pctVol)) %>%
  pivot_wider(names_from = toxVal, 
              values_from = toxPct) %>%
  rename("missing Tox value" = `FALSE`,
         "Tox value present" = `TRUE`)
```
```{r, warning = FALSE, message = FALSE}
LarvaOralLD50.summary
```

## Larval Oral NOEL

```{r, warning = FALSE, message = FALSE, results = 'hide'}
LarvaOralNOEL <- ecotox.dat %>%
  filter(LifeStage == "Larva" &
           ExposureType == "Food" & 
           Endpoint == "NOEL") %>%
  group_by(ai) %>%
  mutate(checkAI = any(grepl("AI|ai|ae", ToxUnit)),
         hasAI = grepl("AI|ai|ae", ToxUnit)) %>%
  group_by(ai, hasAI) %>%
  mutate(min.d.grp = min(abs(22 - ObservedDuration.d) + 22, 
                         na.rm = TRUE)) %>%
  filter(case_when(checkAI == TRUE ~ 
                     hasAI == TRUE & ObservedDuration.d == min.d.grp,
                   checkAI == FALSE ~
                     ObservedDuration.d == min.d.grp)) %>%
  ungroup() %>%
  select(-checkAI, -hasAI, -min.d.grp) %>%
  group_by(ai) %>%
  slice(which.min(ObsRespMean))
glimpse(LarvaOralNOEL)
unique(LarvaOralNOEL$ConcType)
unique(LarvaOralNOEL$ToxUnit)
AgroTrak.LarvaOralNOEL.Rank <- left_join(AgroTrak.Ranked, LarvaOralNOEL)
#write.xlsx(AgroTrak.LarvaOralNOEL.Rank,
#          file = "Draft_LarvaOralNOEL.xlsx", asTable = TRUE)

LarvaOralNOEL.summary <- AgroTrak.LarvaOralNOEL.Rank %>%
  group_by(Type) %>%
  mutate(totalVol = sum(Volume.lbs),
         pctVol = Volume.lbs / totalVol * 100,
         toxVal = !is.na(ObsRespMean)) %>%
  #select(Type, ai, Volume.lbs, totalVol, pctVol, toxVal)
  group_by(Type, toxVal) %>%
  summarize(toxPct = sum(pctVol)) %>%
  pivot_wider(names_from = toxVal, 
              values_from = toxPct) %>%
  rename("missing Tox value" = `FALSE`,
         "Tox value present" = `TRUE`)
```
```{r, warning = FALSE, message = FALSE}
LarvaOralNOEL.summary
```


## Insecticides used since 2005:

```{r, echo = FALSE, fig.width = 7, fig.height = 3, warning = FALSE}
jkl <- ggplot(CornSoyAdultContact.post2005 %>% 
                filter(Type == "Insecticide"),
              aes(x = VolumeContrib, y = RQContrib, text = ai)) +
  facet_grid(Type ~ Crop) +
  geom_abline(intercept = 0, slope = 1, color = "blue") +
  geom_point(aes(size = RQContrib, 
                 color = abs(Diff.11) < 5), 
             alpha = 0.3) +
  ylab("") +
  xlab("") +
  cowplot::theme_minimal_grid() +
  theme(legend.position = "none")

ggplotly(jkl, tooltip = "text") %>%
  layout(xaxis = list(title = "Volume Contribution (%)"),
         yaxis = list(title = "RQ Contribution (%)"))
```

---
  
  ### Corn Insecticides:
  
  ```{r, echo = FALSE}
knitr::kable(
  CornSoyAdultContact.post2005 %>%
    filter(Crop == "Corn", Type == "Insecticide"))
```

### Soybean Insecticides:

```{r, echo = FALSE}
knitr::kable(
  CornSoyAdultContact.post2005 %>%
    filter(Crop == "Soybeans", Type == "Insecticide"))
```

AgroTrak.SoyPest.dat %>%
  mutate(Active.Ingredient = factor(Active.Ingredient,
                                    levels = soy.aiOrder$Active.Ingredient)) %>%
  group_by(Active.Ingredient, Pest) %>%
  summarize(Base.Area.Treated = mean(Base.Area.Treated, na.rm = TRUE)/1000,
            BeeREX.RQ = mean(BeeREX.RQ, na.rm = TRUE)) %>%
  group_by(Active.Ingredient) %>%
  filter(Pest != "No Answer Insect") %>%
  slice_max(Base.Area.Treated, n = 10) %>%
  ggplot(aes(x = Base.Area.Treated, 
             y = reorder_within(Pest, Base.Area.Treated, Active.Ingredient))) +
  scale_y_reordered() +
  #scale_x_continuous(limits = c(0, NA)) +
  facet_wrap(~ Active.Ingredient, scales = "free", ncol = 2) +
  geom_point() +
  xlab("Annual base acres treated (thousands)") +
  ylab(element_blank())
ggsave("SoybeanPestTargets.png", width = 12, height = 12, units = "in", dpi = 300)
