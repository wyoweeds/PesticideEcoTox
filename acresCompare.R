library("tidyverse")

agrotrak <- read.csv("AgroTrakCropAcres.csv") %>%
    rename(AgroTrak.grown = Crop.Area.Grown.Acres)
glimpse(agrotrak)

usda <- read.csv("USDACropAcres.csv") %>%
    mutate(Crop = str_to_title(Commodity)) %>%
    select(-Commodity)

compare.dat <- left_join(usda, agrotrak) %>%
  mutate(pct.difference = 100* ((AgroTrak.grown - USDA.planted) / AgroTrak.grown)) %>%
    pivot_longer(cols = c(-Year, -Crop),
                 names_to = "data.source",
                 values_to = "acres")
compare.dat

top <- ggplot(compare.dat %>% 
         filter(!data.source %in% c("USDA.harvested", "pct.difference")), 
       aes(x = Year, y = acres)) +
    geom_point(aes(color = data.source, shape = data.source)) +
    ylim(c(0, NA)) +
  xlab(element_blank()) +
  theme(legend.position = "top",
        legend.title = element_blank(),
        legend.justification = "left") +
  facet_wrap(~Crop)

bottom <- ggplot(compare.dat %>% 
         filter(data.source == "pct.difference"), 
       aes(x = Year, y = acres)) +
  geom_point() +
  xlab(element_blank()) +
  ylab("Difference (%)") +
  geom_hline(yintercept = 0, color = "blue") +
  facet_wrap(~Crop)

cowplot::plot_grid(top, bottom, 
                   ncol = 1, align = "v",
                   rel_heights = c(1.2, 1))
ggsave("AcresCompare.png", width = 6, height = 5, units = "in", dpi = 600)
