# Ento Poster Figs

NCEnto.Fig1 <- ggsave("ncEntoFig1.png", tisum.area.gg, 
                      width = 8, height = 3.5, units = "in", 
                      dpi = 900)


NCEnto.Fig2 <- ggplot(data = ToxIndex.summary.yr %>%
         filter(Type == "Insecticide"),
       aes(y = ToxIndex.area, x = Year)) +
  facet_grid(Crop ~ Timing, scales = "free_y") +
  geom_point() + 
  stat_smooth(se = FALSE, span = 0.9,
              aes(color = Crop)) +
  ylab("Honeybee acute toxicity index") +
  theme_minimal_grid() +
  scale_y_continuous(limits = c(0, NA)) +
  theme(legend.position = "none")

ggsave("ncEntoFig2.png", NCEnto.Fig2, 
       width = 8, height = 5, units = "in", 
       dpi = 900)


NCEnto.Fig3 <- ggdraw(plot_grid(cornPre.aigg + 
                   annotate("text", x = 2008, y = 3, label = "B", 
                            size = 5, fontface = "bold"), 
                 corn.aigg, soy.aigg,
                 ncol = 1, align = "v",
                 labels = LETTERS[c(1,3,4)])) + 
  draw_plot(cornPre.inset, .36, .75, .29, .2)

ggsave("ncEntoFig3.png", NCEnto.Fig3, 
       width = 9, height = 12, units = "in", 
       dpi = 900)


NCEnto.Fig4 <- ggplot(cornPostPestTox.summary, aes(x = Year, y = pestToxIndex.sum)) +
  geom_point() + stat_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ TaxonGroup, ncol = 3) +
  ggtitle("Corn POST Insecticide Targets") +
  ylab("Total Pest Toxicity Index") +
  theme_minimal_grid() + 
  theme(panel.background = element_rect(fill = "#F5F5F5"),
        axis.text.x = element_text(size = 9)) +
  geom_text(data = cornPost.lm.summary,
            aes(x = Inf, y = 0.5, label = paste("P", pval.display)),
            hjust = 1, vjust = 1, color = "blue")

ggsave("ncEntoFig4.png", NCEnto.Fig4, 
       width = 9, height = 9, units = "in", 
       dpi = 900)


NCEnto.Fig5 <- ggplot(soyPostPestTox.summary %>%
         filter(TaxonGroup != "Hymenoptera"), 
       aes(x = Year, y = pestToxIndex.sum)) +
  geom_point() + stat_smooth(method = "lm", se = FALSE) +
  facet_wrap(~ TaxonGroup, ncol = 3) +
  ggtitle("Soybean POST Insecticide Targets") +
  ylab("Total Pest Toxicity Index") +
  theme_minimal_grid() + 
  theme(panel.background = element_rect(fill = "#F5F5F5"),
        axis.text.x = element_text(size = 9)) +
  geom_text(data = soyPost.lm.summary %>%
              filter(TaxonGroup != "Hymenoptera"),
            aes(x = Inf, y = 0.75, label = paste("P", pval.display)),
            hjust = 1, vjust = 1, color = "blue")
ggsave("ncEntoFig5.png", NCEnto.Fig5, 
       width = 9, height = 9, units = "in", 
       dpi = 900)


NCEnto.Fig6 <- plot_grid(yieldCorn.gg, Corn.EcoY.gg, Corn.EcoV.gg, 
          yieldSoy.gg,  Soy.EcoY.gg,  Soy.EcoV.gg,
          nrow = 2, ncol = 3, align = "hv", labels = LETTERS)
ggsave("ncEntoFig6.png", NCEnto.Fig6, 
       width = 10, height = 5.5, units = "in", 
       dpi = 900)
