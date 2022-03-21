# heat map for meta analysis
library(tidyverse)
mapData <- readxl::read_excel("data/meta heat map data 2022 03 21-rqq.xlsx")

mapData_wg <- filter(mapData, parameter == "weight gain")

syn_data_wg <- expand_grid(age = c(1:6), 
                           protein = c("1.4", "1.7", "1.8", "1.9", "2", "2.1", "2.2", "over 2.2"))

syn_data_wg <- left_join(syn_data_wg, mapData_wg, by =c("age", "protein"))

syn_data_wg <- syn_data_wg %>% mutate(
  effect = case_when(is.na(effect) ~ "miss",
                     TRUE ~ effect)
)

fillColor <- c(h = "red",
               l = "green",
               ns = "white")

ggplot()+
  geom_raster(data = syn_data_wg, 
              aes(x = protein, y = as.factor(age), fill = effect)) +
  theme_bw() +
  scale_fill_manual(name = "Mean difference",
                    breaks = c("h","l","ns", "miss"),
                    labels = c("Higher than BM", "Lower than BM",
                               "Not significant", "No data"),
                    values = c(h = "#b2182b",
                               l = "#d9ef8b",
                               ns = "#2166ac",
                               miss = "#f7f7f7")) +
  scale_x_discrete(labels = c("1.4", "1.7", "1.8", "1.9", "2.0", "2.1", "2.2", "Over 2.2"),
                   expand = c(0,0)) +
  scale_y_discrete(breaks = c(1,2,3,4,5,6),
                   labels = c("Month 1", "Month 2", "Month 3",
                              "Month 4", "Month 5", "Month 6"),
                   expand = c(0,0)) +
  theme(
    axis.title = element_text(size = 20,
                              family = "serif"),
    axis.text = element_text(size = 15, 
                             family = "serif"),
    legend.title = element_text(size = 20,
                                family = "serif"),
    legend.text = element_text(size = 15, 
                               family = "serif"),
  ) +
  ylab("Infant age\n")+
  xlab("Protein content (g/100 kcal)")

