# heat map for meta analysis
library(tidyverse)
mapData <- readxl::read_excel("data/meta heat map data 2022 03 21-rqq.xlsx")

syn_data <- expand_grid(age = c(1:6), 
                           protein = c("1.4", "1.7", "1.8", "1.9", "2", "2.1", "2.2", "over 2.2"))

# Weight gain -------------------------------------------------------------
mapData_wg <- filter(mapData, parameter == "weight gain")

syn_data_wg <- left_join(syn_data, mapData_wg, by =c("age", "protein"))

syn_data_wg <- syn_data_wg %>% mutate(
  effect = case_when(is.na(effect) ~ "miss",
                     TRUE ~ effect)
)

fillColor <- c(h = "red",
               l = "green",
               ns = "white")

wg_heat <- ggplot()+
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

# Weight gain -------------------------------------------------------------
mapData_hg <- filter(mapData, parameter == "height gain")

syn_data_hg <- left_join(syn_data, mapData_hg, by =c("age", "protein"))

syn_data_hg <- syn_data_hg %>% mutate(
  effect = case_when(is.na(effect) ~ "miss",
                     TRUE ~ effect)
)


hg_heat <- ggplot()+
  geom_raster(data = syn_data_hg, 
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

# BMI -------------------------------------------------------------
mapData_bmi <- filter(mapData, parameter == "bmi")

syn_data_bmi <- left_join(syn_data, mapData_bmi, by =c("age", "protein"))

syn_data_bmi <- syn_data_bmi %>% mutate(
  effect = case_when(is.na(effect) ~ "miss",
                     TRUE ~ effect)
)


bmi_heat <- ggplot()+
  geom_raster(data = syn_data_bmi, 
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
bmi_heat


# 2022 04 14 --------------------------------------------------------------

# 重新对蛋白质进行分组

mapData_2 <- readxl::read_excel("data/meta heat map data 2022 04 12-rqq.xlsx")
syn_data_2 <- expand_grid(age = c(1:6), 
                        protein = c("<1.8", "1.8~2", "2.1~2.2", ">2.2"))

# weight gain
mapData_wg2 <- filter(mapData_2, parameter == "weight gain")

syn_data_wg2 <- left_join(syn_data_2, mapData_wg2, by =c("age", "protein"))

syn_data_wg2 <- syn_data_wg2 %>% mutate(
  effect = case_when(is.na(effect) ~ "miss",
                     TRUE ~ effect)
)

wg_heat2 <- ggplot()+
  geom_raster(data = syn_data_wg2, 
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
  scale_x_discrete(labels = c("<1.8", "1.8~2", "2.1~2.2", ">2.2"),
                   expand = c(0,0)) +
  scale_y_discrete(breaks = c(1,2,3,4,5,6),
                   labels = c("Month 1", "Month 2", "Month 3",
                              "Month 4", "Month 5", "Month 6"),
                   expand = c(0,0)) +
  theme(
    
    axis.title = element_text(size = 30,
                              family = "serif"),
    axis.text = element_text(size = 30, 
                             family = "serif"),
    legend.title = element_text(size = 20,
                                family = "serif"),
    legend.text = element_text(size = 20, 
                               family = "serif"),
  ) +
  ylab("Infant age\n")+
  xlab("\n Protein content (g/100 kcal)")
wg_heat2


# height gain

mapData_hg2 <- filter(mapData_2, parameter == "height gain")

syn_data_hg2 <- left_join(syn_data_2, mapData_hg2, by =c("age", "protein"))

syn_data_hg2 <- syn_data_hg2 %>% mutate(
  effect = case_when(is.na(effect) ~ "miss",
                     TRUE ~ effect)
)

hg_heat2 <- ggplot()+
  geom_raster(data = syn_data_hg2, 
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
  scale_x_discrete(labels = c("<1.8", "1.8~2", "2.1~2.2", ">2.2"),
                   expand = c(0,0)) +
  scale_y_discrete(breaks = c(1,2,3,4,5,6),
                   labels = c("Month 1", "Month 2", "Month 3",
                              "Month 4", "Month 5", "Month 6"),
                   expand = c(0,0)) +
  theme(
    
    axis.title = element_text(size = 30,
                              family = "serif"),
    axis.text = element_text(size = 30, 
                             family = "serif"),
    legend.title = element_text(size = 20,
                                family = "serif"),
    legend.text = element_text(size = 20, 
                               family = "serif"),
  ) +
  ylab("Infant age\n")+
  xlab("\n Protein content (g/100 kcal)")
hg_heat2

# BMI

mapData_bmi2 <- filter(mapData_2, parameter == "bmi")

syn_data_bmi2 <- left_join(syn_data_2, mapData_bmi2, by =c("age", "protein"))

syn_data_bmi2 <- syn_data_bmi2 %>% mutate(
  effect = case_when(is.na(effect) ~ "miss",
                     TRUE ~ effect)
)

bmi_heat2 <- ggplot()+
  geom_raster(data = syn_data_bmi2, 
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
  scale_x_discrete(labels = c("<1.8", "1.8~2", "2.1~2.2", ">2.2"),
                   expand = c(0,0)) +
  scale_y_discrete(breaks = c(1,2,3,4,5,6),
                   labels = c("Month 1", "Month 2", "Month 3",
                              "Month 4", "Month 5", "Month 6"),
                   expand = c(0,0)) +
  theme(
    
    axis.title = element_text(size = 30,
                              family = "serif"),
    axis.text = element_text(size = 30, 
                             family = "serif"),
    legend.title = element_text(size = 20,
                                family = "serif"),
    legend.text = element_text(size = 20, 
                               family = "serif"),
  ) +
  ylab("Infant age\n")+
  xlab("\n Protein content (g/100 kcal)")
bmi_heat2
