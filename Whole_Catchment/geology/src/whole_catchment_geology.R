setwd("C:/Studies/Donguzorun/FP/FP_2020-2022/Whole_Catchment/geology/src")

library(tidyverse)
library(dplyr)
library(fingerPro)
library(readxl)
library(corrr)

# Пример исходных данных
data <- catchment

# 1) ЧТЕНИЕ и подготовка ДАННЫХ
df_whole_catchment <- 
  read_excel(path = "../data/whole_delta_geology_data.xlsx") %>% 
  filter(Name != "Размерность") %>% 
  select(-X, -Y) %>% 
  rename(id = Name) %>% 
  mutate_at(vars(-Source), ~ as.numeric(.)) %>% 
  na_if(0) %>% 
  as.data.frame() %>% 
  filter(id != 2007) %>%
  filter(id != 2023) %>%
  filter(id != 2026) %>%
  filter(id != 1017) %>%
  filter(id != 1020) %>%
  filter(id != 1027) %>%
  filter(id != 1031) %>%
  filter(id != 2013) %>%
  filter(id != 3009) %>%
  filter(id != 3011) %>% 
  filter(id != 1028) %>% 
  filter(id != 1015) %>% 
  filter(id != 2005) %>%
  filter(id != 2017) %>%
  filter(id != 2016) %>% 
  filter(id != 1030) %>%
  filter(id != 1024) %>%
  filter(id != 1009) %>%
  filter(id != 1023) %>%
  filter(id != 1021) %>% 
  filter(id != 2022) %>%
  filter(id != 2020) %>%
  filter(id != 3003) %>% 
  filter(id != 2004) %>%
  filter(id != 1007) %>%
  filter(id != 1004) %>%
  filter(id != 2009) %>%
  filter(id != 2027) %>%
  filter(id != 2011) %>% 
  filter(id != 3008) %>%
  filter(id != 2014) %>%
  filter(id != 3007) %>% 
  filter(id != 1013) %>%
  filter(id != 2006) %>%
  filter(id != 2021) %>%
  filter(id != 3002) %>% 
  filter(id != 1005) %>% 
  filter(id != 1008) %>% 
  filter(id != 1022) %>%
  filter(id != 1006) %>% 
  filter(id != 1001) %>%
  filter(id != 1012)
  # filter(id != 1008) %>%
  # filter(id != 1005) %>%
  # filter(id != 1006) %>%
  # filter(id != 1012) %>%
  # filter(id != 1011)

  # filter(id != 1010) %>% 
  # filter(id != 1023) %>%
  # filter(id != 3003) %>% 
  # filter(id != 1022) %>% 
  # filter(id != 1002)
  # filter(id != 1010)
  # filter(id != 1024) %>%
  # filter(id != 1011) %>%
  # filter(id != 1013)


# оставляю только одну целевую точку
v <- 1 : 45
for (i in v) {
  df_whole_catchment <-
    df_whole_catchment %>%
    filter(id != i)
}
 
# v2 <- 37 : 45
# for (i in v2) {
#   df_whole_catchment <- 
#     df_whole_catchment %>% 
#     filter(id != i)
# }

# список элементов, которых нет в мишени
mix_na <-
  df_whole_catchment %>% 
  filter(Source == "Mix") %>% # оставляем только строку целевого образца
  select_if(is.na) %>% # выбираем столбцы с na
  gather(var, val) %>% # переменную и значение ориентируем вертикально
  pull(var) # извлекаем вектор названий

# получение итогового набора данных
df <-
  df_whole_catchment %>% 
  select(!all_of(mix_na)) %>%  # оставляем только те элементы, которые есть в целевом образце
  mutate_all(~replace(., is.na(.), 0))

# 2) ПРОВЕРКА НА КОЛЛИНЕАРНОСТЬ
collinears <-
  df %>% 
  select(-id, -Source) %>% 
  correlate(method = "spearman") %>% 
  rearrange() %>% 
  shave() %>% 
  mutate_if(is.numeric, ~abs(.)) %>% 
  filter_if(is.numeric, any_vars(. > 0.85)) %>% 
  pull(term)

collinears


# 3) LDA
# boxPlot(df, columns = 1:6, ncol = 3)
# correlationPlot(df, columns = 1:7, mixtures = TRUE)

df_lda <-
  df %>% 
  select(!any_of(collinears)) # создаем дф без коллинеарных элементов

df_lda %>% 
  LDAPlot(text = T)

# 4) ВЫБОР ТРАССЕРОВ
df_lda %>% 
  rangeTest() %>% 
  KWTest(pvalue = 0.05)

DFATest(df_lda, niveau = 0.1)

# 5) БОКСПЛОТЫ
df %>% 
  select(id, Source, Zn, Bi, kps, plagioklaz) %>% 
  gather(elem, cons, -id, -Source) %>% 
  ggplot(aes(x = Source,
             y = cons,
             color = Source)) +
  geom_boxplot() +
  geom_text(aes(label = id)) +
  facet_wrap(~elem,
             scales = "free_y")

# 6) ПОДТВЕРЖДЕНИЕ ТРАССЕРОВ
df_lda %>% 
  select(id, Source, Zn, Bi, kps, plagioklaz) %>% 
  LDAPlot(text = T)

# 7) Размешивание
results <- 
  df_lda %>% 
  select(id, Source, Zn, Bi, kps, plagioklaz) %>% 
  unmix(samples = 100, iter = 1000)

results %>% 
  plotResults()

