setwd("C:/Studies/Donguzorun/FP/FP_2020-2022/Whole_Catchment/geology/src")

library(tidyverse)
library(dplyr)
library(fingerPro)
library(readxl)
library(corrr)

# ?????? ???????? ??????
data <- catchment

# 1) ?????? ? ?????????? ??????
df_whole_catchment <- 
  read_excel(path = "../data/whole_delta_geology_data.xlsx") %>% 
  filter(Name != "???????????") %>% 
  select(-X, -Y) %>% 
  rename(id = Name) %>% 
  mutate_at(vars(-Source), ~ as.numeric(.)) %>% 
  na_if(0) %>% 
  as.data.frame()

# ???????? ?????? ???? ??????? ?????
v <- 1 : 45
for (i in v) {
  df_whole_catchment <-
    df_whole_catchment %>%
    filter(id != i)
}

# ?????? ?????????, ??????? ??? ? ??????
mix_na <-
  df_whole_catchment %>% 
  filter(Source == "Mix") %>% # ????????? ?????? ?????? ???????? ???????
  select_if(is.na) %>% # ???????? ??????? ? na
  gather(var, val) %>% # ?????????? ? ???????? ??????????? ???????????
  pull(var) # ????????? ?????? ????????

# ????????? ????????? ?????? ??????
df <-
  df_whole_catchment %>% 
  select(!all_of(mix_na)) %>%  # ????????? ?????? ?? ????????, ??????? ???? ? ??????? ???????
  mutate_all(~replace(., is.na(.), 0))

# 2) ???????? ?? ??????????????
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
  select(!any_of(collinears)) # ??????? ?? ??? ???????????? ?????????

df_lda %>% 
  LDAPlot(text = T)

# 4) ????? ?????????
df_lda %>% 
  rangeTest() %>% 
  KWTest(pvalue = 0.05)

DFATest(df_lda, niveau = 0.1)

# 5) ?????????
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

# 6) ????????????? ?????????
df_lda %>% 
  select(id, Source, Zn, Bi, kps, plagioklaz) %>% 
  LDAPlot(text = T)

# 7) ????????????
results <- 
  df_lda %>% 
  select(id, Source, Zn, Bi, kps, plagioklaz) %>% 
  unmix(samples = 100, iter = 1000)

results %>% 
  plotResults()

