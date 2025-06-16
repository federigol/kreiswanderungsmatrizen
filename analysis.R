library(needs)
needs(tidyverse)


# 1. Load data ------------------------------------------------------------

twentytwo <- read.csv("input/KWM_GES_NAT_ALT_Salden_2022.csv", sep= ";")
twentythree <- read.csv("input/KWM_GES_NAT_ALT_Salden_2023.csv", sep = ";")
bevoelkerung_23 <- read.csv("input/bevoelkerung-23.csv", sep = ";", skip = 5)


# 2. Clean data -----------------------------------------------------------
twentythree <- twentythree %>%
  select(herkunftskreis, zielkreis, herkunftskreis_ags, zielkreis_ags, altersgruppe, saldo_ins_i)

twentythree <- twentythree %>%
  pivot_wider(
    names_from = altersgruppe,     # Die neuen Spaltennamen
    values_from = saldo_ins_i      # Die Werte, die in die neuen Spalten kommen
  )


zielkreis_aggregiert <- twentythree %>%
  group_by(zielkreis_ags, zielkreis) %>%
  summarise(
    `unter 18 Jahre` = sum(`unter 18 Jahre`, na.rm = TRUE),
    `18 bis 24 Jahre` = sum(`18 bis 24 Jahre`, na.rm = TRUE),
    `25 bis 29 Jahre` = sum(`25 bis 29 Jahre`, na.rm = TRUE),
    `30 bis 49 Jahre` = sum(`30 bis 49 Jahre`, na.rm = TRUE),
    `50 bis 64 Jahre` = sum(`50 bis 64 Jahre`, na.rm = TRUE),
    `65 Jahre und älter` = sum(`65 Jahre und älter`, na.rm = TRUE)
  )
