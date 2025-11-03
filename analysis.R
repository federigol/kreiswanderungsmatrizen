#Skript zum Analysieren der Kreiswanderungsmatrizen des Statistischen Bundesamts
library(needs)
needs(tidyverse, openxlsx, stringr, readxl, janitor, ggalluvial, forcats)

# 1. Load data ------------------------------------------------------------
#Daten stammen von hier:
#Kreiswanderungen: https://www.statistikportal.de/de/veroeffentlichungen/kreiswanderungsmatrix
#bzw: https://bscw.bund.de/pub/bscw.cgi/338090909?client_size=1440x665

kwm_22 <- read.csv("input/kreiswanderungen/kreiswanderungen_2022.csv", sep= ";")
kwm_23 <- read.csv("input/kreiswanderungen/kreiswanderungen_2023.csv", sep = ";")
kwm_24 <- read.csv("input/kreiswanderungen/kreiswanderungen_2024.csv", sep = ";")
bev_23 <- read.csv("input/bevoelkerung/bevoelkerung-23.csv", sep = ";", skip = 5)


# Nur relevante Spalten behalten (alles außer saldo_aus_*)
kwm_22 <- kwm_22 %>%
  select(zielkreis, zielkreis_ags, altersgruppe, saldo_deu_i, saldo_deu_m, saldo_deu_w) %>%
  mutate(jahr = 2022)

kwm_23 <- kwm_23 %>%
  select(zielkreis, zielkreis_ags, altersgruppe, saldo_deu_i, saldo_deu_m, saldo_deu_w)  %>%
  mutate(jahr = 2023)

kwm_24 <- kwm_24 %>%
  select(zielkreis, zielkreis_ags, altersgruppe, saldo_deu_i, saldo_deu_m, saldo_deu_w)  %>%
  mutate(jahr = 2024)

# Jetzt zusammenführen
kwm_all <- bind_rows(kwm_22, kwm_23, kwm_24)

# 1) Kernergebnis: Saldo (insgesamt) je Kreis x Altersgruppe x Jahr
saldo_kreis_ag <- kwm_all %>%
  group_by(jahr, zielkreis_ags, zielkreis, altersgruppe) %>%
  summarise(
    saldo_ins_gesamt = sum(saldo_deu_i, na.rm = TRUE),  # Wanderungssaldo insgesamt
    .groups = "drop"
  )

saldo_kreis_ag_sum <- saldo_kreis_ag %>%
  group_by(zielkreis_ags, zielkreis, altersgruppe) %>%
  summarise(
    saldo_summe = sum(saldo_ins_gesamt, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  tidyr::pivot_wider(
    names_from = altersgruppe,
    values_from = saldo_summe
  ) %>%
  mutate(
    # Gesamtsumme aller Altersgruppen (ohne AGS-Spalten)
    total = rowSums(across(-c(zielkreis_ags, zielkreis)), na.rm = TRUE),
    
    # Kombinierte Summe der 18–24 und 30–49-Jährigen
    summe_18_49 = (`18 bis 24 Jahre` + `30 bis 49 Jahre`)
  )


jugend_saldo <- saldo_kreis_ag_sum %>%
  mutate(
    jugend = `18 bis 24 Jahre` + `25 bis 29 Jahre`,
    rueckkehr = `30 bis 49 Jahre`,
    rueckkehrindikator = jugend + rueckkehr,
    jugend_wegzugsindex = jugend + 0.5 * rueckkehr
  )


# 1️⃣ Kombinierte Altersgruppen berechnen
abwanderungsanalyse <- saldo_kreis_ag_sum %>%
  mutate(
    saldo_18_29 = coalesce(`18 bis 24 Jahre`, 0) + coalesce(`25 bis 29 Jahre`, 0),
    saldo_30_49 = coalesce(`30 bis 49 Jahre`, 0)
  )

# 2️⃣ Filter: Junge wandern ab, kaum Ältere ziehen zu
abwanderungsregionen <- abwanderungsanalyse %>%
  filter(
    saldo_18_29 < 0,             # Netto-Wegzug junger Menschen
    saldo_30_49 <= 50            # kaum Zuzug im Alter 30–49 (Grenze anpassbar)
  ) %>%
  arrange(saldo_18_29)
