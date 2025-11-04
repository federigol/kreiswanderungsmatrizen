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
bev_22 <- read.csv("input/bevoelkerung/bevoelkerung_22.csv", sep = ";")

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

kreise_mit_landkreis <- c(
  "Heilbronn", "Karlsruhe", "Rosenheim", "München", "Landshut", "Passau", 
  "Regensburg", "Bamberg", "Bayreuth", "Coburg", "Hof", "Ansbach", 
  "Fürth", "Aschaffenburg", "Schweinfurt", "Würzburg", "Augsburg"
)

# Werte anpassen
kwm_all <- kwm_all %>%
  mutate(
    zielkreis = if_else(zielkreis %in% kreise_mit_landkreis,
                        paste0(zielkreis, ", Landkreis"),
                        zielkreis)
  )

rm(kwm_22, kwm_23, kwm_24, kreise_mit_landkreis)

bev_22 <- bev_22 %>%
  filter(X3_variable_attribute_label == "Deutschland") %>%   # nur Deutschland-Daten
  select(
    region = X1_variable_attribute_label,
    ags = X1_variable_attribute_code,
    altersgruppe = X2_variable_attribute_label,
    value
  )


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
  ) %>%
  clean_names()

# 1) Bevölkerungs-Altersgruppen passend zu den Saldo-Buckets bündeln
bev_buckets <- bev_22 %>%
  mutate(bucket = case_when(
    altersgruppe %in% c("Unter 3 Jahre", "3 bis 5 Jahre", "6 bis 14 Jahre", "15 bis 17 Jahre") ~ "unter_18_jahre",
    altersgruppe == "18 bis 24 Jahre" ~ "x18_bis_24_jahre",
    altersgruppe == "25 bis 29 Jahre" ~ "x25_bis_29_jahre",
    altersgruppe %in% c("30 bis 39 Jahre", "40 bis 49 Jahre") ~ "x30_bis_49_jahre",
    altersgruppe == "50 bis 64 Jahre" ~ "x50_bis_64_jahre",
    altersgruppe %in% c("65 bis 74 Jahre", "75 Jahre und älter") ~ "x65_jahre_und_alter",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(bucket)) %>%
  group_by(region, ags, bucket) %>%
  summarise(bev = sum(value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    names_from  = bucket,
    values_from = bev,
    names_prefix = "bev_"   # -> bev_unter_18_jahre, bev_x18_bis_24_jahre, ...
  )

# 2) An saldo_kreis_ag_sum anhängen (per Regionsname)
saldo_kreis_ag_sum <- saldo_kreis_ag_sum %>%
  left_join(bev_buckets, by = c("zielkreis" = "region"))


saldo_kreis_ag_sum <- saldo_kreis_ag_sum %>%
  mutate(
    # Prozentualer Wanderungssaldo (Saldo / Bevölkerungszahl * 100)
    p_unter_18_jahre        = 100 * unter_18_jahre        / bev_unter_18_jahre,
    p_x18_bis_24_jahre      = 100 * x18_bis_24_jahre      / bev_x18_bis_24_jahre,
    p_x25_bis_29_jahre      = 100 * x25_bis_29_jahre      / bev_x25_bis_29_jahre,
    p_x30_bis_49_jahre      = 100 * x30_bis_49_jahre      / bev_x30_bis_49_jahre,
    p_x50_bis_64_jahre      = 100 * x50_bis_64_jahre      / bev_x50_bis_64_jahre,
    p_x65_jahre_und_alter   = 100 * x65_jahre_und_alter   / bev_x65_jahre_und_alter
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
