#Skript zum Analysieren der Kreiswanderungsmatrizen des Statistischen Bundesamts
library(needs)
needs(tidyverse, openxlsx, stringr, readxl, janitor)

# 1. Load data ------------------------------------------------------------
#Daten stammen von hier:
#Kreiswanderungen: https://www.statistikportal.de/de/veroeffentlichungen/kreiswanderungsmatrix
#bzw: https://bscw.bund.de/pub/bscw.cgi/338090909?client_size=1440x665


# 18 bis 21 ---------------------------------------------------------------
# 1) Basisordner: Gesamtpaket 2018–2021
base_path <- "input/kreiswanderungen/KWM_2018-2021"

# 2) Alle Excel-Dateien rekursiv
files <- list.files(path = base_path, pattern = "\\.xls[x]?$", recursive = TRUE, full.names = TRUE)

# 3) Optional: auf Jahre 2018–2021 einschränken (falls im Dateinamen enthalten)
files <- files[str_detect(files, "2018|2019|2020|2021")]

# 4) Deine Modify-Funktion (leicht angepasst, ansonsten unverändert)
modify_one <- function(df, df_name) {
  # 0) Spaltennamen vereinheitlichen (Zeilenumbrüche/Mehrfach-Spaces entfernen)
  nm <- names(df)
  nm <- gsub("\n", " ", nm)
  nm <- gsub("\\s+", " ", nm)
  nm <- trimws(nm)
  nm <- gsub("^Zielkreis-\\s*Schlüssel$", "Zielkreis-Schlüssel", nm)
  nm <- gsub("^Herkunftskreis-\\s*Schlüssel$", "Herkunftskreis-Schlüssel", nm)
  nm <- gsub("^Zielkreis-\\s*Kreistext$", "Zielkreis-Kreistext", nm)
  names(df) <- nm
  
  yr <- as.integer(stringr::str_extract(df_name, "\\b20\\d{2}\\b|\\b19\\d{2}\\b"))
  
  # 1) Umbenennen (defensiv)
  df <- df %>%
    rename(
      zielkreis            = any_of("Zielkreis-Kreistext"),
      zielkreis_ags        = any_of("Zielkreis-Schlüssel"),
      herkunftskreis       = any_of("Herkunftskreis-Kreistext"),
      herkunftskreis_ags   = any_of("Herkunftskreis-Schlüssel"),
      altersgruppe         = any_of("Altersgruppen")
    )
  
  # 2) führende 0 in zielkreis_ags entfernen
  if ("zielkreis_ags" %in% names(df)) {
    df <- df %>% mutate(zielkreis_ags = sub("^0+", "", as.character(zielkreis_ags)))
  }
  
  # 2.1) führende 0 in herkunftskreis_ags entfernen
  if ("herkunftskreis_ags" %in% names(df)) {
    df <- df %>% mutate(herkunftskreis_ags = sub("^0+", "", as.character(herkunftskreis_ags)))
  }
  
  # 3) Altersgruppen-Werte mappen
  if ("altersgruppe" %in% names(df)) {
    df <- df %>%
      mutate(
        altersgruppe = as.character(altersgruppe),
        altersgruppe = trimws(altersgruppe),
        altersgruppe = dplyr::recode(
          altersgruppe,
          "unter 18"           = "unter 18 Jahre",
          "18 - 25"            = "18 bis 24 Jahre",
          "18 bis unter 25"    = "18 bis 24 Jahre",
          "25 - 30"            = "25 bis 29 Jahre",
          "25 bis unter 30"    = "25 bis 29 Jahre",
          "30 - 50"            = "30 bis 49 Jahre",
          "30 bis unter 50"    = "30 bis 49 Jahre",
          "50 - 65"            = "50 bis 64 Jahre",
          "50 bis unter 65"    = "50 bis 64 Jahre",
          "65 und mehr"        = "65 Jahre und älter",
          "65 und älter"       = "65 Jahre und älter",
          .default = altersgruppe,
          .missing = altersgruppe
        )
      )
  }
  
  # 4) saldo_deu_i aus Spalte ...18 (falls vorhanden)
  if ("...18" %in% names(df)) {
    # Falls Excel die Spalte schon numerisch liefert: direkt übernehmen
    if (is.numeric(df[["...18"]])) {
      df <- df %>% mutate(saldo_deu_i = .data[["...18"]])
    } else {
      # Sonst schlicht as.numeric() – das behält "-" bei cleanen Dateien
      df <- df %>% mutate(saldo_deu_i = suppressWarnings(as.numeric(.data[["...18"]])))
    }
  } else if (!("saldo_deu_i" %in% names(df))) {
    df$saldo_deu_i <- NA_real_
  }
  
  # 5) Jahr ergänzen
  df <- df %>% mutate(jahr = yr)
  
  # 6) nur gewünschte Spalten behalten
  keep_cols <- c("zielkreis","zielkreis_ags","altersgruppe",
                 "herkunftskreis","herkunftskreis_ags",
                 "saldo_deu_i","jahr")
  df <- df %>%
    select(any_of(keep_cols))
  
  # 7) Zeilen ohne zielkreis entfernen
  if ("zielkreis" %in% names(df)) {
    df <- df %>% filter(!is.na(zielkreis) & zielkreis != "")
  }
  
  df
}

# 5) Einlesen & vereinheitlichen für ALLE Bundesländer
read_and_modify <- function(file_path) {
  df_raw  <- readxl::read_excel(file_path)
  df_name <- basename(file_path)
  modify_one(df_raw, df_name)
}

dfs_list <- purrr::map(files, read_and_modify)

# 6) Alles zusammenwerfen und dann je Jahr aufteilen
kwm_all_2018_2021 <- bind_rows(dfs_list)

kwm_2018 <- kwm_all_2018_2021 %>% filter(jahr == 2018)
kwm_2019 <- kwm_all_2018_2021 %>% filter(jahr == 2019)
kwm_2020 <- kwm_all_2018_2021 %>% filter(jahr == 2020)
kwm_2021 <- kwm_all_2018_2021 %>% filter(jahr == 2021)

# Aufräumen (optional)
rm(base_path, files, dfs_list, kwm_all_2018_2021, read_and_modify, modify_one)


# 22 bis 24 ---------------------------------------------------------------
kwm_22 <- read.csv("input/kreiswanderungen/kreiswanderungen_2022.csv", sep= ";")
kwm_23 <- read.csv("input/kreiswanderungen/kreiswanderungen_2023.csv", sep = ";")
kwm_24 <- read.csv("input/kreiswanderungen/kreiswanderungen_2024.csv", sep = ";")

# Nur relevante Spalten behalten (alles außer saldo_aus_*)
kwm_22 <- kwm_22 %>%
  select(zielkreis, zielkreis_ags, altersgruppe, herkunftskreis, herkunftskreis_ags, saldo_deu_i) %>%
  mutate(
    zielkreis_ags = as.character(zielkreis_ags),
    herkunftskreis_ags = as.character(herkunftskreis_ags),
    jahr = 2022
  )

kwm_23 <- kwm_23 %>%
  select(zielkreis, zielkreis_ags, altersgruppe, herkunftskreis, herkunftskreis_ags, saldo_deu_i) %>%
  mutate(
    zielkreis_ags = as.character(zielkreis_ags),
    herkunftskreis_ags = as.character(herkunftskreis_ags),
    jahr = 2023)

kwm_24 <- kwm_24 %>%
  select(zielkreis, zielkreis_ags, altersgruppe, herkunftskreis, herkunftskreis_ags, saldo_deu_i) %>%
  mutate(
    zielkreis_ags = as.character(zielkreis_ags),
    herkunftskreis_ags = as.character(herkunftskreis_ags),
    jahr = 2024)


# Join --------------------------------------------------------------------
# Jetzt zusammenführen
kwm_all <- bind_rows(kwm_2018, kwm_2019, kwm_2020, kwm_2021, kwm_22, kwm_23, kwm_24)

rm(kwm_2018, kwm_2019, kwm_2020, kwm_2021, kwm_22, kwm_23, kwm_24)

kwm_all <- kwm_all %>%
  mutate(
    zielkreis = case_when(
      zielkreis_ags == "9561" ~ "Ansbach, Stadt",
      zielkreis_ags == "9571" ~ "Ansbach, Landkreis",
      zielkreis_ags == "9661" ~ "Aschaffenburg, Stadt",
      zielkreis_ags == "9672" ~ "Aschaffenburg, Landkreis",
      zielkreis_ags == "9761" ~ "Augsburg, Stadt",
      zielkreis_ags == "9772" ~ "Augsburg, Landkreis",
      zielkreis_ags == "8211" ~ "Baden-Baden, Stadt",
      zielkreis_ags == "9461" ~ "Bamberg, Stadt",
      zielkreis_ags == "9471" ~ "Bamberg, Landkreis",
      zielkreis_ags == "9462" ~ "Bayreuth, Stadt",
      zielkreis_ags == "9472" ~ "Bayreuth, Landkreis",
      zielkreis_ags == "9463" ~ "Coburg, Stadt",
      zielkreis_ags == "9473" ~ "Coburg, Landkreis",
      zielkreis_ags == "8311" ~ "Freiburg im Breisgau, Stadt",
      zielkreis_ags == "9563" ~ "Fürth, Stadt",
      zielkreis_ags == "9573" ~ "Fürth, Landkreis",
      zielkreis_ags == "5914" ~ "Hagen, Stadt",
      zielkreis_ags == "8221" ~ "Heidelberg, Stadt",
      zielkreis_ags == "8121" ~ "Heilbronn, Stadt",
      zielkreis_ags == "8125" ~ "Heilbronn, Landkreis",
      zielkreis_ags == "9464" ~ "Hof, Stadt",
      zielkreis_ags == "9475" ~ "Hof, Landkreis",
      zielkreis_ags == "7335" ~ "Kaiserslautern, Landkreis",
      zielkreis_ags == "8212" ~ "Karlsruhe, Stadt",
      zielkreis_ags == "8215" ~ "Karlsruhe, Landkreis",
      zielkreis_ags == "6633" ~ "Kassel, Landkreis",
      zielkreis_ags == "9261" ~ "Landshut, Stadt",
      zielkreis_ags == "9274" ~ "Landshut, Landkreis",
      zielkreis_ags == "14729" ~ "Leipzig, Landkreis",
      zielkreis_ags == "8222" ~ "Mannheim, Stadt",
      zielkreis_ags == "9184" ~ "München, Landkreis",
      zielkreis_ags == "6438" ~ "Offenbach, Landkreis",
      zielkreis_ags == "3458" ~ "Oldenburg, Landkreis",
      zielkreis_ags == "3459" ~ "Osnabrück, Landkreis",
      zielkreis_ags == "9262" ~ "Passau, Stadt",
      zielkreis_ags == "9275" ~ "Passau, Landkreis",
      zielkreis_ags == "8231" ~ "Pforzheim, Stadt",
      zielkreis_ags == "9362" ~ "Regensburg, Stadt",
      zielkreis_ags == "9375" ~ "Regensburg, Landkreis",
      zielkreis_ags == "9163" ~ "Rosenheim, Stadt",
      zielkreis_ags == "9187" ~ "Rosenheim, Landkreis",
      zielkreis_ags == "9662" ~ "Schweinfurt, Stadt",
      zielkreis_ags == "9678" ~ "Schweinfurt, Landkreis",
      zielkreis_ags == "5122" ~ "Solingen, Stadt",
      zielkreis_ags == "8111" ~ "Stuttgart, Stadt",
      zielkreis_ags == "8421" ~ "Ulm, Stadt",
      zielkreis_ags == "9663" ~ "Würzburg, Stadt",
      zielkreis_ags == "9679" ~ "Würzburg, Landkreis",
      TRUE ~ zielkreis
    )
  )


# Bevölkerung -------------------------------------------------------------
bev_22 <- read.csv("input/bevoelkerung/bevoelkerung_22.csv", sep = ";")
bev_22 <- bev_22 %>%
  filter(X3_variable_attribute_label == "Deutschland") %>%   # nur Deutschland-Daten
  select(
    region = X1_variable_attribute_label,
    ags = X1_variable_attribute_code,
    altersgruppe = X2_variable_attribute_label,
    value
  ) %>%
  mutate(ags = sub("^0+", "", as.character(ags)))


# Analyse -----------------------------------------------------------------

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
    total = rowSums(across(-c(zielkreis_ags, zielkreis)), na.rm = TRUE)
  ) %>%
  clean_names() 


# --- Eisenach -> Wartburgkreis addieren und Eisenach-Zeile löschen ---

# 1) Spaltenbereich: von x18_bis_24_jahre bis total
cols <- which(names(saldo_kreis_ag_sum) == "x18_bis_24_jahre") :
  which(names(saldo_kreis_ag_sum) == "total")

# 2) Summen aus "Eisenach, Stadt" holen (falls vorhanden)
eisenach_vals <- colSums(
  saldo_kreis_ag_sum[saldo_kreis_ag_sum$zielkreis == "Eisenach, Stadt", cols, drop = FALSE],
  na.rm = TRUE
)

# 3) Index des Wartburgkreis finden
wartburg_idx <- which(saldo_kreis_ag_sum$zielkreis == "Wartburgkreis")

# 4) Addieren (nur wenn beide existieren)
if (length(wartburg_idx) == 1 && length(eisenach_vals) > 0) {
  saldo_kreis_ag_sum[wartburg_idx, cols] <-
    sweep(saldo_kreis_ag_sum[wartburg_idx, cols, drop = FALSE], 2, eisenach_vals, `+`)
}

# 5) Eisenach-Zeile entfernen
saldo_kreis_ag_sum <- dplyr::filter(saldo_kreis_ag_sum, zielkreis != "Eisenach, Stadt")

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
  left_join(bev_buckets, by = c("zielkreis_ags" = "ags"))


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

saldo_kreis_ag_sum <- saldo_kreis_ag_sum %>%
  mutate(
    # Gesamtwanderungssaldo (Summe aller Altersgruppen)
    wanderungssaldo_gesamt = unter_18_jahre + x18_bis_24_jahre + x25_bis_29_jahre +
      x30_bis_49_jahre + x50_bis_64_jahre + x65_jahre_und_alter,
    
    # Gesamtbevölkerung (Summe aller Altersgruppen)
    bev_gesamt = bev_unter_18_jahre + bev_x18_bis_24_jahre + bev_x25_bis_29_jahre +
      bev_x30_bis_49_jahre + bev_x50_bis_64_jahre + bev_x65_jahre_und_alter,
    
    # Prozentualer Gesamtsaldo (in Prozent)
    p_wanderungssaldo_gesamt = 100 * wanderungssaldo_gesamt / bev_gesamt,
    
    # Wanderungssaldo 18–49 Jahre
    wanderungssaldo_18_bis_49 = x18_bis_24_jahre + x25_bis_29_jahre + x30_bis_49_jahre,
    
    # Bevölkerung 18–49 Jahre
    bev_18_bis_49 = bev_x18_bis_24_jahre + bev_x25_bis_29_jahre + bev_x30_bis_49_jahre,
    
    # Prozentualer Wanderungssaldo 18–49 Jahre
    p_wanderungssaldo_18_bis_49 = 100 * wanderungssaldo_18_bis_49 / bev_18_bis_49,
    
    # Wanderungssaldo 18–49 Jahre
    wanderungssaldo_18_bis_29 = x18_bis_24_jahre + x25_bis_29_jahre,
    
    # Bevölkerung 18–49 Jahre
    bev_18_bis_29 = bev_x18_bis_24_jahre + bev_x25_bis_29_jahre,
    
    # Prozentualer Wanderungssaldo 18–49 Jahre
    p_wanderungssaldo_18_bis_29 = 100 * wanderungssaldo_18_bis_29 / bev_18_bis_29
  )



