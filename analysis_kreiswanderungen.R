#Skript zum Analysieren der Kreiswanderungsmatrizen des Statistischen Bundesamts
library(needs)
needs(tidyverse, openxlsx, stringr, readxl, janitor)

# 1. Load data ------------------------------------------------------------
#Daten stammen von hier:
#Kreiswanderungen: https://www.statistikportal.de/de/veroeffentlichungen/kreiswanderungsmatrix
#bzw: https://bscw.bund.de/pub/bscw.cgi/338090909?client_size=1440x665


# # 2018 bis 2021 (auskommentiert weil nicht gebraucht für Analyse) ---------------------------------------------------------------
# # 1) Basisordner: Gesamtpaket 2018–2021
# base_path <- "input/kreiswanderungen/KWM_2018-2021"
# 
# # 2) Alle Excel-Dateien rekursiv
# files <- list.files(path = base_path, pattern = "\\.xls[x]?$", recursive = TRUE, full.names = TRUE)
# 
# # 3) Optional: auf Jahre 2018–2021 einschränken (falls im Dateinamen enthalten)
# files <- files[str_detect(files, "2018|2019|2020|2021")]
# 
# # 4) Deine Modify-Funktion (leicht angepasst, ansonsten unverändert)
# modify_one <- function(df, df_name) {
#   # 0) Spaltennamen vereinheitlichen (Zeilenumbrüche/Mehrfach-Spaces entfernen)
#   nm <- names(df)
#   nm <- gsub("\n", " ", nm)
#   nm <- gsub("\\s+", " ", nm)
#   nm <- trimws(nm)
#   nm <- gsub("^Zielkreis-\\s*Schlüssel$", "Zielkreis-Schlüssel", nm)
#   nm <- gsub("^Herkunftskreis-\\s*Schlüssel$", "Herkunftskreis-Schlüssel", nm)
#   nm <- gsub("^Zielkreis-\\s*Kreistext$", "Zielkreis-Kreistext", nm)
#   names(df) <- nm
#   
#   yr <- as.integer(stringr::str_extract(df_name, "\\b20\\d{2}\\b|\\b19\\d{2}\\b"))
#   
#   # 1) Umbenennen (defensiv)
#   df <- df %>%
#     rename(
#       zielkreis            = any_of("Zielkreis-Kreistext"),
#       zielkreis_ags        = any_of("Zielkreis-Schlüssel"),
#       herkunftskreis       = any_of("Herkunftskreis-Kreistext"),
#       herkunftskreis_ags   = any_of("Herkunftskreis-Schlüssel"),
#       altersgruppe         = any_of("Altersgruppen")
#     )
#   
#   # 2) führende 0 in zielkreis_ags entfernen
#   if ("zielkreis_ags" %in% names(df)) {
#     df <- df %>% mutate(zielkreis_ags = sub("^0+", "", as.character(zielkreis_ags)))
#   }
#   
#   # 2.1) führende 0 in herkunftskreis_ags entfernen
#   if ("herkunftskreis_ags" %in% names(df)) {
#     df <- df %>% mutate(herkunftskreis_ags = sub("^0+", "", as.character(herkunftskreis_ags)))
#   }
#   
#   # 3) Altersgruppen-Werte mappen
#   if ("altersgruppe" %in% names(df)) {
#     df <- df %>%
#       mutate(
#         altersgruppe = as.character(altersgruppe),
#         altersgruppe = trimws(altersgruppe),
#         altersgruppe = dplyr::recode(
#           altersgruppe,
#           "unter 18"           = "unter 18 Jahre",
#           "18 - 25"            = "18 bis 24 Jahre",
#           "18 bis unter 25"    = "18 bis 24 Jahre",
#           "25 - 30"            = "25 bis 29 Jahre",
#           "25 bis unter 30"    = "25 bis 29 Jahre",
#           "30 - 50"            = "30 bis 49 Jahre",
#           "30 bis unter 50"    = "30 bis 49 Jahre",
#           "50 - 65"            = "50 bis 64 Jahre",
#           "50 bis unter 65"    = "50 bis 64 Jahre",
#           "65 und mehr"        = "65 Jahre und älter",
#           "65 und älter"       = "65 Jahre und älter",
#           .default = altersgruppe,
#           .missing = altersgruppe
#         )
#       )
#   }
#   
#   # 4) saldo_deu_i aus Spalte ...18 (falls vorhanden)
#   if ("...18" %in% names(df)) {
#     # Falls Excel die Spalte schon numerisch liefert: direkt übernehmen
#     if (is.numeric(df[["...18"]])) {
#       df <- df %>% mutate(saldo_deu_i = .data[["...18"]])
#     } else {
#       # Sonst schlicht as.numeric() – das behält "-" bei cleanen Dateien
#       df <- df %>% mutate(saldo_deu_i = suppressWarnings(as.numeric(.data[["...18"]])))
#     }
#   } else if (!("saldo_deu_i" %in% names(df))) {
#     df$saldo_deu_i <- NA_real_
#   }
#   
#   # 5) Jahr ergänzen
#   df <- df %>% mutate(jahr = yr)
#   
#   # 6) nur gewünschte Spalten behalten
#   keep_cols <- c("zielkreis","zielkreis_ags","altersgruppe",
#                  "herkunftskreis","herkunftskreis_ags",
#                  "saldo_deu_i","jahr")
#   df <- df %>%
#     select(any_of(keep_cols))
#   
#   # 7) Zeilen ohne zielkreis entfernen
#   if ("zielkreis" %in% names(df)) {
#     df <- df %>% filter(!is.na(zielkreis) & zielkreis != "")
#   }
#   
#   df
# }
# 
# # 5) Einlesen & vereinheitlichen für ALLE Bundesländer
# read_and_modify <- function(file_path) {
#   df_raw  <- readxl::read_excel(file_path)
#   df_name <- basename(file_path)
#   modify_one(df_raw, df_name)
# }
# 
# dfs_list <- purrr::map(files, read_and_modify)
# 
# # 6) Alles zusammenwerfen und dann je Jahr aufteilen
# kwm_all_2018_2021 <- bind_rows(dfs_list)
# 
# kwm_2018 <- kwm_all_2018_2021 %>% filter(jahr == 2018)
# kwm_2019 <- kwm_all_2018_2021 %>% filter(jahr == 2019)
# kwm_2020 <- kwm_all_2018_2021 %>% filter(jahr == 2020)
# kwm_2021 <- kwm_all_2018_2021 %>% filter(jahr == 2021)
# 
# # Aufräumen (optional)
# rm(base_path, files, dfs_list, kwm_all_2018_2021, read_and_modify, modify_one)


# 22 bis 24 ---------------------------------------------------------------
kwm_22 <- read.csv("input/kreiswanderungen/kreiswanderungen_2022.csv", sep= ";")
kwm_23 <- read.csv("input/kreiswanderungen/kreiswanderungen_2023.csv", sep = ";")
kwm_24 <- read.csv("input/kreiswanderungen/kreiswanderungen_2024.csv", sep = ";")

# Nur relevante Spalten behalten
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
kwm_all <- bind_rows(kwm_22, kwm_23, kwm_24)

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


# Bevölkerung (2022 als Basisjahr, um damit den durchschnittlichen Saldo für die Jahre 22 bis 24 zu berechnen) -------------------------------------------------------------
bev_22 <- read.csv("input/bevoelkerung/bevoelkerung_22.csv", sep = ";")
bev_22 <- bev_22 %>%
  filter(X3_variable_label == "Staatsangehörigkeit" & X3_variable_attribute_label == "Deutschland") %>%   # nur Deutschland-Daten
  select(
    region = X1_variable_attribute_label,
    ags = X1_variable_attribute_code,
    altersgruppe = X2_variable_attribute_label,
    value
  ) %>%
  mutate(ags = sub("^0+", "", as.character(ags)))

bev_22 <- bev_22 %>%
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

# 2. Analyse -----------------------------------------------------------------

# 1) Kernergebnis: Saldo (insgesamt) je Kreis x Altersgruppe x Jahr
saldo_kreis <- kwm_all %>%
  group_by(jahr, zielkreis_ags, zielkreis, altersgruppe) %>%
  summarise(
    saldo_ins_gesamt = sum(saldo_deu_i, na.rm = TRUE),  # Wanderungssaldo insgesamt
    .groups = "drop"
  )

saldo_kreis <- saldo_kreis %>%
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

# # --- Falls Jahre vor 2022 inkludiert: Eisenach -> Wartburgkreis addieren und Eisenach-Zeile löschen ---
# 
# # 1) Spaltenbereich: von x18_bis_24_jahre bis total
# cols <- which(names(saldo_kreis) == "x18_bis_24_jahre") :
#   which(names(saldo_kreis) == "total")
# 
# # 2) Summen aus "Eisenach, Stadt" holen (falls vorhanden)
# eisenach_vals <- colSums(
#   saldo_kreis[saldo_kreis$zielkreis == "Eisenach, Stadt", cols, drop = FALSE],
#   na.rm = TRUE
# )
# 
# # 3) Index des Wartburgkreis finden
# wartburg_idx <- which(saldo_kreis$zielkreis == "Wartburgkreis")
# 
# # 4) Addieren (nur wenn beide existieren)
# if (length(wartburg_idx) == 1 && length(eisenach_vals) > 0) {
#   saldo_kreis[wartburg_idx, cols] <-
#     sweep(saldo_kreis[wartburg_idx, cols, drop = FALSE], 2, eisenach_vals, `+`)
# }
# 
# # 5) Eisenach-Zeile entfernen
# saldo_kreis <- dplyr::filter(saldo_kreis, zielkreis != "Eisenach, Stadt")


# 2) Bevölkerung mit Salden joinen
saldo_kreis <- saldo_kreis %>%
  left_join(bev_22, by = c("zielkreis_ags" = "ags"))


saldo_kreis <- saldo_kreis %>%
  mutate(
    # Jährlicher Wanderungssaldo je 1.000 Personen
    p_unter_18_jahre        = unter_18_jahre      / 3 / bev_unter_18_jahre      * 100,
    p_x18_bis_24_jahre      = x18_bis_24_jahre    / 3 / bev_x18_bis_24_jahre    * 100,
    p_x25_bis_29_jahre      = x25_bis_29_jahre    / 3 / bev_x25_bis_29_jahre    * 100,
    p_x30_bis_49_jahre      = x30_bis_49_jahre    / 3 / bev_x30_bis_49_jahre    * 100,
    p_x50_bis_64_jahre      = x50_bis_64_jahre    / 3 / bev_x50_bis_64_jahre    * 100,
    p_x65_jahre_und_alter   = x65_jahre_und_alter / 3 / bev_x65_jahre_und_alter * 100
  )

saldo_kreis <- saldo_kreis %>%
  mutate(
    # AGS immer 5-stellig machen
    zielkreis_ags = str_pad(as.character(zielkreis_ags), width = 5, pad = "0"),
    
    # Gesamtbevölkerung (Summe aller Altersgruppen)
    bev_gesamt = bev_unter_18_jahre + bev_x18_bis_24_jahre + bev_x25_bis_29_jahre +
      bev_x30_bis_49_jahre + bev_x50_bis_64_jahre + bev_x65_jahre_und_alter,
    
    # Je 100 Personen
    p_wanderungssaldo_gesamt = total / 3 / bev_gesamt * 100,
    
    # Wanderungssaldo 18–49 Jahre
    wanderungssaldo_18_bis_49 = x18_bis_24_jahre + x25_bis_29_jahre + x30_bis_49_jahre,
    
    # Bevölkerung 18–49 Jahre
    bev_18_bis_49 = bev_x18_bis_24_jahre + bev_x25_bis_29_jahre + bev_x30_bis_49_jahre,
    
    # Je 100 Personen
    p_wanderungssaldo_18_bis_49 = wanderungssaldo_18_bis_49 / 3 / bev_18_bis_49 * 100,
    
    # Wanderungssaldo 18–29 Jahre
    wanderungssaldo_18_bis_29 = x18_bis_24_jahre + x25_bis_29_jahre,
    
    # Bevölkerung 18–29 Jahre
    bev_18_bis_29 = bev_x18_bis_24_jahre + bev_x25_bis_29_jahre,
    
    # Je 100 Personen
    p_wanderungssaldo_18_bis_29 = wanderungssaldo_18_bis_29 / 3 / bev_18_bis_29 * 100,
    
    # Bevölkerungsanteil 18-24
    bev_anteil_1824 = bev_x18_bis_24_jahre / bev_gesamt * 100
  )


# 3. Datensätze schreiben -------------------------------------------------
# Hilfsfunktion: 5- bis 7-stellige Zahlen mit schmalem Leerzeichen trennen
format_5_6 <- function(x) {
  x_round <- round(x)
  ifelse(
    abs(x_round) >= 10000 & abs(x_round) <= 9999999,
    format(x_round, big.mark = "\u202F", scientific = FALSE),
    format(x_round, scientific = FALSE)
  )
}


# 3.1 Saldo 18-24 ---------------------------------------------------------
saldo_kreis <- saldo_kreis %>%
  mutate(
    # Bevölkerungszahl 18–24 auf 10er runden
    bev_18_24_round = round(bev_x18_bis_24_jahre / 10) * 10,
    bev_18_24_str   = format_5_6(bev_18_24_round),
    
    # Saldo 18–24 absolut (wie bisher in deinem Tooltip)
    saldo_18_24_str = format_5_6(abs(x18_bis_24_jahre)),
    
    # numerisch gerundet
    p_18_24_value = round(p_x18_bis_24_jahre, 1),
    
    # ohne führendes Leerzeichen formatieren
    p_18_24_formatted = format(
      p_18_24_value,
      big.mark     = "\u202F",
      decimal.mark = ",",
      scientific   = FALSE,
      trim         = TRUE
    ),
    
    # Pluszeichen sauber vorne anfügen (ohne Leerzeichen)
    p_18_24_str = ifelse(
      p_18_24_value > 0,
      paste0("+", p_18_24_formatted),
      p_18_24_formatted
    ),
    
    # Textbaustein je nach Vorzeichen des absoluten Saldos
    richtung = dplyr::case_when(
      x18_bis_24_jahre > 0 ~ "zugezogen als fortgezogen",
      x18_bis_24_jahre < 0 ~ "fortgezogen als zugezogen",
      TRUE                 ~ "zugezogen wie fortgezogen"
    ),
    
    tooltip = dplyr::case_when(
      x18_bis_24_jahre == 0 ~ paste0(
        "Hier leben rund <b>", bev_18_24_str, " deutsche Personen</b> zwischen 18 und 24 Jahren. ",
        "Zwischen 2022 und 2024 sind ebenso viele 18- bis 24-Jährige zugezogen wie fortgezogen. ",
        "Das entspricht einem durchschnittlichen jährlichen Wanderungssaldo von ",
        "<b>", p_18_24_str, " je 100 Personen</b>."
      ),
      TRUE ~ paste0(
        "Hier leben rund <b>", bev_18_24_str, " deutsche Personen</b> zwischen 18 und 24 Jahren. ",
        "Zwischen 2022 und 2024 sind <b>", saldo_18_24_str, " mehr 18- bis 24-Jährige</b> ",
        richtung, ". ",
        "Das entspricht einem durchschnittlichen jährlichen Wanderungssaldo von ",
        "<b>", p_18_24_str, " je 100 Personen</b>."
      )
    )
  )




saldo_kreis %>% 
  select(zielkreis_ags, zielkreis, p_x18_bis_24_jahre, tooltip) %>%
  write.csv("output/18_bis_24_wanderungssaldo.csv", row.names = FALSE)

# 3.2 Saldo 30-49 ---------------------------------------------------------

saldo_kreis <- saldo_kreis %>%
  mutate(
    # Bevölkerungszahl 30–49 auf 10er runden
    bev_30_49_round = round(bev_x30_bis_49_jahre / 10) * 10,
    bev_30_49_str   = format_5_6(bev_30_49_round),
    
    # Saldo 30–49 absolut (für die Formulierung "mehr ...")
    saldo_30_49_str = format_5_6(abs(x30_bis_49_jahre)),
    
    # --- Wanderungssaldo 30–49 je 1.000 Personen ------------------------
    
    # numerisch gerundet
    p_30_49_value = round(p_x30_bis_49_jahre, 1),
    
    # ohne führende Leerzeichen formatieren
    p_30_49_formatted = format(
      p_30_49_value,
      big.mark     = "\u202F",
      decimal.mark = ",",
      scientific   = FALSE,
      trim         = TRUE
    ),
    
    # Pluszeichen bei positiven Werten direkt davor (ohne Leerzeichen)
    p_30_49_str = ifelse(
      p_30_49_value > 0,
      paste0("+", p_30_49_formatted),
      p_30_49_formatted
    ),
    
    # --------------------------------------------------------------------
    
    # Textbaustein je nach Vorzeichen des Saldos
    richtung = dplyr::case_when(
      x30_bis_49_jahre > 0 ~ "zugezogen als fortgezogen",
      x30_bis_49_jahre < 0 ~ "fortgezogen als zugezogen",
      TRUE                 ~ "zugezogen wie fortgezogen"
    ),
    
    tooltip = dplyr::case_when(
      x30_bis_49_jahre == 0 ~ paste0(
        "Hier leben rund <b>", bev_30_49_str, " deutsche Personen</b> zwischen 30 und 49 Jahren. ",
        "Zwischen 2022 und 2024 sind ebenso viele 30- bis 49-Jährige zugezogen wie fortgezogen. ",
        "Das entspricht einem durchschnittlichen jährlichen Wanderungssaldo von ",
        "<b>", p_30_49_str, " je 100 Personen</b>."
      ),
      TRUE ~ paste0(
        "Hier leben rund <b>", bev_30_49_str, " deutsche Personen</b> zwischen 30 und 49 Jahren. ",
        "Zwischen 2022 und 2024 sind <b>", saldo_30_49_str, " mehr 30- bis 49-Jährige</b> ",
        richtung, ". ",
        "Das entspricht einem durchschnittlichen jährlichen Wanderungssaldo von ",
        "<b>", p_30_49_str, " je 100 Personen</b>."
      )
    )
  )

saldo_kreis %>% 
  select(zielkreis_ags, zielkreis, p_x30_bis_49_jahre, tooltip) %>%
  write.csv("output/30_bis_49_wanderungssaldo.csv", row.names = FALSE)