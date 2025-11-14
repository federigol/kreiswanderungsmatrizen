#Skript zum Erstellen von Bevölkerungspyramiden für sämtliche Landkreise
library(needs)
needs(tidyverse, openxlsx, stringr, readxl, svglite)
Sys.setlocale("LC_TIME", "de_DE.UTF-8")

# 1. Load data ------------------------------------------------------------
#Daten stammen aus einer Anfrage von Destatis (Bevölkerung nach Altersjahren 2024)
#bzw.  hier abrufbar: https://www.regionalstatistik.de/genesis//online?operation=table&code=12411-04-02-4&bypass=true&levelindex=1&levelid=1763105249918#abreadcrumb

d_maenner <- read.xlsx("input/bevoelkerung/bevoelkerung_24.xlsx", sheet = 5, startRow = 6)
d_frauen <- read.xlsx("input/bevoelkerung/bevoelkerung_24.xlsx", sheet = 6, startRow = 6)
a_maenner <- read.xlsx("input/bevoelkerung/bevoelkerung_24.xlsx", sheet = 8, startRow = 6)
a_frauen <- read.xlsx("input/bevoelkerung/bevoelkerung_24.xlsx", sheet = 9, startRow = 6)


# 2. Clean data -----------------------------------------------------------

## 2.1 Unnötige Spalte löschen, Spalten umbenennen, Kreise filtern --------
clean_df <- function(df) {
  
  # 1. Erste Spalte löschen
  df <- df[, -1]
  
  # 2. Grund-Spaltennamen
  names(df)[1:3] <- c("ags", "region", "gesamt")
  
  # 3. Alters-Spalten 5–95 -> 0–90
  names(df)[4:94] <- as.character(0:90)
  
  # 4. AGS normalisieren
  df <- df %>%
    mutate(
      ags = as.character(ags),
      
      # Spezialfall: "02" → "02000"
      ags = if_else(ags == "02", "02000", ags),
    ) %>%
    
    # 5. Nur fünfstellige AGS behalten
    filter(nchar(ags) == 5)
  
  df
}


# Auf alle vier Dataframes anwenden
a_frauen  <- clean_df(a_frauen)
a_maenner <- clean_df(a_maenner)
d_frauen  <- clean_df(d_frauen)
d_maenner <- clean_df(d_maenner)

rm(clean_df)

## 2.2 Interpolation 90+ ---------------------------------------------------
# Problem: In den Destatis-Daten sind alle Menschen 90+ in einer Altersklasse zusammengefasst.
# -->Ich habe mir aus der Bevölkerungsvorausberechnung die Zahlen zur Verteilung in der Altersklasse 90+ 
# für das Jahr 2024 geholt (https://service.destatis.de/bevoelkerungspyramide/index.html#!y=2024&v=2) 
# und interpoliere mit Hilfe dieser Daten die Werte ab einem Alter von 90 Jahren. 
# So können wir die Alterspyramiden bis zum Alter von 100 Jahren darstellen und das Durchschnittsalter 
# berechnen. Nachteil: Die Interpolation ist nicht ganz exakt und berücksichtigt keine regionalen
# Besonderheiten oder Entwicklungen über die Jahre hinweg. 

# Frauen: Verteilung 90–99
frauen_ratio  <- c(81, 51, 39, 30, 23, 16, 11, 7, 4, 2)

# Männer: Verteilung 90–99
maenner_ratio <- c(155, 105, 87, 72, 60, 44, 32, 21, 15, 10)

split_90_to_99 <- function(df, ratio) {
  # Spalte 90 muss existieren
  stopifnot("90" %in% names(df))
  
  # 1. 90+ als numeric holen
  total_90plus <- suppressWarnings(as.numeric(df[["90"]]))
  
  # 2. ratio als numeric
  r <- suppressWarnings(as.numeric(ratio))
  
  if (any(is.na(r))) {
    stop("ratio enthält nicht-numerische Werte. Bitte ratio prüfen.")
  }
  
  # 3. Anteile berechnen
  prop <- r / sum(r)
  
  # 4. Matrix: jede Zeile = total_90plus * prop
  m <- outer(total_90plus, prop)  # nrow(df) x length(r)
  
  colnames(m) <- as.character(90:(89 + length(r)))  # 90–99 bei 10 Werten
  
  # 5. Spalten 90–99 im df setzen/überschreiben
  for (j in seq_len(ncol(m))) {
    df[[colnames(m)[j]]] <- m[, j]
  }
  
  df
}

# Frauen-Daten
a_frauen <- split_90_to_99(a_frauen, frauen_ratio)
d_frauen <- split_90_to_99(d_frauen, frauen_ratio)

# Männer-Daten
a_maenner <- split_90_to_99(a_maenner, maenner_ratio)
d_maenner <- split_90_to_99(d_maenner, maenner_ratio)

rm(frauen_ratio, maenner_ratio, split_90_to_99)


## 2.3 Einen langen df erstellen -------------------------------------------
make_long <- function(df, geschlecht, staatsb) {
  
  # Alle vorhandenen Altersspalten 0–99 finden
  age_cols <- intersect(as.character(0:99), names(df))
  
  df %>%
    # 1. Alle Altersspalten nach numeric casten
    mutate(across(all_of(age_cols), ~ suppressWarnings(as.numeric(.x)))) %>%
    
    # 2. Long-Format bauen
    pivot_longer(
      cols = all_of(age_cols),
      names_to  = "alter",
      values_to = "value"
    ) %>%
    
    # 3. Zusatzinfos ergänzen
    mutate(
      geschlecht          = geschlecht,
      staatsbuergerschaft = staatsb,
      alter = as.integer(alter)
    ) %>%
    
    # 4. Spaltenreihenfolge
    select(ags, region, geschlecht, staatsbuergerschaft, alter, value)
}


a_frauen  <- make_long(a_frauen,  "Weiblich", "Deutsch")
a_maenner <- make_long(a_maenner, "Männlich", "Deutsch")

d_frauen  <- make_long(d_frauen,  "Weiblich", "Ausländisch")
d_maenner <- make_long(d_maenner, "Männlich", "Ausländisch")

df_all <- bind_rows(a_frauen, a_maenner, d_frauen, d_maenner)



rm(a_frauen, a_maenner, d_frauen, d_maenner, make_long)


## 2.4 Namen anpassen ------------------------------------------------------
#Add ", Land" to all LKR which need differentiation from Stadtkreise
# Create a vector of names to modify
names_to_modify <- c(
  "Ansbach", "Aschaffenburg", "Augsburg", "Bamberg", "Bayreuth", "Coburg",
  "Fürth", "Heilbronn", "Hof", "Karlsruhe", "Kassel", "Landshut", "München", "Offenbach", 
  "Passau", "Regensburg", "Rosenheim", "Rostock", "Schweinfurt", "Würzburg"
)

df_all <- df_all %>%
  mutate(
    # 1. Sonderfälle zuerst korrigieren
    region = dplyr::case_when(
      region == "Hamburg (BL)" ~ "Hamburg",
      region == "Oldenburg (Oldenburg), Stadt" ~ "Oldenburg, Stadt",
      TRUE ~ region
    ),
    
    # 2. Landkreise aus deiner Liste zu (Land) machen
    region = ifelse(region %in% names_to_modify,
                    paste0(region, " (Land)"),
                    region),
    
    # 3. Einheitliche Klammern statt Komma
    region = gsub(", Stadt", " (Stadt)", region),
    region = gsub(", Landkreis", " (Land)", region)
  )

# Ohne Split nach Staatsbürgerschaft
df_all_geschlecht <- df_all %>%
  group_by(ags, region, geschlecht, alter) %>%
  summarise(
    value = sum(value, na.rm = TRUE),
    .groups = "drop"
  )

rm(names_to_modify)

# 3. Alterspyramide für jeden Kreis ---------------------------------------
## 3.1 Theme und Farben -------------------------------------------------------------------
colors <- c(
  Männlich = "#8122DD",
  Weiblich = "#FC6B44",
  Männlich_highlighted = "#BF3C81",
  Weiblich_highlighted = "#E4B13C"
)


theme_mobile <- theme(
  legend.position = "none",
  legend.text = element_text(family = "SZSansDigital", size = 30, color = "#EEE9E0"),
  plot.title = element_text(hjust = 0.65, vjust = 0, size = 30*130/300, family = "SZSansDigital", face = "bold", color = "#EEE9E0"),
  plot.subtitle = element_text(hjust = 0, face = "plain", size = 20, vjust = 14, family = "SZSansDigital"),
  plot.title.position = "plot",
  legend.key.width = unit(0.5, "cm"),
  text = element_text(color = "#232238", family = "SZSansDigital", size = 20),
  plot.margin = margin(0.5/8, 2/16, 1/16, 0.5/8, unit = "in"),
  panel.grid = element_blank(),
  panel.grid.major = element_line(color = "#232238", linetype = "blank"),
  axis.text = element_text(size = 5),
  axis.title = element_text(size = 30),
  axis.title.x = element_text(margin = margin(5, 0, 5, 0), color = "#EEE9E0", size = 12),
  axis.text.x = element_text(margin = margin(-10, 0, 0, 0), color = "#EEE9E0", size = 6),
  axis.text.y = element_text(margin = margin(0, 0, 0, 0), color = "#EEE9E0", size = 6),
  legend.margin = margin(-0.5, -0.5, 0, 0),
  legend.justification = c(0, 0.25),
  panel.ontop = TRUE)

theme_desktop <- theme(
  legend.position = "none",
  legend.text = element_text(family = "SZSansDigital", size = 60*130/300, color = "#EEE9E0"),
  plot.title = element_text(hjust = 0.535, vjust = 0, family = "SZSansDigital", face = "bold", size = 70*130/300, color = "#EEE9E0"),
  plot.subtitle = element_text(hjust = 0.5, face = "plain", size = 30*130/300, vjust = 0, family = "SZSansDigital"),
  plot.title.position = "plot",
  legend.key.width = unit(1, "cm"),
  legend.justification = c(0.5, 1),
  text = element_text(color = "#232238", family = "SZSansDigital", size = 12*130/300),
  plot.margin = margin(2.5/6, 6.5/4, 2.5/6, 6/4, unit = "in"),
  panel.grid.major = element_line(color = "#232238", linetype = "blank"),
  axis.text.x = element_text(margin = margin(-10, 0, 5, 0), color = "#EEE9E0", size = 30*130/300),
  axis.text.y = element_text(margin = margin(0, -5, 0, 0), color = "#EEE9E0",  size = 30*130/300),
  axis.text = element_text(size = 60*130/300),
  axis.title = element_text(size = 60*130/300))


## 3.2 Funktion um URLs zu bereinigen ------------------------------------------
sanitize_url <- function(input) {
  
  # 1. Deutsche Umlaute explizit ersetzen (vor iconv)
  sanitized <- input
  sanitized <- gsub("ä", "ae", sanitized)
  sanitized <- gsub("Ä", "Ae", sanitized)
  sanitized <- gsub("ö", "oe", sanitized)
  sanitized <- gsub("Ö", "Oe", sanitized)
  sanitized <- gsub("ü", "ue", sanitized)
  sanitized <- gsub("Ü", "Ue", sanitized)
  sanitized <- gsub("ß", "ss", sanitized)
  
  # 2. Danach restliche Sonderzeichen transliterieren
  sanitized <- iconv(sanitized, from = "UTF-8", to = "ASCII//TRANSLIT", sub = "")
  
  # 3. Leerzeichen durch Unterstriche
  sanitized <- gsub(" ", "_", sanitized)
  
  # 4. Entferne Zeichen, die in Dateinamen stören könnten
  sanitized <- gsub("[^A-Za-z0-9_\\-]", "", sanitized)
  
  return(sanitized)
}


## 3.3 Normale Pyramiden erstellen -----------------------------------------------------
landkreis_names <- unique(df_all_geschlecht$region)
for (landkreis in landkreis_names) {
  df_landkreis <- df_all_geschlecht %>%
    filter(region == landkreis)
  
  p <- df_landkreis %>%
    mutate(AgeMidpoint = (alter + 0.5)) %>%
    ggplot(aes(x = AgeMidpoint, y = value, fill = geschlecht)) +
    geom_bar(data = . %>% filter(geschlecht == "Weiblich"), stat = "identity", position = "dodge", width = 1) +
    geom_bar(data = . %>% filter(geschlecht == "Männlich"), stat = "identity", position = "dodge", width = 1, aes(y = -value)) +
    scale_y_continuous(labels = abs, breaks = NULL) +  
    labs(
      x = "",
      y = "",
      title = landkreis,
      fill = ""
    ) +
    theme_minimal() +
    theme(panel.grid = element_blank()) +
    coord_flip() +
    scale_fill_manual(values = colors) 
  
  # Save the plot for desktop and mobile formats
  landkreis_sanitized <- sanitize_url(landkreis)
  ggsave(
    filename = paste0("output/pyrs/pyr_", landkreis_sanitized, "_desktop.png"),
    plot = p + theme_desktop +  theme(axis.text.x = element_blank(), plot.background = element_blank()),
    device = "png", width = 1440/150, height = 900/150, units = "in", bg = NA, dpi = 300
  )
  
  ggsave(
    filename = paste0("output/pyrs/pyr_", landkreis_sanitized, "_mobile.png"),
    plot = p + theme_mobile + theme(axis.text.x = element_blank(), plot.background = element_blank()),
    device = "png", width = 360/150, height = 640/150, units = "in", bg = NA, dpi = 300
  )
}

## 3.4 Pyramide mit Einfärbung 18-24 -----------------------------------------------------
landkreis_names <- unique(df_all_geschlecht$region)

for (landkreis in landkreis_names) {
  df_landkreis <- df_all_geschlecht %>%
    filter(region == landkreis) %>%
    mutate(
      AgeMidpoint = alter + 0.5,
      fill_cat = case_when(
        geschlecht == "Weiblich" & alter >= 18 & alter <= 24 ~ "Weiblich_highlighted",
        geschlecht == "Männlich" & alter >= 18 & alter <= 24 ~ "Männlich_highlighted",
        TRUE                                                ~ geschlecht
      )
    )
  
  p <- ggplot(df_landkreis, aes(x = AgeMidpoint, fill = fill_cat)) +
    # Frauen positiv
    geom_bar(
      data = ~ subset(.x, geschlecht == "Weiblich"),
      aes(y = value),
      stat = "identity",
      position = "dodge",
      width = 1
    ) +
    # Männer negativ
    geom_bar(
      data = ~ subset(.x, geschlecht == "Männlich"),
      aes(y = -value),
      stat = "identity",
      position = "dodge",
      width = 1
    ) +
    scale_y_continuous(labels = abs, breaks = NULL) +  
    labs(
      x = "",
      y = "",
      title = landkreis,
      fill = ""
    ) +
    theme_minimal() +
    theme(panel.grid = element_blank()) +
    coord_flip() +
    scale_fill_manual(values = colors)
  
  # Dateinamen säubern
  landkreis_sanitized <- sanitize_url(landkreis)
  
  # Desktop
  ggsave(
    filename = paste0("output/pyrs_18_24/pyr_", landkreis_sanitized, "_desktop.png"),
    plot = p + theme_desktop +
      theme(axis.text.x = element_blank(), plot.background = element_blank()),
    device = "png",
    width = 1920/150, height = 1080/150,
    units = "in", bg = NA, dpi = 300
  )
  
  # Mobile
  ggsave(
    filename = paste0("output/pyrs_18_24/pyr_", landkreis_sanitized, "_mobile.png"),
    plot = p + theme_mobile +
      theme(axis.text.x = element_blank(), plot.background = element_blank()),
    device = "png",
    width = 360/150, height = 640/150,
    units = "in", bg = NA, dpi = 300
  )
}



# Für Widget --------------------------------------------------------------
theme_mobile <- theme(
  legend.position = "none",
  legend.text = element_text(family = "SZSansDigital", size = 0),
  plot.title = element_text(hjust = 0.5, vjust = 0, size = 0, family = "SZSansDigital", face = "bold"),
  plot.subtitle = element_text(hjust = 0, face = "plain", size = 0, vjust = 14, family = "SZSansDigital"),
  legend.key.width = unit(0.5, "cm"),
  text = element_text(color = "#EEE9E0", family = "SZSansDigital", size = 0),
  plot.background = element_rect(fill = "#232238"), 
  plot.margin = margin(0, 0, 0, 0, unit = "in"),
  panel.grid = element_blank(),
  axis.text = element_text(size = 8, color = "#EEE9E0"),
  axis.title = element_text(size = 0),
  axis.text.x = element_text(margin = margin(-0, 0, 0, 0), color = "#EEE9E0"),
  axis.text.y = element_text(margin = margin(0, 0, 0, 0), color = "#EEE9E0"),
  legend.margin = margin(0, 0, 0, 0),
  legend.justification = c(0, 0))


landkreis_names <- unique(df_all_geschlecht$region)

for (landkreis in landkreis_names) {
  df_landkreis <- df_all_geschlecht %>%
    filter(region == landkreis) %>%
    mutate(
      AgeMidpoint = alter + 0.5,
      fill_cat = case_when(
        geschlecht == "Weiblich" & alter >= 18 & alter <= 24 ~ "Weiblich_highlighted",
        geschlecht == "Männlich" & alter >= 18 & alter <= 24 ~ "Männlich_highlighted",
        TRUE                                                ~ geschlecht
      )
    )
  
  p <- ggplot(df_landkreis, aes(x = AgeMidpoint, fill = fill_cat)) +
    # Frauen positiv
    geom_bar(
      data = ~ subset(.x, geschlecht == "Weiblich"),
      aes(y = value),
      stat = "identity",
      position = "dodge",
      width = 1
    ) +
    # Männer negativ
    geom_bar(
      data = ~ subset(.x, geschlecht == "Männlich"),
      aes(y = -value),
      stat = "identity",
      position = "dodge",
      width = 1
    ) +
    scale_y_continuous(labels = abs, breaks = NULL) +  
    labs(
      x = "",
      y = "",
      title = landkreis,
      fill = ""
    ) +
    theme_minimal() +
    theme(panel.grid = element_blank()) +
    coord_flip() +
    scale_fill_manual(values = colors)
  
  # Dateinamen säubern
  landkreis_sanitized <- sanitize_url(landkreis)
  
    ggsave(
    filename = paste0("output/widget/pyr_", gsub(" ", "_", landkreis_sanitized), "_widget.png"),
    plot = p + theme_mobile + theme(axis.text.x = element_blank(), plot.background = element_blank()),
    device = "png", width = 450/150, height = 450/150, units = "in", bg = NA, dpi = 300
  )
}


