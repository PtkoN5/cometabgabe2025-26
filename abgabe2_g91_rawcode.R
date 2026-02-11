## Packages installieren
install.packages("dplyr")
install.packages("ggplot2")
install.packages("plotly")
install.packages("testthat")

# Aufgabe 3
#3.1 : Shut the box
simulate_shut_the_box <- function() {
  tiles <- 1:12
  
  repeat {
    roll <- sum(sample(1:6, 2, replace = TRUE))
    
    # Finde Kombination: Wir gehen von der höchsten Zahl abwärts
    to_remove <- c()
    temp_roll <- roll
    
    for (t in rev(tiles)) {
      if (t <= temp_roll) {
        temp_roll <- temp_roll - t
        to_remove <- c(to_remove, t)
      }
    }
    
    # Nur wenn die Summe exakt aufgeht, werden Tiles entfernt
    if (temp_roll == 0) {
      tiles <- setdiff(tiles, to_remove)
      if (length(tiles) == 0) return(0) # 0 bedeutet "Box shut"
    } else {
      return(sum(tiles)) # Game Over, gib Restsumme zurück
    }
  }
}

# Simulation von 1.000.000 Runden
results <- replicate(1000000, simulate_simple())
win_rate <- mean(results == 0)

cat("Gewinnrate:", win_rate * 100, "%\n")


#3.2: Palindrom(s)
# Funktion zur Prüfung auf ein Palindrom
palindrom <- function(s) {
  # Umwandlung in Kleinschreibung, um Case-Sensitivity zu vermeiden
  s <- tolower(s)
  
  # String in einzelne Zeichen zerlegen
  chars <- strsplit(s, "")[[1]]
  
  # Reihenfolge der Zeichen umkehren
  reversed_chars <- rev(chars)
  
  # Zeichen wieder zu einem String zusammenfügen
  reversed_s <- paste(reversed_chars, collapse = "")
  
  # Vergleich: Original vs. Umgekehrt
  return(s == reversed_s)
}


# Laden des benötigten Pakets
library(testthat)

# Durchführung der Tests
test_that("Palindrom-Funktion arbeitet korrekt", {
  expect_true(palindrom("rentner"))
  expect_false(palindrom("comet"))
  
  
#3.3 : Dataframes
  # --- Schritt 1: Daten einlesen ---
  # Wir lesen die CSV-Datei ein 
  setwd("C:/Users/mikah/Desktop/CoMet")  #Setzen einer Working Direction
  data.frame <- read.csv("bike_sharing_data_(with_NAs).csv")
  df_filtered <- data.frame[df$group==91,]
  anyNA(df_filtered) # Überprüfen, ob es noch fehlende Werte gibt
  colSums(is.na(df_filtered)) # Anzahl der fehlenden Werte pro Spalte anzeigen
  
  # --- Schritt 3: Imputationsverfahren ---
  # Fehlende Werte werden durch den Mittelwert der jeweiligen Spalte ersetzt 
  df_filtered$precipitation[is.na(df_filtered$precipitation)] <- mean(df_filtered$precipitation, na.rm = TRUE)
  df_filtered$average_temperature[is.na(df_filtered$average_temperature)] <- mean(df_filtered$average_temperature, na.rm = TRUE)
  df_filtered$windspeed[is.na(df_filtered$windspeed)] <- mean(df_filtered$windspeed, na.rm = TRUE)
  df_filtered$count[is.na(df_filtered$count)] <- mean(df_filtered$count, na.rm = TRUE)
  
  # Umrechnung der Temperaturspalten von Fahrenheit in Celsius
  df_filtered$average_temperature <- (df_filtered$average_temperature - 32) * 5/9
  df_filtered$min_temperature <- (df_filtered$min_temperature - 32) * 5/9
  df_filtered$max_temperature <- (df_filtered$max_temperature - 32) * 5/9
  
  # Danach folgen wie gehabt die Imputation und die Anomalieprüfung...
  # --- Schritt 4: Untersuchung auf Datenanomalien ---
  # Wir filtern unplausible Werte aus (z.B. negative Windgeschwindigkeiten) 
  # Wir behalten nur Zeilen mit plausiblen Werten (count >= 0, etc.) 
  df_final <- subset(df_filtered, windspeed >= 0 & count >= 0)
  
  # --- Schritt 5: Monat mit der höchsten Gesamtanzahl ---
  # Aggregation der Ausleihen pro Monat 
  monthly_totals <- aggregate(count ~ month_of_year, data = df_final, FUN = sum)
  max_month <- monthly_totals[which.max(monthly_totals$count), ] 
  
  print(paste("Gruppe 91: Der Monat mit den meisten Ausleihen ist Monat", 
              max_month$month_of_year, "mit", round(max_month$count), "Fahrrädern."))
  
  #Aufgabe 4 
  #4.2 : Visualisierung der Daten
  library(ggplot2)
  
  # Da wir 4 Grafiken erstellen, definieren wir ein einheitliches Design (Theme)
  my_theme <- theme_minimal() + 
    theme(plot.title = element_text(face = "bold", hjust = 0.5))
  
  # 1. Zusammenhang: Ausleihen vs. Mittlere Temperatur
  p1 <- ggplot(df_final, aes(x = average_temperature, y = count)) +
    geom_point(alpha = 0.5, color = "orange") +
    geom_smooth(method = "lm", color = "darkred") + # Optional: Trendlinie
    labs(title = "Ausleihen nach Temperatur",
         x = "Mittlere Temperatur (°C)",
         y = "Anzahl ausgeliehener Fahrräder") +
    my_theme
  
  # 2. Zusammenhang: Ausleihen vs. Niederschlagsmenge
  p2 <- ggplot(df_final, aes(x = precipitation, y = count)) +
    geom_point(alpha = 0.5, color = "blue") +
    labs(title = "Ausleihen nach Niederschlag",
         x = "Niederschlagsmenge (mm)",
         y = "Anzahl ausgeliehener Fahrräder") +
    my_theme
  
  # 3. Zusammenhang: Ausleihen vs. Windgeschwindigkeit
  p3 <- ggplot(df_final, aes(x = windspeed, y = count)) +
    geom_point(alpha = 0.5, color = "darkgreen") +
    labs(title = "Ausleihen nach Windgeschwindigkeit",
         x = "Windgeschwindigkeit (km/h)",
         y = "Anzahl ausgeliehener Fahrräder") +
    my_theme
  
  # 4. Zusammenhang: Ausleihen vs. Tag des Jahres
  p4 <- ggplot(df_final, aes(x = day_of_year, y = count)) +
    geom_line(color = "purple") + # Hier bietet sich eine Linie an, um den Verlauf über das Jahr zu sehen
    labs(title = "Saisonaler Verlauf der Ausleihen",
         x = "Tag des Jahres",
         y = "Anzahl ausgeliehener Fahrräder") +
    my_theme
  
  # Anzeigen der Grafiken (in RStudio nacheinander ausführen)
  print(p1)
  print(p2)
  print(p3)
  print(p4)
  
  
  
  
  #4.3 Zusammenhänge Trocken und Regen
  # Vorbereitung: Neue Variable 'regen' erstellen
  # Wir definieren: Regen hat stattgefunden, wenn precipitation > 0
  df_final$regen <- ifelse(df_final$precipitation > 0, "Regentag", "Trocken")
  
  # Grafik 1: Tage, an denen es geregnet hat
  p_regen <- ggplot(subset(df_final, regen == "Regentag"), 
                    aes(x = average_temperature, y = count)) +
    geom_point(alpha = 0.6, color = "steelblue") +
    geom_smooth(method = "lm", color = "darkblue") +
    labs(title = "Zusammenhang Temperatur & Ausleihen (Regentage)",
         x = "Mittlere Temperatur (°C)",
         y = "Anzahl ausgeliehener Fahrräder") +
    theme_minimal()
  
  # Grafik 2: Tage, an denen es NICHT geregnet hat
  p_trocken <- ggplot(subset(df_final, regen == "Trocken"), 
                      aes(x = average_temperature, y = count)) +
    geom_point(alpha = 0.6, color = "orange") +
    geom_smooth(method = "lm", color = "darkred") +
    labs(title = "Zusammenhang Temperatur & Ausleihen (Trockene Tage)",
         x = "Mittlere Temperatur (°C)",
         y = "Anzahl ausgeliehener Fahrräder") +
    theme_minimal()
  
  # Anzeigen der Grafiken
  print(p_regen)
  print(p_trocken)
  
  
  
  #4.4 Verteilung der Variablen
  # 1. Verteilung der Anzahl der Ausleihen
  p44_1 <- ggplot(df_final, aes(x = count)) +
    geom_histogram(binwidth = 10, fill = "blue", color = "white") +
    labs(title = "Verteilung der Fahrradausleihen", x = "Anzahl Ausleihen", y = "Häufigkeit") +
    theme_minimal()
  
  # 2. Verteilung der mittleren Temperatur (jetzt in Celsius!)
  p44_2 <- ggplot(df_final, aes(x = average_temperature)) +
    geom_histogram(binwidth = 2, fill = "orange", color = "white") +
    labs(title = "Verteilung der Temperatur", x = "Temperatur (°C)", y = "Häufigkeit") +
    theme_minimal()
  
  # 3. Verteilung der Niederschlagsmenge
  p44_3 <- ggplot(df_final, aes(x = precipitation)) +
    geom_histogram(binwidth = 0.1, fill = "black", color = "white") +
    labs(title = "Verteilung des Niederschlags", x = "Niederschlag (mm)", y = "Häufigkeit") +
    theme_minimal()
  
  # 4. Verteilung der Windgeschwindigkeit
  p44_4 <- ggplot(df_final, aes(x = windspeed)) +
    geom_histogram(binwidth = 2, fill = "grey", color = "white") +
    labs(title = "Verteilung der Windgeschwindigkeit", x = "Windgeschwindigkeit (km/h)", y = "Häufigkeit") +
    theme_minimal()
  
  # Anzeigen
  print(p44_1)
  print(p44_2)
  print(p44_3)
  print(p44_4)
  
  
  #4.5 Jahreszeiten und Dichteschätzer
  # 1. Jahreszeiten definieren (basierend auf den Monaten)
  df_final$season <- cut(df_final$month_of_year,
                         breaks = c(0, 2, 5, 8, 11, 12),
                         labels = c("Winter", "Frühling", "Sommer", "Herbst", "Winter"))
  
  # Korrektur für den Winter (Dezember ist 12, Jan/Feb sind 1-2)
  # Da cut() bei 12 stoppt, mappen wir Monat 12 manuell auf Winter, falls nötig
  levels(df_final$season)[levels(df_final$season) == "Winter"] <- "Winter"
  
  # 2. Grafik erstellen mit überlagernden Dichteschätzern
  ggplot(df_final, aes(x = count, fill = season)) +
    geom_density(alpha = 0.4) + # alpha sorgt für die geforderte Transparenz
    scale_fill_manual(values = c("Frühling" = "green", 
                                 "Sommer" = "yellow", 
                                 "Herbst" = "red", 
                                 "Winter" = "blue")) +
    labs(title = "Verteilung der Ausleihen nach Jahreszeit",
         x = "Anzahl ausgeliehener Fahrräder",
         y = "Dichte",
         fill = "Jahreszeit") +
    theme_minimal()
  
  
  
  # 4.6 3D-Analyse der Zusammenhänge zwischen Wetterfaktoren und Ausleihen
  library(plotly)
  
  plot_ly(df_final, 
          x = ~average_temperature, 
          y = ~windspeed, 
          z = ~count, 
          type = "scatter3d", 
          mode = "markers",
          marker = list(size = 5, 
                        color = ~count, # Farbe abhängig von der Anzahl
                        colorscale = 'Viridis', # Eine geeignete Farbskala
                        showscale = TRUE)) %>%
    layout(scene = list(xaxis = list(title = 'Temperatur (°C)'),
                        yaxis = list(title = 'Windgeschwindigkeit (km/h)'),
                        zaxis = list(title = 'Anzahl Ausleihen')),
           title = "3D-Analyse: Wetterfaktoren vs. Ausleihen")
  