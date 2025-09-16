library(readxl)
colnames(ma)
str(Mal)
summary(ma)
# Load libraries
library(dplyr)
library(ggplot2)

# ---- Filter dataset for PPCB, Pasteurellose and Brucellose ----
ma_region <- ma %>%
  filter(Maladies %in% c("PPCB", "Pasteurolose", "Brucellose"))

# ---- Table: Summary per Region and Disease ----
table_region <- ma_region %>%
  group_by(Region, Maladies) %>%
  summarise(
    Total_foyers = sum(`Nombre de foyer`, na.rm = TRUE),
    Total_animaux_sensibles = sum(`Nombre des animaux sensible`, na.rm = TRUE),
    Total_cas = sum(`Nombre de cas`, na.rm = TRUE),
    Total_morts = sum(`Nombre de morts`, na.rm = TRUE)
  ) %>%
  arrange(Region, Maladies)

print(table_region)
# Show the full table
print(table_region, width = Inf)

# Or if you want a clearer display
View(table_region)  # opens an Excel-like viewer in RStudio


# ---- Barplot: Cases per Region and Disease ----
ggplot(table_region, aes(x = Region, y = Total_cas, fill = Maladies)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Nombre de cas par Region pour les Maladies Bacteriennes 2025",
       x = "Région", y = "Nombre de cas") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# ---- Line Plot: Trend over Time per Region ----
# Convert date
mal_region$Date <- as.Date(mal_region$`Date de notification de la maladie`,
                           format = "%d/%m/%Y")

# Aggregate by month, region and disease
trend_region <- mal_region %>%
  mutate(Month = format(Date, "%Y-%m")) %>%
  group_by(Region, Maladies, Month) %>%
  summarise(Cas = sum(`Nombre de cas`, na.rm = TRUE)) %>%
  ungroup()

# Plot trends
ggplot(trend_region, aes(x = Month, y = Cas, color = Maladies, group = Maladies)) +
  geom_line(size = 1) +
  geom_point() +
  facet_wrap(~ Region, scales = "free_y") +  # one panel per region
  labs(title = "Trend of Cases Over Time per Region",
       x = "Month", y = "Number of Cases") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

library(dplyr)

# Convert columns to numeric
ma <- ma %>%
  mutate(
    `Nombre de foyer` = as.numeric(`Nombre de foyer`),
    `Nombre des animaux sensible` = as.numeric(`Nombre des animaux sensible`),
    `Nombre de cas` = as.numeric(`Nombre de cas`),
    `Nombre de morts` = as.numeric(`Nombre de morts`)
  )

# ---- Filter dataset for PPCB, Pasteurellose and Brucellose ----
ma_region <- ma %>%
  filter(Maladies %in% c("PPCB", "Pasteurellose", "Brucellose"))

# ---- Table: Summary per Region and Disease ----
table_region <- ma_region %>%
  group_by(Region, Maladies) %>%
  summarise(
    Total_foyers = sum(`Nombre de foyer`, na.rm = TRUE),
    Total_animaux_sensibles = sum(`Nombre des animaux sensible`, na.rm = TRUE),
    Total_cas = sum(`Nombre de cas`, na.rm = TRUE),
    Total_morts = sum(`Nombre de morts`, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(Region, Maladies)

print(table_region, width = Inf)
ggplot(table_region, aes(x = Region, y = Total_animaux_sensibles, fill = Maladies)) +
  geom_col() +
  facet_wrap(~ Maladies, scales = "free_y") +
  labs(
    title = "Animaux sensibles par région et par maladie",
    x = "Région",
    y = "Nombre d’animaux sensibles"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(table_region, aes(x = Region, y = Total_animaux_sensibles, fill = Maladies)) +
  geom_col() +
  facet_wrap(~ Maladies, scales = "free_y") +
  labs(
    title = "Animaux sensibles par région et par maladie",
    x = "Région",
    y = "Nombre d’animaux sensibles"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


