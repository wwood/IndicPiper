# Example: aggregate a single Sandpiper sample's genus profile by indicator
# habitat using the pre-built IndicPiper v2 database, and plot the result.
#
# The genus profile below comes straight from the Sandpiper API for
# SRR34514425, a human gut metagenome from:
# Fernandes, R., Jabbarizadeh, B., Rajeh, A. et al. Fecal microbiota
# transplantation plus immunotherapy in metastatic renal cell carcinoma: the
# phase 1 PERFORM trial. Nat Med 32, 1325-1336 (2026).
# https://doi.org/10.1038/s41591-025-04183-8
# https://sandpiper.qut.edu.au/api/condensed_csv_with_extras/SRR34514425?taxonomy_type=gtdb

library(dplyr)
library(ggplot2)

# Load the pre-built IndicPiper database (habitat indicator genera)
ind <- read.csv("genus_habitat_indicators_v2.csv")

# Load a GTDB condensed taxonomic profile for one sample from Sandpiper
profile <- read.delim("https://sandpiper.qut.edu.au/api/condensed_csv_with_extras/SRR34514425?taxonomy_type=gtdb")

# Keep genus-level rows and strip the rank prefixes so the taxonomy strings
# match the format used in genus_habitat_indicators_v2.csv
genus <- profile %>%
  filter(level == "genus") %>%
  mutate(Taxonomy = gsub("^Root; |d__|p__|c__|o__|f__|g__", "", taxonomy))

# Exact-match genera to their indicator habitat; anything not in the
# database is "Non-indicator"
genus_annotated <- genus %>%
  left_join(ind %>% select(Taxonomy, Habitat), by = "Taxonomy") %>%
  mutate(Habitat = ifelse(is.na(Habitat), "Non-indicator", Habitat))

# Aggregate relative abundance by indicator habitat
habitat_abund <- genus_annotated %>%
  group_by(Habitat) %>%
  summarise(relative_abundance = sum(relative_abundance), .groups = "drop") %>%
  arrange(desc(relative_abundance))

# Plot
p <- ggplot(habitat_abund, aes(x = reorder(Habitat, relative_abundance), y = relative_abundance)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(x = "Indicator habitat", y = "Relative abundance (%)",
       title = "SRR34514425: abundance by indicator habitat") +
  theme_bw()

ggsave("SRR34514425_habitat_abundance.pdf", p, width = 7, height = 5)
