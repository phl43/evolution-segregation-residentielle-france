library(tidyverse)
library(readxl)

# calcule l'indice d'isolement d'un groupe par rapport à une population de référence
indice_isolement <- function (groupe, population_référence) {
  total_groupe <- sum(groupe, na.rm = TRUE)
  
  sum(groupe / total_groupe * groupe / population_référence, na.rm = TRUE)
}

# calcule le pourcentage des membres d'un groupe qui vivent à un endroit où leur
# proportion dans la population de référence est supérieure à un certain seuil
fonction_répartition_complémentaire <- function (groupe, population_référence, seuil) {
  total_groupe <- sum(groupe, na.rm = TRUE)
  
  sum(groupe / total_groupe * ifelse(groupe / population_référence > seuil, 1, 0), na.rm = TRUE)
}

données_TRIRIS <- read_excel(
  "données_recensement.xlsx",
  sheet = "recensement_TRIRIS",
  na = "NA"
) %>%
  mutate(
    enfants_non_euro = ENF_DESC_ou_IMM_NON_EURO / 100 * ENF_POP / 100 * Population,
    enfants = ENF_POP / 100 * Population,
    proportion_enfants_non_euro = enfants_non_euro / enfants
  )

données_TRIRIS_1990 <- filter(données_TRIRIS, RPOP == 1990)
données_TRIRIS_1999 <- filter(données_TRIRIS, RPOP == 1999)
données_TRIRIS_2010 <- filter(données_TRIRIS, RPOP == 2010)
données_TRIRIS_2015 <- filter(données_TRIRIS, RPOP == 2015)

fonction_répartition_complémentaire_enfants_non_euro_1990 <- partial(
  fonction_répartition_complémentaire,
  données_TRIRIS_1990$enfants_non_euro,
  données_TRIRIS_1990$enfants
)

fonction_répartition_complémentaire_enfants_non_euro_1999 <- partial(
  fonction_répartition_complémentaire,
  données_TRIRIS_1999$enfants_non_euro,
  données_TRIRIS_1999$enfants
)

fonction_répartition_complémentaire_enfants_non_euro_2010 <- partial(
  fonction_répartition_complémentaire,
  données_TRIRIS_2010$enfants_non_euro,
  données_TRIRIS_2010$enfants
)

fonction_répartition_complémentaire_enfants_non_euro_2015 <- partial(
  fonction_répartition_complémentaire,
  données_TRIRIS_2015$enfants_non_euro,
  données_TRIRIS_2015$enfants
  )

répartition_enfants_non_euro_1990 <- tibble(
  année = 1990,
  seuil = seq(0, 1, 0.01),
  proportion = unlist(map(seuil, fonction_répartition_complémentaire_enfants_non_euro_1990))
)

répartition_enfants_non_euro_1999 <- tibble(
  année = 1999,
  seuil = seq(0, 1, 0.01),
  proportion = unlist(map(seuil, fonction_répartition_complémentaire_enfants_non_euro_1999))
)

répartition_enfants_non_euro_2010 <- tibble(
  année = 2010,
  seuil = seq(0, 1, 0.01),
  proportion = unlist(map(seuil, fonction_répartition_complémentaire_enfants_non_euro_2010))
)

répartition_enfants_non_euro_2015 <- tibble(
  année = 2015,
  seuil = seq(0, 1, 0.01),
  proportion = unlist(map(seuil, fonction_répartition_complémentaire_enfants_non_euro_2015))
)

répartition_enfants_non_euro <- bind_rows(
  répartition_enfants_non_euro_1990,
  répartition_enfants_non_euro_1999,
  répartition_enfants_non_euro_2010,
  répartition_enfants_non_euro_2015,
) %>%
  mutate(
    année = factor(année)
  )

ggplot(répartition_enfants_non_euro, aes(x = seuil, y = proportion, group = année, color = année)) +
  geom_line(size = 1) +
  theme_bw() +
  ggtitle("Pourcentage des enfants immigrés non-européens ou vivant avec au moins un parent immigré non-européen qui habitent dans un quartier où les enfants immigrés non-européens\nou vivant avec au moins un parent immigré non-européen représentent plus d'un certain seuil des moins de 18 ans dans les unités urbaines de plus de 100 000 habitants\n(à l'échelle des TRIRIS après exclusion de ceux pour lesquels les données ne sont pas disponibles à cause du secret statistique)") +
  xlab("Seuil ") +
  ylab("Pourcentage") +
  labs(color = "Année") +
  scale_x_continuous(
    labels = scales::percent_format(accuracy = 1)
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1)
  ) +
  theme(
    plot.title = element_text(hjust = 0.5)
    ) +
  labs(caption = "Source : base Saphir de l'INSEE - France Stratégie (https://strategie.gouv.fr/publications/evolution-de-segregation-residentielle-france) - Graphique par Philippe Lemoine (@phl43)") +
  ggsave(
    "Répartition des enfants immigrés non-européens ou vivant avec au moins un parent immigré non-européen.png",
    width = 15,
    height = 7.5
  )