install.packages(c("tidyverse", "janitor", "kableExtra", "psych"), dep=TRUE)

library(tidyverse)   # duomenų tvarkymui (dplyr, tidyr, readr ir t.t.)
library(janitor)     # tvarkingiems stulpelių pavadinimams
library(kableExtra)  # lentelėms gražiai atvaizduoti
library(psych)       # aprašomajai statistikai (describe, describeBy)

# Nustatome darbinį katalogą, kur yra failas
setwd("C:/Users/kaspa/Desktop/DTMM")

# Nuskaitome CSV failą su kabliataškio skirtuku ir tašku kaip dešimtainiu ženklu
df_raw <- read.csv("EKG_pupsniu_analize.csv",
                   sep = ";", dec = ".", check.names = FALSE)

# Išsaugome stulpelių pavadinimus
nm <- names(df_raw)

# Sutvarkome stulpelių pavadinimus (mažosios raidės, be tarpų, be simbolių)
nm <- janitor::make_clean_names(nm)

# Pakeičiame "/" į "_div_" pavadinimuose, kad būtų patogiau naudoti R
nm <- gsub("/", "_div_", nm, fixed = TRUE)

# Pritaikome pakeistus pavadinimus duomenims
names(df_raw) <- nm

# Automatiškai konvertuojame stulpelius į tinkamus tipus (numeric, factor ir pan.)
df_raw <- readr::type_convert(df_raw, locale = locale(decimal_mark = ".", grouping_mark=","))

# Rankiniu būdu paverčiame kai kuriuos stulpelius į numeric ir label į faktorinius kintamuosius
df_raw <- df_raw %>%
  mutate(
    rr_r_0        = as.numeric(rr_r_0),         # stulpelį rr_r_0 paverčiam į numeric
    rr_r_0_rr_r_1 = as.numeric(rr_r_0_rr_r_1),  # stulpelį rr_r_0_rr_r_1 paverčiam į numeric
    label         = factor(label, levels = c(0,1,2), labels = c("N","S","V")) # klasės su etiketėmis
  )

# Išvalome duomenis: paliekame tik N, S, V klases (be NA ir kitų)
df_clean <- df_raw %>%
  filter(label %in% c("N","S","V")) %>%  # pašaliname NA klases
  droplevels()                           # numetame nenaudojamus faktoriaus lygius

# Patikriname, kiek turime kiekvienos klasės
table(df_clean$label)

# Atsitiktinės atrankos nustatymas (kad rezultatai būtų atkartojami)
set.seed(1111111)

# Iš kiekvienos klasės atsitiktinai paimame po 500 įrašų be pakartojimo
df <- df_clean %>%
  group_by(label) %>%
  slice_sample(n = 500, replace = FALSE) %>%
  ungroup()

# Patikriname, ar iš viso 1500 įrašų ir po 500 kiekvienai klasei
nrow(df)
table(df$label)

# Pasirenkame 6 dominančius požymius analizei
feats <- c("rr_l_0", "rr_l_0_rr_l_1", "rr_r_0", "seq_size", "signal_mean", "signal_std")

# Funkcija aprašomosios statistikos lentelei sugeneruoti
make_stats_table <- function(data, columns){
  # Funkcijų sąrašas, kurios bus taikomos kiekvienam požymiui
  stat_funs <- list(
    Min        = ~min(., na.rm=TRUE),
    Q1         = ~quantile(., 0.25, na.rm=TRUE),
    Mediana    = ~median(., na.rm=TRUE),
    Vidurkis   = ~mean(., na.rm=TRUE),
    Q3         = ~quantile(., 0.75, na.rm=TRUE),
    Max        = ~max(., na.rm=TRUE),
    Dispersija = ~var(., na.rm=TRUE)  # dispersija = sd^2
  )
  
  data %>%
    # Pritaikome visus skaičiavimus pasirinktiems stulpeliams
    summarise(across(all_of(columns), stat_funs, .names="{.col}__{.fn}")) %>%
    # Paverčiame į ilgą formatą: stulpeliuose „Požymis“ ir „Statistika“
    pivot_longer(everything(),
                 names_to=c("Požymis","Statistika"),
                 names_sep="__") %>%
    # Paverčiame atgal į platų formatą: stulpeliai = požymiai, eilutės = statistikos
    pivot_wider(names_from=Požymis, values_from=value) %>%
    # Nustatome statistikos eilučių tvarką
    mutate(Statistika = factor(Statistika,
                               levels=c("Min","Q1","Mediana","Vidurkis",
                                        "Q3","Max","Dispersija"))) %>%
    arrange(Statistika)
}

# Sugeneruojame bendrą aprašomosios statistikos lentelę
tbl_bendra <- make_stats_table(df, feats)

# Gražiai atvaizduojame lentelę kaip pavyzdyje
tbl_bendra %>%
  kable(digits=3, caption="Aprašomoji statistika (bendra)") %>%
  kable_styling(full_width=FALSE)

# 4 užduotis, praleistų reikšmių pildymas
colSums(is.na(df))

# Šaliname trūkstamas reikšmes
df <- df[complete.cases(df), ]

# 5. Nustatyti taškus atsiskyrėlius, pašalinti juos iš duomenų aibės, palyginti, kaip pasikeitė imties statistiniai duomenys.

# IQR klasifikatorius
classify_iqr <- function(x){
  Q1 <- quantile(x,.25,na.rm=TRUE); Q3 <- quantile(x,.75,na.rm=TRUE); H <- Q3-Q1
  il <- Q1-1.5*H; ih <- Q3+1.5*H; ol <- Q1-3*H; oh <- Q3+3*H
  ifelse(x<ol | x>oh, "extreme", ifelse(x<il | x>ih, "mild", "normal"))
}

# Suvestinė per požymius
out_tbl <- lapply(feats, function(f){
  s <- classify_iqr(df[[f]])
  tab <- table(s)
  data.frame(
    variable=f,
    normal=tab["normal"], mild=tab["mild"], extreme=tab["extreme"],
    normal_pct=round(100*tab["normal"]/length(s),1),
    mild_pct  =round(100*tab["mild"]/length(s),1),
    extreme_pct=round(100*tab["extreme"]/length(s),1)
  )
})
out_tbl <- do.call(rbind, out_tbl)
out_tbl

# Extreme pasiskirstymas tarp klasių
extreme_dist <- df %>%
  select(label, all_of(feats)) %>%
  pivot_longer(cols=all_of(feats), names_to="variable", values_to="value") %>%
  group_by(variable) %>%
  mutate(status=classify_iqr(value)) %>%
  filter(status=="extreme") %>%
  count(variable,label,name="extreme_n") %>%
  group_by(variable) %>%
  mutate(share_pct=round(100*extreme_n/sum(extreme_n),1))
extreme_dist

# Pašalinam extreme iš kitų požymių, bet ne iš rr_l_0_rr_l_1 nes čia net 23 proc išskirčių
extreme_rows <- df %>%
  mutate(row_id=row_number()) %>%
  select(row_id, all_of(feats)) %>%
  pivot_longer(cols=-row_id, names_to="variable", values_to="value") %>%
  mutate(status=classify_iqr(value)) %>%
  filter(variable!="rr_l_0_rr_l_1", status=="extreme") %>%
  pull(row_id)

df_no_extreme <- df[-extreme_rows, ]

tbl_po <- make_stats_table(df_no_extreme, feats)

tbl_po %>%
  kable(digits = 3, caption = "Aprašomoji statistika (po outlierių šalinimo)") %>%
  kable_styling(full_width = FALSE)





# 6. Normavimas (Vidurkis ir dispersija, ir Min–Max) ir rezultatų pateikimas

# Pasiimame tik požymius po outlierių šalinimo
X_no_extreme <- df_no_extreme[, feats, drop = FALSE]

# Normalizuojame tik skaitinius stulpelius
num_cols <- sapply(X_no_extreme, is.numeric)
Xn <- X_no_extreme[, num_cols, drop = FALSE]

# Pagal vidurkį ir dispersiją. Skaičiuojama pagal z-score, kur naudojamas standartinis nuokrypis, bet jis yra šaknis iš dispersijos
X_z  <- as.data.frame(scale(Xn, center = TRUE, scale = TRUE))

# Min–Max
range01 <- function(x){
  r <- range(x, na.rm = TRUE)
  if (r[1] == r[2]) rep(0.5, length(x)) else (x - r[1])/(r[2] - r[1])
}
X_mm <- as.data.frame(lapply(Xn, range01))

# Aprašomosios statistikos lentelės
tbl_z  <- make_stats_table(X_z,  names(Xn)) %>% mutate(across(where(is.numeric), ~ round(., 3)))
tbl_mm <- make_stats_table(X_mm, names(Xn)) %>% mutate(across(where(is.numeric), ~ round(., 3)))

tbl_z %>%
  kable(digits = 3, caption = "Aprašomoji statistika (po normavimo pagal vidurkį ir dispersiją)") %>%
  kable_styling(full_width = FALSE) %>% print()

tbl_mm %>%
  kable(digits = 3, caption = "Aprašomoji statistika (po normavimo Min–Max)") %>%
  kable_styling(full_width = FALSE) %>% print()

# Bar chart’ai: požymių vidurkiai po normavimo
df_means_z  <- tibble(feature = names(Xn), mean = colMeans(X_z[, names(Xn)],  na.rm = TRUE))
df_means_mm <- tibble(feature = names(Xn), mean = colMeans(X_mm[, names(Xn)], na.rm = TRUE))

p_z <- ggplot(df_means_z, aes(x = feature, y = mean)) +
  geom_col() +
  geom_hline(yintercept = 0) +
  coord_flip() +
  scale_y_continuous(limits = c(min(df_means_z$mean)*1.1, max(df_means_z$mean)*1.1)) +
  labs(title = "Normavimas pagal vidurkį ir dispersiją: požymių vidurkiai",
       x = "Požymiai", y = "Vidurkis") +
  theme_minimal(base_size = 12)

p_mm <- ggplot(df_means_mm, aes(x = feature, y = mean)) +
  geom_col() +
  coord_flip() +
  scale_y_continuous(limits = c(0,1)) +
  labs(title = "Min–Max normavimas: požymių vidurkiai",
      x = "Požymiai", y = "Vidurkis") +
  theme_minimal(base_size = 12)

p_z; p_mm

# 7. Vizualizavimas

# rr_l_0 ir rr_r_0 taškinė diagrama pagal klases
ggplot(df_no_extreme, aes(x = rr_l_0, y = rr_r_0, color = label)) +
  geom_point(alpha = 0.6) +
  labs( 
    title = "Scatter plot: RR_l_0 vs RR_r_0",
    x = "RR_l_0 (intervalas į dešinę nuo R)",
    y = "RR_r_0 (intervalas į kairę nuo R)"
  ) + theme_minimal() + theme(legend.position = "top")

# Kad kurti plot density diagramas kiekvienam požymiui viename, pridedame bibliotekas
library(ggplot2)
library(tidyr)

# Pasirenkame požymius vizualizacijai
feats_to_plot <- c("rr_l_0", "rr_l_0_rr_l_1", "rr_r_0", "seq_size", "signal_mean", "signal_std")

# Paverčiame duomenis į "long" formatą, kad butu galima pavaizduoti viename grafike visas
df_long <- df_no_extreme %>%
  select(all_of(feats_to_plot), label) %>%
  pivot_longer(cols = all_of(feats_to_plot), names_to = "feature", values_to = "value")

# Plot density diagramos kiekvienam požymiui
ggplot(df_long, aes(x = value, color = label, fill = label)) +
  geom_density(alpha = 0.3) +
  facet_wrap(~feature, scales = "free") +
  labs(title = "Pasirinktų požymių pasiskirstymas pagal klasę (Density Plot)",
       x = "Reikšmė",
       y = "Tankis") +
  theme_minimal() +
  theme(legend.position = "top")

# rr_l_0/rr_l_1 boxplot pagal klases
ggplot(df, aes(x = label, y = rr_l_0_rr_l_1, fill = label)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 4, alpha = 0.6) +
  geom_jitter(width = 0.15, alpha = 0.3, size = 0.7) +
  labs(
    title = "Boxplot: RR_l_0/RR_l_1 pagal klases",
    x = "Klasė",
    y = "RR_l_0/RR_l_1"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

# 8. Pozymiu tarpusavio priklausomybes tyrimas
# Koreliaciju matrica, diagrama

cor_matrix <- cor(df[, feats], use = "complete.obs")

cor_matrix %>%
  round(3) %>%  # round to 3 decimals
  kable(caption = "Pasirinktų požymių koreliacijos matrica") %>%
  kable_styling(full_width = FALSE)

library(corrplot)
cor_matrix <- cor(df_no_extreme[, feats], use="complete.obs")
corrplot(cor_matrix, method = "circle", type = "lower", addCoef.col = "black")

# rr_r_0 ir seq_size pavaizdavimas (teigiama koreliacija)
ggplot(df_no_extreme, aes(x = rr_r_0, y = seq_size, color = label)) +
  geom_point(alpha = 0.6, size = 1.5) +
  facet_wrap(~label, scales = "free") +
  labs(
    title = "Scatter plot: RR_r_0 ir seq_size pagal klases",
    x = "RR_r_0 (intervalas į dešinę nuo R)",
    y = "seq_size (iškirpto segmento dydis)"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

# rr_l_0 ir signal_std pavaizdavimas (neigiama koreliacija)
ggplot(df_no_extreme, aes(x = rr_l_0, y = signal_std, color = label)) +
  geom_point(alpha = 0.6, size = 1.5) +
  facet_wrap(~label, scales = "free") +
  labs(
    title = "Scatter plot: RR_l_0 ir signal_std pagal klases",
    x = "RR_l_0 (intervalas į kairę nuo R)",
    y = "signal_std (signalo reikšmių dispersija)"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")

# rr_l_0 ir signal_mean pavaizdavimas (neutrali "0" koreliacija)
ggplot(df_no_extreme, aes(x = rr_l_0, y = signal_mean, color = label)) +
  geom_point(alpha = 0.6, size = 1.5) +
  facet_wrap(~label, scales = "free") +
  labs(
    title = "Scatter plot: RR_l_0 ir signal_mean pagal klases",
    x = "RR_l_0 (intervalas į kairę nuo R)",
    y = "signal_mean (signalo reikšmių vidurkis)"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "none")
