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

# Užpildome trūkstamas reikšmes
df[ ] <- df %>%
  mutate(across(where(is.numeric),
                ~ ifelse(is.na(.), median(., na.rm=TRUE), .)))




# 5 Nustatyti taškus atsiskyrėlius, pašalinti juos iš duomenų aibės.
# pasirenkam kintamuosius
X <- df[, feats]

# IQR klasifikatorius
classify_iqr <- function(x){
  Q1 <- quantile(x,.25,na.rm=TRUE); Q3 <- quantile(x,.75,na.rm=TRUE); I <- Q3-Q1
  il <- Q1-1.5*I; ih <- Q3+1.5*I; ol <- Q1-3*I; oh <- Q3+3*I
  ifelse(x<ol | x>oh, "extreme", ifelse(x<il | x>ih, "mild", "normal"))
}

# suvestinė per požymius (normal/mild/extreme)
outlier_summary <- sapply(X, function(col) table(classify_iqr(col)))
n <- nrow(X)
out_tbl <- do.call(rbind, lapply(names(outlier_summary), function(v){
  tab <- outlier_summary[[v]]
  normal  <- as.integer(tab["normal"]); mild <- as.integer(tab["mild"]); extreme <- as.integer(tab["extreme"])
  data.frame(
    variable     = v,
    normal       = ifelse(is.na(normal),  0, normal),
    mild         = ifelse(is.na(mild),    0, mild),
    extreme      = ifelse(is.na(extreme), 0, extreme),
    mild_proc     = round(100*ifelse(is.na(mild),0,mild)/n, 1),
    extreme_proc  = round(100*ifelse(is.na(extreme),0,extreme)/n, 1)
  )
}))
out_tbl

# eilutės su bent vienu EXTREME, pašalinam tik jas
extreme_flags <- apply(sapply(X, function(col) classify_iqr(col)=="extreme"), 1, any)
X_no_extreme <- X[!extreme_flags, , drop=FALSE]

# aprašomoji statistika po outlier'iu pasalinimo

tbl_po_pasalinimo <- make_stats_table(as.data.frame(X_no_extreme), colnames(X_no_extreme))

tbl_po_pasalinimo %>%
  kable(digits=3, caption="Aprašomoji statistika (Po extreme outlier'ių pašalinimo)") %>%
  kable_styling(full_width=FALSE)






# 6 Sunormuoti duomenų aibę naudojant du normavimo metodus: pagal vidurkį ir dispersiją, min - max.

# Z-score normalizacija: vidurkis ir dispersija
X_z <- as.data.frame(scale(X_no_extreme, center = TRUE, scale = TRUE))

# Min–Max normalizacija: (x - min) / (max - min)
range01 <- function(x){
  r <- range(x, na.rm = TRUE)
  if (r[1] == r[2]) rep(0.5, length(x)) else (x - r[1])/(r[2] - r[1])
}
X_mm <- as.data.frame(lapply(X_no_extreme, range01))

# Aprašomosios statistikos lentelės
tbl_z  <- make_stats_table(X_z,  colnames(X_z)) %>%
  mutate(across(where(is.numeric), ~ round(., 3)))
tbl_mm <- make_stats_table(X_mm, colnames(X_mm)) %>%
  mutate(across(where(is.numeric), ~ round(., 3)))


tbl_z %>%
  kable(digits = 3, caption = "Aprašomoji statistika (pagal vidurkį ir dispersija po normalizavimo)") %>%
  kable_styling(full_width = FALSE) %>%
  print()

tbl_mm %>%
  kable(digits = 3, caption = "Aprašomoji statistika (Min–Max po normalizavimo)") %>%
  kable_styling(full_width = FALSE) %>%
  print()

# Bar chart’ai: požymių vidurkiai po normalizavimo
df_means_z  <- tibble(feature = colnames(X_z),
                      mean    = colMeans(X_z,  na.rm = TRUE))
df_means_mm <- tibble(feature = colnames(X_mm),
                      mean    = colMeans(X_mm, na.rm = TRUE))

# Vidurkio ir dispersijos bar chart'as
p_z <- ggplot(df_means_z, aes(x = feature, y = mean)) +
  geom_col(fill = "steelblue") +
  geom_hline(yintercept = 0, linewidth = 0.6) +
  coord_flip() +
  labs(title = "Z-score normalizacija: požymių vidurkiai",
       x = NULL, y = "Vidurkis (SD vienetais)") +
  theme_minimal(base_size = 12)

# Min–Max bar chart'as
p_mm <- ggplot(df_means_mm, aes(x = feature, y = mean)) +
  geom_col(fill = "darkorange") +
  coord_flip(ylim = c(0,1)) +
  labs(title = "Min–Max normalizacija: požymių vidurkiai",
       x = NULL, y = "Vidurkis (0–1)") +
  theme_minimal(base_size = 12)

# chartai
p_z
p_mm                              
