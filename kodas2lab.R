
install.packages(c("tidyverse", "janitor", "kableExtra", "psych"), dep=TRUE)

library(tidyverse)   # duomenų tvarkymui (dplyr, tidyr, readr ir t.t.)
library(janitor)     # tvarkingiems stulpelių pavadinimams
library(kableExtra)  # lentelėms gražiai atvaizduoti
library(psych)       # aprašomajai statistikai (describe, describeBy)

# Nustatome darbinį katalogą, kur yra failas
wd <- "C:/Users/kaspa/Desktop/Mokslas/DTMM"   # arba "C:\\Users\\kaspa\\Desktop\\Mokslas\\DTMM"
setwd(wd)
getwd()                      # patikrai
file.exists(file.path(wd, "EKG_pupsniu_analize.csv"))  # turi būti TRUE

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
feats <- c("rr_l_0_rr_l_1", "signal_std", "wl_side", "r_val", "p_val", "q_val")

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

#Aprasmoji statistika kiekvienai klasei N S V

stat_funs <- list(
  Min        = ~min(., na.rm = TRUE),
  Q1         = ~quantile(., 0.25, na.rm = TRUE),
  Mediana    = ~median(., na.rm = TRUE),
  Vidurkis   = ~mean(., na.rm = TRUE),
  Q3         = ~quantile(., 0.75, na.rm = TRUE),
  Max        = ~max(., na.rm = TRUE),
  Dispersija = ~var(., na.rm = TRUE)
)

# ---- make table for one class, with optional rounding ----
make_table_for_class <- function(data, class_label, columns, digits = 0) {
  out <- data %>%
    dplyr::filter(label == class_label) %>%
    dplyr::summarise(dplyr::across(dplyr::all_of(columns), stat_funs, .names = "{.col}__{.fn}")) %>%
    tidyr::pivot_longer(dplyr::everything(),
                        names_to = c("Požymis", "Statistika"),
                        names_sep = "__") %>%
    tidyr::pivot_wider(names_from = Požymis, values_from = value) %>%
    dplyr::mutate(
      Statistika = factor(
        Statistika,
        levels = c("Min", "Q1", "Mediana", "Vidurkis", "Q3", "Max", "Dispersija")
      )
    ) %>%
    dplyr::arrange(Statistika)
  
  # rounding for display/export
  if (!is.null(digits)) {
    num_cols <- names(out)[vapply(out, is.numeric, logical(1))]
    out[num_cols] <- lapply(out[num_cols], function(x) round(x, digits))
  }
  out
}

# build the three class tables (0 decimals by default)
tbl_N <- make_table_for_class(df, "N", feats, digits = 3)
tbl_S <- make_table_for_class(df, "S", feats, digits = 3)
tbl_V <- make_table_for_class(df, "V", feats, digits = 3)

tbl_N %>%
  kable(digits=3, caption="Aprašomoji statistika – klasė N") %>%
  kable_styling(full_width = FALSE)

tbl_S %>%
  kable(digits=3, caption="Aprašomoji statistika – klasė S") %>%
  kable_styling(full_width = FALSE)

tbl_V %>%
  kable(digits=3, caption="Aprašomoji statistika – klasė V") %>%
  kable_styling(full_width = FALSE)


# 4 užduotis, praleistų reikšmių pildymas
colSums(is.na(df))
df <- df[complete.cases(df), ]


# ===== 3. Min–Max normavimas su išskirčių tvarkymu (IQR) =====

# 1) IQR 
winsor_iqr <- function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  H  <- Q3 - Q1
  lo <- Q1 - 1.5 * H
  hi <- Q3 + 1.5 * H
  x[x < lo] <- lo
  x[x > hi] <- hi
  x
}

# kuriuos požymius kapoti (RR_l_0/RR_l_1 paliekam kaip yra)
wins_cols <- setdiff(feats, "rr_l_0_rr_l_1")

X_orig <- df[, feats, drop = FALSE]

X_winz <- X_orig
for (nm in wins_cols) {
  X_winz[[nm]] <- winsor_iqr(X_winz[[nm]])
}

# 2) Min–Max normavimas į [0,1]
range01 <- function(x) {
  r <- range(x, na.rm = TRUE)
  if (r[1] == r[2]) rep(0.5, length(x)) else (x - r[1])/(r[2] - r[1])
}

X_mm <- as.data.frame(lapply(X_winz, range01))

# 3) Aprašomoji statistika po normavimo (bus Min≈0, Max≈1)
tbl_mm <- make_stats_table(X_mm, names(X_mm)) %>%
  mutate(across(where(is.numeric), ~round(., 3)))

tbl_mm %>%
  kable(digits = 3, caption = "Aprašomoji statistika (po Min–Max normavimo)") %>%
  kable_styling(full_width = FALSE)

# 4) Jei reikia toliau dirbti su normalizuotais duomenimis ir klasėmis:
df_mm <- bind_cols(label = df$label, X_mm)

# Pvz.: tankio grafikai jau su normuotais požymiais
df_long_mm <- df_mm %>%
  pivot_longer(cols = all_of(feats), names_to = "feature", values_to = "value")

ggplot(df_long_mm, aes(x = value, color = label, fill = label)) +
  geom_density(alpha = 0.3) +
  facet_wrap(~feature, scales = "free") +
  labs(title = "Pasirinktų požymių pasiskirstymas po Min–Max normavimo",
       x = "Normalizuota reikšmė (0–1)", y = "Tankis") +
  theme_minimal() +
  theme(legend.position = "top")

ggplot(df_means_mm, aes(x = reorder(Požymis, Vidurkis), y = Vidurkis)) +
  geom_col(fill = "gray40") +
  coord_flip() +
  labs(
    title = "Požymių vidurkiai normuojant pagal Min–Max reikšmes",
    x = "Požymiai",
    y = "Vidurkis"
  ) +
  theme_minimal(base_size = 12)
