install.packages(c("tidyverse", "janitor", "kableExtra", "psych", "MASS", "smacof", "dplyr", "scales", "cluster", "Rtsne", "factoextra", "ggplot2"), dep=TRUE)

library(tidyverse)   # duomenų tvarkymui (dplyr, tidyr, readr ir t.t.)
library(janitor)     # tvarkingiems stulpelių pavadinimams
library(kableExtra)  # lentelėms gražiai atvaizduoti
library(psych)       # aprašomajai statistikai (describe, describeBy)
library(MASS)
library(smacof)
library(dplyr)
library(scales)
library(cluster)
library(Rtsne)
library(factoextra)
library(ggplot2)
library(dplyr)
library(tidyverse)

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

# ---------------MDS-----------------

# -- Paruošiame matricas (be jokio papildomo normavimo) --
X_norm <- df_mm %>% dplyr::select(all_of(feats)) %>% as.data.frame()
y      <- df_mm$label
X_unn  <- df    %>% dplyr::select(all_of(feats)) %>% as.data.frame()

# -- IQR išskirtys (mild/extreme) skaičiuojamos pagal NENORMUOTUS požymius --
flag_outliers_iqr <- function(df_num) {
  mild <- rep(FALSE, nrow(df_num))
  extr <- rep(FALSE, nrow(df_num))
  for (nm in names(df_num)) {
    x  <- df_num[[nm]]
    Q1 <- quantile(x, 0.25, na.rm=TRUE)
    Q3 <- quantile(x, 0.75, na.rm=TRUE)
    H  <- Q3 - Q1
    lo_m <- Q1 - 1.5*H; hi_m <- Q3 + 1.5*H
    lo_e <- Q1 - 3.0*H; hi_e <- Q3 + 3.0*H
    extr <- extr | x < lo_e | x > hi_e
    mild <- mild | ((x < lo_m | x > hi_m) & !(x < lo_e | x > hi_e))
  }
  tibble(mild_outlier = mild, extreme_outlier = extr)
}
plot_flags <- bind_cols(label = y, flag_outliers_iqr(X_unn))

# -- Helperiai: klasikinis MDS su var. paaiškinimu + piešimas --
mds_cmd <- function(D, k=2) {
  cm  <- cmdscale(D, k=k, eig=TRUE)
  eig <- cm$eig[cm$eig > 0]
  var_expl <- if (length(eig)) sum(eig[1:k]) / sum(eig) else NA_real_
  list(points = as.data.frame(cm$points), var = var_expl)
}
plot_mds_df <- function(df2, title, subtitle=NULL) {
  df2$shape_flag <- dplyr::case_when(
    df2$extreme_outlier ~ "Išorinė išskirtis (extreme)",
    df2$mild_outlier    ~ "Vidinė išskirtis (mild)",
    TRUE                ~ "Įprastas taškas"
  )
  ggplot(df2, aes(Dim1, Dim2, color = label, shape = shape_flag)) +
    geom_point(alpha = 0.9, size = 2.4) +
    scale_shape_manual(values = c("Įprastas taškas" = 16,
                                  "Vidinė išskirtis (mild)" = 17,
                                  "Išorinė išskirtis (extreme)" = 8)) +
    labs(title = title, subtitle = subtitle, x = "Dim 1", y = "Dim 2",
         color = "Klasė", shape = "Išskirtys") +
    theme_minimal(base_size = 12) + theme(legend.position = "top")
}

# -- (A) Klasikinis MDS (euclidean), NORMUOTA --
D_euc_norm <- dist(X_norm, method = "euclidean")
mdsA <- mds_cmd(D_euc_norm, k=2)
dfA  <- bind_cols(setNames(mdsA$points, c("Dim1","Dim2")), plot_flags)
pA <- plot_mds_df(dfA,
                  "Klasikinis MDS (euclidean) – normuota",
                  paste0("Paaiškinta variacija ≈ ", scales::percent(mdsA$var, 0.1))
)

# -- (B) Klasikinis MDS (manhattan), NORMUOTA --
D_man_norm <- dist(X_norm, method = "manhattan")
mdsB <- mds_cmd(D_man_norm, k=2)
dfB  <- bind_cols(setNames(mdsB$points, c("Dim1","Dim2")), plot_flags)
pB <- plot_mds_df(dfB,
                  "Klasikinis MDS (manhattan) – normuota",
                  paste0("Paaiškinta variacija ≈ ", scales::percent(mdsB$var, 0.1))
)

# -- (C) Palyginimas: Klasikinis MDS (euclidean), NENORMUOTA --
D_euc_unn <- dist(X_unn, method = "euclidean")
mdsE <- mds_cmd(D_euc_unn, k=2)
dfE  <- bind_cols(setNames(mdsE$points, c("Dim1","Dim2")), plot_flags)
pE <- plot_mds_df(dfE,
                  "Klasikinis MDS (euclidean) – NENORMUOTA",
                  paste0("Paaiškinta variacija ≈ ", scales::percent(mdsE$var, 0.1))
)

# -- Parodyti grafikus --
print(pA); print(pB); print(pE)

# MDS

# Trustworthiness/Continuity
trustworthiness_continuity_corrected <- function(X_high, X_low, k) {
  n <- nrow(X_high)
  
  # Calculate distance matrices
  dist_high <- as.matrix(dist(X_high))
  dist_low <- as.matrix(dist(X_low))
  
  T_sum <- 0
  C_sum <- 0
  
  for (i in 1:n) {
    # Get distances for point i (excluding self)
    dists_high_i <- dist_high[i, -i]
    dists_low_i <- dist_low[i, -i]
    
    # Get ranks in each space (ranks are for the n-1 other points)
    ranks_high <- rank(dists_high_i, ties.method = "first")
    ranks_low <- rank(dists_low_i, ties.method = "first")
    
    # Get indices of k-nearest neighbors (in the original indexing)
    indices <- (1:n)[-i]  # all other points
    
    neighbors_high <- indices[order(dists_high_i)[1:k]]
    neighbors_low <- indices[order(dists_low_i)[1:k]]
    
    # Trustworthiness: points in low-dim neighbors but not in high-dim
    U <- setdiff(neighbors_low, neighbors_high)
    for (j in U) {
      # Find the rank of j in high-dimensional space
      rank_j_high <- ranks_high[which(indices == j)]
      if (rank_j_high > k) {
        T_sum <- T_sum + (rank_j_high - k)
      }
    }
    
    # Continuity: points in high-dim neighbors but not in low-dim
    V <- setdiff(neighbors_high, neighbors_low)
    for (j in V) {
      rank_j_low <- ranks_low[which(indices == j)]
      if (rank_j_low > k) {
        C_sum <- C_sum + (rank_j_low - k)
      }
    }
  }
  
  # Normalization constant
  if (2*n - 3*k - 1 <= 0) return(c(T = NA, C = NA))
  cst <- 2 / (n * k * (2*n - 3*k - 1))
  
  c(T = max(0, 1 - cst * T_sum), C = max(0, 1 - cst * C_sum))
}

compute_stress1 <- function(D, Y, rescale = TRUE) {
  d  <- as.vector(if (inherits(D, "dist")) D else dist(D))
  dy <- as.vector(dist(as.matrix(Y)))
  if (rescale) { a <- sum(dy * d) / sum(dy^2); dy <- a * dy }
  sqrt(sum((dy - d)^2) / sum(d^2))
}

# --- VAF per cmdscale eigreikšmes (Euclidiniams D – kanoninis, Manhattan atveju – diagnostinis)
var_expl_cmd <- function(D, k = 2) {
  cm  <- cmdscale(D, k = k, eig = TRUE)
  eig <- cm$eig[cm$eig > 0]
  if (!length(eig)) return(NA_real_)
  sum(eig[1:k]) / sum(eig)
}

# --- Vienas paleidimas: SMACOF MDS + metrikos iš tavo trustworthiness_continuity_corrected()
run_metric_mds_TC <- function(D, X_high, title, itmax = 300, k_nn = 10,
                              init = c("torgerson","random"), seed = NULL) {
  init <- match.arg(init)
  Y0 <- NULL
  if (init == "random") {
    if (!is.null(seed)) set.seed(seed)
    n <- if (inherits(D,"dist")) attr(D, "Size") else nrow(D)
    Y0 <- matrix(rnorm(n * 2), ncol = 2)
  }
  fit <- smacof::mds(delta = D, type = "ratio", itmax = itmax, ndim = 2,
                     verbose = FALSE, init = if (is.null(Y0)) "torgerson" else Y0)
  
  Y <- as.data.frame(fit$conf); colnames(Y) <- c("Dim1","Dim2")
  dfp <- bind_cols(Y, plot_flags)
  
  VAF    <- var_expl_cmd(D, k = 2)
  Stress <- compute_stress1(D, Y)
  # Naudojame tavo funkciją – Trust/Continuity skaičiuojami pagal EUCLIDEAN dist() X_high ir Y
  TC     <- trustworthiness_continuity_corrected(as.matrix(X_high), as.matrix(Y), k = k_nn)
  
  sub <- paste0(
    "VAF ≈ ", scales::percent(VAF, accuracy = 0.1),
    " | Stress-1 ≈ ", scales::number(Stress, accuracy = 0.001),
    " | Trust(", k_nn, ") ≈ ", scales::number(TC["T"], accuracy = 0.001),
    " | Continuity(", k_nn, ") ≈ ", scales::number(TC["C"], accuracy = 0.001),
    " | itmax=", itmax
  )
  list(df = dfp, subtitle = sub, title = title)
}

# ================== GRAFIKAI (NAUDOJANT TAVO FUNKCIJĄ) ==================

# 1) NENORMUOTA (EUCLIDEAN)
D_euc_unn <- dist(X_unn,  method = "euclidean")
g_UN_E <- run_metric_mds_TC(D_euc_unn, X_high = X_unn,
                            title = "Metrinis MDS (euclidean) – NENORMUOTA",
                            itmax = 600, k_nn = 10, init = "torgerson")
pEU_UN <- plot_mds_df(g_UN_E$df, g_UN_E$title, g_UN_E$subtitle)

# 2) NORMUOTA (EUCLIDEAN), 3 skirtingi itmax (skirtingos pradinės būsenos, kad metrikos skirtųsi)
D_euc_norm <- dist(X_norm, method = "euclidean")
g_E_100 <- run_metric_mds_TC(D_euc_norm, X_high = X_norm,
                             title = "Metrinis MDS (euclidean) – NORMUOTA",
                             itmax = 100, k_nn = 10, init = "random", seed = 1111111)
g_E_300 <- run_metric_mds_TC(D_euc_norm, X_high = X_norm,
                             title = "Metrinis MDS (euclidean) – NORMUOTA",
                             itmax = 300, k_nn = 10, init = "random", seed = 1111111)
g_E_600 <- run_metric_mds_TC(D_euc_norm, X_high = X_norm,
                             title = "Metrinis MDS (euclidean) – NORMUOTA",
                             itmax = 600, k_nn = 10, init = "random", seed = 1111111)
pEU_100 <- plot_mds_df(g_E_100$df, g_E_100$title, g_E_100$subtitle)
pEU_300 <- plot_mds_df(g_E_300$df, g_E_300$title, g_E_300$subtitle)
pEU_600 <- plot_mds_df(g_E_600$df, g_E_600$title, g_E_600$subtitle)

# 3) NORMUOTA (MANHATTAN), 3 skirtingi itmax (irgi skirtingos pradinės būsenos)
# Pastaba: VAF čia – diagnostinis (ne-Euclidiniams atstumams eigenės gali būti neigiamos)
D_man_norm <- dist(X_norm, method = "manhattan")
g_M_100 <- run_metric_mds_TC(D_man_norm, X_high = X_norm,
                             title = "Metrinis MDS (manhattan) – NORMUOTA",
                             itmax = 100, k_nn = 10, init = "random", seed = 1111111)
g_M_300 <- run_metric_mds_TC(D_man_norm, X_high = X_norm,
                             title = "Metrinis MDS (manhattan) – NORMUOTA",
                             itmax = 300, k_nn = 10, init = "random", seed = 1111111)
g_M_600 <- run_metric_mds_TC(D_man_norm, X_high = X_norm,
                             title = "Metrinis MDS (manhattan) – NORMUOTA",
                             itmax = 600, k_nn = 10, init = "random", seed = 1111111)
pMAN_100 <- plot_mds_df(g_M_100$df, g_M_100$title, g_M_100$subtitle)
pMAN_300 <- plot_mds_df(g_M_300$df, g_M_300$title, g_M_300$subtitle)
pMAN_600 <- plot_mds_df(g_M_600$df, g_M_600$title, g_M_600$subtitle)

# ---- Spausdinimas (pridėtiniai; pA/pB/pE neliečiami) ----
print(pEU_UN)
print(pEU_100); print(pEU_300); print(pEU_600)
print(pMAN_100); print(pMAN_300); print(pMAN_600)

# Naudojant empyrinį, Elbow ir vidutinio silueto metodą įvertinti optimalų klasterių skaičių. Galima naudoti ir kitus rastus klasterių skaičiaus nustatymo algoritmus, mokėti juos paaiškinti.
X <- df_mm %>% dplyr::select(all_of(feats)) %>% as.matrix()

# ---- 3.1 Empyrinis (domain-knowledge) metodas ----
empirical_K <- 3
cat("Empyrinis pasirinkimas (remiantis N/S/V fiziologija): K =", empirical_K, "\n")

# ---- Pagalbinė funkcija: WCSS (within-cluster sum of squares) bet kuriems klasteriams ----
wcss_from_partition <- function(X, clusters) {
  # X: n x p matrica; clusters: ilgio n vektorius (1..K)
  split_idx <- split(seq_len(nrow(X)), clusters)
  wlist <- lapply(split_idx, function(idx) {
    Xi <- X[idx, , drop = FALSE]
    center <- colMeans(Xi)
    sum(rowSums( (Xi - matrix(center, nrow(Xi), ncol(Xi), byrow = TRUE))^2 ))
  })
  sum(unlist(wlist))
}

# ---- 3.2 Alkūnės (Elbow) metodas su hierarchiniu klasterizavimu (Ward.D2) ----
# Idėja: sudarom dendrogramą vieną kartą; kiekvienam K -> cutree -> WCSS
D  <- dist(X, method = "euclidean")
hc <- hclust(D, method = "ward.D2")

K_grid <- 2:10  # gali keisti diapazoną pagal poreikį
elbow_df <- purrr::map_dfr(K_grid, function(k) {
  cl <- cutree(hc, k = k)
  tibble(K = k, WCSS = wcss_from_partition(X, cl))
})

# Normalizuotas WCSS, kad grafikas būtų aiškesnis (nebūtina)
elbow_df <- elbow_df %>%
  mutate(WCSS_norm = WCSS / max(WCSS))

p_elbow <- ggplot(elbow_df, aes(x = K, y = WCSS_norm)) +
  geom_line() + geom_point() +
  labs(title = "Alkūnės (Elbow) metodas (hierarchinis Ward.D2)",
       x = "Klasterių skaičius K", y = "Normalizuotas WCSS") +
  theme_minimal(base_size = 12)

print(p_elbow)

# ---- 3.3 Vidutinio silueto metodas su tais pačiais klasteriais (be k-means) ----
sil_df <- purrr::map_dfr(K_grid, function(k) {
  cl <- cutree(hc, k = k)
  # silhouette() reikalauja skaitinių etikečių 1..K
  sil <- silhouette(cl, D)
  tibble(K = k, silhouette_mean = mean(sil[, "sil_width"]))
})

p_sil <- ggplot(sil_df, aes(x = K, y = silhouette_mean)) +
  geom_line() + geom_point() +
  labs(title = "Vidutinio silueto metodas (hierarchinis Ward.D2)",
       x = "Klasterių skaičius K", y = "Vidutinis silueto koeficientas") +
  theme_minimal(base_size = 12)

print(p_elbow)
print(p_sil)

# ---- 3.4 Reziume: automatinės rekomendacijos iš dviejų grafikų ----
elbow_K  <- elbow_df %>% arrange(WCSS) %>% slice_tail(n = 1) %>% pull(K) # (čia tik informacinis)
silbestK <- sil_df %>% arrange(desc(silhouette_mean)) %>% slice(1) %>% pull(K)

cat("Empyrinis K:", empirical_K, "\n")
cat("informacinis min-WCSS K):", elbow_K, "\n")
cat("Silueto maksimumas K:", silbestK, "\n")

plot(hc, main = "Hierarchinis klasterizavimas (Ward.D2)",
     xlab = "Stebiniai (EKG pūpsniai)", ylab = "Atstumas",
     sub = "", cex = 0.4)

# Pažymime pasirinktą klasterių skaičių (čia K = 3)
rect.hclust(hc, k = 3, border = "red")

###############################################
### K-means klasterizavimas (6 ir 31 požymiai)
###############################################

library(dplyr)
library(ggplot2)
library(cluster)
library(Rtsne)

# Spalvos tikrosioms klasėms (N, S, V)
COLS_CLASS <- c(
  "N" = "#E41A1C",  # raudona
  "S" = "#377EB8",  # mėlyna
  "V" = "#4DAF4A"   # žalia
)

range01 <- function(x) {
  r <- range(x, na.rm = TRUE)
  if (r[1] == r[2]) return(rep(0.5, length(x)))
  (x - r[1]) / (r[2] - r[1])
}

# 6 požymiai (normuotas 6D rinkinys)
X_6 <- df_mm %>%
  dplyr::select(all_of(feats)) %>%
  as.matrix()

# 31 požymis (pilnas rinkinys, tie patys 1500 stebinių)
X_31 <- df %>%
  dplyr::select(where(is.numeric)) %>%          # visi skaitiniai stulpeliai (be label, nes ji factor)
  dplyr::mutate(across(everything(), range01)) %>% 
  as.matrix()

ncol(X_6); ncol(X_31)  # turi būti 6 ir 31



# --- Pagalbinė funkcija analizės pakartojimui ---
analyze_kmeans <- function(X, df_labels, label_text) {
  message("\n====================")
  message("Analizė: ", label_text)
  message("====================\n")
  
  # 1️⃣ Alkūnės ir silueto metodai
  K_grid <- 2:10
  D <- dist(X, method = "euclidean")
  
  elbow_df <- data.frame(
    K = K_grid,
    WCSS = sapply(K_grid, function(k) {
      set.seed(1111111)
      km <- kmeans(X, centers = k, nstart = 25)
      km$tot.withinss
    })
  )
  elbow_df$WCSS_norm <- elbow_df$WCSS / max(elbow_df$WCSS)
  
  p_elbow <- ggplot(elbow_df, aes(x = K, y = WCSS_norm)) +
    geom_line() +
    geom_point() +
    labs(
      title = paste("K-means – Alkūnės (Elbow) metodas –", label_text),
      x = "Klasterių skaičius K",
      y = "Normalizuotas WCSS"
    ) +
    theme_minimal(base_size = 12)
  
  sil_df <- data.frame(
    K = K_grid,
    silhouette_mean = sapply(K_grid, function(k) {
      set.seed(1111111)
      km <- kmeans(X, centers = k, nstart = 25)
      sil <- silhouette(km$cluster, D)
      mean(sil[, "sil_width"])
    })
  )
  
  p_sil <- ggplot(sil_df, aes(x = K, y = silhouette_mean)) +
    geom_line() +
    geom_point() +
    labs(
      title = paste("K-means – vidutinio silueto metodas –", label_text),
      x = "Klasterių skaičius K",
      y = "Vidutinis silueto koeficientas"
    ) +
    theme_minimal(base_size = 12)
  
  print(p_elbow)
  print(p_sil)
  
  # 2️⃣ K-means K=3 ir K=4
  set.seed(1111111)
  km3 <- kmeans(X, centers = 3, nstart = 50)
  set.seed(1111111)
  km4 <- kmeans(X, centers = 4, nstart = 50)
  
  cat("\nSutapimų lentelė – K-means (K=3),", label_text, "\n")
  print(table(Klasė = df_labels, Kmeans3 = km3$cluster))
  
  cat("\nSutapimų lentelė – K-means (K=4),", label_text, "\n")
  print(table(Klasė = df_labels, Kmeans4 = km4$cluster))
  
  # 3️⃣ t-SNE vizualizacija (p=15, lr=500)
  set.seed(1111111)
  tsne <- Rtsne(
    X,
    dims = 2,
    perplexity = 15,
    eta = 500,
    verbose = FALSE,
    max_iter = 1000
  )
  
  tsne_df <- as.data.frame(tsne$Y)
  colnames(tsne_df) <- c("tSNE1", "tSNE2")
  df_vis <- dplyr::bind_cols(
    label = df_labels,
    tsne_df,
    mild_outlier = plot_flags$mild_outlier,
    extreme_outlier = plot_flags$extreme_outlier
  )
  df_vis$kmeans3 <- factor(km3$cluster)
  df_vis$kmeans4 <- factor(km4$cluster)
  
  # 4️⃣ Piešimo funkcija
  plot_tsne_kmeans <- function(df, cluster_col, title) {
    df <- df %>%
      dplyr::mutate(
        alpha_pt = dplyr::case_when(
          extreme_outlier ~ 0.25,
          mild_outlier ~ 0.45,
          TRUE ~ 0.85
        )
      )
    
    ggplot(df, aes(x = tSNE1, y = tSNE2)) +
      stat_ellipse(
        aes_string(group = cluster_col),
        type = "norm",
        linetype = "dashed",
        size = 0.6,
        color = "grey40"
      ) +
      geom_point(
        aes(color = label),
        shape = 18,
        size = 2.2,
        alpha = df$alpha_pt
      ) +
      scale_color_manual(values = COLS_CLASS, name = "Klasė") +
      labs(
        title = title,
        subtitle = paste(label_text, "– Išskirtys pažymėtos mažesniu skaidrumu"),
        x = "t-SNE1", y = "t-SNE2"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        legend.position = "right",
        plot.title = element_text(face = "bold")
      )
  }
  
  p_tsne_k3 <- plot_tsne_kmeans(
    df_vis,
    "kmeans3",
    paste("t-SNE (p=15, lr=500) – klasės spalvos, K-means elipsės (K=3) –", label_text)
  )
  
  p_tsne_k4 <- plot_tsne_kmeans(
    df_vis,
    "kmeans4",
    paste("t-SNE (p=15, lr=500) – klasės spalvos, K-means elipsės (K=4) –", label_text)
  )
  
  print(p_tsne_k3)
  print(p_tsne_k4)
}

############################################
# Paleidžiame analizę abiem rinkiniams
############################################

# a) 6 pasirinkti požymiai
analyze_kmeans(X_6, df_mm$label, "6 požymiai")

# b) 31 požymis (pilnas rinkinys)
analyze_kmeans(X_31, df_mm$label, "31 požymis (pilnas rinkinys)")

#########################################
### K-means metrikų palyginimo lentelė ###
### (6 požymiai vs 31 požymis)
#########################################

library(cluster)
library(mclust)

# Funkcija Purity apskaičiuoti
purity_score <- function(true_labels, pred_labels) {
  tab <- table(true_labels, pred_labels)
  sum(apply(tab, 2, max)) / sum(tab)
}

range01 <- function(x) {
  r <- range(x, na.rm = TRUE)
  if (r[1] == r[2]) return(rep(0.5, length(x)))
  (x - r[1]) / (r[2] - r[1])
}

X_6 <- df_mm %>%
  dplyr::select(all_of(feats)) %>%
  as.matrix()

X_31 <- df %>%
  dplyr::select(where(is.numeric)) %>%   # visi skaitiniai požymiai, be label (ji factor)
  dplyr::mutate(across(everything(), range01)) %>%
  as.matrix()

ncol(X_6); ncol(X_31)


results <- data.frame(K = 2:10)

# Skaičiuojame visoms K reikšmėms
for (k in 2:10) {
  cat("Skaičiuojama K =", k, "\n")
  set.seed(1111111)
  
  # ----- 6 požymiai -----
  km6 <- kmeans(X_6, centers = k, nstart = 25)
  sil6 <- silhouette(km6$cluster, dist(X_6))
  results[results$K == k, "Silhouette_6D"] <- mean(sil6[, "sil_width"])
  results[results$K == k, "ARI_6D"] <- adjustedRandIndex(df_mm$label, km6$cluster)
  results[results$K == k, "Purity_6D"] <- purity_score(df_mm$label, km6$cluster)
  
  # ----- 31 požymis -----
  km31 <- kmeans(X_31, centers = k, nstart = 25)
  sil31 <- silhouette(km31$cluster, dist(X_31))
  results[results$K == k, "Silhouette_31D"] <- mean(sil31[, "sil_width"])
  results[results$K == k, "ARI_31D"] <- adjustedRandIndex(df_mm$label, km31$cluster)
  results[results$K == k, "Purity_31D"] <- purity_score(df_mm$label, km31$cluster)
}

results <- results %>%
  mutate(across(-K, ~ round(., 3)))

print(results)

library(kableExtra)
results %>%
  kbl(caption = "K-means klasterizavimo (6D ir 31D erdvėse) metrikų palyginimas keičiant klasterių skaičių") %>%
  kable_styling(full_width = FALSE, position = "center")

#########################################
### K-means NSTART ###
#########################################
library(cluster)
library(mclust)

purity_score <- function(true_labels, pred_labels) {
  tab <- table(true_labels, pred_labels)
  sum(apply(tab, 2, max)) / sum(tab)
}

nstart_values <- c(1, 5, 10, 25, 50, 100)
res_nstart <- data.frame(nstart = nstart_values)

for (n in nstart_values) {
  set.seed(1111111)
  km <- kmeans(X_6, centers = 3, nstart = n)
  sil <- silhouette(km$cluster, dist(X_6))
  res_nstart[res_nstart$nstart == n, "Silhouette"] <- mean(sil[, "sil_width"])
  res_nstart[res_nstart$nstart == n, "ARI"] <- adjustedRandIndex(df_mm$label, km$cluster)
  res_nstart[res_nstart$nstart == n, "Purity"] <- purity_score(df_mm$label, km$cluster)
  res_nstart[res_nstart$nstart == n, "WCSS"] <- km$tot.withinss
}

print(res_nstart)

res_nstart %>%
  mutate(across(where(is.numeric), ~round(., 3))) %>%  # suapvalinam iki 3 skaičių po kablelio
  kable(
    caption = "K-means (K=3, 6 požymiai) rezultatų priklausomybė nuo parametro nstart",
    align = "c",
    col.names = c("nstart", "Silhouette", "ARI", "Purity", "WCSS")
  ) %>%
  kable_styling(full_width = FALSE, position = "center", font_size = 12)

#########################################
# K-means (K=3, 6 požymiai) centroidų lentelė
#########################################

library(dplyr)
library(kableExtra)

X_6 <- df_mm %>% dplyr::select(all_of(feats))

# Jei km_k3 dar nėra, per-suskaičiuojam:
if (!exists("km_k3")) {
  set.seed(1111111)
  km_k3 <- kmeans(as.matrix(X_6), centers = 3, nstart = 50)
}

# Pridedame klasterio numerį prie duomenų
df_centers_6 <- X_6 %>%
  mutate(Cluster = km_k3$cluster) %>%
  group_by(Cluster) %>%
  summarise(across(all_of(feats), mean, na.rm = TRUE), .groups = "drop") %>%
  # suapvalinam kad būtų įskaitoma
  mutate(across(all_of(feats), ~round(., 3)))

df_centers_6 %>%
  kbl(
    caption = "K-means (K=3, 6 požymiai) klasterių centrai (požymių vidurkiai)",
    align = "c"
  ) %>%
  kable_styling(full_width = FALSE, position = "center")

#########################################
# K-means (K=4, 6 požymiai) centroidų lentelė
#########################################

library(dplyr)
library(kableExtra)

X_6 <- df_mm %>% dplyr::select(all_of(feats))

# Jei km_k4 dar nėra, atliekame klasterizavimą
set.seed(1111111)
km_k4 <- kmeans(as.matrix(X_6), centers = 4, nstart = 50)

# Pridedame klasterio numerį prie duomenų ir apskaičiuojame centrus
df_centers_6_k4 <- X_6 %>%
  mutate(Cluster = km_k4$cluster) %>%
  group_by(Cluster) %>%
  summarise(across(all_of(feats), mean, na.rm = TRUE), .groups = "drop") %>%
  mutate(across(all_of(feats), ~round(., 3)))

df_centers_6_k4 %>%
  kbl(
    caption = "K-means (K=4, 6 požymiai) klasterių centrai (požymių vidurkiai)",
    align = "c"
  ) %>%
  kable_styling(full_width = FALSE, position = "center", font_size = 12)

########################################################
### t-SNE + K-means (K=5, 6 požymiai) su išskirtimis
########################################################

library(dplyr)
library(ggplot2)
library(cluster)
library(Rtsne)

COLS_CLASS <- c(
  "N" = "#E41A1C",
  "S" = "#377EB8",
  "V" = "#4DAF4A"
)

# 6 požymiai (normuotas rinkinys)
X_6 <- df_mm %>%
  dplyr::select(all_of(feats)) %>%
  as.matrix()

labels_true <- df_mm$label

# K-means K = 5
set.seed(1111111)
km5 <- kmeans(X_6, centers = 5, nstart = 25)

# t-SNE
set.seed(1111111)
tsne_6_k5 <- Rtsne(
  X_6,
  dims = 2,
  perplexity = 15,
  eta = 500,
  verbose = FALSE,
  max_iter = 1000
)

tsne_df_6_k5 <- as.data.frame(tsne_6_k5$Y)
colnames(tsne_df_6_k5) <- c("tSNE1", "tSNE2")

plot_df_6_k5 <- dplyr::bind_cols(
  label = labels_true,
  tsne_df_6_k5,
  mild_outlier = plot_flags$mild_outlier,
  extreme_outlier = plot_flags$extreme_outlier
)
plot_df_6_k5$cluster <- factor(km5$cluster)

# Vizualizacija su elipsėmis ir išskirčių skaidrumu
ggplot(plot_df_6_k5, aes(x = tSNE1, y = tSNE2)) +
  stat_ellipse(
    aes(group = cluster),
    type = "norm",
    linetype = "dashed",
    size = 0.6,
    color = "grey40"
  ) +
  geom_point(
    aes(
      color = label,
      alpha = case_when(
        extreme_outlier ~ 0.25,
        mild_outlier    ~ 0.45,
        TRUE            ~ 0.85
      )
    ),
    shape = 18,
    size = 2.2
  ) +
  scale_color_manual(values = COLS_CLASS, name = "Klasė") +
  scale_alpha_identity() +
  labs(
    title = "t-SNE (p=15, lr=500) – klasės spalvos, K-means elipsės (K=5) – 6 požymiai",
    subtitle = "6 požymiai – išskirtys pažymėtos mažesniu skaidrumu",
    x = "t-SNE1", y = "t-SNE2"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold")
  )

tab_k5 <- table(Klasė = df_mm$label, Klasteris = km5$cluster)

print(tab_k5)

tab_k5 %>%
  kbl(
    caption = "Sutapimų lentelė – K-means (K=5), 6 požymiai",
    align = "c"
  ) %>%
  kable_styling(full_width = FALSE, position = "center", font_size = 12)

#########################################
# K-means (K=5, 6 požymiai) centroidų lentelė
#########################################

library(dplyr)
library(kableExtra)

# 6 požymiai (turėtum jau turėti 'feats' apibrėžtą)
X_6 <- df_mm %>% dplyr::select(all_of(feats))

# Atliekame klasterizavimą su K=5
set.seed(1111111)
km_k5 <- kmeans(as.matrix(X_6), centers = 5, nstart =25)

# Pridedame klasterio numerį prie duomenų ir apskaičiuojame centrus
df_centers_6_k5 <- X_6 %>%
  mutate(Cluster = km_k5$cluster) %>%
  group_by(Cluster) %>%
  summarise(across(all_of(feats), mean, na.rm = TRUE), .groups = "drop") %>%
  mutate(across(all_of(feats), ~round(., 3)))

# Gražiai suformatuota lentelė
df_centers_6_k5 %>%
  kbl(
    caption = "K-means (K=5, 6 požymiai), klasterių centrai (požymių vidurkiai)",
    align = "c"
  ) %>%
  kable_styling(full_width = FALSE, position = "center", font_size = 12)

#########################################
### K-means (K=5, 6 požymiai, be išskirčių)
#########################################

# 1) Duomenys be išskirčių (naudojam tuos pačius plot_flags)
keep_idx <- !plot_flags$mild_outlier & !plot_flags$extreme_outlier

X_6_no_outliers <- df_mm[keep_idx, ] %>%
  dplyr::select(all_of(feats)) %>%
  as.matrix()

labels_no_outliers <- df_mm$label[keep_idx]

# 2) K-means K=5
set.seed(1111111)
km_k5_no_outliers <- kmeans(X_6_no_outliers, centers = 5, nstart = 50)

# 3) Sutapimų lentelė (confusion table)
cat("\nSutapimų lentelė – K-means (K=5), 6 požymiai, be išskirčių\n")
conf_table_no_outliers <- table(
  Klasė    = labels_no_outliers,
  Klasteris = km_k5_no_outliers$cluster
)
print(conf_table_no_outliers)

library(kableExtra)
conf_table_no_outliers %>%
  kbl(
    caption = "Sutapimų lentelė – K-means (K=5), 6 požymiai, be išskirčių",
    align = "c"
  ) %>%
  kable_styling(full_width = FALSE, position = "center", font_size = 12)

# 4) t-SNE vizualizacija su K=5 elipsėmis
set.seed(1111111)
tsne_k5_no <- Rtsne(
  X_6_no_outliers,
  dims = 2,
  perplexity = 15,
  eta = 500,
  verbose = FALSE,
  max_iter = 1000
)

tsne_df_k5 <- as.data.frame(tsne_k5_no$Y)
colnames(tsne_df_k5) <- c("tSNE1", "tSNE2")

plot_k5_no_outliers <- tsne_df_k5 %>%
  dplyr::mutate(
    label   = labels_no_outliers,
    cluster = factor(km_k5_no_outliers$cluster)
  )

ggplot(plot_k5_no_outliers, aes(x = tSNE1, y = tSNE2)) +
  stat_ellipse(
    aes(group = cluster),
    type = "norm",
    linetype = "dashed",
    size = 0.6,
    color = "grey40"
  ) +
  geom_point(
    aes(color = label),
    shape = 18,
    size = 2.2,
    alpha = 0.9
  ) +
  scale_color_manual(values = COLS_CLASS, name = "Klasė") +
  labs(
    title = "t-SNE (p=15, lr=500) – klasės spalvos, K-means elipsės (K=5) – 6 požymiai, be išskirčių",
    x = "t-SNE1", y = "t-SNE2"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold")
  )

X_6_no_outliers <- df_mm %>%
  dplyr::filter(!plot_flags$mild_outlier & !plot_flags$extreme_outlier) %>%
  dplyr::select(all_of(feats)) %>%
  as.matrix()

labels_no_outliers <- df_mm$label[!plot_flags$mild_outlier & !plot_flags$extreme_outlier]

# K-means klasterizavimas
set.seed(1111111)
km_k5_no_outliers <- kmeans(X_6_no_outliers, centers = 5, nstart = 50)

# Sutapimų lentelė
cat("\nSutapimų lentelė – K-means (K=5, 6 požymiai, be išskirčių)\n")
conf_table_no_outliers <- table(Klasė = labels_no_outliers, Klasteris = km_k5_no_outliers$cluster)
print(conf_table_no_outliers)

library(kableExtra)
conf_table_no_outliers %>%
  kbl(
    caption = "Sutapimų lentelė – K-means (K=5, 6 požymiai, be išskirčių)",
    align = "c"
  ) %>%
  kable_styling(full_width = FALSE, position = "center", font_size = 12)
