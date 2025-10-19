
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







                            
install.packages("Rtsne")
install.packages("colorspace")
install.packages("tibble")

library(Rtsne)
library(colorspace)
library(tibble)

# išskirčių aptikimas
iqr_labels <- function(x){
    Q1 <- quantile(x, 0.25, na.rm = TRUE); Q3 <- quantile(x, 0.75, na.rm = TRUE)
    I  <- Q3 - Q1
    inner_lo <- Q1 - 1.5*I; inner_hi <- Q3 + 1.5*I
    outer_lo <- Q1 - 3*I;   outer_hi <- Q3 + 3*I
    out <- ifelse(x < outer_lo | x > outer_hi, "Išorinė",
                  ifelse(x < inner_lo | x > inner_hi, "Vidinė", "Nėra"))
    factor(out, levels = c("Nėra","Vidinė","Išorinė"))
}


# t-SNE vaizdavimas su klaidų patikrinimu
run_tsne_checked <- function(X, labels, perplexity, learning_rate, max_iter = 1000, seed = 1111111){
    set.seed(seed)
    fit <- Rtsne(as.matrix(X), dims = 2, perplexity = perplexity, eta = learning_rate,
                 max_iter = max_iter, check_duplicates = FALSE, verbose = FALSE)
    
    # tikriname ar algoritmas susilygino
    if (length(fit$itercosts) > 1) {
        final_cost <- fit$itercosts[length(fit$itercosts)]
        initial_cost <- fit$itercosts[1]
        if (final_cost > initial_cost * 0.1) {
            warning(sprintf("t-SNE greičiausiai neužbaigė darbą: perplexity=%d, lr=%d, final_cost=%.4f", 
                            perplexity, learning_rate, final_cost))
        }
    }
    
    tibble::tibble(tSNE1 = fit$Y[,1], tSNE2 = fit$Y[,2], label = labels)
}

# klasių spalvos
base_colors <- c(N = "#D62728", S = "#2CA02C", V = "#1F77B4")
lighten_color <- function(hex_color, factor = 0.0) {
    col_obj <- colorspace::hex2RGB(hex_color)
    lighter <- colorspace::mixcolor(factor, col_obj, colorspace::RGB(1,1,1))
    colorspace::hex(lighter)
}

apply_color_map <- function(df) {
    df %>%
        dplyr::mutate(
            color = dplyr::case_when(
                rr_outlier == "Nėra"    ~ base_colors[label],
                rr_outlier == "Vidinė"  ~ lighten_color(base_colors[label], 0.35),
                rr_outlier == "Išorinė" ~ lighten_color(base_colors[label], 0.65)
            )
        )
}

# trustworthiness ir continuity skaičiavimas

trustworthiness_continuity_corrected <- function(X_high, X_low, k) {
    n <- nrow(X_high)
    
    # skaičiuojame atstumus tarp visų taškų
    dist_high <- as.matrix(dist(X_high))
    dist_low <- as.matrix(dist(X_low))
    
    T_sum <- 0
    C_sum <- 0
    
    for (i in 1:n) {
        # atstumai nuo i-ojo taško iki kitų
        dists_high_i <- dist_high[i, -i]
        dists_low_i <- dist_low[i, -i]
        
        # kaimynų eilės pagal artumą
        ranks_high <- rank(dists_high_i, ties.method = "first")
        ranks_low <- rank(dists_low_i, ties.method = "first")
        
        # visų taškų numeriai be i-ojo
        indices <- (1:n)[-i]
        
        # artimiausi k taškų originalioje ir sumažintoje erdvėse
        neighbors_high <- indices[order(dists_high_i)[1:k]] # kaimynai pradiniuose duomenyse
        neighbors_low <- indices[order(dists_low_i)[1:k]]   # kaimynai pritaikytame metode
        
        # trustworthiness: klaidos žemoje erdvėje
        U <- setdiff(neighbors_low, neighbors_high)
        for (j in U) {
            rank_j_high <- ranks_high[which(indices == j)]
            if (rank_j_high > k) {
                T_sum <- T_sum + (rank_j_high - k)
            }
        }
        
        # continuity: klaidos aukštoje erdvėje
        V <- setdiff(neighbors_high, neighbors_low)
        for (j in V) {
            rank_j_low <- ranks_low[which(indices == j)]
            if (rank_j_low > k) {
                C_sum <- C_sum + (rank_j_low - k)
            }
        }
    }
    
    # normuojame continuity ir trustworthiness rezultatus [0, 1]
    if (2*n - 3*k - 1 <= 0) return(c(T = NA, C = NA))
    cst <- 2 / (n * k * (2*n - 3*k - 1))
    
    c(T = max(0, 1 - cst * T_sum), C = max(0, 1 - cst * C_sum))
}


# parametrų pasirinkimas

grid_perp <- c(5, 10, 15, 20, 30, 40, 50)
grid_perp <- grid_perp[grid_perp <= max_reasonable_perplexity]

grid_lr <- c(10, 50, 100, 200, 500, 800, 1000)
k_vec <- c(5, 10, 20)  # kaimynų skaičiai

X_hi <- as.matrix(X_mm)

set.seed(1111111)
res <- list()
j <- 1

cat("\nIeškome geriausių parametrų...\n")
for(p in grid_perp){
    for(lr in grid_lr){
        cat(sprintf("Tikriname perplexity=%d, learning_rate=%d... ", p, lr))
        
        emb <- run_tsne_checked(X_hi, df$label, perplexity = p, learning_rate = lr, max_iter = 1000)
        
        # vertiname kiekvieną kaimynų skaičių
        mets <- sapply(k_vec, function(k) {
            trustworthiness_continuity_corrected(X_hi, as.matrix(emb[, c("tSNE1","tSNE2")]), k)
        })
        
        res[[j]] <- data.frame(
            perplexity = p, 
            learning_rate = lr,
            mean_T = mean(mets["T",], na.rm = TRUE),
            mean_C = mean(mets["C",], na.rm = TRUE), 
            mean_TC = mean((mets["T",] + mets["C",])/2, na.rm = TRUE)
        )
        
        cat(sprintf("patikimumas=%.4f, tęstinumas=%.4f, vidurkis=%.4f\n", 
                    res[[j]]$mean_T, res[[j]]$mean_C, res[[j]]$mean_TC))
        j <- j + 1
    }
}

grid_scores <- do.call(rbind, res)
grid_scores <- grid_scores[order(-grid_scores$mean_TC), ]

cat("\nGeriausi variantai:\n")
print(head(grid_scores, 10))

# grafikai

# perplexity įtaka
perp_trip <- tibble::tibble(
    case = factor(c("5", "30", "50"), levels = c("5", "30", "50")),
    perplexity = c(5, 30, 50),
    learning_rate = 200
)

tsne_perp3 <- do.call(rbind, lapply(seq_len(nrow(perp_trip)), function(i){
    p <- perp_trip$perplexity[i]; lr <- perp_trip$learning_rate[i]
    emb <- run_tsne_checked(X_mm, df$label, perplexity = p, learning_rate = lr, max_iter = 1000)
    emb$rr_outlier <- rr_outlier_vec
    emb$case <- perp_trip$case[i]
    emb
}))
tsne_perp3_c <- apply_color_map(tsne_perp3)

p_perp3 <- ggplot(tsne_perp3_c, aes(tSNE1, tSNE2)) +
    geom_point(aes(color = I(color)), size = 1.7, alpha = 0.9) +
    facet_wrap(~ case, nrow = 1) +
    theme_minimal() +
    labs(title = "t-SNE: perplexity parametro įtaka",
         subtitle = "(learning_rate = 200)",
         x = "t-SNE1", y = "t-SNE2") +
    theme(legend.position = "none")
print(p_perp3)

# learning rate įtaka
lr_trip <- tibble::tibble(
    case = factor(c("10", "200", "1000"), levels = c("10", "200", "1000")),
    learning_rate = c(10, 200, 1000),
    perplexity = 30
)

tsne_lr3 <- do.call(rbind, lapply(seq_len(nrow(lr_trip)), function(i){
    lr <- lr_trip$learning_rate[i]; p <- lr_trip$perplexity[i]
    emb <- run_tsne_checked(X_mm, df$label, perplexity = p, learning_rate = lr, max_iter = 1000)
    emb$rr_outlier <- rr_outlier_vec
    emb$case <- lr_trip$case[i]
    emb
}))
tsne_lr3_c <- apply_color_map(tsne_lr3)

p_lr3 <- ggplot(tsne_lr3_c, aes(tSNE1, tSNE2)) +
    geom_point(aes(color = I(color)), size = 1.7, alpha = 0.9) +
    facet_wrap(~ case, nrow = 1) +
    theme_minimal() +
    labs(title = "t-SNE: learning rate parametro įtaka",
         subtitle = "(perplexity = 30)",
         x = "t-SNE1", y = "t-SNE2") +
    theme(legend.position = "none")
print(p_lr3)

# nenormuota ir normuota aibė
emb_norm_best <- run_tsne_checked(X_mm, df$label, perplexity = p_best, learning_rate = lr_best, max_iter = 1500) %>%
    dplyr::mutate(rr_outlier = rr_outlier_vec, scale = "normuoti")

emb_nenorm_best <- run_tsne_checked(X_orig, df$label, perplexity = p_best, learning_rate = lr_best, max_iter = 1500) %>%
    dplyr::mutate(rr_outlier = rr_outlier_vec, scale = "neapdoroti")

emb_best_both <- dplyr::bind_rows(emb_norm_best, emb_nenorm_best)
emb_best_both_c <- apply_color_map(emb_best_both)

p_best_both <- ggplot(emb_best_both_c, aes(tSNE1, tSNE2)) +
    geom_point(aes(color = I(color)), size = 1.9, alpha = 0.9) +
    facet_wrap(~ scale, nrow = 1) +
    theme_minimal() +
    labs(title = sprintf("t-SNE: geriausi parametrai (p=%d, lr=%d)", p_best, lr_best),
         subtitle = "išskirtys pažymėtos šviesesnėmis spalvomis",
         x = "t-SNE1", y = "t-SNE2") +
    theme(legend.position = "none")
print(p_best_both)

                            
# --- PCA IR UMAP DALIS ---

library(umap)
library(ggplot2)
library(dplyr)

# Išskirčių susižymėjimas duomenų vaizdavimui
# Išskirčių (outlier) sužymėjimas prieš normavimą naudojant Mahalanobis
X_orig <- df[, feats]
center_orig <- colMeans(X_orig)
covmat_orig <- cov(X_orig)
md_orig <- mahalanobis(X_orig, center_orig, covmat_orig)

thr_inner_orig <- qchisq(0.95, df = ncol(X_orig))
thr_outer_orig <- qchisq(0.99, df = ncol(X_orig))

df$outlier_type <- case_when(
  md_orig > thr_outer_orig ~ "išorinė išskirtis",
  md_orig > thr_inner_orig ~ "vidinė išskirtis",
  TRUE ~ "normali reikšmė"
)

# Išskirčių (outlier) sužymėjimas po normavimo naudojant Mahalanobis
X_mm <- df_mm[, feats]
center_mm <- colMeans(X_mm)
covmat_mm <- cov(X_mm)
md_mm <- mahalanobis(X_mm, center_mm, covmat_mm)

thr_inner_mm <- qchisq(0.95, df = ncol(X_mm))
thr_outer_mm <- qchisq(0.99, df = ncol(X_mm))

df_mm$outlier_type <- case_when(
  md_mm > thr_outer_mm ~ "išorinė išskirtis",
  md_mm > thr_inner_mm ~ "vidinė išskirtis",
  TRUE ~ "normali reikšmė"
)

# --- MAZINIMAS su PCA ---
# PCA prieš normavimą
pca_orig <- prcomp(df[, feats], scale. = FALSE)
pca_df_orig <- data.frame(pca_orig$x[, 1:2],
                          klasė = df$label,
                          išskirtis = df$outlier_type)

p_pca_orig <- ggplot(pca_df_orig, aes(PC1, PC2)) +
  geom_point(aes(color = klasė, shape = išskirtis), size = 2, alpha = 0.8) +
  scale_shape_manual(values = c("normali reikšmė" = 16,
                                "vidinė išskirtis" = 3,
                                "išorinė išskirtis" = 17)) +
  labs(
    title = "PCA – prieš normavimą",
    x = "Pagrindinis komponentas 1",
    y = "Pagrindinis komponentas 2",
    color = "Klasė",
    shape = "Išskirčių tipas"
  ) +
  theme_minimal() +
  theme(legend.position = "top")

# PCA po normavimo
pca_mm <- prcomp(df_mm[, feats], scale. = TRUE)
pca_df_mm <- data.frame(pca_mm$x[, 1:2],
                        klasė = df_mm$label,
                        išskirtis = df_mm$outlier_type)

p_pca_mm <- ggplot(pca_df_mm, aes(PC1, PC2)) +
  geom_point(aes(color = klasė, shape = išskirtis), size = 2, alpha = 0.8) +
  scale_shape_manual(values = c("normali reikšmė" = 16,
                                "vidinė išskirtis" = 3,
                                "išorinė išskirtis" = 17)) +
  labs(
    title = "PCA – po normavimo",
    x = "Pagrindinis komponentas 1",
    y = "Pagrindinis komponentas 2",
    color = "Klasė",
    shape = "Išskirčių tipas"
  ) +
  theme_minimal() +
  theme(legend.position = "top")

# Grafikų vaizdavimas
p_pca_orig
p_pca_mm

# --- MAZINIMAS SU UMAP ---
# UMAP prieš normavimą
umap_orig <- umap(df[, feats])
umap_df_orig <- data.frame(umap_orig$layout,
                           klasė = df$label,
                           išskirtis = df$outlier_type)
colnames(umap_df_orig)[1:2] <- c("UMAP1", "UMAP2")

p_umap_orig <- ggplot(umap_df_orig, aes(UMAP1, UMAP2)) +
  geom_point(aes(color = klasė, shape = išskirtis), size = 1, alpha = 0.8) +
  scale_shape_manual(values = c("normali reikšmė" = 16,
                                "vidinė išskirtis" = 3,
                                "išorinė išskirtis" = 17)) +
  labs(
    title = "UMAP – prieš normavimą",
    x = "UMAP1",
    y = "UMAP2",
    color = "Klasė",
    shape = "Išskirčių tipas"
  ) +
  theme_minimal() +
  theme(legend.position = "top")

# UMAP po normavimo
umap_mm <- umap(df_mm[, feats])
umap_df_mm <- data.frame(umap_mm$layout,
                         klasė = df_mm$label,
                         išskirtis = df_mm$outlier_type)
colnames(umap_df_mm)[1:2] <- c("UMAP1", "UMAP2")

p_umap_mm <- ggplot(umap_df_mm, aes(UMAP1, UMAP2)) +
  geom_point(aes(color = klasė, shape = išskirtis), size = 1, alpha = 0.8) +
  scale_shape_manual(values = c("normali reikšmė" = 16,
                                "vidinė išskirtis" = 3,
                                "išorinė išskirtis" = 17)) +
  labs(
    title = "UMAP – po normavimo",
    x = "UMAP1",
    y = "UMAP2",
    color = "Klasė",
    shape = "Išskirčių tipas"
  ) +
  theme_minimal() +
  theme(legend.position = "top")

# Grafikų atvaizdavimas
p_umap_orig
p_umap_mm


# --- UMAP parametrų analizė ---
# Parametrai
neighbors_values <- c(5, 30, 100)
min_dist_values <- c(0.01, 0.5, 0.9)
metrics <- c("euclidean", "manhattan", "cosine")

# Išskirčių žymėjimas
shape_values <- c("normali reikšmė" = 16,
                  "vidinė išskirtis" = 3,
                  "išorinė išskirtis" = 17)

# Kaimynų skaičius
for (k in neighbors_values) {
  set.seed(123) # Dar kartą pakeičiamas atsitiktinumas
  u <- umap(df_mm[, feats], n_neighbors = k)
  
  df_u <- data.frame(u$layout,
                     klasė = df_mm$label,
                     išskirtis = df_mm$outlier_type)
  colnames(df_u)[1:2] <- c("UMAP1", "UMAP2")
  
  p <- ggplot(df_u, aes(UMAP1, UMAP2, color = klasė, shape = išskirtis)) +
    geom_point(alpha = 0.8, size = 1) +
    scale_shape_manual(values = shape_values) +
    labs(
      title = paste("UMAP – kaimynų skaičius =", k),
      x = "UMAP1",
      y = "UMAP2",
      color = "Klasė",
      shape = "Išskirčių tipas"
    ) +
    theme_minimal() +
    theme(legend.position = "top")
  
  print(p)  # grafikai rodomi atskirai vienas po kito
}

# Taškų gretumo skaičius min_dist
for (md in min_dist_values) {
  set.seed(123) # Dar kartą pakeičiamas atsitiktinumas
  u <- umap(df_mm[, feats], min_dist = md)
  
  df_u <- data.frame(u$layout,
                     klasė = df_mm$label,
                     išskirtis = df_mm$outlier_type)
  colnames(df_u)[1:2] <- c("UMAP1", "UMAP2")
  
  p <- ggplot(df_u, aes(UMAP1, UMAP2, color = klasė, shape = išskirtis)) +
    geom_point(alpha = 0.8, size = 2) +
    scale_shape_manual(values = shape_values) +
    labs(
      title = paste("UMAP – min_dist =", md),
      x = "UMAP1",
      y = "UMAP2",
      color = "Klasė",
      shape = "Išskirčių tipas"
    ) +
    theme_minimal() +
    theme(legend.position = "top")
  
  print(p) # grafikai rodomi atskirai vienas po kito
}

# Skirtingos atstumo matavimo metrikos

for (m in metrics) {
  set.seed(123) # Dar kartą pakeičiamas atsitiktinumas
  u <- umap(df_mm[, feats], metric = m)
  
  df_u <- data.frame(u$layout,
                     klasė = df_mm$label,
                     išskirtis = df_mm$outlier_type)
  colnames(df_u)[1:2] <- c("UMAP1", "UMAP2")
  
  p <- ggplot(df_u, aes(UMAP1, UMAP2, color = klasė, shape = išskirtis)) +
    geom_point(alpha = 0.8, size = 2) +
    scale_shape_manual(values = shape_values) +
    labs(
      title = paste("UMAP – metrika:", m),
      x = "UMAP1",
      y = "UMAP2",
      color = "Klasė",
      shape = "Išskirčių tipas"
    ) +
    theme_minimal() +
    theme(legend.position = "top")
  
  print(p) # grafikai rodomi atskirai vienas po kito
}

# Keičiaame k reikšmes manualiai, siekiant palyginti (tikriname 5, 10, 20)
# PCA T ir C reikšmės
k <- 5

res_PCA <- trustworthiness_continuity_corrected(df_mm, pca_df_mm, k)
print(res_PCA)

# UMAP T ir C reikšmės, kiekvienam parametrui
# Rezultatu vaizdavimui sukuriami "placeholderiai" kuriuos veliau užpildysime reikšmėmis
results_umap <- data.frame(
  n_neighbors = integer(),
  min_dist    = numeric(),
  metric      = character(),
  T           = numeric(),
  C           = numeric(),
  stringsAsFactors = FALSE
)

k_T_C <- 10  # kaimynystės skaičius

# Iteruojama kiekvienam n_neighbors
for (k in neighbors_values) {
  set.seed(123)
  u <- umap(df_mm[, feats], n_neighbors = k)
  
  df_u <- data.frame(u$layout,
                     klasė = df_mm$label,
                     išskirtis = df_mm$outlier_type)
  colnames(df_u)[1:2] <- c("UMAP1", "UMAP2")
  
  # Apskaičiuojami C ir T
  res <- trustworthiness_continuity_corrected(df_mm[, feats], df_u[, c("UMAP1","UMAP2")], k_T_C)
  
  # Išsaugome įrašą į lentelę
  results_umap <- rbind(results_umap, data.frame(
    n_neighbors = k,
    min_dist = NA,
    metric = "euclidean",
    T = res["T"],
    C = res["C"]
  ))
}

# Iteracija kiekvienai min_dist reiksmei
for (md in min_dist_values) {
  set.seed(123)
  u <- umap(df_mm[, feats], min_dist = md)
  
  df_u <- data.frame(u$layout,
                     klasė = df_mm$label,
                     išskirtis = df_mm$outlier_type)
  colnames(df_u)[1:2] <- c("UMAP1", "UMAP2")
  
  res <- trustworthiness_continuity_corrected(df_mm[, feats], df_u[, c("UMAP1","UMAP2")], k_T_C)
  
  results_umap <- rbind(results_umap, data.frame(
    n_neighbors = NA,
    min_dist = md,
    metric = "euclidean",
    T = res["T"],
    C = res["C"]
  ))
}

# Iteracija kiekvienai atsumo metrikai
for (m in metrics) {
  set.seed(123)
  u <- umap(df_mm[, feats], metric = m)
  
  df_u <- data.frame(u$layout,
                     klasė = df_mm$label,
                     išskirtis = df_mm$outlier_type)
  colnames(df_u)[1:2] <- c("UMAP1", "UMAP2")
  
  res <- trustworthiness_continuity_corrected(df_mm[, feats], df_u[, c("UMAP1","UMAP2")], k_T_C)
  
  results_umap <- rbind(results_umap, data.frame(
    n_neighbors = NA,
    min_dist = NA,
    metric = m,
    T = res["T"],
    C = res["C"]
  ))
}

# Pateikiama visa lentelė
results_umap %>%
  arrange(n_neighbors, min_dist, metric) %>%
  kable(digits = 3, caption = "Trustworthiness and Continuity UMAP metodui") %>%
  kable_styling(full_width = FALSE)                       
