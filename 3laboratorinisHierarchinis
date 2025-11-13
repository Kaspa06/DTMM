# =========================
# HIERARCHICAL – 6D (Ward.D2) vs t-SNE 2D (Ward.D2), k=4
# =========================

need <- c("dplyr","ggplot2","cluster","mclust","Rtsne","kableExtra","tidyr","rlang","purrr")
to_install <- setdiff(need, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, dep = TRUE)
invisible(lapply(need, library, character.only = TRUE))

# --- Patikrinimai ir baziniai objektai ---
stopifnot(exists("df_mm"), exists("feats"))
X <- df_mm[, feats, drop = FALSE]
y <- df_mm$label
stopifnot(nrow(X) == length(y))

# --- Pagalbinės funkcijos ---
purity_vec <- function(tab){ apply(prop.table(tab, 1), 1, max) }
sil_mean   <- function(cl, D){ mean(cluster::silhouette(cl, D)[, "sil_width"]) }

# --- IQR išskirtys eilutei per visus požymius (naudota tik vizualiai, analytics – be filtravimo) ---
iqr_one <- function(x){
    Q1 <- quantile(x, 0.25, na.rm = TRUE); Q3 <- quantile(x, 0.75, na.rm = TRUE); I <- Q3 - Q1
    inner_lo <- Q1 - 1.5*I; inner_hi <- Q3 + 1.5*I
    outer_lo <- Q1 - 3*I;   outer_hi <- Q3 + 3*I
    ifelse(x < outer_lo | x > outer_hi, "Išorinė",
           ifelse(x < inner_lo | x > inner_hi, "Vidinė", "Nėra"))
}
out_mat <- sapply(X, iqr_one)
out_row <- apply(out_mat, 1, function(v) if ("Išorinė" %in% v) "Išorinė" else if ("Vidinė" %in% v) "Vidinė" else "Nėra")
out_row <- factor(out_row, levels = c("Nėra","Vidinė","Išorinė"))

# ---------------------------------------------------------
# 1) KLASTERIZACIJA 6D ERDVĖJE (Ward.D2, k = 4)
# ---------------------------------------------------------
D6  <- dist(X, method = "euclidean")
hc6 <- hclust(D6, method = "ward.D2")
k   <- 4
cl6 <- cutree(hc6, k = k)

tab6 <- table(Cluster = cl6, True = y)
sil6 <- sil_mean(cl6, D6)
ari6 <- mclust::adjustedRandIndex(cl6, y)
pur6 <- mean(purity_vec(tab6))

cat("\n--- 6D (Ward.D2), k=4 ---\n")
print(tab6)
cat(sprintf("Silhouette: %.3f   ARI: %.3f   Purity(avg): %.3f\n", sil6, ari6, pur6))

# ---------------------------------------------------------
# 2) t-SNE 2D → KLASTERIZACIJA 2D ERDVĖJE (Ward.D2, k = 4)
# ---------------------------------------------------------
set.seed(1111111)
p_tsne  <- 15
lr_tsne <- 500
ts <- Rtsne(as.matrix(X), dims = 2, perplexity = p_tsne, eta = lr_tsne,
            max_iter = 1000, check_duplicates = FALSE, verbose = FALSE)

X2   <- ts$Y
D2   <- dist(X2, method = "euclidean")
hc2  <- hclust(D2, method = "ward.D2")
cl2  <- cutree(hc2, k = k)

tab2 <- table(Cluster = cl2, True = y)
sil2 <- sil_mean(cl2, D2)
ari2 <- mclust::adjustedRandIndex(cl2, y)
pur2 <- mean(purity_vec(tab2))

cat("\n--- 2D t-SNE (Ward.D2 ant 2D), k=4 ---\n")
print(tab2)
cat(sprintf("Silhouette: %.3f   ARI: %.3f   Purity(avg): %.3f\n", sil2, ari2, pur2))

# ---------------------------------------------------------
# 3) SANTRAUKA: 6D vs 2D PALYGINIMAS
# ---------------------------------------------------------
cmp <- dplyr::tibble(
    Erdvė = c("6D (originalūs požymiai)", "2D (t-SNE embedding)"),
    Silhouette = c(sil6, sil2),
    ARI        = c(ari6,  ari2),
    Purity_avg = c(pur6,  pur2)
)
cmp %>%
    kable(digits = 3, caption = "6D vs 2D (t-SNE) – Ward.D2, k=4: metrikų palyginimas") %>%
    kable_styling(full_width = FALSE)

# priskyrimų sutapimai (kiek taškų turi tą patį cluster ID tarp 6D ir 2D)
overlap_tbl <- table(Cluster6 = cl6, Cluster2 = cl2)
overlap_tbl %>%
    kable(caption = "Sutapimų lentelė: 6D klasteriai vs 2D klasteriai (k=4)") %>%
    kable_styling(full_width = FALSE)

# sutapimų dalis (maksimalus per-6D klasterio atitikimas)
agree_rate <- sum(apply(overlap_tbl, 1, max)) / length(cl6)
cat(sprintf("\nSutapimo rodiklis (max-per-klasterį): %.1f%%\n", 100*agree_rate))

# ---------------------------------------------------------
# 4) VIZUALIZACIJOS t-SNE plokštumoje
# ---------------------------------------------------------
viz_df <- dplyr::tibble(
    tSNE1 = X2[,1],
    tSNE2 = X2[,2],
    label = y,
    cl6   = factor(cl6),
    cl2   = factor(cl2),
    out   = out_row
) %>%
    dplyr::mutate(alpha_val = dplyr::case_when(
        out == "Nėra" ~ 1.00,
        out == "Vidinė" ~ 0.45,
        out == "Išorinė" ~ 0.15
    ),
    agree = ifelse(cl6 == cl2, "sutampa", "nesutampa"))

class_cols <- c(N="#e41a1c", S="#4daf4a", V="#377eb8")

# (a) t-SNE, klasės spalvos, 6D klasterių elipsės
p_tsne_6D <- ggplot(viz_df, aes(tSNE1, tSNE2)) +
    stat_ellipse(aes(group = cl6, color = cl6),
                 level = 0.90, type = "norm", linetype = 2, linewidth = 1, show.legend = FALSE) +
    geom_point(aes(color = label, alpha = alpha_val), size = 1.9) +
    scale_color_manual(values = class_cols, breaks = names(class_cols), name = "Klasė") +
    scale_alpha_identity() +
    labs(title = sprintf("t-SNE (p=%d, lr=%d) – klasės spalvos, 6D Ward.D2 elipsės (k=%d)", p_tsne, lr_tsne, k),
         subtitle = "Išskirtys pažymėtos mažesniu skaidrumu",
         x = "t-SNE1", y = "t-SNE2") +
    theme_minimal(base_size = 12) +
    theme(legend.position = "right")

# (b) t-SNE, klasės spalvos, 2D klasterių elipsės
p_tsne_2D <- ggplot(viz_df, aes(tSNE1, tSNE2)) +
    stat_ellipse(aes(group = cl2, color = cl2),
                 level = 0.90, type = "norm", linetype = 2, linewidth = 1, show.legend = FALSE) +
    geom_point(aes(color = label, alpha = alpha_val), size = 1.9) +
    scale_color_manual(values = class_cols, breaks = names(class_cols), name = "Klasė") +
    scale_alpha_identity() +
    labs(title = sprintf("t-SNE (p=%d, lr=%d) – klasės spalvos, 2D Ward.D2 elipsės (k=%d)", p_tsne, lr_tsne, k),
         subtitle = "Išskirtys pažymėtos mažesniu skaidrumu",
         x = "t-SNE1", y = "t-SNE2") +
    theme_minimal(base_size = 12) +
    theme(legend.position = "right")

# (c) t-SNE, „sutampa vs nesutampa“ tarp 6D ir 2D klasterių
p_agree <- ggplot(viz_df, aes(tSNE1, tSNE2)) +
    geom_point(aes(shape = agree, alpha = alpha_val), size = 2) +
    scale_shape_manual(values = c(sutampa = 16, nesutampa = 4), name = "Sutapimas") +
    scale_alpha_identity() +
    labs(title = "6D vs 2D Ward.D2 priskyrimų sutapimas t-SNE plokštumoje",
         subtitle = "Taškai, kur 6D ir 2D klasterio priskyrimai nesutampa, pažymėti '×'",
         x = "t-SNE1", y = "t-SNE2") +
    theme_minimal(base_size = 12) +
    theme(legend.position = "right")

print(p_tsne_6D)
print(p_tsne_2D)
print(p_agree)

# ---------------------------------------------------------
# 5) Trumpa tekstinė išvada (spausdinama konsolėje)
# ---------------------------------------------------------
delta_sil <- sil2 - sil6
delta_ari <- ari2 - ari6
delta_pur <- pur2 - pur6
cat("\nIŠVADA (6D vs 2D):\n",
    sprintf("Silhouette pokytis: %.3f (2D - 6D)\n", delta_sil),
    sprintf("ARI pokytis: %.3f (2D - 6D)\n", delta_ari),
    sprintf("Purity pokytis: %.3f (2D - 6D)\n", delta_pur),
    "Pastaba: 6D klasteriai sudaryti požymių erdvėje; 2D klasteriai sudaryti t-SNE plokštumoje.\n",
    "Skirtumai parodo, kaip dimensijų mažinimas keičia klasterizavimo tendencijas (ne vien tik vizualiai).\n", sep="")
















# =========================
# PAPILDOMA: Ward.D2 vs Average vs Complete (6D ir 2D), k=4
# =========================

# Pagalbinės funkcijos (naudojam tas pačias metrikas)
purity_vec <- function(tab){ apply(prop.table(tab, 1), 1, max) }
sil_mean   <- function(cl, D){ mean(cluster::silhouette(cl, D)[, "sil_width"]) }

evaluate_linkages <- function(D, y, linkages = c("ward.D2","average","complete"), k = 4) {
  out_rows <- list()
  cl_assign <- list()
  for (lnk in linkages) {
    hc  <- hclust(D, method = lnk)
    cl  <- cutree(hc, k = k)
    tab <- table(Cluster = cl, True = y)
    sil <- sil_mean(cl, D)
    ari <- mclust::adjustedRandIndex(cl, y)
    pur <- mean(purity_vec(tab))
    out_rows[[lnk]] <- data.frame(
      Linkage = lnk, K = k,
      Silhouette = sil, ARI = ari, Purity_avg = pur,
      stringsAsFactors = FALSE
    )
    cl_assign[[lnk]] <- cl

    cat(sprintf("\n[%s] k=%d – confusion:\n", lnk, k)); print(tab)
    cat(sprintf("Silhouette: %.3f   ARI: %.3f   Purity(avg): %.3f\n", sil, ari, pur))
  }
  list(summary = dplyr::bind_rows(out_rows), clusters = cl_assign)
}

# --- 6D (požymių erdvė) ---
D6 <- dist(X, method = "euclidean")  # Ward.D2 reikalauja Euclidean
res6 <- evaluate_linkages(D6, y, linkages = c("ward.D2","average","complete"), k = 4)
tbl6 <- res6$summary %>%
  dplyr::mutate(Space = "6D") %>%
  dplyr::select(Space, Linkage, K, Silhouette, ARI, Purity_avg)

# --- 2D (t-SNE embedding erdvė) ---
D2 <- dist(X2, method = "euclidean")
res2 <- evaluate_linkages(D2, y, linkages = c("ward.D2","average","complete"), k = 4)
tbl2 <- res2$summary %>%
  dplyr::mutate(Space = "2D (t-SNE)") %>%
  dplyr::select(Space, Linkage, K, Silhouette, ARI, Purity_avg)

# --- Bendra suvestinė: 6D vs 2D, Ward.D2/Average/Complete, k=4 ---
comp_linkages <- dplyr::bind_rows(tbl6, tbl2) %>%
  dplyr::arrange(Space, dplyr::desc(ARI))
comp_linkages %>%
  kable(digits = 3, caption = "Ward.D2 vs Average vs Complete – 6D ir 2D (t-SNE), k=4: metrikų palyginimas") %>%
  kable_styling(full_width = FALSE)

# =========================
# Pasirinktinai: 2D vizualizacijos su elipsėmis kiekvienam linkažui (k=4)
# =========================
# Naudojame jau turimą viz_df (tSNE1, tSNE2, label, out), tik pridedame klasterius iš 2D atvejo

viz2 <- viz_df %>%
  dplyr::mutate(
    cl2_ward     = factor(res2$clusters[["ward.D2"]]),
    cl2_average  = factor(res2$clusters[["average"]]),
    cl2_complete = factor(res2$clusters[["complete"]])
  )

plot_tsne_linkage <- function(df, cluster_col, title_suffix){
  ggplot(df, aes(tSNE1, tSNE2)) +
    stat_ellipse(aes(group = !!rlang::sym(cluster_col), color = !!rlang::sym(cluster_col)),
                 level = 0.90, type = "norm", linetype = 2, linewidth = 1, show.legend = FALSE) +
    geom_point(aes(color = label, alpha = alpha_val), size = 1.9) +
    scale_color_manual(values = c(N="#e41a1c", S="#4daf4a", V="#377eb8"),
                       breaks = c("N","S","V"), name = "Klasė") +
    scale_alpha_identity() +
    labs(title = sprintf("2D (t-SNE) – klasės spalvos, elipsės (%s, k=4)", title_suffix),
         subtitle = "Išskirtys pažymėtos mažesniu skaidrumu",
         x = "t-SNE1", y = "t-SNE2") +
    theme_minimal(base_size = 12) +
    theme(legend.position = "right")
}

p2_ward     <- plot_tsne_linkage(viz2, "cl2_ward",     "Ward.D2")
p2_average  <- plot_tsne_linkage(viz2, "cl2_average",  "Average")
p2_complete <- plot_tsne_linkage(viz2, "cl2_complete", "Complete")

print(p2_ward)
print(p2_average)
print(p2_complete)

# =========================
# Trumpa konsolinė išvada: kuris linkažas geriausias 6D ir 2D?
# =========================
best6 <- tbl6[order(-tbl6$ARI, -tbl6$Purity_avg), ][1, c("Linkage","ARI","Silhouette","Purity_avg")]
best2 <- tbl2[order(-tbl2$ARI, -tbl2$Purity_avg), ][1, c("Linkage","ARI","Silhouette","Purity_avg")]
cat("\nGERIAUSIAS 6D pagal ARI:", best6$Linkage, 
    sprintf("(ARI=%.3f; Sil=%.3f; Pur=%.3f)\n", best6$ARI, best6$Silhouette, best6$Purity_avg))
cat("GERIAUSIAS 2D (t-SNE) pagal ARI:", best2$Linkage, 
    sprintf("(ARI=%.3f; Sil=%.3f; Pur=%.3f)\n", best2$ARI, best2$Silhouette, best2$Purity_avg))

cat("\nPASTABA:\n",
    "- Ward.D2 teoriškai ir praktiškai dažniausiai geriausias 6D erdvėje, nes minimizuoja WCSS (vidinę dispersiją).\n",
    "- 2D (t-SNE) erdvėje galutinis rezultatas gali skirtis, nes embeddingas iškraipo atstumus – todėl lyginame atskirai.\n", sep="")








# =========================
# Pilna aibė (≈31 požymis): Ward.D2, k = 2..10 + išskirtys + t-SNE vizualizacija
# =========================

need <- c("dplyr","tidyr","ggplot2","cluster","mclust","kableExtra","Rtsne")
to_install <- setdiff(need, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, dep = TRUE)
invisible(lapply(need, library, character.only = TRUE))

# ---- 0) PRIELAIDOS ----
# Turi būti data.frame df_clean arba df su stulpeliu 'label' (N/S/V),
# o visi kiti – skaitiniai požymiai (arba bent dauguma).
stopifnot(exists("df_clean") || exists("df"))
df_all <- if (exists("df_clean")) df_clean else df

# Užtikrinam faktorių tvarką:
df_all <- df_all %>% dplyr::filter(label %in% c("N","S","V")) %>% droplevels()
y <- df_all$label

# ---- 1) Pasirenkam požymius (VISUS skaitinius be 'label') ----
num_cols <- names(df_all)[vapply(df_all, is.numeric, logical(1))]
feats_all <- setdiff(num_cols, "label")
stopifnot(length(feats_all) >= 10)  # turėtų būti ~31

X_orig <- df_all[, feats_all, drop = FALSE]

# ---- 2) Winsorizacija (IQR) + Min–Max normalizavimas ----
winsor_iqr <- function(x){
  Q1 <- quantile(x, 0.25, na.rm = TRUE); Q3 <- quantile(x, 0.75, na.rm = TRUE)
  I  <- Q3 - Q1
  lo <- Q1 - 1.5*I; hi <- Q3 + 1.5*I
  x[x < lo] <- lo; x[x > hi] <- hi
  x
}
range01 <- function(x){
  r <- range(x, na.rm = TRUE)
  if (r[1] == r[2]) rep(0.5, length(x)) else (x - r[1])/(r[2] - r[1])
}
X_winz <- as.data.frame(lapply(X_orig, winsor_iqr))
X_mm   <- as.data.frame(lapply(X_winz, range01))

# (pasirinktinai) apjungtas df su label
df_mm_full <- dplyr::bind_cols(label = y, X_mm)

# ---- 3) Išskirčių žymėjimas (eilutei per visus požymius) ----
iqr_one <- function(x){
  Q1 <- quantile(x, 0.25, na.rm = TRUE); Q3 <- quantile(x, 0.75, na.rm = TRUE)
  I <- Q3 - Q1
  inner_lo <- Q1 - 1.5*I; inner_hi <- Q3 + 1.5*I
  outer_lo <- Q1 - 3*I;   outer_hi <- Q3 + 3*I
  ifelse(x < outer_lo | x > outer_hi, "Išorinė",
         ifelse(x < inner_lo | x > inner_hi, "Vidinė", "Nėra"))
}
out_mat <- sapply(X_orig, iqr_one)  # čia vertinam originalias (nenormuotas) skales
out_row <- apply(out_mat, 1, function(v){
  if ("Išorinė" %in% v) "Išorinė" else if ("Vidinė" %in% v) "Vidinė" else "Nėra"
})
out_row <- factor(out_row, levels = c("Nėra","Vidinė","Išorinė"))

# ---- 4) Funkcijos metrikoms ir k-skenavimui ----
purity_vec <- function(tab){ apply(prop.table(tab, 1), 1, max) }
sil_mean   <- function(cl, D){ mean(cluster::silhouette(cl, D)[, "sil_width"]) }

evaluate_k <- function(X, y, k_min = 2, k_max = 10){
  D  <- dist(X, method = "euclidean")
  hc <- hclust(D, method = "ward.D2")
  rows <- list()
  assign_list <- list()
  for (k in k_min:k_max){
    cl  <- cutree(hc, k = k)
    tab <- table(Cluster = cl, True = y)
    sil <- sil_mean(cl, D)
    ari <- mclust::adjustedRandIndex(cl, y)
    pur <- mean(purity_vec(tab))
    rows[[as.character(k)]] <- data.frame(
      K = k, Silhouette = sil, ARI = ari, Purity_avg = pur
    )
    assign_list[[as.character(k)]] <- list(cl = cl, tab = tab)
  }
  list(summary = dplyr::bind_rows(rows), details = assign_list, hc = hc, D = D)
}

# ---- 5) Įvertinam k = 2..10 31D erdvėje ----
set.seed(1111111)   # deterministiškas pjūvio stabilumas
res_full <- evaluate_k(X_mm, y, k_min = 2, k_max = 10)

# Suvestinė lentelei į ataskaitą
tbl_k <- res_full$summary %>%
  dplyr::arrange(K)

# Atspausdink (į ataskaitą įterpsi kaip lentelę)
tbl_k %>%
  kable(digits = 3, caption = "Ward.D2 (31 požymiai, Min–Max) – metrikos k = 2…10") %>%
  kable_styling(full_width = FALSE)

# ---- 6) Pasirenkam „geriausią“ k (pagal ARI; gali keisti į Silhouette) ----
k_best <- tbl_k$K[ which.max(tbl_k$ARI) ]   # pvz., jei anksčiau buvo geriausias k=4
best     <- res_full$details[[as.character(k_best)]]
cl_best  <- best$cl
tab_best <- best$tab
sil_best <- tbl_k$Silhouette[tbl_k$K == k_best]
ari_best <- tbl_k$ARI[tbl_k$K == k_best]
pur_best <- tbl_k$Purity_avg[tbl_k$K == k_best]

cat(sprintf("\nGERIAUSIAS pagal ARI: k=%d  (Sil=%.3f, ARI=%.3f, Pur=%.3f)\n",
            k_best, sil_best, ari_best, pur_best))
cat("\nConfusion (k=", k_best, "):\n", sep=""); print(tab_best)

# ---- 7) Dendrograma (patogiai parodyti pjūvį) ----
plot(res_full$hc, labels = FALSE,
     main = "Dendrograma (Ward.D2, Euclidean, 31D Min–Max)",
     xlab = "Stebiniai", ylab = "Aglomeracijos atstumas")
abline(h = mean(range(res_full$hc$height[ (length(res_full$hc$height)-(k_best-1)) ])), col="red", lty=2)

# ---- 8) Išskirtys: bendros ir pagal klasterį (naudojant k_best) ----
iqr_df <- as.data.frame(table(out_row))
names(iqr_df) <- c("Išskirties_tipai","Kiekis")
iqr_df$Procentas <- round(100 * iqr_df$Kiekis / sum(iqr_df$Kiekis), 1)

iqr_df %>%
  kable(caption = "Išskirčių (IQR) suvestinė visoje aibėje") %>%
  kable_styling(full_width = FALSE)

out_by_cl <- dplyr::tibble(cluster = factor(cl_best), outlier = out_row) %>%
  dplyr::count(cluster, outlier) %>%
  dplyr::group_by(cluster) %>%
  dplyr::mutate(Proc = round(100*n/sum(n), 1)) %>%
  dplyr::arrange(cluster, desc(outlier))

out_by_cl %>%
  kable(caption = sprintf("Išskirčių (IQR) pasiskirstymas pagal klasterius (k=%d)", k_best)) %>%
  kable_styling(full_width = FALSE)



# ---- 9) t-SNE 2D vizualizacija (p=15, lr=500) su 31D klasterių elipsėmis ----
set.seed(1111111)
p_tsne  <- 15
lr_tsne <- 500

# 9.1 Paruošiam t-SNE įėjimą: tik skaitiniai, be NA, be dublikatų
X_ts <- df_mm_full %>% dplyr::select(-label)  # jau Min-Max
X_ts <- X_ts[, vapply(X_ts, is.numeric, logical(1)), drop = FALSE]

# išmetam NA eilutes (ir label, kad priderintume vėliau)
keep_idx <- stats::complete.cases(X_ts)
X_ts     <- X_ts[keep_idx, , drop = FALSE]
y_ts     <- y[keep_idx]
out_ts   <- out_row[keep_idx]

# suvaldom dublikatus: jei yra, pridedam labai mažą jitter
if (any(duplicated(X_ts))) {
    X_ts <- as.matrix(X_ts) + matrix(rnorm(length(X_ts), sd = 1e-8), nrow = nrow(X_ts))
} else {
    X_ts <- as.matrix(X_ts)
}

# 9.2 Rtsne (leidžiam patikrinti dublikatus, jau sutvarkėm)
ts <- Rtsne(
    X_ts, dims = 2, perplexity = p_tsne, eta = lr_tsne,
    max_iter = 1000, check_duplicates = TRUE, verbose = FALSE
)

# 9.3 t-SNE koordinatės + 31D klasteriai (k_best ir k=4)
X31  <- df_mm_full[keep_idx, -1, drop = FALSE]                 # 31D (Min-Max), be NA
D31  <- dist(X31, method = "euclidean")
hc31 <- hclust(D31, method = "ward.D2")

k_vis <- 4  # papildomas vizualinis pjūvis

cl31_kbest <- cutree(hc31, k = k_best)
cl31_kvis  <- cutree(hc31, k = k_vis)

# (nebūtina) 2D klasterizacija toje pačioje t-SNE plokštumoje – palyginimui
D2  <- dist(ts$Y, method = "euclidean")
hc2 <- hclust(D2, method = "ward.D2")
cl2_kbest <- cutree(hc2, k = k_best)
cl2_kvis  <- cutree(hc2, k = k_vis)

# 9.4 Duomenų rėmelis braižymui
viz_df <- dplyr::tibble(
    tSNE1 = ts$Y[,1],
    tSNE2 = ts$Y[,2],
    label = y_ts,
    out   = out_ts,
    cl31_kbest = factor(cl31_kbest),
    cl31_kvis  = factor(cl31_kvis),
    cl2_kbest  = factor(cl2_kbest),
    cl2_kvis   = factor(cl2_kvis)
) %>%
    dplyr::mutate(alpha_val = dplyr::case_when(
        out == "Nėra"    ~ 1.00,
        out == "Vidinė"  ~ 0.45,
        out == "Išorinė" ~ 0.15
    ))

class_cols <- c(N="#e41a1c", S="#4daf4a", V="#377eb8")

# 9.5 t-SNE: klasės spalvos, elipsės – 31D (Ward.D2) k = k_best
p_tsne_31_kbest <- ggplot(viz_df, aes(tSNE1, tSNE2)) +
    stat_ellipse(aes(group = cl31_kbest), level = 0.90,
                 type = "norm", linetype = 2, linewidth = 1, color = "grey30") +
    geom_point(aes(color = label, alpha = alpha_val), size = 1.9) +
    scale_color_manual(values = class_cols, breaks = names(class_cols), name = "Klasė") +
    scale_alpha_identity() +
    labs(
        title = sprintf("t-SNE (p=%d, lr=%d) – klasės spalvos, 31D Ward.D2 elipsės (k=%d)", p_tsne, lr_tsne, k_best),
        subtitle = "Išskirtys pažymėtos mažesniu skaidrumu",
        x = "t-SNE1", y = "t-SNE2"
    ) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "right")

# 9.6 t-SNE: klasės spalvos, elipsės – 31D (Ward.D2) k = 4 (dažnai naudojamas pristatymui)
p_tsne_31_k4 <- ggplot(viz_df, aes(tSNE1, tSNE2)) +
    stat_ellipse(aes(group = cl31_kvis), level = 0.90,
                 type = "norm", linetype = 2, linewidth = 1, color = "grey30") +
    geom_point(aes(color = label, alpha = alpha_val), size = 1.9) +
    scale_color_manual(values = class_cols, breaks = names(class_cols), name = "Klasė") +
    scale_alpha_identity() +
    labs(
        title = sprintf("t-SNE (p=%d, lr=%d) – klasės spalvos, 31D Ward.D2 elipsės (k=%d)", p_tsne, lr_tsne, k_vis),
        subtitle = "Išskirtys pažymėtos mažesniu skaidrumu",
        x = "t-SNE1", y = "t-SNE2"
    ) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "right")

# (pasirinktinai) 9.7 t-SNE elipsės pagal 2D klasterizaciją – jei nori parodyti skirtumą
p_tsne_2d_k4 <- ggplot(viz_df, aes(tSNE1, tSNE2)) +
    stat_ellipse(aes(group = cl2_kvis), level = 0.90,
                 type = "norm", linetype = 2, linewidth = 1, color = "grey30") +
    geom_point(aes(color = label, alpha = alpha_val), size = 1.9) +
    scale_color_manual(values = class_cols, breaks = names(class_cols), name = "Klasė") +
    scale_alpha_identity() +
    labs(
        title = sprintf("t-SNE (p=%d, lr=%d) – klasės spalvos, 2D Ward.D2 elipsės (k=%d)", p_tsne, lr_tsne, k_vis),
        subtitle = "Elipsės skaičiuotos 2D atstumais (ne 31D)",
        x = "t-SNE1", y = "t-SNE2"
    ) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "right")

# 9.8 Parodyti grafikus
print(p_tsne_31_kbest)
print(p_tsne_31_k4)
# jei reikia ir 2D linkažo elipsių:
#print(p_tsne_2d_k4)

# =========================
# Pilna aibė (≈31 požymis): Ward.D2, k = 2..10 + išskirtys + t-SNE vizualizacija
# =========================

need <- c("dplyr","tidyr","ggplot2","cluster","mclust","kableExtra","Rtsne")
to_install <- setdiff(need, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, dep = TRUE)
invisible(lapply(need, library, character.only = TRUE))

# ---- 0) PRIELAIDOS ----
# Turi būti data.frame df_clean arba df su stulpeliu 'label' (N/S/V),
# o visi kiti – skaitiniai požymiai (arba bent dauguma).
stopifnot(exists("df_clean") || exists("df"))
df_all <- if (exists("df_clean")) df_clean else df

# Užtikrinam faktorių tvarką ir PASIRENKAM 1500 ĮRAŠŲ
df_all <- df_all %>% dplyr::filter(label %in% c("N","S","V")) %>% droplevels()
set.seed(1111111)
n_keep <- min(1500, nrow(df_all))
idx_sample <- sample(seq_len(nrow(df_all)), n_keep)
df_all <- df_all[idx_sample, , drop = FALSE]
y <- df_all$label

# ---- 1) Pasirenkam požymius (VISUS skaitinius be 'label') ----
num_cols  <- names(df_all)[vapply(df_all, is.numeric, logical(1))]
feats_all <- setdiff(num_cols, "label")
stopifnot(length(feats_all) >= 10)  # turėtų būti ~31

X_orig <- df_all[, feats_all, drop = FALSE]

# ---- 2) Winsorizacija (IQR) + Min–Max normalizavimas ----
winsor_iqr <- function(x){
  Q1 <- quantile(x, 0.25, na.rm = TRUE); Q3 <- quantile(x, 0.75, na.rm = TRUE)
  I  <- Q3 - Q1
  lo <- Q1 - 1.5*I; hi <- Q3 + 1.5*I
  x[x < lo] <- lo; x[x > hi] <- hi
  x
}
range01 <- function(x){
  r <- range(x, na.rm = TRUE)
  if (r[1] == r[2]) rep(0.5, length(x)) else (x - r[1])/(r[2] - r[1])
}
X_winz <- as.data.frame(lapply(X_orig, winsor_iqr))
X_mm   <- as.data.frame(lapply(X_winz, range01))

# (pasirinktinai) apjungtas df su label
df_mm_full <- dplyr::bind_cols(label = y, X_mm)

# ---- 3) Išskirčių žymėjimas (eilutei per visus požymius) ----
iqr_one <- function(x){
  Q1 <- quantile(x, 0.25, na.rm = TRUE); Q3 <- quantile(x, 0.75, na.rm = TRUE)
  I <- Q3 - Q1
  inner_lo <- Q1 - 1.5*I; inner_hi <- Q3 + 1.5*I
  outer_lo <- Q1 - 3*I;   outer_hi <- Q3 + 3*I
  ifelse(x < outer_lo | x > outer_hi, "Išorinė",
         ifelse(x < inner_lo | x > inner_hi, "Vidinė", "Nėra"))
}
out_mat <- sapply(X_orig, iqr_one)  # vertinam originalias (nenormuotas) skales
out_row <- apply(out_mat, 1, function(v){
  if ("Išorinė" %in% v) "Išorinė" else if ("Vidinė" %in% v) "Vidinė" else "Nėra"
})
out_row <- factor(out_row, levels = c("Nėra","Vidinė","Išorinė"))

# ---- 4) Funkcijos metrikoms ir k-skenavimui ----
purity_vec <- function(tab){ apply(prop.table(tab, 1), 1, max) }
sil_mean   <- function(cl, D){ mean(cluster::silhouette(cl, D)[, "sil_width"]) }

evaluate_k <- function(X, y, k_min = 2, k_max = 10){
  D  <- dist(X, method = "euclidean")
  hc <- hclust(D, method = "ward.D2")
  rows <- list()
  assign_list <- list()
  for (k in k_min:k_max){
    cl  <- cutree(hc, k = k)
    tab <- table(Cluster = cl, True = y)
    sil <- sil_mean(cl, D)
    ari <- mclust::adjustedRandIndex(cl, y)
    pur <- mean(purity_vec(tab))
    rows[[as.character(k)]] <- data.frame(
      K = k, Silhouette = sil, ARI = ari, Purity_avg = pur
    )
    assign_list[[as.character(k)]] <- list(cl = cl, tab = tab)
  }
  list(summary = dplyr::bind_rows(rows), details = assign_list, hc = hc, D = D)
}

# ---- 5) Įvertinam k = 2..10 31D erdvėje ----
set.seed(1111111)   # deterministiškas pjūvio stabilumas
res_full <- evaluate_k(X_mm, y, k_min = 2, k_max = 10)

# Suvestinė lentelei į ataskaitą
tbl_k <- res_full$summary %>% dplyr::arrange(K)

# Atspausdink (į ataskaitą įterpsi kaip lentelę)
tbl_k %>%
  kable(digits = 3, caption = "Ward.D2 (31 požymiai, Min–Max) – metrikos k = 2…10") %>%
  kable_styling(full_width = FALSE)

# ---- 6) Pasirenkam „geriausią“ k (pagal ARI; gali keisti į Silhouette) ----
k_best  <- tbl_k$K[ which.max(tbl_k$ARI) ]
best    <- res_full$details[[as.character(k_best)]]
cl_best <- best$cl
tab_best <- best$tab
sil_best <- tbl_k$Silhouette[tbl_k$K == k_best]
ari_best <- tbl_k$ARI[tbl_k$K == k_best]
pur_best <- tbl_k$Purity_avg[tbl_k$K == k_best]

cat(sprintf("\nGERIAUSIAS pagal ARI: k=%d  (Sil=%.3f, ARI=%.3f, Pur=%.3f)\n",
            k_best, sil_best, ari_best, pur_best))
cat("\nConfusion (k=", k_best, "):\n", sep=""); print(tab_best)

# ---- 7) Dendrograma (patogiai parodyti pjūvį) ----
plot(res_full$hc, labels = FALSE,
     main = "Dendrograma (Ward.D2, Euclidean, 31D Min–Max)",
     xlab = "Eilutės", ylab = "Aglomeracijos atstumas")
abline(h = mean(range(res_full$hc$height[(length(res_full$hc$height)-(k_best-1))])), col="red", lty=2)

# ---- 8) Išskirtys: bendros ir pagal klasterį (naudojant k_best) ----
iqr_df <- as.data.frame(table(out_row))
names(iqr_df) <- c("Išskirties_tipai","Kiekis")
iqr_df$Procentas <- round(100 * iqr_df$Kiekis / sum(iqr_df$Kiekis), 1)

iqr_df %>%
  kable(caption = "Išskirčių (IQR) suvestinė visoje aibėje") %>%
  kable_styling(full_width = FALSE)

out_by_cl <- dplyr::tibble(cluster = factor(cl_best), outlier = out_row) %>%
  dplyr::count(cluster, outlier) %>%
  dplyr::group_by(cluster) %>%
  dplyr::mutate(Proc = round(100*n/sum(n), 1)) %>%
  dplyr::arrange(cluster, desc(outlier))

out_by_cl %>%
  kable(caption = sprintf("Išskirčių (IQR) pasiskirstymas pagal klasterius (k=%d)", k_best)) %>%
  kable_styling(full_width = FALSE)

# ---- 9) t-SNE 2D vizualizacija (p=15, lr=500) su 31D klasterių elipsėmis ----
set.seed(1111111)
p_tsne  <- 15
lr_tsne <- 500

# 9.1 Paruošiam t-SNE įėjimą: tik skaitiniai, be NA, be dublikatų
X_ts <- df_mm_full %>% dplyr::select(-label)
X_ts <- X_ts[, vapply(X_ts, is.numeric, logical(1)), drop = FALSE]

# išmetam NA eilutes (ir label, kad priderintume vėliau)
keep_idx <- stats::complete.cases(X_ts)
X_ts     <- X_ts[keep_idx, , drop = FALSE]
y_ts     <- y[keep_idx]
out_ts   <- out_row[keep_idx]

# suvaldom dublikatus: jei yra, pridedam labai mažą jitter
if (any(duplicated(X_ts))) {
  X_ts <- as.matrix(X_ts) + matrix(rnorm(length(X_ts), sd = 1e-8), nrow = nrow(X_ts))
} else {
  X_ts <- as.matrix(X_ts)
}

# 9.2 Rtsne
ts <- Rtsne(
  X_ts, dims = 2, perplexity = p_tsne, eta = lr_tsne,
  max_iter = 1000, check_duplicates = TRUE, verbose = FALSE
)

# 9.3 t-SNE koordinatės + 31D klasteriai (k_best ir k=4)
X31  <- df_mm_full[keep_idx, -1, drop = FALSE]
D31  <- dist(X31, method = "euclidean")
hc31 <- hclust(D31, method = "ward.D2")

k_vis <- 4
cl31_kbest <- cutree(hc31, k = k_best)
cl31_kvis  <- cutree(hc31, k = k_vis)

# (nebūtina) 2D klasterizacija toje pačioje t-SNE plokštumoje – palyginimui
D2  <- dist(ts$Y, method = "euclidean")
hc2 <- hclust(D2, method = "ward.D2")
cl2_kbest <- cutree(hc2, k = k_best)
cl2_kvis  <- cutree(hc2, k = k_vis)

# 9.4 Duomenų rėmelis braižymui
viz_df <- dplyr::tibble(
  tSNE1 = ts$Y[,1],
  tSNE2 = ts$Y[,2],
  label = y_ts,
  out   = out_ts,
  cl31_kbest = factor(cl31_kbest),
  cl31_kvis  = factor(cl31_kvis),
  cl2_kbest  = factor(cl2_kbest),
  cl2_kvis   = factor(cl2_kvis)
) %>%
  dplyr::mutate(alpha_val = dplyr::case_when(
    out == "Nėra"    ~ 1.00,
    out == "Vidinė"  ~ 0.45,
    out == "Išorinė" ~ 0.15
  ))

class_cols <- c(N="#e41a1c", S="#4daf4a", V="#377eb8")

# 9.5 t-SNE: klasės spalvos, elipsės – 31D (Ward.D2) k = k_best
p_tsne_31_kbest <- ggplot(viz_df, aes(tSNE1, tSNE2)) +
  stat_ellipse(aes(group = cl31_kbest), level = 0.90,
               type = "norm", linetype = 2, linewidth = 1, color = "grey30") +
  geom_point(aes(color = label, alpha = alpha_val), size = 1.9) +
  scale_color_manual(values = class_cols, breaks = names(class_cols), name = "Klasė") +
  scale_alpha_identity() +
  labs(
    title = sprintf("t-SNE (p=%d, lr=%d) – klasės spalvos, 31D Ward.D2 elipsės (k=%d)", p_tsne, lr_tsne, k_best),
    subtitle = "Išskirtys pažymėtos mažesniu skaidrumu",
    x = "t-SNE1", y = "t-SNE2"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "right")

# 9.6 t-SNE: klasės spalvos, elipsės – 31D (Ward.D2) k = 4
p_tsne_31_k4 <- ggplot(viz_df, aes(tSNE1, tSNE2)) +
  stat_ellipse(aes(group = cl31_kvis), level = 0.90,
               type = "norm", linetype = 2, linewidth = 1, color = "grey30") +
  geom_point(aes(color = label, alpha = alpha_val), size = 1.9) +
  scale_color_manual(values = class_cols, breaks = names(class_cols), name = "Klasė") +
  scale_alpha_identity() +
  labs(
    title = sprintf("t-SNE (p=%d, lr=%d) – klasės spalvos, 31D Ward.D2 elipsės (k=%d)", p_tsne, lr_tsne, k_vis),
    subtitle = "Išskirtys pažymėtos mažesniu skaidrumu",
    x = "t-SNE1", y = "t-SNE2"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "right")

# (pasirinktinai) 9.7 t-SNE elipsės pagal 2D klasterizaciją
p_tsne_2d_k4 <- ggplot(viz_df, aes(tSNE1, tSNE2)) +
  stat_ellipse(aes(group = cl2_kvis), level = 0.90,
               type = "norm", linetype = 2, linewidth = 1, color = "grey30") +
  geom_point(aes(color = label, alpha = alpha_val), size = 1.9) +
  scale_color_manual(values = class_cols, breaks = names(class_cols), name = "Klasė") +
  scale_alpha_identity() +
  labs(
    title = sprintf("t-SNE (p=%d, lr=%d) – klasės spalvos, 2D Ward.D2 elipsės (k=%d)", p_tsne, lr_tsne, k_vis),
    subtitle = "Elipsės skaičiuotos 2D atstumais (ne 31D)",
    x = "t-SNE1", y = "t-SNE2"
  ) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "right")

# 9.8 Parodyti grafikus
print(p_tsne_31_kbest)
print(p_tsne_31_k4)
# jei reikia ir 2D linkažo elipsių:
# print(p_tsne_2d_k4)
