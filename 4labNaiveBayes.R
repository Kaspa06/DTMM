need <- c(
  "tidyverse",   # dplyr, ggplot2, readr ir t.t.
  "janitor",
  "kableExtra",
  "psych",
  "smacof",
  "caret",
  "e1071",
  "dplyr",
  "ggplot2"
)

to_install <- setdiff(need, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, dep = TRUE)

invisible(lapply(need, library, character.only = TRUE))

# 1. DUOMENŲ NUSKAITYMAS IR PIRMINIS TVARKYMAS

setwd("C:/Users/einor/OneDrive/stalinis kompiuteris/DTMM")

# Nuskaitom CSV
df_raw <- read.csv("EKG_pupsniu_analize.csv",
                   sep = ";", dec = ".", check.names = FALSE)

# Pavadinimų sutvarkymas
nm <- names(df_raw)
nm <- janitor::make_clean_names(nm)
nm <- gsub("/", "_div_", nm, fixed = TRUE)
names(df_raw) <- nm

# Tipų konvertavimas
df_raw <- readr::type_convert(
  df_raw,
  locale = readr::locale(decimal_mark = ".", grouping_mark = ",")
)

# label -> faktorius, keli stulpeliai į numeric
df_raw <- df_raw %>%
  mutate(
    rr_r_0        = as.numeric(rr_r_0),
    rr_r_0_rr_r_1 = as.numeric(rr_r_0_rr_r_1),
    label         = factor(label, levels = c(0,1,2),
                           labels = c("N","S","V"))
  )

# Paliekam tik N, S, V ir šalinam NA
df_clean <- df_raw %>%
  filter(label %in% c("N","S","V")) %>%
  droplevels()
df_clean <- df_clean[complete.cases(df_clean), ]

table(df_clean$label)


# 2. N IR S KLASĖS, PO 1000 KIEKVIENOS + 31 POŽYMIS


set.seed(1111111)

df_ns <- df_clean %>%
  filter(label %in% c("N","S")) %>%
  group_by(label) %>%
  slice_sample(n = 1000, replace = FALSE) %>%
  ungroup() %>%
  droplevels()

table(df_ns$label)   # N = 1000, S = 1000
nrow(df_ns)          # 2000

# 31 požymis = visi skaitiniai stulpeliai, išskyrus label
feat_all <- names(df_ns)[sapply(df_ns, is.numeric)]
feat_all <- setdiff(feat_all, "label")

length(feat_all)     # ~31

X_orig_full <- df_ns[, feat_all, drop = FALSE]  # nenormuoti požymiai
y_ns        <- df_ns$label                      # klasės N / S


# 3. APRAŠOMOJI STATISTIKA (NENORMUOTA, POŽYMIAI EILUTĖSE)


make_stats_table_long <- function(data, columns){
  stats_list <- lapply(columns, function(col){
    x <- data[[col]]
    c(
      Min        = min(x, na.rm = TRUE),
      Q1         = quantile(x, 0.25, na.rm = TRUE),
      Mediana    = median(x, na.rm = TRUE),
      Vidurkis   = mean(x, na.rm = TRUE),
      Q3         = quantile(x, 0.75, na.rm = TRUE),
      Max        = max(x, na.rm = TRUE),
      Dispersija = var(x, na.rm = TRUE)
    )
  })
  out <- as.data.frame(do.call(rbind, stats_list))
  out <- cbind(Požymis = columns, out)
  rownames(out) <- NULL
  out
}

# N klasė
df_N <- df_ns[df_ns$label == "N", ]
tbl_N_31 <- make_stats_table_long(df_N, feat_all)

tbl_N_31 %>%
  kable(digits = 3,
        caption = "Aprašomoji statistika – klasė N (nenormuota, požymiai eilutėse)") %>%
  kable_styling(full_width = FALSE)

# S klasė
df_S <- df_ns[df_ns$label == "S", ]
tbl_S_31 <- make_stats_table_long(df_S, feat_all)

tbl_S_31 %>%
  kable(digits = 3,
        caption = "Aprašomoji statistika – klasė S (nenormuota, požymiai eilutėse)") %>%
  kable_styling(full_width = FALSE)


# 4. MIN–MAX NORMAVIMAS (31 POŽYMIS)


range01 <- function(x) {
  r <- range(x, na.rm = TRUE)
  if (r[1] == r[2]) rep(0.5, length(x)) else (x - r[1])/(r[2] - r[1])
}

X_mm_full <- as.data.frame(lapply(X_orig_full, range01))

# Pagrindinė NORMUOTA aibė klasifikavimui / klasterizavimui
df_ns_mm <- bind_cols(label = y_ns, X_mm_full)


# 5. DIMENSIJOS MAŽINIMAS MDS (31 POŽYMIS, Manhattan, 600 iter.)


D_man_31 <- dist(X_mm_full, method = "manhattan")

set.seed(1111111)
fit_mds_31 <- smacof::mds(
  delta   = D_man_31,
  type    = "ratio",
  ndim    = 2,
  itmax   = 600,
  init    = "random",
  verbose = FALSE
)

mds_coords_31 <- as.data.frame(fit_mds_31$conf)
colnames(mds_coords_31) <- c("MDS1", "MDS2")

df_mds_31 <- bind_cols(label = y_ns, mds_coords_31)

p_mds_31 <- ggplot(df_mds_31, aes(MDS1, MDS2, color = label)) +
  geom_point(size = 1.8, alpha = 0.8) +
  theme_minimal() +
  labs(
    title = "Metrinis MDS (Manhattan, 600 iteracijų) – N ir S klasės (31 požymis)",
    x = "MDS1", y = "MDS2", color = "Klasė"
  )
print(p_mds_31)


# 6. DUOMENŲ PADALIJIMAS Į TRAIN / VALIDATION / TEST (NORMUOTA AIBĖ)


set.seed(1111111)

# 6.1. Test – 20 % visos imties
idx_test  <- caret::createDataPartition(df_ns_mm$label, p = 0.20, list = FALSE)
test_set  <- df_ns_mm[idx_test, ]
trainval  <- df_ns_mm[-idx_test, ]

# 6.2. Iš likusių 80 % – 80 % train, 20 % val
idx_train <- caret::createDataPartition(trainval$label, p = 0.80, list = FALSE)
train_set <- trainval[idx_train, ]
val_set   <- trainval[-idx_train, ]

# žymime, kuriai aibei priklauso kiekvienas įrašas (vėliau reikalinga grafike)
n_all <- nrow(df_ns_mm)
set_label <- rep(NA_character_, n_all)
set_label[idx_test] <- "test"
set_label[-idx_test][idx_train]      <- "train"
set_label[-idx_test][-idx_train]     <- "val"
df_ns_mm$set <- factor(set_label, levels = c("train","val","test"))

# proporcijų patikra
c(
  Train = nrow(train_set) / n_all,
  Val   = nrow(val_set)   / n_all,
  Test  = nrow(test_set)  / n_all
)

prop.table(table(train_set$label))
prop.table(table(val_set$label))
prop.table(table(test_set$label))


# 7. POŽYMIŲ ATRANKA NAIVE BAYES KLASIFIKAVIMUI (N vs S)


feats_all_nb <- setdiff(names(train_set), c("label","set"))
length(feats_all_nb)   # 31

# 7.1. Vienpožyminis reikšmingumo įvertinimas (t-test)
t_scores <- sapply(feats_all_nb, function(f) {
  x <- train_set[[f]]
  y <- train_set$label
  tt <- t.test(x ~ y)
  abs(as.numeric(tt$statistic))
})

t_rank <- sort(t_scores, decreasing = TRUE)
t_rank

# 7.2. Imame TOP-12 požymių, iš jų formuosim 6-požymių rinkinius
top_k     <- 12
top_feats <- names(t_rank)[1:top_k]
top_feats

# 7.3. Pagalbinė funkcija metrikoms iš confusion matrix

get_metrics <- function(cm) {
  if (!all(rownames(cm) %in% colnames(cm))) {
    stop("Confusion matrix rows/cols nesutampa pagal klases.")
  }
  
  acc <- sum(diag(cm)) / sum(cm)
  
  prec_vec <- diag(cm) / colSums(cm)
  rec_vec  <- diag(cm) / rowSums(cm)
  f1_vec   <- 2 * prec_vec * rec_vec / (prec_vec + rec_vec)
  
  prec_macro <- mean(prec_vec, na.rm = TRUE)
  rec_macro  <- mean(rec_vec,  na.rm = TRUE)
  f1_macro   <- mean(f1_vec,   na.rm = TRUE)
  
  c(
    accuracy        = acc,
    precision_macro = prec_macro,
    recall_macro    = rec_macro,
    f1_macro        = f1_macro
  )
}

# 7.4. Visos 6-požymių kombinacijos iš top_feats

comb_mat <- combn(top_feats, 6)
n_comb   <- ncol(comb_mat)
n_comb   # 924

# 7.5. Ciklas per kombinacijas – mokom NB ir vertinam val aibėje

results_list <- vector("list", n_comb)
set.seed(1111111)

for (i in seq_len(n_comb)) {
  feats_i <- comb_mat[, i]
  
  train_i <- train_set[, c("label", feats_i), drop = FALSE]
  val_i   <- val_set[,   c("label", feats_i), drop = FALSE]
  
  nb_model <- naiveBayes(label ~ ., data = train_i, laplace = 1)
  
  pred_val <- predict(nb_model, newdata = val_i)
  
  cm   <- table(Actual = val_i$label, Predicted = pred_val)
  mets <- get_metrics(cm)
  
  results_list[[i]] <- data.frame(
    combo_id        = i,
    features        = paste(feats_i, collapse = " + "),
    accuracy        = mets["accuracy"],
    precision_macro = mets["precision_macro"],
    recall_macro    = mets["recall_macro"],
    f1_macro        = mets["f1_macro"],
    stringsAsFactors = FALSE
  )
}

results_df <- dplyr::bind_rows(results_list)

# 7.6. Rikiuojam pagal F1 ir accuracy

results_sorted <- results_df %>%
  arrange(desc(f1_macro), desc(accuracy))

head(results_sorted, 10)   # top10 lentelė ataskaitai

# 7.7. Galutinis geriausias 6-požymių rinkinys

best_row <- results_sorted[1, ]
best_row

best_features <- unlist(strsplit(best_row$features, " \\+ "))
best_features
# tikėtina: c("r_val","q_val","signal_std","wl_side","s_pos","t_pos")


# 8. GALUTINIS NAIVE BAYES MODELIS (TRAIN+VAL) IR TEST AIBĖ


# 8.1. Suformuojame train+val aibę tik su geriausiais požymiais

trainval_idx <- which(df_ns_mm$set %in% c("train","val"))
test_idx     <- which(df_ns_mm$set == "test")

trainval_best <- df_ns_mm[trainval_idx, c("label", best_features)]
test_best     <- df_ns_mm[test_idx,     c("label", best_features)]

# 8.2. Galutinis modelis (laplace=1, Gaussian NB skaitiniams kintamiesiems)

nb_final <- naiveBayes(label ~ ., data = trainval_best, laplace = 1)

# 8.3. Prognozės train, val, test atskirai

# train
train_best <- df_ns_mm[trainval_idx[df_ns_mm$set[trainval_idx]=="train"], 
                       c("label", best_features)]
pred_train <- predict(nb_final, newdata = train_best)
cm_train   <- table(Actual = train_best$label, Predicted = pred_train)
mets_train <- get_metrics(cm_train)

# val (čia jau tik patikra, nors modelis mokytas ir su val duomenimis – ataskaitoje pabrėžk)
val_best <- df_ns_mm[trainval_idx[df_ns_mm$set[trainval_idx]=="val"], 
                     c("label", best_features)]
pred_val_final <- predict(nb_final, newdata = val_best)
cm_val   <- table(Actual = val_best$label, Predicted = pred_val_final)
mets_val <- get_metrics(cm_val)

# test (pagrindinis objektyvus įvertinimas)
pred_test <- predict(nb_final, newdata = test_best)
cm_test   <- table(Actual = test_best$label, Predicted = pred_test)
mets_test <- get_metrics(cm_test)

cm_train
cm_val
cm_test

res_sets <- rbind(
  data.frame(set = "Train", t(mets_train)),
  data.frame(set = "Val",   t(mets_val)),
  data.frame(set = "Test",  t(mets_test))
)
res_sets


# 9. PARAMETRŲ KEITIMO PALYGINIMAS (laplace, usekernel) – ant VAL aibės


param_grid <- expand.grid(
  laplace   = c(0, 1),
  usekernel = c(FALSE, TRUE),
  KEEP.OUT.ATTRS = FALSE
)

param_results <- vector("list", nrow(param_grid))

for (i in seq_len(nrow(param_grid))) {
  p <- param_grid[i, ]
  
  nb_tmp <- naiveBayes(
    label ~ ., 
    data      = train_set[, c("label", best_features), drop = FALSE],
    laplace   = p$laplace,
    usekernel = p$usekernel
  )
  
  pred_val_p <- predict(nb_tmp,
                        newdata = val_set[, c("label", best_features), drop = FALSE])
  cm_p   <- table(Actual = val_set$label, Predicted = pred_val_p)
  mets_p <- get_metrics(cm_p)
  
  param_results[[i]] <- data.frame(
    laplace   = p$laplace,
    usekernel = p$usekernel,
    accuracy        = mets_p["accuracy"],
    precision_macro = mets_p["precision_macro"],
    recall_macro    = mets_p["recall_macro"],
    f1_macro        = mets_p["f1_macro"]
  )
}

param_table <- bind_rows(param_results) %>%
  arrange(desc(f1_macro), desc(accuracy))

param_table   # lentelei apie parametrų įtaką


# 10. MDS 2D GRAFIKAS SU 6 OPTIMALIŲ POŽYMIŲ RINKINIU


X_best <- df_ns_mm[, best_features, drop = FALSE]

D_man_6 <- dist(X_best, method = "manhattan")

set.seed(1111111)
fit_mds_6 <- smacof::mds(
  delta   = D_man_6,
  type    = "ratio",
  ndim    = 2,
  itmax   = 600,
  init    = "random",
  verbose = FALSE
)

mds_coords_6 <- as.data.frame(fit_mds_6$conf)
colnames(mds_coords_6) <- c("MDS1", "MDS2")

df_mds_6 <- bind_cols(
  label = df_ns_mm$label,
  set   = df_ns_mm$set,
  mds_coords_6
)

p_mds_6 <- ggplot(df_mds_6, aes(MDS1, MDS2, color = label)) +
  geom_point(size = 1.8, alpha = 0.8) +
  theme_minimal() +
  labs(
    title = "Metrinis MDS (Manhattan, 600 iteracijų) – 6 optimalūs požymiai",
    x = "MDS1", y = "MDS2", color = "Klasė"
  )
print(p_mds_6)


# 11. GRAFIKAS: KUR KLASIFIKATORIUS KLYDO (TEST AIBĖ)


# prognozuojam VISUS taškus su galutiniu modeliu
all_best <- df_ns_mm[, c("label", "set", best_features)]
pred_all <- predict(nb_final, newdata = all_best[, best_features])

df_plot <- df_mds_6 %>%
  mutate(
    set        = df_ns_mm$set,
    pred       = pred_all,
    correct    = (pred == label),
    misclass   = ifelse(correct, "Teisingai", "Neteisingai")
  )

# rodom tik test taškus, kiti – fonas (pilki)
p_mis <- ggplot() +
  geom_point(data = df_plot %>% filter(set != "test"),
             aes(MDS1, MDS2),
             color = "grey85", alpha = 0.4, size = 1.5) +
  geom_point(data = df_plot %>% filter(set == "test"),
             aes(MDS1, MDS2, shape = misclass, color = label),
             size = 2.2, alpha = 0.9) +
  theme_minimal() +
  scale_shape_manual(values = c("Teisingai" = 16, "Neteisingai" = 4)) +
  labs(
    title = "Naive Bayes klasifikavimo rezultatai test aibėje (MDS 2D, 6 požymiai)",
    subtitle = "Kryželiais pažymėti neteisingai suklasifikuoti test taškai",
    x = "MDS1", y = "MDS2",
    color = "Tikra klasė",
    shape = "Klasifikavimo rezultatas"
  )
print(p_mis)






# 31 ir 6 požymių klasifikavimo palyginimas


# Paketai – įdiegiame tik jei trūksta ir užkrauname
need <- c("e1071", "dplyr", "ggplot2", "smacof")

to_install <- setdiff(need, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, dep = TRUE)

invisible(lapply(need, library, character.only = TRUE))

# Pagalbinė metrikų funkcija
get_metrics <- function(cm) {
  acc <- sum(diag(cm)) / sum(cm)
  prec <- diag(cm) / colSums(cm)
  rec  <- diag(cm) / rowSums(cm)
  f1   <- 2 * prec * rec / (prec + rec)
  c(
    accuracy        = acc,
    precision_macro = mean(prec, na.rm = TRUE),
    recall_macro    = mean(rec,  na.rm = TRUE),
    f1_macro        = mean(f1,   na.rm = TRUE)
  )
}


# A) 31 požymiai – ORIGINALI ERDVĖ


train_31 <- train_set[, c("label", feat_all)]
val_31   <- val_set[,   c("label", feat_all)]
test_31  <- test_set[,  c("label", feat_all)]

nb_31 <- naiveBayes(label ~ ., data = train_31, laplace = 1)

met_31_train <- get_metrics(table(train_31$label, predict(nb_31, train_31)))
met_31_val   <- get_metrics(table(val_31$label,   predict(nb_31, val_31)))
met_31_test  <- get_metrics(table(test_31$label,  predict(nb_31, test_31)))


# B) 31 požymiai → MDS 2D


X_31 <- df_ns_mm[, feat_all]
D_31 <- dist(X_31, method = "manhattan")

set.seed(1111111)
mds_31 <- smacof::mds(delta = D_31, ndim = 2, itmax = 600)

df_mds31 <- data.frame(
  label = df_ns_mm$label,
  set   = df_ns_mm$set,
  M1    = mds_31$conf[,1],
  M2    = mds_31$conf[,2]
)

train_mds31 <- df_mds31[df_mds31$set == "train", ]
val_mds31   <- df_mds31[df_mds31$set == "val", ]
test_mds31  <- df_mds31[df_mds31$set == "test", ]

nb_mds31 <- naiveBayes(label ~ M1 + M2, data = train_mds31)

met_mds31_train <- get_metrics(table(train_mds31$label, predict(nb_mds31, train_mds31)))
met_mds31_val   <- get_metrics(table(val_mds31$label,   predict(nb_mds31, val_mds31)))
met_mds31_test  <- get_metrics(table(test_mds31$label,  predict(nb_mds31, test_mds31)))


# C) 6 požymiai (optimalūs) – ORIGINALI ERDVĖ


train_6 <- train_set[, c("label", best_features)]
val_6   <- val_set[,   c("label", best_features)]
test_6  <- test_set[,  c("label", best_features)]

nb_6 <- naiveBayes(label ~ ., data = train_6, laplace = 1)

met_6_train <- get_metrics(table(train_6$label, predict(nb_6, train_6)))
met_6_val   <- get_metrics(table(val_6$label,   predict(nb_6, val_6)))
met_6_test  <- get_metrics(table(test_6$label,  predict(nb_6, test_6)))


# D) 6 požymiai → MDS 2D


X_6 <- df_ns_mm[, best_features]
D_6 <- dist(X_6, method = "manhattan")

set.seed(1111111)
mds_6 <- smacof::mds(delta = D_6, ndim = 2, itmax = 600)

df_mds6 <- data.frame(
  label = df_ns_mm$label,
  set   = df_ns_mm$set,
  M1    = mds_6$conf[,1],
  M2    = mds_6$conf[,2]
)

train_mds6 <- df_mds6[df_mds6$set == "train", ]
val_mds6   <- df_mds6[df_mds6$set == "val", ]
test_mds6  <- df_mds6[df_mds6$set == "test", ]

nb_mds6 <- naiveBayes(label ~ M1 + M2, data = train_mds6)

met_mds6_train <- get_metrics(table(train_mds6$label, predict(nb_mds6, train_mds6)))
met_mds6_val   <- get_metrics(table(val_mds6$label,   predict(nb_mds6, val_mds6)))
met_mds6_test  <- get_metrics(table(test_mds6$label,  predict(nb_mds6, test_mds6)))


# REZULTATŲ PALYGINIMO LENTELĖ (tik test metrikos)


compare_table <- data.frame(
  Modelis   = c("31D originali", "31D MDS", "6D originali", "6D MDS"),
  Accuracy  = c(met_31_test["accuracy"], 
                met_mds31_test["accuracy"],
                met_6_test["accuracy"],
                met_mds6_test["accuracy"]),
  Precision = c(met_31_test["precision_macro"], 
                met_mds31_test["precision_macro"],
                met_6_test["precision_macro"],
                met_mds6_test["precision_macro"]),
  Recall    = c(met_31_test["recall_macro"], 
                met_mds31_test["recall_macro"],
                met_6_test["recall_macro"],
                met_mds6_test["recall_macro"]),
  F1        = c(met_31_test["f1_macro"], 
                met_mds31_test["f1_macro"],
                met_6_test["f1_macro"],
                met_mds6_test["f1_macro"])
)

print(compare_table)






# VIZUALIZACIJA – TIK TEST AIBĖ, SU IŠSKIRTIMIS IR RIBA


need <- c("dplyr","ggplot2","e1071","smacof")
to_install <- setdiff(need, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, dep = TRUE)
invisible(lapply(need, library, character.only = TRUE))

# 1. IQR išskirtys 31D ir 6D

iqr_labels_one <- function(x){
    Q1 <- quantile(x, 0.25, na.rm = TRUE)
    Q3 <- quantile(x, 0.75, na.rm = TRUE)
    I  <- Q3 - Q1
    
    inner_lo <- Q1 - 1.5 * I
    inner_hi <- Q3 + 1.5 * I
    outer_lo <- Q1 - 3   * I
    outer_hi <- Q3 + 3   * I
    
    out <- ifelse(x < outer_lo | x > outer_hi, "Išorinė",
                  ifelse(x < inner_lo | x > inner_hi, "Vidinė", "Nėra"))
    factor(out, levels = c("Nėra","Vidinė","Išorinė"))
}

combine_outliers <- function(mat_levels){
    # Nėra=0, Vidinė=1, Išorinė=2 -> imam max per eilutę
    num_mat <- sapply(mat_levels, function(f) as.integer(f) - 1L)
    max_sev <- apply(num_mat, 1, max, na.rm = TRUE)
    factor(c("Nėra","Vidinė","Išorinė")[max_sev + 1L],
           levels = c("Nėra","Vidinė","Išorinė"))
}

alpha_map <- c("Nėra" = 1.0, "Vidinė" = 0.68, "Išorinė" = 0.43)

## 31 požymiai
X_31 <- df_ns_mm[, feat_all, drop = FALSE]
out_mat_31 <- as.data.frame(lapply(X_31, iqr_labels_one))
outlier_31 <- combine_outliers(out_mat_31)

## 6 požymiai (geriausi)
X_6 <- df_ns_mm[, best_features, drop = FALSE]
out_mat_6 <- as.data.frame(lapply(X_6, iqr_labels_one))
outlier_6 <- combine_outliers(out_mat_6)

# 2. MDS koordinačių data.frame-ai

df_mds31 <- data.frame(
    label = df_ns_mm$label,
    set   = df_ns_mm$set,
    M1    = mds_31$conf[,1],
    M2    = mds_31$conf[,2]
)

df_mds6 <- data.frame(
    label = df_ns_mm$label,
    set   = df_ns_mm$set,
    M1    = mds_6$conf[,1],
    M2    = mds_6$conf[,2]
)

test_idx <- which(df_ns_mm$set == "test")

# 3. Sprendimo ribos (Naive Bayes 2D)

make_nb_boundary <- function(nb_model, df_mds){
    x_seq <- seq(min(df_mds$M1), max(df_mds$M1), length.out = 200)
    y_seq <- seq(min(df_mds$M2), max(df_mds$M2), length.out = 200)
    grid  <- expand.grid(M1 = x_seq, M2 = y_seq)
    
    pr <- predict(nb_model, newdata = grid, type = "raw")
    # darom prielaidą, kad 1 stulpelis = N, 2 = S
    grid$diff <- pr[,1] - pr[,2]
    grid
}

# ribai scenarijuose A ir C naudosim atskirai treniruotą NB MDS2,
# tik vizualizacijai (klasifikavimo matų tai nekeičia)

# 31D vizualizacijai 2D
nb_31_vis  <- naiveBayes(label ~ M1 + M2,
                         data = df_mds31[df_mds31$set == "train", ])
bd_31_vis  <- make_nb_boundary(nb_31_vis, df_mds31)

# 31D MDS2 – naudojam jau esamą nb_mds31
bd_31_mds  <- make_nb_boundary(nb_mds31, df_mds31)

# 6D vizualizacijai 2D
nb_6_vis   <- naiveBayes(label ~ M1 + M2,
                         data = df_mds6[df_mds6$set == "train", ])
bd_6_vis   <- make_nb_boundary(nb_6_vis, df_mds6)

# 6D MDS2 – naudojam nb_mds6
bd_6_mds   <- make_nb_boundary(nb_mds6, df_mds6)

# 4. Helperis vienam grafiko scenarijui

make_nb_mds_plot <- function(df_mds, outlier_vec, pred_test,
                             boundary_df, main_title, mets_vec){
    
    # tik test aibė
    df_test <- df_mds[test_idx, ]
    df_test$outlier   <- outlier_vec[test_idx]
    df_test$alpha_out <- alpha_map[as.character(df_test$outlier)]
    df_test$pred      <- pred_test
    df_test$misclass  <- factor(
        ifelse(df_test$pred == df_test$label, "Teisingai", "Neteisingai"),
        levels = c("Teisingai","Neteisingai")
    )
    
    subt <- sprintf("Test: Accuracy = %.3f, Precision = %.3f, Recall = %.3f, F1 = %.3f",
                    mets_vec["accuracy"],
                    mets_vec["precision_macro"],
                    mets_vec["recall_macro"],
                    mets_vec["f1_macro"])
    
    ggplot(df_test, aes(M1, M2)) +
        geom_contour(
            data    = boundary_df,
            aes(z = diff),
            breaks   = 0,
            colour   = "black",
            linewidth = 0.6
        ) +
        geom_point(
            aes(color = label,
                shape = misclass,
                alpha = alpha_out),
            size   = ifelse(df_test$misclass == "Neteisingai", 5.2, 3),
            stroke = ifelse(df_test$misclass == "Neteisingai", 1.4, 0.4)
        ) +
        scale_alpha_identity(guide = "none") +
        scale_shape_manual(
            values = c("Teisingai" = 16, "Neteisingai" = 4),
            name   = "Klasifikavimo rezultatas"
        ) +
        scale_color_discrete(name = "Tikra klasė") +
        theme_minimal() +
        theme(
            plot.title.position = "plot",
            plot.title    = element_text(hjust = 0.5, face = "bold"),
            plot.subtitle = element_text(hjust = 0.5),
            legend.position = "right"
        ) +
        labs(
            title    = main_title,
            subtitle = subt,
            x = "MDS1",
            y = "MDS2"
        ) +
        guides(
            color = guide_legend(order = 1, override.aes = list(size = 3)),
            shape = guide_legend(order = 2, override.aes = list(size = 3))
        )
}

# 5. Prognozės TEST aibei kiekvienam scenarijui

# A) 31D originali erdvė – pred iš nb_31 (test_31 turi tik šiuos test indeksus)
test_31  <- test_set[, c("label", feat_all)]
pred_31T <- predict(nb_31, newdata = test_31)

# B) 31D MDS2 – pred iš nb_mds31
test_mds31  <- df_mds31[df_mds31$set == "test", ]
pred_31MDS2 <- predict(nb_mds31, newdata = test_mds31)

# C) 6D originali erdvė
test_6   <- test_set[, c("label", best_features)]
pred_6T  <- predict(nb_6, newdata = test_6)

# D) 6D MDS2
test_mds6  <- df_mds6[df_mds6$set == "test", ]
pred_6MDS2 <- predict(nb_mds6, newdata = test_mds6)

# 6. Keturi grafikai

p_A_31_orig <- make_nb_mds_plot(
    df_mds      = df_mds31,
    outlier_vec = outlier_31,
    pred_test   = pred_31T,
    boundary_df = bd_31_vis,
    main_title  = "Naive Bayes test aibės klasifikavimas (31 požymis, klasifikuota prieš MDS 2D pritaikymą)",
    mets_vec    = met_31_test
)

p_B_31_mds <- make_nb_mds_plot(
    df_mds      = df_mds31,
    outlier_vec = outlier_31,
    pred_test   = pred_31MDS2,
    boundary_df = bd_31_mds,
    main_title  = "Naive Bayes test aibės klasifikavimas (31 požymis, klasifikuota po MDS 2D pritaikymo)",
    mets_vec    = met_mds31_test
)

p_C_6_orig <- make_nb_mds_plot(
    df_mds      = df_mds6,
    outlier_vec = outlier_6,
    pred_test   = pred_6T,
    boundary_df = bd_6_vis,
    main_title  = "Naive Bayes test aibės klasifikavimas (6 požymiai, klasifikuota prieš MDS 2D pritaikymą)",
    mets_vec    = met_6_test
)

p_D_6_mds <- make_nb_mds_plot(
    df_mds      = df_mds6,
    outlier_vec = outlier_6,
    pred_test   = pred_6MDS2,
    boundary_df = bd_6_mds,
    main_title  = "Naive Bayes test aibės klasifikavimas (6 požymiai, klasifikuota po MDS 2D pritaikymo)",
    mets_vec    = met_mds6_test
)

# parodyti
print(p_A_31_orig)
print(p_B_31_mds)
print(p_C_6_orig)
print(p_D_6_mds)
