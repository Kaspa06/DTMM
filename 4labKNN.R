need <- c("tidyverse", "janitor", "kableExtra", "psych",
          "smacof", "caret")

to_install <- setdiff(need, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, dep = TRUE)

invisible(lapply(need, library, character.only = TRUE))

# 1. DUOMENŲ NUSKAITYMAS IR PIRMINIS TVARKYMAS

# PASIKEISK, jei pas tave kitas kelias:
setwd("C:/Users/kaspa/Desktop/Mokslas/DTMM")

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

# Paliekam tik N, S, V
df_clean <- df_raw %>%
  filter(label %in% c("N","S","V")) %>%
  droplevels()

# Šalinam NA eilutes
df_clean <- df_clean[complete.cases(df_clean), ]

table(df_clean$label)

# 2. N IR S KLASĖS, PO 1000 KIEKVIENOS

set.seed(1111111)

df_ns <- df_clean %>%
  filter(label %in% c("N","S")) %>%
  group_by(label) %>%
  slice_sample(n = 1000, replace = FALSE) %>%
  ungroup() %>%
  droplevels()

table(df_ns$label)  # N = 1000, S = 1000
nrow(df_ns)         # 2000

# 31 požymis = visi skaitiniai stulpeliai, išskyrus label
feat_all <- names(df_ns)[sapply(df_ns, is.numeric)]
feat_all <- setdiff(feat_all, "label")

length(feat_all)    # turėtų būti apie 31

X_orig_full <- df_ns[, feat_all, drop = FALSE]  # nenormuoti požymiai
y_ns        <- df_ns$label                      # klasės N / S

# 3. APRAŠOMOJI STATISTIKA

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

# N klasės aprašomoji statistika
df_N <- df_ns[df_ns$label == "N", ]
tbl_N_31 <- make_stats_table_long(df_N, feat_all)

tbl_N_31 %>%
  kable(digits = 3,
        caption = "Aprašomoji statistika – klasė N (nenormuota, požymiai eilutėse)") %>%
  kable_styling(full_width = FALSE)

# S klasės aprašomoji statistika (nenormuota)
df_S <- df_ns[df_ns$label == "S", ]
tbl_S_31 <- make_stats_table_long(df_S, feat_all)

tbl_S_31 %>%
  kable(digits = 3,
        caption = "Aprašomoji statistika – klasė S (nenormuota, požymiai eilutėse)") %>%
  kable_styling(full_width = FALSE)

# 4. MIN–MAX NORMAVIMAS

range01 <- function(x) {
  r <- range(x, na.rm = TRUE)
  if (r[1] == r[2]) rep(0.5, length(x)) else (x - r[1])/(r[2] - r[1])
}

X_mm_full <- as.data.frame(lapply(X_orig_full, range01))

# Pagrindinė NORMUOTA aibė klasifikavimui / klasterizavimui
df_ns_mm <- bind_cols(label = y_ns, X_mm_full)

# 5. DIMENSIJOS MAŽINIMAS MDS (Manhattan, 600 iteracijų)

D_man <- dist(X_mm_full, method = "manhattan")

set.seed(1111111)
fit_mds_ns <- smacof::mds(
  delta   = D_man,
  type    = "ratio",
  ndim    = 2,
  itmax   = 600,
  init    = "random",
  verbose = FALSE
)

mds_coords <- as.data.frame(fit_mds_ns$conf)
colnames(mds_coords) <- c("MDS1", "MDS2")

# Aibė po dimensijos mažinimo (2D koordinatės)
df_ns_mds <- bind_cols(label = y_ns, mds_coords)

# 2D MDS grafikas
p_mds <- ggplot(df_ns_mds, aes(x = MDS1, y = MDS2, color = label)) +
  geom_point(size = 1.8, alpha = 0.8) +
  theme_minimal() +
  labs(
    title = "Metrinis MDS (Manhattan, 600 iteracijų) – N ir S klasės",
    x = "MDS1",
    y = "MDS2",
    color = "Klasė"
  )

print(p_mds)

# 6. DUOMENŲ PADALIJIMAS Į TRAIN / VALIDATION / TEST (NORMUOTA AIBĖ)

set.seed(1111111)

# 6.1. Test – 20 % visos imties
idx_test <- caret::createDataPartition(df_ns_mm$label, p = 0.20, list = FALSE)
test_set <- df_ns_mm[idx_test, ]
trainval <- df_ns_mm[-idx_test, ]

# 6.2. Iš likusių 80 % – 80 % train, 20 % val
idx_train <- caret::createDataPartition(trainval$label, p = 0.80, list = FALSE)
train_set <- trainval[idx_train, ]
val_set   <- trainval[-idx_train, ]

# proporcijų patikra
n_all <- nrow(df_ns_mm)
c(
  Train = nrow(train_set) / n_all,
  Val   = nrow(val_set)   / n_all,
  Test  = nrow(test_set)  / n_all
)

prop.table(table(train_set$label))
prop.table(table(val_set$label))
prop.table(table(test_set$label))

# ============================================================
# 7. K-NN KLASIFIKAVIMAS (ORIGINALŪS 31 POŽYMIS)
# ============================================================

library(class)

# Atskiriame požymius ir klases
X_train <- train_set[, -1]
y_train <- train_set$label

X_val   <- val_set[, -1]
y_val   <- val_set$label

X_test  <- test_set[, -1]
y_test  <- test_set$label

# ------------------------------------------------------------
# 7.1. OPTIMALAUS k PAIEŠKA (1–15) PAGAL VALIDACIJĄ
# ------------------------------------------------------------

k_vals <- seq(1, 15, by = 2)
acc_vec <- numeric(length(k_vals))

for (i in seq_along(k_vals)) {
  k <- k_vals[i]
  y_val_pred <- knn(
    train = X_train,
    test  = X_val,
    cl    = y_train,
    k     = k
  )
  cm_val <- table(y_val, y_val_pred)
  acc_vec[i] <- sum(diag(cm_val)) / sum(cm_val)
}

k_results <- data.frame(
  k = k_vals,
  Validation_Accuracy = acc_vec
)

k_results

# Optimalus k
k_opt <- k_results$k[which.max(k_results$Validation_Accuracy)]
k_opt

k_opt <- 3
# ------------------------------------------------------------
# 7.2. GALUTINIS K-NN SU OPTIMALIU k (TEST RINKINYS)
# ------------------------------------------------------------

y_test_pred <- knn(
  train = X_train,
  test  = X_test,
  cl    = y_train,
  k     = k_opt,
  prob  = TRUE
)

# SUMAIŠYMO MATRICA
cm_test <- table(Actual = y_test, Predicted = y_test_pred)
cm_test

# ------------------------------------------------------------
# 7.3. KLASIFIKAVIMO METRIKOS
# ------------------------------------------------------------

n  <- sum(cm_test)
diag_vals <- diag(cm_test)
rowsums <- apply(cm_test, 1, sum)
colsums <- apply(cm_test, 2, sum)

precision <- diag_vals / colsums
recall    <- diag_vals / rowsums
f1        <- 2 * precision * recall / (precision + recall)

metrics_class <- data.frame(precision, recall, f1)
metrics_class

accuracy <- sum(diag_vals) / n
macroPrecision <- mean(precision)
macroRecall    <- mean(recall)
macroF1        <- mean(f1)

metrics_global <- data.frame(
  Accuracy = accuracy,
  MacroPrecision = macroPrecision,
  MacroRecall = macroRecall,
  MacroF1 = macroF1
)

metrics_global


# ============================================================
# 8. K-NN KLASIFIKAVIMAS MDS ERDVĖJE (2D)
# ============================================================

df_mds_full <- bind_cols(label = y_ns, mds_coords)

df_mds_train <- df_mds_full[idx_train, ]
df_mds_test  <- df_mds_full[idx_test, ]

X_train_mds <- df_mds_train[, c("MDS1","MDS2")]
y_train_mds <- df_mds_train$label

X_test_mds  <- df_mds_test[, c("MDS1","MDS2")]
y_test_mds  <- df_mds_test$label

y_test_pred_mds <- knn(
  train = X_train_mds,
  test  = X_test_mds,
  cl    = y_train_mds,
  k     = k_opt
)

cm_test_mds <- table(Actual = y_test_mds, Predicted = y_test_pred_mds)
cm_test_mds
# ------------------------------------------------------------
# 8.1. KLASIFIKAVIMO METRIKOS (MDS ERDVĖ)
# ------------------------------------------------------------

n_mds  <- sum(cm_test_mds)
diag_vals_mds <- diag(cm_test_mds)
rowsums_mds <- apply(cm_test_mds, 1, sum)
colsums_mds <- apply(cm_test_mds, 2, sum)

precision_mds <- diag_vals_mds / colsums_mds
recall_mds    <- diag_vals_mds / rowsums_mds
f1_mds        <- 2 * precision_mds * recall_mds / 
  (precision_mds + recall_mds)

metrics_class_mds <- data.frame(
  precision = precision_mds,
  recall    = recall_mds,
  f1        = f1_mds
)

metrics_class_mds


accuracy_mds <- sum(diag_vals_mds) / n_mds
macroPrecision_mds <- mean(precision_mds)
macroRecall_mds    <- mean(recall_mds)
macroF1_mds        <- mean(f1_mds)

metrics_global_mds <- data.frame(
  Accuracy = accuracy_mds,
  MacroPrecision = macroPrecision_mds,
  MacroRecall = macroRecall_mds,
  MacroF1 = macroF1_mds
)

metrics_global_mds

# ============================================================
# 9. SPRENDIMO RIBŲ VIZUALIZACIJA (MDS)
# ============================================================

set <- df_mds_train

X1 <- seq(min(set$MDS1)-0.1, max(set$MDS1)+0.1, by = 0.01)
X2 <- seq(min(set$MDS2)-0.1, max(set$MDS2)+0.1, by = 0.01)
grid_set <- expand.grid(X1, X2)
colnames(grid_set) <- c("MDS1", "MDS2")

y_grid <- knn(
  train = X_train_mds,
  test  = grid_set,
  cl    = y_train_mds,
  k     = k_opt
)

plot(set$MDS1, set$MDS2,
     col = ifelse(set$label == "N", "red3", "green4"),
     pch = 19,
     xlab = "MDS1",
     ylab = "MDS2",
     main = paste("K-NN klasifikavimas MDS erdvėje (k =", k_opt, ")")
)

contour(X1, X2,
        matrix(as.numeric(y_grid), length(X1), length(X2)),
        add = TRUE)

points(set$MDS1, set$MDS2,
       pch = 21,
       bg = ifelse(set$label == "N", "red3", "green4"))

# ============================================================
# 10. K-NN PARAMETRŲ ĮTAKOS TYRIMAS:
#     k, POŽYMIŲ SKAIČIUS (31 vs MDS2) IR BALSAVIMO TIPAS (paprastas, svorinis)
# ============================================================

# Svoriniam K-NN reikalinga papildoma biblioteka
if (!"kknn" %in% rownames(installed.packages())) {
  install.packages("kknn", dep = TRUE)
}
library(kknn)

# ---- 10.1. MDS train/validation rinkinių sudarymas taip pat,
# kaip ir originalių požymių atveju ----

df_mds_full <- bind_cols(label = y_ns, mds_coords)

# Train+validation indeksai yra tie patys kaip originalioje aibėje:
# df_ns_mm -> idx_test (20 % test)
df_mds_trainval <- df_mds_full[-idx_test, ]      # 80 % (train + val)
df_mds_train    <- df_mds_trainval[idx_train, ]  # 64 % train
df_mds_val      <- df_mds_trainval[-idx_train, ] # 16 % val

# ---- 10.2. Pagalbinė funkcija metrikoms iš sumaišymo matricos ----

compute_metrics_from_cm <- function(cm) {
  n  <- sum(cm)
  diag_vals <- diag(cm)
  rowsums <- apply(cm, 1, sum)
  colsums <- apply(cm, 2, sum)
  
  precision <- diag_vals / colsums
  recall    <- diag_vals / rowsums
  f1        <- 2 * precision * recall / (precision + recall)
  
  acc  <- sum(diag_vals) / n
  mp   <- mean(precision)
  mr   <- mean(recall)
  mf1  <- mean(f1)
  
  data.frame(
    Accuracy       = acc,
    MacroPrecision = mp,
    MacroRecall    = mr,
    MacroF1        = mf1
  )
}

# ---- 10.3. Nagrinėjami parametrai ----

k_vals       <- seq(1, 15, by = 2)            # k = 1,3,5,7,9,11,13,15
feature_sets <- c("original", "mds")          # 31 požymis vs MDS2
voting_types <- c("simple", "weighted")       # paprastas vs svorinis balsavimas

# Rezultatų kaupykla
results_params <- data.frame(
  FeatureSet     = character(),
  Voting         = character(),
  k              = integer(),
  Accuracy       = numeric(),
  MacroPrecision = numeric(),
  MacroRecall    = numeric(),
  MacroF1        = numeric(),
  stringsAsFactors = FALSE
)

# ---- 10.4. Parametrų kombinacijų tyrimas ----

for (fs in feature_sets) {
  for (vt in voting_types) {
    for (k in k_vals) {
      
      if (fs == "original") {
        # 31 požymis: train_set ir val_set
        train_data <- train_set
        val_data   <- val_set
        
        if (vt == "simple") {
          # Paprastas balsavimas – class::knn
          X_train_fs <- train_data[, -1]
          y_train_fs <- train_data$label
          
          X_val_fs   <- val_data[, -1]
          y_val_fs   <- val_data$label
          
          y_val_pred <- knn(
            train = X_train_fs,
            test  = X_val_fs,
            cl    = y_train_fs,
            k     = k
          )
          
        } else if (vt == "weighted") {
          # Svorinis balsavimas – kknn
          fit_kknn <- kknn(
            label ~ .,
            train = train_data,
            test  = val_data,
            k     = k,
            kernel = "triangular"   # svorinė kaimynų funkcija
          )
          
          y_val_pred <- fitted(fit_kknn)
          y_val_fs   <- val_data$label
        }
        
      } else if (fs == "mds") {
        # MDS2 požymiai: MDS1, MDS2
        train_data <- data.frame(
          label = df_mds_train$label,
          MDS1  = df_mds_train$MDS1,
          MDS2  = df_mds_train$MDS2
        )
        
        val_data <- data.frame(
          label = df_mds_val$label,
          MDS1  = df_mds_val$MDS1,
          MDS2  = df_mds_val$MDS2
        )
        
        if (vt == "simple") {
          X_train_fs <- train_data[, c("MDS1", "MDS2")]
          y_train_fs <- train_data$label
          
          X_val_fs   <- val_data[, c("MDS1", "MDS2")]
          y_val_fs   <- val_data$label
          
          y_val_pred <- knn(
            train = X_train_fs,
            test  = X_val_fs,
            cl    = y_train_fs,
            k     = k
          )
          
        } else if (vt == "weighted") {
          fit_kknn <- kknn(
            label ~ .,
            train = train_data,
            test  = val_data,
            k     = k,
            kernel = "triangular"
          )
          
          y_val_pred <- fitted(fit_kknn)
          y_val_fs   <- val_data$label
        }
      }
      
      # Jei naudojom simple voting, y_val_fs apibrėžiame aiškiai
      if (vt == "simple") {
        y_val_fs <- val_data$label
      }
      
      # Sumaišymo matrica ir metrikos
      cm_val <- table(Actual = y_val_fs, Predicted = y_val_pred)
      mets   <- compute_metrics_from_cm(cm_val)
      
      results_params <- rbind(
        results_params,
        data.frame(
          FeatureSet     = fs,
          Voting         = vt,
          k              = k,
          mets,
          stringsAsFactors = FALSE
        )
      )
    }
  }
}

# ---- 10.5. Rezultatų suapvalinimas ir lentelės ataskaitai ----

results_params_round <- results_params %>%
  mutate(
    Accuracy       = round(Accuracy, 4),
    MacroPrecision = round(MacroPrecision, 4),
    MacroRecall    = round(MacroRecall, 4),
    MacroF1        = round(MacroF1, 4)
  )

# Pilna parametrų lentelė (viskas: k, FeatureSet, Voting)
results_params_round

# Tik paprastas balsavimas – lentelė, kurią jau naudoji ataskaitoje
results_simple_only <- results_params_round %>%
  filter(Voting == "simple")

results_simple_only %>%
  knitr::kable(
    caption = "K-NN klasifikatoriaus kokybės matų priklausomybė nuo kaimynų skaičiaus k ir požymių skaičiaus (paprastas balsavimas, validavimo rinkinys)",
    align = "c"
  ) %>%
  kableExtra::kable_styling(full_width = FALSE)

# Papildomai – tik svorinis balsavimas (jei norėsi atskiros lentelės)
results_weighted_only <- results_params_round %>%
  filter(Voting == "weighted")

results_weighted_only %>%
  knitr::kable(
    caption = "K-NN klasifikatoriaus kokybės matų priklausomybė nuo kaimynų skaičiaus k ir požymių skaičiaus (svorinis balsavimas, validavimo rinkinys)",
    align = "c"
  ) %>%
  kableExtra::kable_styling(full_width = FALSE)


# ============================================================
# 11. POŽYMIŲ ATRANKA PAGAL t-STATISTIKĄ IR 6 POŽYMIŲ KOMBINACIJOS (K-NN)
#    (VISKAS DAROMA TIK TRAIN AIBĖJE)
# ============================================================

library(class)

# Naudosime K-NN su fiksuotu k = 3 požymių rinkinių tyrimui
k_fs <- 3

# ------------------------------------------------------------
# 11.1. t-STATISTIKOS APSKAIČIAVIMAS VISIEMS POŽYMIAMS (TRAIN)
# ------------------------------------------------------------

# visi požymiai train aibėje, išskyrus label
all_feats_train <- setdiff(colnames(train_set), "label")

t_stats <- sapply(all_feats_train, function(f) {
  xN <- train_set[train_set$label == "N", f]
  xS <- train_set[train_set$label == "S", f]
  abs(t.test(xN, xS)$statistic)
})

t_table <- data.frame(
  Požymis      = all_feats_train,
  t_statistika = as.numeric(t_stats)
) %>%
  arrange(desc(t_statistika))

# 12 geriausių požymių pagal t-statistiką (4 lentelė)
top12_features <- t_table[1:12, ]

top12_features %>%
  kable(
    digits  = 6,
    caption = "4 lentelė. Geriausi 12 požymių pagal t-statistiką (apskaičiuota train aibėje)"
  ) %>%
  kable_styling(full_width = FALSE)

# sąrašas tolesniam naudojimui (tai YRA train_set stulpelių vardai)
top12_list <- as.character(top12_features$Požymis)

# ------------------------------------------------------------
# 11.2. VISOS 6 POŽYMIŲ KOMBINACIJOS IŠ 12 GERIAUSIŲ (TRAIN)
# ------------------------------------------------------------

feature_combinations <- combn(top12_list, 6, simplify = FALSE)
length(feature_combinations)  # turėtų būti 924

# ------------------------------------------------------------
# 11.3. K-NN TRENIRAVIMAS IR VERTINIMAS 6-POŽYMIŲ RINKINIAMS (TRAIN)
# ------------------------------------------------------------

knn_fs_results <- data.frame(
  FeatureSet = character(),
  Accuracy   = numeric(),
  Precision  = numeric(),
  Recall     = numeric(),
  F1         = numeric(),
  stringsAsFactors = FALSE
)

for (comb in feature_combinations) {
  
  comb <- as.character(comb)  # 6 požymių vardai
  
  # indeksai train_set stulpeliuose
  idx <- match(comb, colnames(train_set))
  
  # jei kažkurio požymio nerado – praleidžiam kombinaciją (kad nekristų su error)
  if (any(is.na(idx))) {
    # gali pasitikrinti kokie, jei nori:
    # warning("Praleidžiama kombinacija: ", paste(comb[is.na(idx)], collapse = ", "))
    next
  }
  
  # Poaibis iš train aibės (tik šie 6 požymiai)
  train_sub_X <- train_set[, idx, drop = FALSE]
  train_sub_y <- train_set$label
  
  # K-NN su fiksuotu k_fs, testuojame ant tų pačių train duomenų
  y_train_pred <- knn(
    train = train_sub_X,
    test  = train_sub_X,
    cl    = train_sub_y,
    k     = k_fs
  )
  
  # sumaišymo matrica ir metrikos
  cm <- table(Actual = train_sub_y, Predicted = y_train_pred)
  
  n         <- sum(cm)
  diag_vals <- diag(cm)
  rowsums   <- apply(cm, 1, sum)
  colsums   <- apply(cm, 2, sum)
  
  precision_class <- diag_vals / colsums
  recall_class    <- diag_vals / rowsums
  f1_class        <- 2 * precision_class * recall_class /
    (precision_class + recall_class)
  
  accuracy  <- sum(diag_vals) / n
  precision <- mean(precision_class, na.rm = TRUE)
  recall    <- mean(recall_class,    na.rm = TRUE)
  f1        <- mean(f1_class,        na.rm = TRUE)
  
  knn_fs_results <- rbind(
    knn_fs_results,
    data.frame(
      FeatureSet = paste(comb, collapse = " + "),
      Accuracy   = accuracy,
      Precision  = precision,
      Recall     = recall,
      F1         = f1,
      stringsAsFactors = FALSE
    )
  )
}

# ------------------------------------------------------------
# 11.4. 10 GERIAUSIŲ 6 POŽYMIŲ RINKINIŲ PAGAL F1 (TRAIN REZULTATAI)
# ------------------------------------------------------------

top10_knn_fs <- knn_fs_results %>%
  arrange(desc(F1), desc(Accuracy)) %>%
  head(10) %>%
  mutate(
    Accuracy  = round(Accuracy, 5),
    Precision = round(Precision, 5),
    Recall    = round(Recall, 5),
    F1        = round(F1, 5)
  )

# 5 lentelė ataskaitai
top10_knn_fs %>%
  kable(
    caption = paste0(
      "5 lentelė. 10 geriausių 6 požymių rinkinių (K-NN, k = ",
      k_fs,
      ", įvertinta pagal train aibę)"
    ),
    align = "c"
  ) %>%
  kable_styling(full_width = FALSE)

# ============================================================
# 12. K-NN su 6 atrinktais požymiais
#     r_val + signal_std + wl_side + q_pos + r_pos + signal_mean
#     (k = 3, paprastas balsavimas)
# ============================================================

library(class)

# 6 geriausių požymių rinkinys
top6_feats <- c("r_val", "signal_std", "wl_side",
                "q_pos", "r_pos", "signal_mean")

# ------------------------------------------------------------
# 12.1. NORMUOTA AIBĖ TIK SU 6 POŽYMIAIS
#      naudojame tuos pačius train/val/test indeksus
# ------------------------------------------------------------

# df_ns_mm: label + 31 normuoti požymiai
df_ns_mm_6 <- df_ns_mm[, c("label", top6_feats)]

# train/val/test su tais pačiais indeksais kaip ir anksčiau
train_set_6 <- df_ns_mm_6[rownames(train_set), ]
val_set_6   <- df_ns_mm_6[rownames(val_set), ]
test_set_6  <- df_ns_mm_6[rownames(test_set), ]

# ------------------------------------------------------------
# 12.2. K-NN KLASIFIKAVIMAS SU 6 POŽYMIAIS (ORIGINALI ERDVĖ)
#      k = 3, paprastas balsavimas, vertiname TEST rinkinyje
# ------------------------------------------------------------

k_6 <- 3

X_train_6 <- train_set_6[, -1]
y_train_6 <- train_set_6$label

X_test_6  <- test_set_6[, -1]
y_test_6  <- test_set_6$label

y_test_pred_6 <- knn(
  train = X_train_6,
  test  = X_test_6,
  cl    = y_train_6,
  k     = k_6,
  prob  = TRUE
)

# Sumaišymo matrica
cm_test_6 <- table(Actual = y_test_6, Predicted = y_test_pred_6)
cm_test_6

# Klasifikavimo kokybės metrikos
n6        <- sum(cm_test_6)
diag6     <- diag(cm_test_6)
rowsums6  <- apply(cm_test_6, 1, sum)
colsums6  <- apply(cm_test_6, 2, sum)

precision6 <- diag6 / colsums6
recall6    <- diag6 / rowsums6
f1_6       <- 2 * precision6 * recall6 / (precision6 + recall6)

metrics_class_6 <- data.frame(
  Class     = names(precision6),
  Precision = precision6,
  Recall    = recall6,
  F1        = f1_6
)
metrics_class_6

accuracy_6       <- sum(diag6) / n6
macroPrecision_6 <- mean(precision6)
macroRecall_6    <- mean(recall6)
macroF1_6        <- mean(f1_6)

metrics_global_6 <- data.frame(
  Accuracy       = accuracy_6,
  MacroPrecision = macroPrecision_6,
  MacroRecall    = macroRecall_6,
  MacroF1        = macroF1_6
)

metrics_global_6 %>%
  kable(
    digits = 5,
    caption = paste0(
      "K-NN rezultatai su 6 požymiais (k = ", k_6,
      ", test rinkinys)"
    )
  ) %>%
  kable_styling(full_width = FALSE)

# ============================================================
# 13. K-NN SU 6 POŽYMIAIS MDS ERDVĖJE (2D)
# ============================================================

# ------------------------------------------------------------
# 13.1. MDS (Manhattan) tik iš 6 požymių
# ------------------------------------------------------------

X_6_full <- df_ns_mm_6[, top6_feats]          # be 'label'
D_man_6  <- dist(X_6_full, method = "manhattan")

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

df_ns_mds_6 <- bind_cols(label = df_ns_mm_6$label, mds_coords_6)

# train ir test MDS erdvėje (naudojame tuos pačius indeksus)
df_mds_train_6 <- df_ns_mds_6[rownames(train_set), ]
df_mds_test_6  <- df_ns_mds_6[rownames(test_set), ]

X_train_mds_6 <- df_mds_train_6[, c("MDS1","MDS2")]
y_train_mds_6 <- df_mds_train_6$label

X_test_mds_6  <- df_mds_test_6[, c("MDS1","MDS2")]
y_test_mds_6  <- df_mds_test_6$label

# ------------------------------------------------------------
# 13.2. K-NN MDS ERDVĖJE (k = 3)
# ------------------------------------------------------------

y_test_pred_mds_6 <- knn(
  train = X_train_mds_6,
  test  = X_test_mds_6,
  cl    = y_train_mds_6,
  k     = k_6
)

cm_test_mds_6 <- table(Actual = y_test_mds_6, Predicted = y_test_pred_mds_6)
cm_test_mds_6

# Metrikos MDS erdvėje
n_mds_6   <- sum(cm_test_mds_6)
diag_mds6 <- diag(cm_test_mds_6)
rows_mds6 <- apply(cm_test_mds_6, 1, sum)
cols_mds6 <- apply(cm_test_mds_6, 2, sum)

precision_mds6 <- diag_mds6 / cols_mds6
recall_mds6    <- diag_mds6 / rows_mds6
f1_mds6        <- 2 * precision_mds6 * recall_mds6 /
  (precision_mds6 + recall_mds6)

metrics_class_mds_6 <- data.frame(
  Class     = names(precision_mds6),
  Precision = precision_mds6,
  Recall    = recall_mds6,
  F1        = f1_mds6
)
metrics_class_mds_6

accuracy_mds6       <- sum(diag_mds6) / n_mds_6
macroPrecision_mds6 <- mean(precision_mds6)
macroRecall_mds6    <- mean(recall_mds6)
macroF1_mds6        <- mean(f1_mds6)

metrics_global_mds_6 <- data.frame(
  Accuracy       = accuracy_mds6,
  MacroPrecision = macroPrecision_mds6,
  MacroRecall    = macroRecall_mds6,
  MacroF1        = macroF1_mds6
)

metrics_global_mds_6 %>%
  kable(
    digits = 5,
    caption = paste0(
      "K-NN rezultatai MDS erdvėje (6 požymiai, k = ",
      k_6, ", test rinkinys)"
    )
  ) %>%
  kable_styling(full_width = FALSE)

# ------------------------------------------------------------
# 13.3. Sprendimo ribų vizualizacija MDS erdvėje – TEST rinkinys
# ------------------------------------------------------------

# Ribas skaičiuojame pagal TRAIN
set_train <- df_mds_train_6

X1_6 <- seq(min(set_train$MDS1) - 0.1,
            max(set_train$MDS1) + 0.1,
            by = 0.01)

X2_6 <- seq(min(set_train$MDS2) - 0.1,
            max(set_train$MDS2) + 0.1,
            by = 0.01)

grid_set_6 <- expand.grid(X1_6, X2_6)
colnames(grid_set_6) <- c("MDS1", "MDS2")

# K-NN prognozės tinkleliui (naudojam TRAIN!)
y_grid_6 <- knn(
  train = X_train_mds_6,
  test  = grid_set_6,
  cl    = y_train_mds_6,
  k     = k_6
)

# -----------------------------
# TEST taškai + sprendimo ribos
# -----------------------------

mis_idx <- y_test_mds_6 != y_test_pred_mds_6   # klaidos

plot(X_test_mds_6$MDS1, X_test_mds_6$MDS2,
     col = ifelse(y_test_mds_6 == "N", "red3", "green4"),
     pch = 19,
     xlab = "MDS1",
     ylab = "MDS2",
     main = paste("K-NN klasifikavimas MDS erdvėje (6 požymiai, k =",
                  k_6, ") – TEST rinkinys")
)

# Sprendimo ribos
contour(X1_6, X2_6,
        matrix(as.numeric(y_grid_6),
               length(X1_6), length(X2_6)),
        add = TRUE)
