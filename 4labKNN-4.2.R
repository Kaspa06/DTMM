# 14. GERIAUSIO K-NN (6 POŽYMIAI, k = 3) VERTINIMAS SKIRTINGOMIS VALIDAVIMO STRATEGIJOMIS
#      (HOLDOUT, 10-FOLD CV, BOOTSTRAP 0.632)

library(caret)
library(ggplot2)

# 14.1. Pagalbinė funkcija metrikoms iš (actual, predicted)

compute_metrics_general <- function(actual, predicted) {
    cm <- table(Actual = actual, Predicted = predicted)
    
    n         <- sum(cm)
    diag_vals <- diag(cm)
    rowsums   <- apply(cm, 1, sum)
    colsums   <- apply(cm, 2, sum)
    
    precision <- diag_vals / colsums
    recall    <- diag_vals / rowsums
    f1        <- 2 * precision * recall / (precision + recall)
    
    data.frame(
        Accuracy       = sum(diag_vals) / n,
        MacroPrecision = mean(precision),
        MacroRecall    = mean(recall),
        MacroF1        = mean(f1)
    )
}

# 14.2. HOLDOUT STRATEGIJA (jau turima, tik tvarkingai įvyniojam)
#      Modelis: K-NN, 6 požymiai, k = 3, paprastas balsavimas
#      Train: train_set_6, Test: test_set_6

# cm_test_6 ir metrics_global_6 jau apskaičiuoti anksčiau,
# bet jei nori aiškiai:

cm_holdout_6   <- cm_test_6
mets_holdout_6 <- compute_metrics_general(y_test_6, y_test_pred_6)

cm_holdout_6
mets_holdout_6

acc_holdout <- mets_holdout_6$Accuracy


# 14.3. CROSS-VALIDATION (10-fold CV)
#      Train: visa train+validation aibė (BE test), t.y. df_ns_mm_6[-idx_test, ]
#      Test: test_set_6


# Pilna 6 požymių aibė:
# df_ns_mm_6: label + r_val, signal_std, wl_side, q_pos, r_pos, signal_mean

df_trainval_6 <- df_ns_mm_6[-idx_test, ]  # 80 % be test
df_test_6     <- df_ns_mm_6[ idx_test, ]  # turi sutapti su test_set_6

# užtikriname faktoriaus lygmenis
df_trainval_6$label <- factor(df_trainval_6$label, levels = c("N","S"))
df_test_6$label     <- factor(df_test_6$label,     levels = c("N","S"))

set.seed(1111111)
ctrl_cv <- trainControl(
    method = "cv",
    number = 10
)

knn_cv_6 <- train(
    label ~ .,
    data      = df_trainval_6,
    method    = "knn",
    tuneGrid  = data.frame(k = k_6),  # k_6 = 3
    metric    = "Accuracy",
    trControl = ctrl_cv
)

# CV vidutinis tikslumas (resampling rezultatas)
acc_cv_resample <- knn_cv_6$results$Accuracy[1]

# Prognozės TEST aibei (holdout test) iš CV metu parinkto modelio
y_test_cv_pred <- predict(knn_cv_6, newdata = df_test_6)

cm_cv_test   <- table(Actual = df_test_6$label, Predicted = y_test_cv_pred)
mets_cv_test <- compute_metrics_general(df_test_6$label, y_test_cv_pred)

cm_cv_test
mets_cv_test

acc_cv_test <- mets_cv_test$Accuracy


# 14.4. BOOTSTRAP (0.632) STRATEGIJA
#      Train: df_trainval_6, Test: df_test_6


set.seed(1111111)
ctrl_boot <- trainControl(
    method = "boot632",
    number = 100
)

knn_boot_6 <- train(
    label ~ .,
    data      = df_trainval_6,
    method    = "knn",
    tuneGrid  = data.frame(k = k_6),
    metric    = "Accuracy",
    trControl = ctrl_boot
)

# Bootstrap vidutinis tikslumas (resampling rezultatas)
acc_boot_resample <- knn_boot_6$results$Accuracy[1]

# Prognozės TEST aibei
y_test_boot_pred <- predict(knn_boot_6, newdata = df_test_6)

cm_boot_test   <- table(Actual = df_test_6$label, Predicted = y_test_boot_pred)
mets_boot_test <- compute_metrics_general(df_test_6$label, y_test_boot_pred)

cm_boot_test
mets_boot_test

acc_boot_test <- mets_boot_test$Accuracy


# 14.5. Stačiakampės diagramos – validavimo strategijų palyginimas


# (A) Palyginam RESAMPLING vidutinius tikslumus (ką duoda CV ir Bootstrap kaip strategijos)
df_valid_resample <- data.frame(
    Strategija = factor(c("Holdout (train/test)", "10-fold CV", "Bootstrap 0.632"),
                        levels = c("Holdout (train/test)", "10-fold CV", "Bootstrap 0.632")),
    Accuracy   = c(acc_holdout, acc_cv_resample, acc_boot_resample)
)

ggplot(df_valid_resample, aes(x = Strategija, y = Accuracy)) +
    geom_col() +
    geom_text(aes(label = round(Accuracy, 4)),
              vjust = -0.5, size = 4) +
    ylim(0, 1) +
    theme_minimal() +
    labs(
        title = "Geriausio K-NN klasifikatoriaus tikslumas pagal validavimo strategiją (resampling įvertis)",
        x = "Validavimo strategija",
        y = "Accuracy"
    )

# (B) Jei nori, gali palyginti IR TEST aibės tikslumus (kaip skiriasi galutinio modelio veikimas)
df_valid_test <- data.frame(
    Strategija = factor(c("Holdout", "10-fold CV", "Bootstrap 0.632"),
                        levels = c("Holdout", "10-fold CV", "Bootstrap 0.632")),
    Accuracy   = c(acc_holdout, acc_cv_test, acc_boot_test)
)

ggplot(df_valid_test, aes(x = Strategija, y = Accuracy)) +
    geom_col() +
    geom_text(aes(label = round(Accuracy, 4)),
              vjust = -0.5, size = 4) +
    ylim(0, 1) +
    theme_minimal() +
    labs(
        title = "Geriausio K-NN klasifikatoriaus tikslumas TEST aibėje\n(po skirtingų validavimo strategijų)",
        x = "Validavimo strategija",
        y = "Accuracy (test rinkinys)"
    )


# 15. ROC KREIVĖ IR AUC GERIAUSIAM MODELIUI
#      (K-NN, 6 požymiai, k = 3, holdout test rinkinys)


if (!"pROC" %in% rownames(installed.packages())) {
    install.packages("pROC", dep = TRUE)
}
library(pROC)

# y_test_pred_6 buvo gautas su prob = TRUE, todėl turime tikimybes
prob_hat_6 <- attr(y_test_pred_6, "prob")

# P(S) = prob(S), jei prognozuota S; kitaip 1 - prob(S)
p_S_6 <- ifelse(y_test_pred_6 == "S", prob_hat_6, 1 - prob_hat_6)

# ROC ir AUC (pozityvi klasė – S)
roc_knn_6 <- pROC::roc(
    response  = y_test_6,
    predictor = as.numeric(p_S_6),
    levels    = c("N","S"),
    direction = "<"
)

auc_knn_6 <- pROC::auc(roc_knn_6)
auc_knn_6  # čia matysi skaičių konsolėje

# ROC grafikas
plot(
    roc_knn_6,
    main        = paste0("ROC kreivė: K-NN (6 požymiai, k = ", k_6,
                         "), AUC = ", round(auc_knn_6, 4)),
    legacy.axes = TRUE,
    lwd         = 2
)
abline(a = 0, b = 1, lty = 2, col = "gray50")



# 16. SKIRTINGŲ KLASIFIKATORIŲ (K-NN VARIANTŲ) PALYGINIMAS TEST RINKINYJE
#      Modeliai:
#        1) K-NN (31 požymis)                   – y_test, y_test_pred
#        2) K-NN (6 požymiai)                   – y_test_6, y_test_pred_6
#        3) K-NN (31 požymis, MDS2)             – y_test_mds, y_test_pred_mds
#        4) K-NN (6 požymiai, MDS2)             – y_test_mds_6, y_test_pred_mds_6


# 16.1. Jei dar neturi bendros funkcijos, naudojame tą pačią compute_metrics_general()

# 31 požymis (originali normuota 31D erdvė)
metrics_knn_31 <- compute_metrics_general(y_test, y_test_pred)

# 6 požymiai (geriausias modelis)
metrics_knn_6 <- compute_metrics_general(y_test_6, y_test_pred_6)

# 31 požymis MDS2
metrics_knn_31_mds <- compute_metrics_general(y_test_mds, y_test_pred_mds)

# 6 požymiai MDS2
metrics_knn_6_mds <- compute_metrics_general(y_test_mds_6, y_test_pred_mds_6)

# 16.2. Sudedam viską į vieną lentelę

results_models <- dplyr::bind_rows(
    `K-NN (31 požymis)`        = metrics_knn_31,
    `K-NN (6 požymiai)`        = metrics_knn_6,
    `K-NN (31 požymis, MDS2)`  = metrics_knn_31_mds,
    `K-NN (6 požymiai, MDS2)`  = metrics_knn_6_mds,
    .id = "Modelis"
)

results_models_round <- results_models %>%
    mutate(
        Accuracy       = round(Accuracy, 4),
        MacroPrecision = round(MacroPrecision, 4),
        MacroRecall    = round(MacroRecall, 4),
        MacroF1        = round(MacroF1, 4)
    )

results_models_round %>%
    knitr::kable(
        caption = "Skirtingų K-NN modelių palyginimas testavimo rinkinyje",
        align   = "c"
    ) %>%
    kableExtra::kable_styling(full_width = FALSE)





# 17. Multi-ROC: K-NN Holdout vs 10-fold CV vs Bootstrap 0.632

# jei dar neįdiegta
if (!"pROC" %in% rownames(installed.packages())) {
    install.packages("pROC", dep = TRUE)
}
library(pROC)

# 17.1. ROC objektai kiekvienai strategijai

## 1) HOLDOUT – naudojam klasikinį K-NN su 6 požymiais ir k = 3
## y_test_pred_6 jau gautas su prob = TRUE

prob_hat_6 <- attr(y_test_pred_6, "prob")
prob_S_holdout <- ifelse(y_test_pred_6 == "S", prob_hat_6, 1 - prob_hat_6)

roc_holdout <- pROC::roc(
    response  = y_test_6,
    predictor = prob_S_holdout,
    levels    = c("N","S"),
    direction = "<"
)

# 2) 10-fold CV modelis (knn_cv_6) – tikimybės P(S) TEST aibei
prob_cv <- predict(knn_cv_6, newdata = df_test_6, type = "prob")[,"S"]

roc_cv <- pROC::roc(
    response  = df_test_6$label,
    predictor = prob_cv,
    levels    = c("N","S"),
    direction = "<"
)

# 3) Bootstrap 0.632 modelis (knn_boot_6) – tikimybės P(S) TEST aibei
prob_boot <- predict(knn_boot_6, newdata = df_test_6, type = "prob")[,"S"]

roc_boot <- pROC::roc(
    response  = df_test_6$label,
    predictor = prob_boot,
    levels    = c("N","S"),
    direction = "<"
)

# 18.2. AUC kiekvienai kreivei

auc_holdout <- pROC::auc(roc_holdout)
auc_cv      <- pROC::auc(roc_cv)
auc_boot    <- pROC::auc(roc_boot)

auc_holdout
auc_cv
auc_boot

# 17.3. Gražus multi-ROC grafikas

plot(
    roc_holdout,
    col         = "black",
    lwd         = 3,
    legacy.axes = TRUE,      # x = FPR, y = TPR
    xlim        = c(0, 1),
    ylim        = c(0, 1),
    main        = "K-NN klasifikatoriaus ROC kreivės\nHoldout, 10-fold CV ir Bootstrap",
    xlab        = "False Positive Rate",
    ylab        = "True Positive Rate"
)

lines(roc_cv,   col = "blue",  lwd = 3)
lines(roc_boot, col = "darkgreen", lwd = 3)

abline(a = 0, b = 1, lty = 2, col = "gray60")  # atsitiktinė klasifikacija

legend(
    "bottomright",
    legend = c(
        paste0("K-NN Holdout (AUC = ",   round(auc_holdout, 4), ")"),
        paste0("K-NN 10-fold CV (AUC = ", round(auc_cv, 4), ")"),
        paste0("K-NN Bootstrap (AUC = ",  round(auc_boot, 4), ")")
    ),
    col  = c("black","blue","darkgreen"),
    lwd  = 3,
    bty  = "n"
)
