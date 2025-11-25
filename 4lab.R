need <- c("tidyverse", "janitor", "kableExtra", "psych",
          "smacof", "caret")

to_install <- setdiff(need, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, dep = TRUE)

invisible(lapply(need, library, character.only = TRUE))

# 1. DUOMENŲ NUSKAITYMAS IR PIRMINIS TVARKYMAS

# PASIKEISK, jei pas tave kitas kelias:
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
