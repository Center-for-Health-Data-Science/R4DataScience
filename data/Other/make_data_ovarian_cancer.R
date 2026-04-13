
#if (!require("BiocManager", quietly = TRUE))
#  install.packages("BiocManager")
#BiocManager::install("TCGAbiolinks")
library(TCGAbiolinks)
library(SummarizedExperiment)
library(tidyverse)
library(DESeq2)

# ----------------------------
# 1. Download TCGA-OV STAR counts
# ----------------------------
query_exp <- GDCquery(
  project = "TCGA-OV",
  data.category = "Transcriptome Profiling",
  data.type = "Gene Expression Quantification",
  workflow.type = "STAR - Counts"
)

res <- getResults(query_exp)
colnames(res)
table(res$sample_type)

GDCdownload(
  query = query_exp,
  method = "api",
  files.per.chunk = 50
)

se_ov <- GDCprepare(query_exp)

# counts: genes x samples
count_mat <- assay(se_ov, "unstranded")

# optional checks
dim(count_mat)
assayNames(se_ov)
head(colnames(count_mat))
head(rownames(count_mat))


# ----------------------------
# 2. Map Ensembl IDs to gene symbols
# ----------------------------
row_annot <- as.data.frame(rowData(se_ov))
colnames(row_annot)

# choose the gene-symbol column automatically
symbol_col <- intersect(
  c("gene_name", "external_gene_name", "gene_symbol", "symbol"),
  colnames(row_annot)
)[1]

if (is.na(symbol_col)) {
  stop("Could not find a gene symbol column in rowData(se_ov).")
}

gene_symbols <- row_annot[[symbol_col]]

# keep genes with a valid symbol
keep_symbol <- !is.na(gene_symbols) & gene_symbols != ""
count_mat2 <- count_mat[keep_symbol, , drop = FALSE]
gene_symbols2 <- gene_symbols[keep_symbol]

# collapse duplicated gene symbols by summing counts
count_df <- as.data.frame(count_mat2)
count_df$gene_symbol <- gene_symbols2

count_by_symbol <- count_df %>%
  group_by(gene_symbol) %>%
  summarise(across(everything(), sum), .groups = "drop") %>%
  as.data.frame()

rownames(count_by_symbol) <- count_by_symbol$gene_symbol
count_by_symbol$gene_symbol <- NULL

count_by_symbol <- as.matrix(count_by_symbol)

dim(count_by_symbol)
head(rownames(count_by_symbol))

# ----------------------------
# 3. Filter low-expressed genes
# ----------------------------
min_count <- 10
min_prop_samples <- 0.20
min_samples <- ceiling(ncol(count_by_symbol) * min_prop_samples)

keep_genes <- rowSums(count_by_symbol >= min_count) >= min_samples
table(keep_genes)

count_filt <- count_by_symbol[keep_genes, , drop = FALSE]

dim(count_filt)

# ----------------------------
# 4. Make a simple expression matrix for scoring
# ----------------------------
# Practical and robust for this use case
expr_mat <- log2(count_filt + 1)

summary(as.vector(expr_mat))


#if (!require("BiocManager", quietly = TRUE))
#  install.packages("BiocManager")

#BiocManager::install("EDASeq")
library(EDASeq)

# ----------------------------
# 5. Compute stemness score
# ----------------------------
data("SC_PCBC_stemSig", package = "TCGAbiolinks")

# check overlap before scoring
common_stem_genes <- intersect(names(SC_PCBC_stemSig), rownames(expr_mat))
length(common_stem_genes)
head(common_stem_genes)

stemness_df <- TCGAanalyze_Stemness(
  stemSig = SC_PCBC_stemSig,
  dataGE = expr_mat,
  colname.score = "stemness_score"
)

head(stemness_df)
dim(stemness_df)



# ----------------------------
# 6. Build sample metadata and attach stemness
# ----------------------------
meta_ov <- as.data.frame(colData(se_ov)) %>%
  rownames_to_column("sample_barcode_full") %>%
  mutate(
    sample_id16 = substr(sample_barcode_full, 1, 16),
    case_id12   = substr(sample_barcode_full, 1, 12)
  )

# make stemness join robust to column naming
names(stemness_df)[1] <- "sample_barcode_full"
stemness_df <- stemness_df %>%
  mutate(
    sample_id16 = substr(sample_barcode_full, 1, 16),
    case_id12   = substr(sample_barcode_full, 1, 12)
  )

meta_scores <- meta_ov %>%
  left_join(
    stemness_df %>% select(sample_id16, stemness_score),
    by = "sample_id16"
  )

head(meta_scores[, c("sample_barcode_full", "sample_id16", "stemness_score")])


# ----------------------------
# 7. Optional: add tumor purity
# ----------------------------
data("Tumor.purity", package = "TCGAbiolinks")

purity_ov <- Tumor.purity %>%
  filter(Cancer.type == "OV") %>%
  transmute(
    sample_id16 = substr(Sample.ID, 1, 16),
    ESTIMATE,
    ABSOLUTE,
    LUMP,
    IHC,
    CPE
  )

meta_scores <- meta_scores %>%
  left_join(purity_ov, by = "sample_id16")

head(meta_scores[, c("sample_id16", "stemness_score", "CPE", "ESTIMATE")])



############################################################

## row annotations
#row_df <- as.data.frame(rowData(se_ov))

# inspect available columns once
#colnames(row_df)

# use gene symbols when available
#gene_symbols <- row_df$gene_name

# fallback to Ensembl ID if symbol is missing/blank
#gene_symbols[is.na(gene_symbols) | gene_symbols == ""] <- row_df$gene_id[is.na(gene_symbols) | gene_symbols == ""]

# make unique, because multiple Ensembl IDs can share the same symbol
#gene_symbols <- make.unique(gene_symbols)

# replace rownames
#rownames(count_mat) <- gene_symbols


dim(se_ov)
dim(count_mat)
expr_mat <- count_mat[rowData(se_ov)$gene_type == "protein_coding", , drop = FALSE]
dim(expr_mat)

df_sample <- as.data.frame(colData(se_ov)) %>%
  tibble::rownames_to_column(var = "unique_patient_ID") %>%
  mutate(
    unique_patient_ID = gsub("\\.", "-", unique_patient_ID),
    case_id = substr(unique_patient_ID, 1, 12)
  )

#df_clin <- GDCquery_clinic(project = "TCGA-OV", type = "clinical")

#df_ov <- df_sample %>%
#  left_join(df_clin, by = c("case_id" = "submitter_id"))

df_ov <- df_sample
getwd()
save(expr_mat, df_ov, meta_scores, file = "data/other/GDC_Ovarian_RAW_B.RData")

##############################################################################################################
##############################################################################################################
##############################################################################################################

sel_col <- c("sample_barcode_full",
             "unique_patient_ID", 
             "vital_status" ,  
             "classification_of_tumor" , 
             "days_to_death",
             "figo_stage"  , 
             "follow_ups_disease_response"  ,
             "tumor_grade", 
             "vital_status" , 
             "sample" , 
             "sample_type",
             "classification_of_tumor",
             "days_to_last_follow_up",
             "CPE", 
             "stemness_score")


df_ov <- meta_scores[, colnames(meta_scores) %in% sel_col ]

names(df_ov)[names(df_ov) == "sample_barcode_full"] <- "unique_patient_ID"


library(TCGAbiolinks)
library(dplyr)


getwd()
save(expr_mat, df_ov, file = "data/other/GDC_Ovarian_RAW.RData")

# PLOT ####
##############################################################################################################
##############################################################################################################
##############################################################################################################

#if (!require("BiocManager", quietly = TRUE))
#  install.packages("BiocManager")
#BiocManager::install("edgeR")
library(edgeR)
library(tidyverse)
library(ggpubr)
library(viridis)

expr_mat2 <- expr_mat

keep_genes <- rowSums(expr_mat2 >= 1) >= 0.01 * ncol(expr_mat2)
table(keep_genes)
#keep_genes <- filterByExpr(expr_mat2, min.prop = 0.01) ############ 1
#table(keep_genes)
expr_mat2 <- expr_mat2[keep_genes, , drop = FALSE]

sample_stats <- tibble(
  sample_id = colnames(expr_mat2),
  mean_expr = colMeans2(expr_mat2, na.rm = TRUE)
) %>%
  mutate(decile = ntile(mean_expr, 10))

set.seed(123)

gene_stats <- tibble(
  gene = rownames(expr_mat2),
  mean_expr = rowMeans(expr_mat2, na.rm = TRUE)
) %>%
  mutate(decile = ntile(mean_expr, 10))

sel_genes <- gene_stats %>%
  #filter(decile!=10) %>%
  group_by(decile) %>%
  slice_sample(n = 200) %>%
  ungroup()

expr_mat2 <- expr_mat2[sel_genes$gene, , drop = FALSE]

expr_mat_plot <- as.data.frame(t(expr_mat2), check.names = FALSE)
dim(expr_mat_plot)
head(expr_mat_plot)[1:5,1:5]

set.seed(123)
#expr_mat_plot <- expr_mat_plot[sample(nrow(expr_mat_plot), size = 300),]

# 4 samples from each decile = 40 total
sel_samples <- sample_stats %>%
  filter(decile!=1) %>%
  group_by(decile) %>%
  group_modify(~ dplyr::slice_sample(.x, n = min(5, nrow(.x)))) %>%
  ungroup()

expr_mat_plot <- expr_mat_plot[sel_samples$sample_id, , drop = FALSE]


# 2. Create variance stabilized versions
df_matrix <- expr_mat_plot
log2_df <- log2(expr_mat_plot+1)
rlog_df <- rlog(as.matrix(df_matrix+1), blind = TRUE)
vst_df  <- varianceStabilizingTransformation(as.matrix(df_matrix+1), blind = TRUE)

# Function to prepare mean-SD data
make_mean_sd_df <- function(mat) {
  tibble(
    mean = colMeans(mat, na.rm = TRUE),
    sd   = apply(mat, 2, sd, na.rm = TRUE)
  ) %>%
    mutate(rank = rank(mean, ties.method = "first"))
}

# Prepare data frames
df_raw  <- make_mean_sd_df(df_matrix)
df_log2 <- make_mean_sd_df(log2_df)
df_rlog <- make_mean_sd_df(rlog_df)
df_vst  <- make_mean_sd_df(vst_df)

# Plot function
plot_mean_sd <- function(df, title, y_max = NULL) {
  p <- ggplot(df, aes(x = rank, y = sd, color = rank)) +
    geom_point(alpha = 0.65, size = 1) +
    scale_color_viridis_c(option = "D", end = 0.95, guide = "none") +
    theme_minimal() +
    labs(
      title = title,
      x = "Mean Rank",
      y = "Standard Deviation"
    ) 
  #+geom_smooth(method = "loess")
  
  if (!is.null(y_max)) {
    p <- p + coord_cartesian(ylim = c(0, y_max))
  }
  
  p
}

# Make plots
p1 <- plot_mean_sd(df_raw,  "Mean-Sd: Raw Counts", y_max = quantile(df_raw$sd, 0.99))
p2 <- plot_mean_sd(df_log2, "Mean-Sd: log2(counts + 1)", y_max = quantile(df_log2$sd, 0.999))
p3 <- plot_mean_sd(df_rlog, "Mean-Sd: rlog", y_max = quantile(df_rlog$sd, 0.97))
p3 <- plot_mean_sd(df_rlog, "Mean-Sd: rlog", y_max = 3.5)
p4 <- plot_mean_sd(df_vst,  "Mean-Sd: VST",  y_max = quantile(df_vst$sd, 1))
p4 <- plot_mean_sd(df_vst,  "Mean-Sd: VST",  y_max = 4)

# Arrange
p_all <- ggarrange(p1, p2, p3, p4, ncol = 2, nrow = 2)
p_all
ggsave("fugures/mean_sd_plot_small.png", plot = p_all, width = 6, height = 5.2, dpi = 300)
ggsave("fugures/mean_sd_plot_large.png", plot = p_all, width = 7.5, height = 6.5, dpi = 300)

# UPDATE LOAD ####
##############################################################################################################
##############################################################################################################
##############################################################################################################


load("./data/Ovarian_data.RData")
df_ov_old <- df_ov
df_ov_old$unique_patient_ID <- str_sub(df_ov_old$alt_sample_name, 1,16)

load("./data/Other/GDC_Ovarian_RAW.RData")
head(df_ov_old)
head(df_ov)

names(df_ov_old)
names(df_ov)

dim(expr_mat)
keep_genes <- rowSums(expr_mat >= 3) >= 0.05 * ncol(expr_mat)
table(keep_genes)
expr_mat <- expr_mat[keep_genes, , drop = FALSE]
dim(expr_mat)

library(dplyr)

df_ov_old2 <- df_ov_old %>%
  mutate(unique_patient_ID = unique_patient_ID)

df_ov2 <- df_ov %>%
  mutate(unique_patient_ID = substr(unique_patient_ID, 1, 16))

dim(df_ov_old2)
dim(df_ov2)

meta_cols <- c(
  "alt_sample_name",
  "unique_patient_ID",
  "sample_type",
  "histological_type",
  "primarysite",
  "tumorstage",
  "summarygrade",
  "grade",
  "age_at_initial_path_diagn",
  "os_binary",
  "percent_normal_cells",
  "percent_stromal_cells",
  "percent_tumor_cells",
  "batch"
)

df_ov_old2 <- df_ov_old2[, meta_cols]



df_ov_overlap <- df_ov_old2 %>%
  full_join(df_ov2, by = "unique_patient_ID")
dim(df_ov_overlap)


meta_cols <- c(
  "alt_sample_name",
  "unique_patient_ID",
  "sample_type.y",
  "histological_type",
  "primarysite",
  "tumor_grade",
  "summarygrade",
  "grade",
  "figo_stage",
  "tumorstage",
  "age_at_initial_path_diagn",
  "percent_normal_cells",
  "percent_stromal_cells",
  "percent_tumor_cells",
  "os_binary",
  "days_to_death",
  "days_to_last_follow_up",
  "CPE",
  "stemness_score",
  "batch",
  "vital_status",
  "classification_of_tumor"
)

df_ov_overlap <- df_ov_overlap[, meta_cols]
df_ov_overlap$primarysite <- NA

#names(df_ov_overlap)[names(df_ov_overlap) == "age_at_initial_pathologic_diagnosis"] <- "age_at_initial_path_diagn"


# Remove .x and .y from column names
names(df_ov_overlap) <- names(df_ov_overlap) %>%
  gsub("\\.x$", "", .) %>%
  gsub("\\.y$", "", .)


set.seed(123)

# Recode vital_status
df_ov_overlap <- df_ov_overlap %>%
  mutate(
    vital_status = case_when(
      vital_status == "Alive" ~ "living",
      vital_status == "Dead"  ~ "deceased",
      TRUE ~ as.character(vital_status)
    )
  )

# Work on character first
df_ov_overlap$vital_status <- as.character(df_ov_overlap$vital_status)

# 10% of deceased -> "disease "
idx_deceased <- which(df_ov_overlap$vital_status == "deceased")
n_deceased <- ceiling(0.10 * length(idx_deceased))
sel_deceased <- sample(idx_deceased, n_deceased)

df_ov_overlap$vital_status[sel_deceased] <- "deceased "

# 5% of living -> "living "
idx_living <- which(df_ov_overlap$vital_status == "living")
n_living <- ceiling(0.05 * length(idx_living))
sel_living <- sample(idx_living, n_living)

df_ov_overlap$vital_status[sel_living] <- "living "
table(df_ov_overlap$vital_status)

# 5% of living -> "NA "
idx_living <- which(is.na(df_ov_overlap$vital_status))
n_living <- ceiling(0.05 * length(idx_living))
sel_living <- sample(idx_living, n_living)
df_ov_overlap$vital_status[sel_living] <- "NA "
table(df_ov_overlap$vital_status)


# 5% of living -> "NA "
idx_living <- which(is.na(df_ov_overlap$summarygrade))
n_living <- ceiling(0.05 * length(idx_living))
sel_living <- sample(idx_living, n_living)
df_ov_overlap$summarygrade[sel_living] <- "NA "
table(df_ov_overlap$summarygrade)


# Convert back to factor if you want
df_ov_overlap$summarygrade <- toupper(df_ov_overlap$summarygrade)
table(df_ov_overlap$summarygrade)
df_ov_overlap$summarygrade[85] 
df_ov_overlap$summarygrade[85] <- "NA"
df_ov_overlap$summarygrade[85] 

df_ov_overlap$summarygrade[106] 
df_ov_overlap$summarygrade[106] <- "NA"
df_ov_overlap$summarygrade[106] 

df_ov_overlap <- df_ov_overlap %>%
  mutate(
    n_missing_pct = rowSums(is.na(across(c(
      percent_normal_cells,
      percent_stromal_cells,
      percent_tumor_cells
    )))),
    
    percent_normal_cells = case_when(
      n_missing_pct == 1 & is.na(percent_normal_cells) ~ 100 - percent_stromal_cells - percent_tumor_cells,
      TRUE ~ percent_normal_cells
    ),
    
    percent_stromal_cells = case_when(
      n_missing_pct == 1 & is.na(percent_stromal_cells) ~ 100 - percent_normal_cells - percent_tumor_cells,
      TRUE ~ percent_stromal_cells
    ),
    
    percent_tumor_cells = case_when(
      n_missing_pct == 1 & is.na(percent_tumor_cells) ~ 100 - percent_normal_cells - percent_stromal_cells,
      TRUE ~ percent_tumor_cells
    )
  ) 

table(df_ov_overlap$n_missing_pct)
df_ov_overlap <- df_ov_overlap %>% select(-n_missing_pct)

df_ov_overlap %>%
  filter(
    percent_normal_cells < 0 | percent_normal_cells > 100 |
      percent_stromal_cells < 0 | percent_stromal_cells > 100 |
      percent_tumor_cells < 0 | percent_tumor_cells > 100
  )

df_ov <- df_ov_overlap



df_ov <- df_ov %>% 
  mutate(
    figo_stage = if_else(
      is.na(figo_stage) & !is.na(tumorstage),
      paste("Stage", as.character(as.roman(tumorstage))),
      figo_stage
    )
  )

df_ov <- df_ov %>% 
  mutate(
    tumor_grade = if_else(
      is.na(tumor_grade) & !is.na(grade),
      paste0("G", as.character((grade))),
      tumor_grade
    )
  )

df_ov$tumor_grade[df_ov$tumor_grade == "GB"&!is.na(df_ov$tumor_grade)] <- "G1"
df_ov$tumor_grade[df_ov$tumor_grade == "GX"&!is.na(df_ov$tumor_grade)] <- NA
table(df_ov$tumor_grade)


df_ov <- df_ov %>% 
  mutate(
    summarygrade = if_else(
      is.na(summarygrade) & !is.na(tumor_grade),
      paste0("G-", as.character((tumor_grade))),
      summarygrade
    )
  )

df_ov$summarygrade[df_ov$summarygrade == "G-GB"&!is.na(df_ov$summarygrade)] <- "LOW"
df_ov$summarygrade[df_ov$summarygrade == "G-GX"&!is.na(df_ov$summarygrade)] <- NA
df_ov$summarygrade[df_ov$summarygrade == "G-G2"&!is.na(df_ov$summarygrade)] <- "LOW"
df_ov$summarygrade[df_ov$summarygrade == "G-G1"&!is.na(df_ov$summarygrade)] <- "LOW"
df_ov$summarygrade[df_ov$summarygrade == "G-G3"&!is.na(df_ov$summarygrade)] <- "HIGH"
table(df_ov$summarygrade)

df_ov <- df_ov %>% 
  select(-tumorstage, -grade, -sample_type)

##############################################################################################################
##############################################################################################################
##############################################################################################################

dim(expr_mat)

head(expr_mat)[1:4, 1:4]
gene_symbols <- row.names(expr_mat)
#sel_idx <- grepl("^(COL\\d|CLDN)", gene_symbols)
sel_idx <- grepl("^(COL\\d)", gene_symbols)

table(sel_idx)
# Expression matrix: selected genes only
sel_expr_mat <- expr_mat[sel_idx, , drop = FALSE]
rownames(sel_expr_mat) <- gene_symbols[sel_idx]
dim(sel_expr_mat)


table(
  family = case_when(
    grepl("^COL",  rownames(sel_expr_mat)) ~ "Collagen",
    grepl("^CDH",  rownames(sel_expr_mat)) ~ "Cadherin",
    #grepl("^KRT",  rownames(sel_expr_mat)) ~ "Keratin",
    grepl("^MMP",  rownames(sel_expr_mat)) ~ "Matrix",
    grepl("^ITGA|^ITGB", rownames(sel_expr_mat)) ~ "Integrins",
    grepl("^CLDN|^OCLN$|^TJP", rownames(sel_expr_mat)) ~ "Tight junction",
    
    #grepl("^FN1",  rownames(sel_expr_mat)) ~ "Fibronectin",
    #grepl("^FBN",  rownames(sel_expr_mat)) ~ "Fibrillin",
    TRUE ~ "Other"
  )
)


sel_expr_mat[1:3,1:3]

# Make gene names unique if needed
rownames(sel_expr_mat) <- make.unique(rownames(sel_expr_mat))

# Transpose: now samples are rows, genes are columns
df_exp <- as.data.frame(t(sel_expr_mat), check.names = FALSE)

# Add patient ID as first column
df_exp <- rownames_to_column(df_exp, var = "unique_patient_ID")
df_exp[1:3,1:3]


# Replace "." with "-" in TCGA IDs
df_exp$unique_patient_ID <- gsub("\\.", "-", df_exp$unique_patient_ID)

ids <- df_exp$unique_patient_ID
ids<- str_sub(ids, 1,16)
head(ids)

df_ov <- df_ov %>%
  filter(unique_patient_ID %in% ids)

df_exp$unique_patient_ID <- str_sub(df_exp$unique_patient_ID, 1,16)

head(df_exp)[1:4, 1:4]
head(df_ov)[1:4, 1:4]


df_ov$classification_of_tumor[seq(1,430,4)] <- NA
df_ov$classification_of_tumor[seq(3,230,4)] <- NA
df_ov$classification_of_tumor[seq(200,430,4)] <- NA
df_ov$classification_of_tumor[seq(250,400,5)] <- NA
table(is.na(df_ov$classification_of_tumor))


df_ov <- df_ov %>%
  mutate(
    new_stromal = 100 - percent_tumor_cells - percent_normal_cells,
    percent_stromal_cells = case_when(
      percent_stromal_cells == 0 &
        percent_normal_cells > 0 &
        percent_tumor_cells > 0 &
        new_stromal >= 0 ~ new_stromal,
      TRUE ~ percent_stromal_cells
    )
  ) %>%
  select(-new_stromal)


df_ov <- df_ov %>% select(-percent_normal_cells, -days_to_death, -tumor_grade)

names(df_ov)[names(df_ov) == "days_to_last_follow_up"] <- "days_to_death_or_last_follow_up"

df_ov$CPE <- str_replace_all(df_ov$CPE, ",", ".")
df_ov$CPE <- as.numeric(df_ov$CPE)

df_ov[df_ov$unique_patient_ID == "TCGA-04-1514-01A",] 

df_ov[df_ov$unique_patient_ID == "TCGA-04-1514-01A", "CPE"] <- 0.1
df_ov[df_ov$unique_patient_ID == "TCGA-04-1514-01A", "percent_stromal_cells"] <- 1
df_ov[df_ov$unique_patient_ID == "TCGA-04-1514-01A", "percent_tumor_cells"] <- 3
df_ov[df_ov$unique_patient_ID == "TCGA-04-1514-01A", "stemness_score"] <- 0.01
df_ov[df_ov$unique_patient_ID == "TCGA-04-1514-01A", "age_at_initial_path_diagn"] <- 18
df_ov[df_ov$unique_patient_ID == "TCGA-04-1514-01A", "days_to_death_or_last_follow_up"] <- 5010


df_ov[df_ov$unique_patient_ID == "TCGA-04-1514-01A",] 

getwd()
save(df_ov,df_exp, file = "data/other/GDC_Ovarian.RData")


















