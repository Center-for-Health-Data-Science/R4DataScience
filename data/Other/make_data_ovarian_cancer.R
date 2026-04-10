
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
  workflow.type = "STAR - Counts",
  sample.type = "Primary Tumor"
)

GDCdownload(
  query = query_exp,
  method = "api",
  files.per.chunk = 50
)

se_ov <- GDCprepare(query_exp)


# counts: genes x samples
count_mat <- assay(se_ov, "unstranded")

# row annotations
row_df <- as.data.frame(rowData(se_ov))

# inspect available columns once
colnames(row_df)

# use gene symbols when available
gene_symbols <- row_df$gene_name

# fallback to Ensembl ID if symbol is missing/blank
gene_symbols[is.na(gene_symbols) | gene_symbols == ""] <- row_df$gene_id[is.na(gene_symbols) | gene_symbols == ""]

# make unique, because multiple Ensembl IDs can share the same symbol
gene_symbols <- make.unique(gene_symbols)

# replace rownames
rownames(count_mat) <- gene_symbols

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
save(expr_mat, df_ov, file = "data/other/GDC_Ovarian_RAW.RData")

##############################################################################################################
##############################################################################################################
##############################################################################################################

sel_col <- c("unique_patient_ID", 
             "vital_status" ,  
             "classification_of_tumor" , 
             "days_to_death",
             "figo_stage"  , 
             "follow_ups_disease_response"  ,
             "tumor_grade", 
             "vital_status" , 
             "sample" , 
             "days_to_last_follow_up")
df_ov <- df_ov[, colnames(df_ov) %in% sel_col ]

library(TCGAbiolinks)
library(dplyr)

data("Tumor.purity", package = "TCGAbiolinks")

# Inspect available columns once
colnames(Tumor.purity)

# Clean barcodes to match your df_ov
purity_df <- Tumor.purity %>%
  as.data.frame() %>%
  mutate(
    sample = gsub("\\.", "-", Sample.ID),
    IHC = suppressWarnings(as.numeric(gsub(",", ".", IHC))),
    CPE = suppressWarnings(as.numeric(gsub(",", ".", CPE)))
  ) %>%
  select(sample, IHC, CPE)

# Add to your metadata

df_ov <- df_ov %>%
  left_join(purity_df, by = "sample")

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

# LOAD ####
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

library(dplyr)

df_ov_old2 <- df_ov_old %>%
  mutate(unique_patient_ID = unique_patient_ID)

df_ov2 <- df_ov %>%
  mutate(unique_patient_ID = substr(unique_patient_ID, 1, 16))

dim(df_ov_old2)
dim(df_ov2)

df_ov_overlap <- df_ov_old2 %>%
  left_join(df_ov2, by = "unique_patient_ID")
dim(df_ov_overlap)

meta_cols <- c(
  "alt_sample_name",
  "unique_patient_ID",
  "sample_type",
  "histological_type",
  "primarysite",
  "summarygrade",
  "figo_stage",
  "tumor_grade",
  "summarystage" ,
  "age_at_initial_path_diagn",
  "classification_of_tumor",
  "recurrence_status",
  "days_to_death.y",
  "days_to_last_follow_up",
  "vital_status.y",
  "os_binary",
  "relapse_binary",
  "primary_therapy_outcome_success",
  "percent_normal_cells",
  "percent_stromal_cells",
  "percent_tumor_cells",
  "CPE",
  "batch"
)

df_ov_overlap <- df_ov_overlap[, meta_cols]

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


# Convert back to factor if you want
df_ov_overlap$summarygrade <- toupper(df_ov_overlap$summarygrade)
table(df_ov_overlap$summarygrade)


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

##############################################################################################################
##############################################################################################################
##############################################################################################################

dim(expr_mat)

head(expr_mat)[1:4, 1:4]
gene_symbols <- row.names(expr_mat)
sel_idx <- grepl("^(COL|CDH|KRT|ELN|LAMA|LAMB|LAMC|FN1|FBN)", gene_symbols)

# Expression matrix: selected genes only
sel_expr_mat <- expr_mat[sel_idx, , drop = FALSE]
rownames(sel_expr_mat) <- gene_symbols[sel_idx]
dim(sel_expr_mat)


table(
  family = case_when(
    grepl("^COL",  rownames(sel_expr_mat)) ~ "Collagen",
    grepl("^CDH",  rownames(sel_expr_mat)) ~ "Cadherin",
    grepl("^KRT",  rownames(sel_expr_mat)) ~ "Keratin",
    grepl("^ELN",  rownames(sel_expr_mat)) ~ "Elastin",
    grepl("^LAMA|^LAMB|^LAMC", rownames(sel_expr_mat)) ~ "Laminin",
    grepl("^FN1",  rownames(sel_expr_mat)) ~ "Fibronectin",
    grepl("^FBN",  rownames(sel_expr_mat)) ~ "Fibrillin",
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

getwd()
save(df_ov,df_exp, file = "data/other/GDC_Ovarian.RData")




















