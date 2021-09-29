ref <- readRDS("/home/lab401/Desktop/Bioinf_job/Analyses/Tool AMD/data/reference/genetic_risk.rds")
df <- data.frame(gene = unique(ref$gene), selected_genotype = NA, or.norm = NA)
combs <- readxl::read_xlsx("/home/lab401/Analyses/Tool AMD/data/reference/combinations.xlsx")
