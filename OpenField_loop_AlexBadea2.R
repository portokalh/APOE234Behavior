library(ggplot2)
library(patternplot)
library(lme4)
library(visreg)
library(tidyr)
library(magrittr) 
library(dplyr)
library(reshape2)
library(ggpubr)
library(ggridges)
library(rstatix)
library(tidyverse)
library(tidyr)
library(EnvStats)
library(openxlsx)
library(emmeans)
library(effectsize)

short_sheet_name <- function(name) {
  max_length <- 20
  #truncated_name <- substr(name, 1, max_length - nchar(prefix))
  truncated_name <- substr(name, 1, max_length)
  paste0(truncated_name)
}

# Define color palette for Diet
diet_colors <- c("Control" = "green", "HFD" = "purple")

file_path <- "NOR_df.csv"  # Replace with the actual file path

# Define the folder path
output_folder <- file.path(getwd(), "OpenField_Results")

# Create the folder if it doesn't exist
if (!dir.exists(output_folder)) {
  dir.create(output_folder)
}

# Save
# Save ANOVA results as CSV
anova_csv_path <- file.path(output_folder, "NOR_anova_results2.csv")
anova_excel_path <- file.path(output_folder,"NOR_anova_results2.xlsx")


df_NOR<-read.csv(file_path)

df_NOR <- df_NOR %>%
  filter(Stage == "openfield")

df_NOR <- df_NOR %>%
  filter(APOE %in% c("APOE2", "APOE3", "APOE4"))

# df_NOR <- df_NOR %>%
#   mutate(APOE = ifelse(is.na(APOE) & Genotype == "KO", "KO", APOE))
# 
# updated_file_path <- file.path(output_folder, "Updated_NOR_df.csv")
#

#df_NOR <- df_NOR %>%
#  mutate(HN = ifelse(APOE == "KO", -1, HN))

# # Save the updated data frame as a CSV
#write.csv(df_NOR, updated_file_path, row.names = FALSE)
#cat("Updated data frame saved as CSV to:", updated_file_path, "\n")

# Convert relevant variables to factors
df_NOR <- df_NOR %>%
  mutate(
    APOE = as.factor(APOE),
    HN = as.factor(HN),
    Age = as.factor(Age),
    Diet = as.factor(Diet),
    Stage = as.factor(Stage)
  )

# Verify the changes
str(df_NOR)

df_NOR <- df_NOR %>%
  filter(!is.na(APOE))

df_NOR <- df_NOR %>%
  select(-starts_with("object"))


# Calculate the threshold for 50% missing values
threshold <- nrow(df_NOR) * 0.5

# Identify columns with index greater than 13 and more than 50% NAs
columns_to_keep <- which(colSums(is.na(df_NOR)) <= threshold | 1:13)

# Filter the columns
df_NOR <- df_NOR[, columns_to_keep]

# Check the updated data frame
str(df_NOR)

#Percentage of Time Spent in the Center Zone
df_NOR <- df_NOR %>%
  mutate(
    Total_time = Time.freezing + Time.mobile,
    Tracked_time = Centerzone_time.mobile + Centerzone_time.immobile + Centerzone.time.freezing + Outerzone_time.mobile + Outerzone_time.immobile + Outerzone_time.freezing,
    Percentage_time_center = (Centerzone_time.mobile / Tracked_time) * 100,
    Percentage_time_outerzone=(Outerzone_time.mobile / Tracked_time) * 100,
  )

# Print column names
colnames(df_NOR)

# ggplot(df_NOR, aes(x = Percentage_time_center, y = Percentage_time_outerzone)) +
#   geom_point() +
#   theme_minimal()

data_openfield <- df_NOR %>% filter(Stage == "openfield")

# Filter out columns with more than 50% NA values
valid_columns <- colnames(data_openfield)[colMeans(is.na(data_openfield)) <= 0.5]
wb <- createWorkbook()




# Loop through valid columns starting from the 11th column
for (i in 13:length(valid_columns)) {
  #for (i in 13) {
  column_name <- valid_columns[i]  # Get the column name
  sheet_name <- short_sheet_name(column_name)
  cat("Processing Column Index:", i, "Column Name:", column_name, "\n")
  
  lm_model <- lm(unlist(data_openfield[, column_name]) ~ APOE * HN * Age * Sex * Diet, data = data_openfield)
  
  # ANOVA results
  anova_results <- anova(lm_model)
  anova_results_df <- as.data.frame(anova_results)
  anova_results_df$Factor <- rownames(anova_results)
  anova_results_df <- anova_results_df %>%
  select(Factor, everything())  # Reorder columns
  rownames(anova_results_df) <- NULL
  addWorksheet(wb, paste0("LM_", sheet_name))
  writeData(wb, paste0("LM_", sheet_name), anova_results_df)
  
  # Eta2 effect sizes and confidence intervals
  effect_sizes <- effectsize::eta_squared(lm_model, partial = TRUE, ci = 0.95)
  effect_sizes_df <- as.data.frame(effect_sizes)
  addWorksheet(wb, paste0("Eta2_", sheet_name))
  writeData(wb, paste0("Eta2_", sheet_name), effect_sizes_df)
  
  # Posthoc pairwise comparisons
  #myres <- na.omit((emmeans(lm_model, list(pairwise ~ APOE), adjust="tukey")$`pairwise differences of APOE`))
  
  posthoc_results <- na.omit(emmeans(lm_model, pairwise ~ APOE * Diet, adjust = "tukey"))
  pairwise_results <- pairs(posthoc_results)
  #pairwise_results_df <- as.data.frame(pairwise_results)
  pairwise_results_df <- as.data.frame(posthoc_results$contrasts)
  addWorksheet(wb, paste0("Pairwise_", sheet_name))
  writeData(wb, paste0("Pairwise_", sheet_name), pairwise_results_df, startRow = 1, colNames = TRUE)
  
  
  
    violin_plot <- ggplot(data_openfield, aes(x = APOE, y = !!sym(column_name), fill = Diet)) +
      geom_violin(alpha = 0.3, trim = FALSE) +
      geom_jitter(aes(color = Diet), position = position_jitterdodge(jitter.width = 0.3, dodge.width = 0.9), alpha = 0.9) +
      geom_boxplot(width = 0.1, position = position_dodge(0.9), outlier.shape = NA, alpha = 0.2) +
      scale_fill_manual(values = diet_colors) +
      scale_color_manual(values = diet_colors) +
      labs(
        title = paste("Distribution of", column_name, "by APOE and Diet"),
        x = "APOE Genotype",
        y = column_name,
        fill = "Diet"
      ) +
      theme_bw()
    
    violin_plot_path <- file.path(output_folder, paste0(sheet_name, "_violin_plot.png"))
    ggsave(violin_plot_path, violin_plot, width = 8, height = 6, dpi = 300)
    
    # Generate and save line plot with standard errors
    summary_data <- data_openfield %>%
      group_by(APOE, Diet) %>%
      summarise(
        Mean = mean(!!sym(column_name), na.rm = TRUE),
        SE = sd(!!sym(column_name), na.rm = TRUE) / sqrt(n()),
        .groups = "drop"
      )
    
    line_plot <- ggplot(summary_data, aes(x = APOE, y = Mean, group = Diet, color = Diet)) +
      geom_line(size = 1) +
      geom_point(size = 2) +
      geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE), width = 0.2, size = 0.8) +
      scale_color_manual(values = diet_colors) +
      labs(
        title = paste("Mean and SE of", column_name, "by APOE and Diet"),
        x = "APOE Genotype",
        y = paste("Mean", column_name),
        color = "Diet"
      ) +
      theme_bw()
    
    line_plot_path <- file.path(output_folder, paste0(sheet_name, "_line_plot.png"))
    ggsave(line_plot_path, line_plot, width = 8, height = 6, dpi = 300)
    
    cat("Saved plots for column:", column_name, "\n")

  
}

# Save the workbook
output_file_path <- file.path(output_folder, "Filtered_ANOVA_and_EffectSizes.xlsx")
saveWorkbook(wb, output_file_path, overwrite = TRUE)

cat("Filtered ANOVA results and effect sizes saved to:", output_file_path, "\n")

# Create an Excel workbook
wb <- createWorkbook()

# Loop through valid columns
for (i in 13:length(valid_columns)) {
  column_name <- valid_columns[i]
  sheet_name <- short_sheet_name(column_name)
  
  
  cat("Processing Column:", column_name, "\n")
  
  # Fit the linear model
  lm_model <- lm(unlist(data_openfield[[column_name]]) ~ APOE * Diet, data = data_openfield)
  
  # Compute estimated marginal means (EMMs)
  emmeans_results <- tryCatch(
    {
      emmeans(lm_model, ~ APOE * Diet)
    },
    error = function(e) {
      cat("Error in EMMs for column:", column_name, "\n")
      NULL
    }
  )
  
  if (!is.null(emmeans_results)) {
    # Extract means, SE, and confidence intervals
    means_df <- as.data.frame(summary(emmeans_results))
    
    # Rearrange columns for clarity
    means_df <- means_df %>%
      select(APOE, Diet, emmean, SE, df, lower.CL, upper.CL) %>%
      rename(
        Mean = emmean,
        Standard_Error = SE,
        Degrees_of_Freedom = df,
        Lower_Confidence_Limit = lower.CL,
        Upper_Confidence_Limit = upper.CL
      )
    
    # Add the sheet to the workbook
    addWorksheet(wb, sheet_name)
    writeData(wb, sheet_name, means_df)
  }
}

# Save the workbook
output_path <- file.path(output_folder, "Column_Means_SEs.xlsx")
saveWorkbook(wb, output_path, overwrite = TRUE)

cat("Means and SEs saved to:", output_path, "\n")

wb <- createWorkbook()
output_path <- file.path(output_folder, "Pairs.xlsx")

for (i in 13:length(valid_columns)) {
  #for (i in 13) {
  column_name <- valid_columns[i]  # Get the column name
  sheet_name <- short_sheet_name(column_name)
  cat("Processing Column Index:", i, "Column Name:", column_name, "\n")
  
  #lm_model <- lm(unlist(data_openfield[, column_name]) ~ APOE * HN * Age * Sex * Diet, data = data_openfield)
  
  # Fit the linear model
  lm_model <- lm(unlist(data_openfield[, column_name]) ~ APOE * Diet, data = data_openfield)
  pairwise_results <- emmeans(lm_model, pairwise ~ APOE * Diet, adjust = "tukey")
  contrast_df <- as.data.frame(pairwise_results$contrasts)
  
  
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet_name, contrast_df)
}

  saveWorkbook(wb, output_path, overwrite = TRUE)
  cat("Pairwise comparisons saved to:", output_path, "\n")