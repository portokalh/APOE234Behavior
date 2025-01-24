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
output_folder <- file.path(getwd(), "NOR_Results")

# Create the folder if it doesn't exist
if (!dir.exists(output_folder)) {
  dir.create(output_folder)
}

# Save
# Save ANOVA results as CSV
anova_csv_path <- file.path(output_folder, "NOR_anova_results2.csv")
anova_excel_path <- file.path(output_folder,"NOR_anova_results2.xlsx")


df_NOR<-read.csv(file_path)

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


#Percentage of Time Spent in the Center Zone
df_NOR <- df_NOR %>%
  mutate(
    Total_time = Time.freezing + Time.mobile,
    Tracked_time = Centerzone_time.mobile + Centerzone_time.immobile + Centerzone.time.freezing + Outerzone_time.mobile + Outerzone_time.immobile + Outerzone_time.freezing,
    #Total_time = Centerzone_time.mobile + Centerzone_time.immobile,
    Percentage_time_center = (Centerzone_time.mobile / Tracked_time) * 100,
    Percentage_time_outerzone=(Outerzone_time.mobile / Tracked_time) * 100,
  )

# Print column names
colnames(df_NOR)

ggplot(df_NOR, aes(x = Percentage_time_center, y = Percentage_time_outerzone)) +
  geom_point() +
  theme_minimal()

data_openfield <- df_NOR %>% filter(Stage == "openfield")
mylm1 <- lm(Percentage_time_center ~ APOE*HN*Age*Sex*Diet , data = data_openfield)
anova_results <- anova(mylm1)
anova_results_df <- as.data.frame(anova_results)
anova_results_df$Factor <- rownames(anova_results_df)

anova_results_df <- anova_results_df %>%
  select(Factor, everything())

rownames(anova_results_df) <- NULL


wb <- createWorkbook()

# Filter out columns with more than 50% NA values
valid_columns <- colnames(data_openfield)[colMeans(is.na(data_openfield)) <= 0.5]


# Loop through valid columns starting from the 11th column
for (i in 13:length(valid_columns)) {
  column_name <- valid_columns[i]  # Get the column name
  sheet_name <- short_sheet_name(column_name)
  # Print the column index and name
  cat("Processing Column Index:", i, "Column Name:", column_name, "\n")
  
  # Fit the linear model
  lm_model <- lm(unlist(data_openfield[, column_name]) ~ APOE * HN * Age * Sex * Diet, data = data_openfield)
  
  # ANOVA results
  anova_results <- anova(lm_model)
  anova_results_df <- as.data.frame(anova_results)
  anova_results_df$Factor <- rownames(anova_results)
  anova_results_df <- anova_results_df %>%
    select(Factor, everything())  # Reorder columns
  rownames(anova_results_df) <- NULL
  
  # Save ANOVA results to a worksheet
  addWorksheet(wb, paste0("LM_", sheet_name))
  writeData(wb, paste0("LM_", sheet_name), anova_results_df)
  
  # Compute Cohen's effect sizes and confidence intervals
  effect_sizes <- effectsize::eta_squared(lm_model, partial = TRUE, ci = 0.95)
  effect_sizes_df <- as.data.frame(effect_sizes)
  
  # Save effect sizes to a worksheet
  addWorksheet(wb, paste0("Cohen_", sheet_name))
  writeData(wb, paste0("Cohen_", sheet_name), effect_sizes_df)
  
  # Perform pairwise comparisons
  pairwise_results <- tryCatch(
    {
      emmeans_res <- emmeans(lm_model, list(pairwise ~ Sex | APOE), adjust = "tukey")
      posthoc_results <- as.data.frame(emmeans_res$`pairwise differences of Sex | APOE`)
      posthoc_results  # Return results
    },
    error = function(e) {
      cat("Error in pairwise comparisons for column:", column_name, "\n")
      NULL  # Return NULL if there's an error
    }
  )
  
  # Save pairwise comparisons to a worksheet if they exist
  if (!is.null(pairwise_results)) {
    addWorksheet(wb, paste0("Pairwise_", sheet_name))
    writeData(wb, paste0("Pairwise_", sheet_name), pairwise_results)
  }
  
  # ss_effect <- anova_results[["Sum Sq"]]
  # ss_error <- anova_results[["Sum Sq"]][length(ss_effect)]  # Residual SS (last entry)
  # 
  # # Calculate partial eta squared for each factor
  # partial_eta_squared <- ss_effect / (ss_effect + ss_error)
  # 
  # # Combine with factor names
  # partial_eta_df <- data.frame(
  #   Factor = rownames(anova_results),
  #   Partial_Eta_Squared = partial_eta_squared
  # )
  #   # Save effect sizes to a worksheet
  #   addWorksheet(wb, paste0("Peta2_", sheet_name))
  #   writeData(wb, paste0("Peta2_", sheet_name), effect_sizes_df)
  
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

# Filter data for APOE genotypes and diets
data_plot <- data_openfield %>%
  filter(!is.na(APOE), !is.na(Diet))  # Ensure no missing values in APOE and Diet

# Define color palette for Diet
diet_colors <- c("Control" = "green", "HFD" = "purple")

# Panel 1: Percentage Time in Center Zone
# Panel 1: Percentage Time in Center Zone
# Panel 1: Percentage Time in Center Zone
# Panel 1: Percentage Time in Center Zone
p1 <- ggplot(data_plot, aes(x = APOE, y = Percentage_time_center, fill = Diet)) +
  geom_violin(alpha = 0.3, trim = FALSE) +  # Increase transparency for violin plot
  geom_jitter(aes(color = Diet), position = position_jitterdodge(jitter.width = 0.3, dodge.width = 0.9), 
              alpha = 0.9) + # Keep point size but increase visibility with higher alpha
  geom_boxplot(width = 0.1, position = position_dodge(0.9), outlier.shape = NA, alpha = 0.2) + # Increase boxplot transparency
  scale_fill_manual(values = diet_colors) +
  scale_color_manual(values = diet_colors) + # Ensure consistent colors
  labs(
    title = "Percentage Time Mobile in Center Zone",
    x = "APOE Genotype",
    y = "Percentage Time",
    fill = "Diet"
  ) +
  theme_bw()

# Panel 2: Percentage Time in Outer Zone
p2 <- ggplot(data_plot, aes(x = APOE, y = Percentage_time_outerzone, fill = Diet)) +
  geom_violin(alpha = 0.3, trim = FALSE) +
  geom_jitter(aes(color = Diet), position = position_jitterdodge(jitter.width = 0.3, dodge.width = 0.9), 
              alpha = 0.9) +
  geom_boxplot(width = 0.1, position = position_dodge(0.9), outlier.shape = NA, alpha = 0.2) +
  scale_fill_manual(values = diet_colors) +
  scale_color_manual(values = diet_colors) +
  labs(
    title = "Percentage Time Mobile in Outer Zone",
    x = "APOE Genotype",
    y = "Percentage Time",
    fill = "Diet"
  ) +
  theme_bw()

# Panel 3: Time.mobile
p3 <- ggplot(data_plot, aes(x = APOE, y = Time.mobile, fill = Diet)) +
  geom_violin(alpha = 0.3, trim = FALSE) +
  geom_jitter(aes(color = Diet), position = position_jitterdodge(jitter.width = 0.3, dodge.width = 0.9), alpha = 0.9) +
  geom_boxplot(width = 0.1, position = position_dodge(0.9), outlier.shape = NA, alpha = 0.2) +
  scale_fill_manual(values = diet_colors) +
  scale_color_manual(values = diet_colors) +
  labs(
    title = "Time Mobile",
    x = "APOE Genotype",
    y = "Time (seconds)",
    fill = "Diet"
  ) +
  theme_bw()

# Panel 4: Time.freezing
p4 <- ggplot(data_plot, aes(x = APOE, y = Time.freezing, fill = Diet)) +
  geom_violin(alpha = 0.3, trim = FALSE) +
  geom_jitter(aes(color = Diet), position = position_jitterdodge(jitter.width = 0.3, dodge.width = 0.9), alpha = 0.9) +
  geom_boxplot(width = 0.1, position = position_dodge(0.9), outlier.shape = NA, alpha = 0.2) +
  scale_fill_manual(values = diet_colors) +
  scale_color_manual(values = diet_colors) +
  labs(
    title = "Time Freezing",
    x = "APOE Genotype",
    y = "Time (seconds)",
    fill = "Diet"
  ) +
  theme_bw()

# Combine the four panels
combined_plot <- ggarrange(p1, p2, p3, p4, 
                           ncol = 2, nrow = 2, 
                           common.legend = TRUE, legend = "bottom")

# Combine the two panels
#combined_plot <- ggarrange(p1, p2, ncol = 2, nrow = 1, common.legend = TRUE, legend = "bottom")

# Save the combined plot
output_plot_path <- file.path(output_folder, "OpenField_by_APOE.png")
#ggsave(output_plot_path, combined_plot, width = 12, height = 6, scale=1, unit=c("in"), dpi=300)
ggsave(output_plot_path, combined_plot, width = 12, height = 12, scale=1, unit=c("in"), dpi=300)

cat("Two-panel figure saved to:", output_plot_path, "\n")

###line plots
# Prepare data for line plot by calculating means and standard errors
data_summary <- data_plot %>%
  group_by(APOE, Diet) %>%
  summarise(
    Mean_Center = mean(Percentage_time_center, na.rm = TRUE),
    SE_Center = sd(Percentage_time_center, na.rm = TRUE) / sqrt(n()),
    Mean_Outer = mean(Percentage_time_outerzone, na.rm = TRUE),
    SE_Outer = sd(Percentage_time_outerzone, na.rm = TRUE) / sqrt(n()),
    Mean_Mobile = mean(Time.mobile, na.rm = TRUE),
    SE_Mobile = sd(Time.mobile, na.rm = TRUE) / sqrt(n()),
    Mean_Freezing = mean(Time.freezing, na.rm = TRUE),
    SE_Freezing = sd(Time.freezing, na.rm = TRUE) / sqrt(n())
  )

# Panel 1: Percentage Time in Center Zone
p1 <- ggplot(data_summary, aes(x = APOE, group = Diet, color = Diet)) +
  geom_line(aes(y = Mean_Center), size = 1) +  # Line for mean
  geom_point(aes(y = Mean_Center), size = 2) + # Points for mean
  geom_errorbar(aes(ymin = Mean_Center - SE_Center, ymax = Mean_Center + SE_Center), 
                width = 0.2, size = 0.8) + # Error bars
  scale_color_manual(values = diet_colors) +
  labs(
    title = "Percentage Time in Center Zone",
    x = "APOE Genotype",
    y = "Mean Percentage Time",
    color = "Diet"
  ) +
  theme_bw()

# Panel 2: Percentage Time in Outer Zone
p2 <- ggplot(data_summary, aes(x = APOE, group = Diet, color = Diet)) +
  geom_line(aes(y = Mean_Outer), size = 1) +
  geom_point(aes(y = Mean_Outer), size = 2) +
  geom_errorbar(aes(ymin = Mean_Outer - SE_Outer, ymax = Mean_Outer + SE_Outer), 
                width = 0.2, size = 0.8) +
  scale_color_manual(values = diet_colors) +
  labs(
    title = "Percentage Time in Outer Zone",
    x = "APOE Genotype",
    y = "Mean Percentage Time",
    color = "Diet"
  ) +
  theme_bw()

# Panel 3: Time.mobile
p3 <- ggplot(data_summary, aes(x = APOE, group = Diet, color = Diet)) +
  geom_line(aes(y = Mean_Mobile), size = 1) +
  geom_point(aes(y = Mean_Mobile), size = 2) +
  geom_errorbar(aes(ymin = Mean_Mobile - SE_Mobile, ymax = Mean_Mobile + SE_Mobile), 
                width = 0.2, size = 0.8) +
  scale_color_manual(values = diet_colors) +
  labs(
    title = "Time Mobile",
    x = "APOE Genotype",
    y = "Mean Time (seconds)",
    color = "Diet"
  ) +
  theme_bw()

# Panel 4: Time.freezing
p4 <- ggplot(data_summary, aes(x = APOE, group = Diet, color = Diet)) +
  geom_line(aes(y = Mean_Freezing), size = 1) +
  geom_point(aes(y = Mean_Freezing), size = 2) +
  geom_errorbar(aes(ymin = Mean_Freezing - SE_Freezing, ymax = Mean_Freezing + SE_Freezing), 
                width = 0.2, size = 0.8) +
  scale_color_manual(values = diet_colors) +
  labs(
    title = "Time Freezing",
    x = "APOE Genotype",
    y = "Mean Time (seconds)",
    color = "Diet"
  ) +
  theme_bw()

# Combine the four panels
combined_plot <- ggarrange(p1, p2, p3, p4, 
                           ncol = 2, nrow = 2, 
                           common.legend = TRUE, legend = "bottom")

# Save the combined plot
output_plot_path <- file.path(output_folder, "Line_Plot_with_Error_Bars_by_APOE_Diet.png")
ggsave(output_plot_path, combined_plot, width = 16, height = 12, scale = 1, units = "in", dpi = 300)

cat("Line plot with error bars saved to:", output_plot_path, "\n")
###
####
# Saving xls sheet with three sheets per metric
# for (i in 11:dim(geno_combined)[2] ) {
#   
#   lm <- lm(unlist(geno_combined[,i]) ~ age*genotype*sex*risk_for_ad, geno_combined)
#   summary(lm)
#   anova(lm)
#   write.xlsx2(as.data.frame(anova(lm)), paste0(path,"ad_decode_results2.xlsx"), sheet =colnames(geno_combined)[i],append = TRUE)
#   
#   eta=effectsize::cohens_f(lm, alternative='two.sided')
#   write.xlsx2(eta, paste0(path,"ad_decode_results2.xlsx"), sheet =paste0(colnames(geno_combined)[i],"_cohen_"),append = TRUE)
#   
#   
#   res<-emmeans(lm, list(pairwise ~ factor(sex)|factor(genotype)), adjust="tukey")
#   contrast(res[[1]], "eff", by = "genotype")
#   posthoc2 <- na.omit((emmeans(lm, list(pairwise ~ sex|genotype), adjust="tukey")$`pairwise differences of sex | genotype`)) 
#   write.xlsx2(posthoc2, paste0(path,"ad_decode_results2.xlsx"), sheet =paste0(colnames(geno_combined)[i],"_emmeans_"),append = TRUE)
#   
# }
# ####