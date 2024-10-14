library(tidyverse)
library(cowplot)
library(gplots)
library(ggplot2)
library(patternplot)
library(lme4)
library(visreg)
library(tidyr)
library(magrittr) 
library(dplyr)
library(ggpubr)
library(lme4)
library(lsmeans)
library(emmeans)
library(rstatix)
library(ggstatsplot)
library(palmerpenguins)
library(AICcmodavg)
library(afex)
library(xlsx)

if (!dir.exists("output")) {
  dir.create("output")
}

setwd(getwd())

# Read in merged_MWM sheet and AWN data
mwm_data<-read.csv('MWM_behavior_101424_hsm.csv', header=TRUE)
mwm_data$Genotype <- factor(mwm_data$Genotype, levels = c("APOE22HN", "APOE33HN", "APOE44HN"))


mwm_data<-mwm_data %>%
  filter(Lifestyle=='Sedentary')%>%
  filter(Age_handling_mastersheet >= 11 & Age_mastersheet <= 30)  
  #filter(Age_handling_mastersheet >= 16 & Age_mastersheet <= 26)  

probe_hn<-mwm_data %>% 
  filter(Genotype == "APOE22HN" | Genotype == "APOE33HN" | Genotype == "APOE44HN") %>% 
  filter(Stage == 'Probe_D5' | Stage == 'Probe_D8') #%>% 
  #filter(!is.na(Age_group))

probe_hn_5<-mwm_data %>% 
  filter(Genotype == "APOE22HN" | Genotype == "APOE33HN" | Genotype == "APOE44HN") %>% 
  filter(Stage == 'Probe_D5' ) #%>% 
#filter(!is.na(Age_group))

probe_hn_8<-mwm_data %>% 
  filter(Genotype == "APOE22HN" | Genotype == "APOE33HN" | Genotype == "APOE44HN") %>% 
  filter(Stage == 'Probe_D8') #%>% 
#filter(!is.na(Age_group))

learning_trials_HN <- mwm_data %>%
  filter(Stage != 'Probe_D5' & Stage != 'Probe_D8') %>%
  #filter(!is.na(Genotype)) %>%
  filter(Genotype == 'APOE22HN' | Genotype == 'APOE33HN' | Genotype == 'APOE44HN') 

unique(learning_trials_HN$Genotype)


#########
#alex plots and stats for learning trials
# Create histogram of Age_mastersheet by Genotype
ggplot(learning_trials_HN, aes(x = Age_handling_mastersheet, fill = Genotype)) +
  geom_histogram(binwidth = 1, position = "dodge", alpha = 0.7) +
  labs(title = "Histogram of Age by Genotype",
       x = "Age (mastersheet)",
       y = "Count") +
  theme_minimal()

plt<-ggplot(learning_trials_HN, aes(x = Age_handling_mastersheet, color = Genotype, fill = Genotype)) +
  geom_density(alpha = 0.5) +
  scale_x_continuous(breaks = seq(min(learning_trials_HN$Age_mastersheet, na.rm = TRUE), 
                                  max(learning_trials_HN$Age_mastersheet, na.rm = TRUE), 
                                  by = 2)) +
  labs(title = "Density Plot of Age by Genotype",
       x = "Age (mastersheet) in months",
       y = "Density") +
  theme_minimal()
# Save the plot in the 'output' directory
ggsave(filename = "output/age_density_plot_age_handling.png", plot = plt, width = 6, height = 4, dpi = 300)


plt1<- ggline(learning_trials_HN, x = 'Stage', y = 'MeanSpeed', 
              color = 'Diet', facet.by = 'Genotype',
              error.plot = 'errorbar', add = 'mean_se', 
              palette = c("#7FFF00", "#6A0DAD"),  # Green and Purple colors
              size = 1, point.size = 1.5, legend = 'top')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("Speed (m/s)")  # Add y-axis title# Rotate x-axis labels
ggsave(filename = "output/Speed_HN_Diet_Learning.png", plot = plt1, width = 6, height = 4, dpi = 300)  


plt1<- ggline(learning_trials_HN, x = 'Stage', y = 'NormSWDist', 
              color = 'Diet', facet.by = 'Genotype',
              error.plot = 'errorbar', add = 'mean_se', 
              palette = c("#7FFF00", "#6A0DAD"),  # Green and Purple colors
              size = 1, point.size = 1.5, legend = 'top')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("NormSWDist (%)")  # Add y-axis title
# Rotate x-axis labels
ggsave(filename = "output/NormSWDist_HN_Diet_Learning.png", plot = plt1, width = 6, height = 4, dpi = 300)  


plt1<- ggline(learning_trials_HN, x = 'Stage', y = 'Distance', 
              color = 'Diet', facet.by = 'Genotype',
              error.plot = 'errorbar', add = 'mean_se', 
              palette = c("#7FFF00", "#6A0DAD"),  # Green and Purple colors
              size = 1, point.size = 1.5, legend = 'top')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+  # Rotate x-axis labels
  ylab("Distance (m)")  # Add y-axis title
ggsave(filename = "output/Distance_HN_Diet_Learning.png", plot = plt1, width = 6, height = 4, dpi = 300)  


plt1<- ggline(learning_trials_HN, x = 'Stage', y = 'Winding_numbers', 
              color = 'Diet', facet.by = 'Genotype',
              error.plot = 'errorbar', add = 'mean_se', 
              palette = c("#7FFF00", "#6A0DAD"),  # Green and Purple colors
              size = 1, point.size = 1.5, legend = 'top')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("AWN  (AU)")  # Add y-axis title # Rotate x-axis labels
ggsave(filename = "output/AWN_HN_Diet_Learning.png", plot = plt1, width = 6, height = 4, dpi = 300) 
#### end plots learning #####




###start plots Day 5

plt1<- ggline(probe_hn_5, x = 'Genotype', y = 'MeanSpeed', 
              color = 'Diet', #facet.by = 'Genotype',
              error.plot = 'errorbar', add = 'mean_se', 
              palette = c("#7FFF00", "#6A0DAD"),  # Green and Purple colors
              size = 1, point.size = 1.5, legend = 'top')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("Speed (m/s)")  # Add y-axis title
# Rotate x-axis labels
ggsave(filename = "output/Speed_HN_Diet_Probe5.png", plot = plt1, width = 6, height = 4, dpi = 300)  


plt1<- ggline(probe_hn_5, x = 'Genotype', y = 'NormSWDist', 
              color = 'Diet', #facet.by = 'Genotype',
              error.plot = 'errorbar', add = 'mean_se', 
              palette = c("#7FFF00", "#6A0DAD"),  # Green and Purple colors
              size = 1, point.size = 1.5, legend = 'top')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("NormSWDist (%)")  # Add y-axis title # Rotate x-axis labels
ggsave(filename = "output/NormSWDist_HN_Diet_Probe5.png", plot = plt1, width = 6, height = 4, dpi = 300)  


plt1<- ggline(probe_hn_5, x = 'Genotype', y = 'Distance', 
              color = 'Diet', #facet.by = 'Genotype',
              error.plot = 'errorbar', add = 'mean_se', 
              palette = c("#7FFF00", "#6A0DAD"),  # Green and Purple colors
              size = 1, point.size = 1.5, legend = 'top')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("Distance (m)")  # Add y-axis title # Rotate x-axis labels
ggsave(filename = "output/Distance_HN_Diet_Probe5.png", plot = plt1, width = 6, height = 4, dpi = 300)  


plt1<- ggline(probe_hn_5, x = 'Genotype', y = 'Winding_numbers', 
              color = 'Diet', #facet.by = 'Genotype',
              error.plot = 'errorbar', add = 'mean_se', 
              palette = c("#7FFF00", "#6A0DAD"),  # Green and Purple colors
              size = 1, point.size = 1.5, legend = 'top')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("AWN (AU)")  # Add y-axis title
# Rotate x-axis labels
ggsave(filename = "output/AWN_HN_Diet_Probe5.png", plot = plt1, width = 6, height = 4, dpi = 300) 




####end plots Day 5



###start plots Day 8

plt1<- ggline(probe_hn_8, x = 'Genotype', y = 'MeanSpeed', 
              color = 'Diet', #facet.by = 'Genotype',
              error.plot = 'errorbar', add = 'mean_se', 
              palette = c("#7FFF00", "#6A0DAD"),  # Green and Purple colors
              size = 1, point.size = 1.5, legend = 'top')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  ylab("Speed (m/s)")  # Add y-axis title
# Rotate x-axis labels
ggsave(filename = "output/Speed_HN_Diet_Probe8.png", plot = plt1, width = 6, height = 4, dpi = 300)  


plt1<- ggline(probe_hn_8, x = 'Genotype', y = 'NormSWDist', 
              color = 'Diet', #facet.by = 'Genotype',
              error.plot = 'errorbar', add = 'mean_se', 
              palette = c("#7FFF00", "#6A0DAD"),  # Green and Purple colors
              size = 1, point.size = 1.5, legend = 'top')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("NormSWDist (%)")  # Add y-axis title
# Rotate x-axis labels
ggsave(filename = "output/NormSWDist_HN_Diet_Probe8.png", plot = plt1, width = 6, height = 4, dpi = 300)  


plt1<- ggline(probe_hn_8, x = 'Genotype', y = 'Distance', 
              color = 'Diet', #facet.by = 'Genotype',
              error.plot = 'errorbar', add = 'mean_se', 
              palette = c("#7FFF00", "#6A0DAD"),  # Green and Purple colors
              size = 1, point.size = 1.5, legend = 'top')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("Distance (m/s)")  # Add y-axis title
# Rotate x-axis labels
ggsave(filename = "output/Distance_HN_Diet_Probe8.png", plot = plt1, width = 6, height = 4, dpi = 300)  


plt1<- ggline(probe_hn_8, x = 'Genotype', y = 'Winding_numbers', 
              color = 'Diet', #facet.by = 'Genotype',
              error.plot = 'errorbar', add = 'mean_se', 
              palette = c("#7FFF00", "#6A0DAD"),  # Green and Purple colors
              size = 1, point.size = 1.5, legend = 'top')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ylab("AWN (AU)")  # Add y-axis title
# Rotate x-axis labels
ggsave(filename = "output/AWN_HN_Diet_Probe8.png", plot = plt1, width = 6, height = 4, dpi = 300) 








####end plots Day 8



# Count animals per Genotype and Sex
unique_animal_count <- learning_trials_HN %>%
  group_by(Genotype, Sex) %>%
  summarise(unique_count = n_distinct(AnimalID))

# Display the result
print(unique_animal_count)

unique_animal_count <- learning_trials_HN %>%
  group_by(Genotype) %>%
  summarise(unique_count = n_distinct(AnimalID))

# Display the result
print(unique_animal_count)

####NOW alex START stats
library(lme4)
library(emmeans)
library(openxlsx)
emm_options(pbkrtest.limit = 10000, lmerTest.limit = 10000)
# Check if the file exists
file_path <- "output/HN_learning.xlsx"

if (file.exists(file_path)) {
  # Load existing workbook
  wb <- loadWorkbook(file_path)
} else {
  # Create a new workbook if it doesn't exist
  wb <- createWorkbook()
}


# AWN analysis (creates new sheets if they don't exist)
if (!"AWN_anova" %in% names(wb)) addWorksheet(wb, "AWN_anova")

mod_1_HN <- lmer(Winding_numbers ~ factor(Genotype) * factor(Sex) * factor(Diet) * Age_handling_mastersheet * factor(Stage) + 
                   (1 + Time | AnimalID), 
                   #(1|AnimalID),
                 control = lmerControl(optCtrl = list(maxfun = 1e5)), 
                 data = learning_trials_HN)

#(1|AnimalID), learning_trials_HN)


a1_a <- anova(mod_1_HN)
a1_a <- data.frame(Factor = rownames(a1_a), a1_a)  # Add factor names as a column
rownames(a1_a) <- NULL  # Remove row names

writeData(wb, sheet = "AWN_anova", a1_a, startRow = 1, colNames = TRUE)

if (!"AWN_emmeans" %in% names(wb)) addWorksheet(wb, "AWN_emmeans")
mix_1 <- emmeans(mod_1_HN, "Genotype", adjust = "Tukey")
writeData(wb, sheet = "AWN_emmeans", mix_1, startRow = 1, colNames = TRUE)

if (!"AWN_posthoc" %in% names(wb)) addWorksheet(wb, "AWN_posthoc")
writeData(wb, sheet = "AWN_posthoc", pairs(mix_1), startRow = 1, colNames = TRUE)

# Distance analysis (creates new sheets if they don't exist)
if (!"Distance_anova" %in% names(wb)) addWorksheet(wb, "Distance_anova")
mod_1_HN <- lmer(Distance ~ factor(Genotype) * factor(Sex) * factor(Diet) * Age_handling_mastersheet * factor(Stage) + 
                   # (1 + Time | AnimalID), 
                   (1|AnimalID), 
                 control = lmerControl(optCtrl = list(maxfun = 1e5)), 
                 data = learning_trials_HN)

#(1|AnimalID), learning_trials_HN)
a1_a <- anova(mod_1_HN)
a1_a <- data.frame(Factor = rownames(a1_a), a1_a)  # Add factor names as a column
rownames(a1_a) <- NULL  # Remove row names
writeData(wb, sheet = "Distance_anova", a1_a, startRow = 1, colNames = TRUE)

if (!"Distance_emmeans" %in% names(wb)) addWorksheet(wb, "Distance_emmeans")
mix_1 <- emmeans(mod_1_HN, "Genotype", adjust = "Tukey")
writeData(wb, sheet = "Distance_emmeans", mix_1, startRow = 1, colNames = TRUE)

if (!"Distance_posthoc" %in% names(wb)) addWorksheet(wb, "Distance_posthoc")
writeData(wb, sheet = "Distance_posthoc", pairs(mix_1), startRow = 1, colNames = TRUE)

# NormSWDist analysis (creates new sheets if they don't exist)
if (!"NormSWDist_anova" %in% names(wb)) addWorksheet(wb, "NormSWDist_anova")
mod_1_HN <- lmer(NormSWDist ~ factor(Genotype) * factor(Sex) * factor(Diet) * Age_handling_mastersheet * factor(Stage) + 
                   #(1 + Time | AnimalID), 
                   (1|AnimalID), 
                 control = lmerControl(optCtrl = list(maxfun = 1e5)), 
                 data = learning_trials_HN)

#(1|AnimalID), learning_trials_HN)
a1_a <- anova(mod_1_HN)
a1_a <- data.frame(Factor = rownames(a1_a), a1_a)  # Add factor names as a column
rownames(a1_a) <- NULL  # Remove row names

writeData(wb, sheet = "NormSWDist_anova", a1_a, startRow = 1, colNames = TRUE)

if (!"NormSWDist_emmeans" %in% names(wb)) addWorksheet(wb, "NormSWDist_emmeans")
mix_1 <- emmeans(mod_1_HN, "Genotype", adjust = "Tukey")
writeData(wb, sheet = "NormSWDist_emmeans", mix_1, startRow = 1, colNames = TRUE)

if (!"NormSWDist_posthoc" %in% names(wb)) addWorksheet(wb, "NormSWDist_posthoc")
writeData(wb, sheet = "NormSWDist_posthoc", pairs(mix_1), startRow = 1, colNames = TRUE)

# Save the workbook after appending all sheets
saveWorkbook(wb, "output/HN_learning.xlsx", overwrite = TRUE)

####NOW END Stats

# Calculate the minimum and maximum age
age_range <- range(learning_trials_HN$Age_handling_mastersheet, na.rm = TRUE)

# Print the age range
cat("The age range for Age__handling_mastersheet is from", age_range[1], "to", age_range[2], "\n")

#######





#########
#alex plots and stats for learning trials
# Create histogram of Age_mastersheet by Genotype
ggplot(learning_trials_HN, aes(x = Age_handling_mastersheet, fill = Genotype)) +
  geom_histogram(binwidth = 1, position = "dodge", alpha = 0.7) +
  labs(title = "Histogram of Age by Genotype",
       x = "Age (mastersheet)",
       y = "Count") +
  theme_minimal()

plt<-ggplot(learning_trials_HN, aes(x = Age_handling_mastersheet, color = Genotype, fill = Genotype)) +
  geom_density(alpha = 0.5) +
  scale_x_continuous(breaks = seq(min(learning_trials_HN$Age_mastersheet, na.rm = TRUE), 
                                  max(learning_trials_HN$Age_mastersheet, na.rm = TRUE), 
                                  by = 2)) +
  labs(title = "Density Plot of Age by Genotype",
       x = "Age (mastersheet) in months",
       y = "Density") +
  theme_minimal()
# Save the plot in the 'output' directory
ggsave(filename = "output/age_density_plot_age_handling.png", plot = plt, width = 6, height = 4, dpi = 300)

# learning_trials_HN <- mwm_data %>%
#    filter(Stage != 'Probe_D5' & Stage != 'Probe_D8') %>%
#    filter(!is.na(Genotype)) %>%
#    filter(Genotype == 'APOE22HN' | Genotype == 'APOE33HN' | Genotype == 'APOE44HN') #%>%
#    #filter(Age_mastersheet >= 16 & Age_mastersheet <= 25)
#    #filter(Age_mastersheet >= 17 & Age_mastersheet <= 25)

# ggline(data = learning_trials_HN_young, x = 'Stage', y = 'NormSWDist', 
#        color = 'Line_type', fill = 'Line_type', 
#        facet.by = 'Genotype', error.plot = 'errorbar', 
#        add = 'mean_se', palette = c("green", "blue", "red"), 
#        size = 1, point.size = 1.5, legend = 'top')


plt1<- ggline(learning_trials_HN, x = 'Stage', y = 'MeanSpeed', 
              color = 'Diet', facet.by = 'Genotype',
              error.plot = 'errorbar', add = 'mean_se', 
              palette = c("#7FFF00", "#6A0DAD"),  # Green and Purple colors
              size = 1, point.size = 1.5, legend = 'top')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels
ggsave(filename = "output/Speed_HN_Diet_Learning.png", plot = plt1, width = 6, height = 4, dpi = 300)  


plt1<- ggline(learning_trials_HN, x = 'Stage', y = 'NormSWDist', 
              color = 'Diet', facet.by = 'Genotype',
              error.plot = 'errorbar', add = 'mean_se', 
              palette = c("#7FFF00", "#6A0DAD"),  # Green and Purple colors
              size = 1, point.size = 1.5, legend = 'top')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels
ggsave(filename = "output/NormSWDist_HN_Diet_Learning.png", plot = plt1, width = 6, height = 4, dpi = 300)  


plt1<- ggline(learning_trials_HN, x = 'Stage', y = 'Distance', 
              color = 'Diet', facet.by = 'Genotype',
              error.plot = 'errorbar', add = 'mean_se', 
              palette = c("#7FFF00", "#6A0DAD"),  # Green and Purple colors
              size = 1, point.size = 1.5, legend = 'top')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels
ggsave(filename = "output/Distance_HN_Diet_Learning.png", plot = plt1, width = 6, height = 4, dpi = 300)  


plt1<- ggline(learning_trials_HN, x = 'Stage', y = 'Winding_numbers', 
              color = 'Diet', facet.by = 'Genotype',
              error.plot = 'errorbar', add = 'mean_se', 
              palette = c("#7FFF00", "#6A0DAD"),  # Green and Purple colors
              size = 1, point.size = 1.5, legend = 'top')+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels
ggsave(filename = "output/AWN_HN_Diet_Learning.png", plot = plt1, width = 6, height = 4, dpi = 300) 


# Count animals per Genotype and Sex
unique_animal_count <- learning_trials_HN %>%
  group_by(Genotype, Sex) %>%
  summarise(unique_count = n_distinct(AnimalID))

# Display the result
print(unique_animal_count)

unique_animal_count <- learning_trials_HN %>%
  group_by(Genotype) %>%
  summarise(unique_count = n_distinct(AnimalID))

# Display the result
print(unique_animal_count)


#######
####NOW alex START Probe Dy 5 stats
library(lme4)
library(emmeans)
library(openxlsx)
emm_options(pbkrtest.limit = 10000, lmerTest.limit = 10000)
# Check if the file exists
file_path <- "output/HN_memory_Probe_Day5.xlsx"

if (file.exists(file_path)) {
  # Load existing workbook
  wb <- loadWorkbook(file_path)
} else {
  # Create a new workbook if it doesn't exist
  wb <- createWorkbook()
}


# AWN analysis (creates new sheets if they don't exist)
if (!"AWN_anova" %in% names(wb)) addWorksheet(wb, "AWN_anova")



mod_1_HN <- lm(Winding_numbers ~ factor(Genotype) * factor(Sex) * factor(Diet) * Age_handling_mastersheet,  
               data = probe_hn_5)


a1_a <- anova(mod_1_HN)
a1_a <- data.frame(Factor = rownames(a1_a), a1_a)  # Add factor names as a column
rownames(a1_a) <- NULL  # Remove row names

writeData(wb, sheet = "AWN_anova", a1_a, startRow = 1, colNames = TRUE)

if (!"AWN_emmeans" %in% names(wb)) addWorksheet(wb, "AWN_emmeans")
mix_1 <- emmeans(mod_1_HN, "Genotype", adjust = "Tukey")
writeData(wb, sheet = "AWN_emmeans", mix_1, startRow = 1, colNames = TRUE)

if (!"AWN_posthoc" %in% names(wb)) addWorksheet(wb, "AWN_posthoc")
writeData(wb, sheet = "AWN_posthoc", pairs(mix_1), startRow = 1, colNames = TRUE)

# Distance analysis (creates new sheets if they don't exist)
if (!"Distance_anova" %in% names(wb)) addWorksheet(wb, "Distance_anova")
mod_1_HN <-  lm(Distance ~ factor(Genotype) * factor(Sex) * factor(Diet) * Age_handling_mastersheet,  
                           data = probe_hn_5)

#(1|AnimalID), learning_trials_HN)
a1_a <- anova(mod_1_HN)
a1_a <- data.frame(Factor = rownames(a1_a), a1_a)  # Add factor names as a column
rownames(a1_a) <- NULL  # Remove row names
writeData(wb, sheet = "Distance_anova", a1_a, startRow = 1, colNames = TRUE)

if (!"Distance_emmeans" %in% names(wb)) addWorksheet(wb, "Distance_emmeans")
mix_1 <- emmeans(mod_1_HN, "Genotype", adjust = "Tukey")
writeData(wb, sheet = "Distance_emmeans", mix_1, startRow = 1, colNames = TRUE)

if (!"Distance_posthoc" %in% names(wb)) addWorksheet(wb, "Distance_posthoc")
writeData(wb, sheet = "Distance_posthoc", pairs(mix_1), startRow = 1, colNames = TRUE)

# NormSWDist analysis (creates new sheets if they don't exist)
if (!"NormSWDist_anova" %in% names(wb)) addWorksheet(wb, "NormSWDist_anova")
mod_1_HN <- lm(NormSWDist ~ factor(Genotype) * factor(Sex) * factor(Diet) * Age_handling_mastersheet,  
                                   data = probe_hn_5)

#(1|AnimalID), learning_trials_HN)
a1_a <- anova(mod_1_HN)
a1_a <- data.frame(Factor = rownames(a1_a), a1_a)  # Add factor names as a column
rownames(a1_a) <- NULL  # Remove row names

writeData(wb, sheet = "NormSWDist_anova", a1_a, startRow = 1, colNames = TRUE)

if (!"NormSWDist_emmeans" %in% names(wb)) addWorksheet(wb, "NormSWDist_emmeans")
mix_1 <- emmeans(mod_1_HN, "Genotype", adjust = "Tukey")
writeData(wb, sheet = "NormSWDist_emmeans", mix_1, startRow = 1, colNames = TRUE)

if (!"NormSWDist_posthoc" %in% names(wb)) addWorksheet(wb, "NormSWDist_posthoc")
writeData(wb, sheet = "NormSWDist_posthoc", pairs(mix_1), startRow = 1, colNames = TRUE)

# Save the workbook after appending all sheets
saveWorkbook(wb, "output/HN_memory_Probe_Day5.xlsx", overwrite = TRUE)

####NOW END ProbeStats Day 5




#######
####NOW alex START Probe Day 8 stats
library(lme4)
library(emmeans)
library(openxlsx)
emm_options(pbkrtest.limit = 10000, lmerTest.limit = 10000)
# Check if the file exists
file_path <- "output/HN_memory_Probe_Day8.xlsx"

if (file.exists(file_path)) {
  # Load existing workbook
  wb <- loadWorkbook(file_path)
} else {
  # Create a new workbook if it doesn't exist
  wb <- createWorkbook()
}


# AWN analysis (creates new sheets if they don't exist)
if (!"AWN_anova" %in% names(wb)) addWorksheet(wb, "AWN_anova")



mod_1_HN <- lm(Winding_numbers ~ factor(Genotype) * factor(Sex) * factor(Diet) * Age_handling_mastersheet,  
               data = probe_hn_8)


a1_a <- anova(mod_1_HN)
a1_a <- data.frame(Factor = rownames(a1_a), a1_a)  # Add factor names as a column
rownames(a1_a) <- NULL  # Remove row names

writeData(wb, sheet = "AWN_anova", a1_a, startRow = 1, colNames = TRUE)

if (!"AWN_emmeans" %in% names(wb)) addWorksheet(wb, "AWN_emmeans")
mix_1 <- emmeans(mod_1_HN, "Genotype", adjust = "Tukey")
writeData(wb, sheet = "AWN_emmeans", mix_1, startRow = 1, colNames = TRUE)

if (!"AWN_posthoc" %in% names(wb)) addWorksheet(wb, "AWN_posthoc")
writeData(wb, sheet = "AWN_posthoc", pairs(mix_1), startRow = 1, colNames = TRUE)

# Distance analysis (creates new sheets if they don't exist)
if (!"Distance_anova" %in% names(wb)) addWorksheet(wb, "Distance_anova")
mod_1_HN <-  lm(Distance ~ factor(Genotype) * factor(Sex) * factor(Diet) * Age_handling_mastersheet,  
                data = probe_hn_8)

#(1|AnimalID), learning_trials_HN)
a1_a <- anova(mod_1_HN)
a1_a <- data.frame(Factor = rownames(a1_a), a1_a)  # Add factor names as a column
rownames(a1_a) <- NULL  # Remove row names
writeData(wb, sheet = "Distance_anova", a1_a, startRow = 1, colNames = TRUE)

if (!"Distance_emmeans" %in% names(wb)) addWorksheet(wb, "Distance_emmeans")
mix_1 <- emmeans(mod_1_HN, "Genotype", adjust = "Tukey")
writeData(wb, sheet = "Distance_emmeans", mix_1, startRow = 1, colNames = TRUE)

if (!"Distance_posthoc" %in% names(wb)) addWorksheet(wb, "Distance_posthoc")
writeData(wb, sheet = "Distance_posthoc", pairs(mix_1), startRow = 1, colNames = TRUE)

# NormSWDist analysis (creates new sheets if they don't exist)
if (!"NormSWDist_anova" %in% names(wb)) addWorksheet(wb, "NormSWDist_anova")
mod_1_HN <- lm(NormSWDist ~ factor(Genotype) * factor(Sex) * factor(Diet) * Age_handling_mastersheet,  
               data = probe_hn_8)

#(1|AnimalID), learning_trials_HN)
a1_a <- anova(mod_1_HN)
a1_a <- data.frame(Factor = rownames(a1_a), a1_a)  # Add factor names as a column
rownames(a1_a) <- NULL  # Remove row names

writeData(wb, sheet = "NormSWDist_anova", a1_a, startRow = 1, colNames = TRUE)

if (!"NormSWDist_emmeans" %in% names(wb)) addWorksheet(wb, "NormSWDist_emmeans")
mix_1 <- emmeans(mod_1_HN, "Genotype", adjust = "Tukey")
writeData(wb, sheet = "NormSWDist_emmeans", mix_1, startRow = 1, colNames = TRUE)

if (!"NormSWDist_posthoc" %in% names(wb)) addWorksheet(wb, "NormSWDist_posthoc")
writeData(wb, sheet = "NormSWDist_posthoc", pairs(mix_1), startRow = 1, colNames = TRUE)

# Save the workbook after appending all sheets
saveWorkbook(wb, "output/HN_memory_Probe_Day8.xlsx", overwrite = TRUE)

####NOW END ProbeStats Day 8

