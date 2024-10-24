library(readxl)
myfile<-'MouseMetaData092024AB.xlsx'
mydata <- read_excel(myfile)


plt2<-ggplot(mydata, aes(x = Weight, color = Diet, fill = Diet)) +
    geom_density(alpha = 0.5) +
    scale_x_continuous(breaks = seq(min(mydata$Weight, na.rm = TRUE),
                                  max(mydata$Weight+10, na.rm = TRUE),
                                  by = 3)) +
  
  labs(title = "Density Plot of Weight by Diet",
       
       x = "Weight(g)",
       
       y = "Density") +
  
  theme_minimal()


ggsave(filename = "output/weight_by_diet.png", plot = plt2, width = 6, height = 4, dpi = 300)



plt<-ggplot(mydata, aes(x = Weight, color = Genotype, fill = Genotype)) +
  geom_density(alpha = 0.5) +
  scale_x_continuous(breaks = seq(min(mydata$Weight, na.rm = TRUE), 
                                  max(mydata$Weight, na.rm = TRUE), 
                                  by = 2)) +
  labs(title = "Density Plot of Weight by Genotype",
       x = "Weight(g)",
       y = "Density") +
  theme_minimal()
# Save the plot in the 'output' directory
ggsave(filename = "output/density_plot_weight_handling.png", plot = plt, width = 6, height = 4, dpi = 300)

control_data <- subset(mydata, Diet == 'Control')
HFD_data <- subset(mydata, Diet == 'HFD')

plt<-ggplot(control_data, aes(x = Weight, color = Genotype, fill = Genotype)) +
  geom_density(alpha = 0.5) +
  scale_x_continuous(breaks = seq(min(control_data$Weight, na.rm = TRUE), 
                                  max(control_data$Weight, na.rm = TRUE), 
                                  by = 2)) +
  labs(title = "Density Plot of Weight by Genotype",
       x = "Weight(g)",
       y = "Density") +
  theme_minimal()
# Save the plot in the 'output' directory
ggsave(filename = "output/density_plot_weight_control_data.png", plot = plt, width = 6, height = 4, dpi = 300)

plt<-ggplot(HFD_data, aes(x = Weight, color = Genotype, fill = Genotype)) +
  geom_density(alpha = 0.5) +
  scale_x_continuous(breaks = seq(min(HFD_data$Weight, na.rm = TRUE), 
                                  max(HFD_data$Weight, na.rm = TRUE), 
                                  by = 2)) +
  labs(title = "Density Plot of Weight by Genotype",
       x = "Weight(g)",
       y = "Density") +
  theme_minimal()
# Save the plot in the 'output' directory
ggsave(filename = "output/density_plot_weight_HFD_data.png", plot = plt, width = 6, height = 4, dpi = 300)

HFD_data_hn<-HFD_data %>% 
  filter(Genotype == "APOE22HN" | Genotype == "APOE33HN" | Genotype == "APOE44HN") 

CTRL_data_hn<-control_data %>% 
  filter(Genotype == "APOE22HN" | Genotype == "APOE33HN" | Genotype == "APOE44HN") 

plt<-ggplot(CTRL_data_hn, aes(x = Weight, color = Genotype, fill = Genotype)) +
  geom_density(alpha = 0.5) +
  scale_x_continuous(breaks = seq(min(control_data$Weight, na.rm = TRUE), 
                                  max(control_data$Weight, na.rm = TRUE), 
                                  by = 2)) +
  labs(title = "Density Plot of Weight by Genotype",
       x = "Weight(g)",
       y = "Density") +
  theme_minimal()
# Save the plot in the 'output' directory
ggsave(filename = "output/weight_HN_Ctrl_data.png", plot = plt, width = 6, height = 4, dpi = 300)


plt<-ggplot(HFD_data_hn, aes(x = Weight, color = Genotype, fill = Genotype)) +
  geom_density(alpha = 0.5) +
  scale_x_continuous(breaks = seq(min(HFD_data$Weight, na.rm = TRUE), 
                                  max(HFD_data$Weight, na.rm = TRUE), 
                                  by = 2)) +
  labs(title = "Density Plot of Weight by Genotype",
       x = "Weight(g)",
       y = "Density") +
  theme_minimal()
# Save the plot in the 'output' directory
ggsave(filename = "output/weight_HN_HFD_data.png", plot = plt, width = 6, height = 4, dpi = 300)

