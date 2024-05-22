
# Load required libraries
library(ggplot2)
library(gridExtra)

#getwd()
#setwd("/Users/marcelo_palermo/Doutorado/R/Olink/Qualification/")
#getwd()

proteins_data = read.csv('Significant_Proteins_Heatmap.csv', sep = ',')
proteins_data <- proteins_data[,-which(names(proteins_data) =='X')]
proteins_data <- proteins_data[proteins_data$Diseases != 'None',]


proteins_disease <- proteins_data[,which(names(proteins_data) %in% c('Age','BMI','Diseases','protein_1'))]
proteins_disease$BMI <- factor(proteins_disease$BMI, levels = 0:4, labels = c("Underweight", "Normal", "Overweight", "Obese", "Sev. Obese"))
proteins_disease$Age <- factor(proteins_disease$Age, levels = 1:5, labels = c("20-34", "35-49", "50-64", "65-79", "80+"))
proteins_disease$Diseases <- 
  ifelse(proteins_disease$Diseases == "HTN", "3. Hypertension", ifelse(proteins_disease$Diseases == "HEART", "2. Heart", ifelse(proteins_disease$Diseases == "DIABETES", "1. Diabetes", ifelse(proteins_disease$Diseases == "KIDNEY", "4. Kidney", ifelse(proteins_disease$Diseases == "LUNG", "5. Lung", ifelse(proteins_disease$Diseases == "None", "No Diseases", proteins_disease$Diseases ))))))



proteins_no_disease <- proteins_data[,which(names(proteins_data) %in% c('Age','BMI','Diseases','protein_0'))]
proteins_no_disease$BMI <- factor(proteins_no_disease$BMI, levels = 0:4, labels = c("Underweight", "Normal", "Overweight", "Obese", "Sev. Obese"))
proteins_no_disease$Age <- factor(proteins_no_disease$Age, levels = 1:5, labels = c("20-34", "35-49", "50-64", "65-79", "80+"))
proteins_no_disease$Diseases <- 
  ifelse(proteins_no_disease$Diseases == "HTN", "3. No\nHypertension", ifelse(proteins_no_disease$Diseases == "HEART", "2. No Heart", ifelse(proteins_no_disease$Diseases == "DIABETES", "1. No\nDiabetes", ifelse(proteins_no_disease$Diseases == "KIDNEY", "4. No Kidney", ifelse(proteins_no_disease$Diseases == "LUNG", "5. No Lung", ifelse(proteins_no_disease$Diseases == "None", "No Diseases", proteins_no_disease$Diseases ))))))



# Create heatmap 1
heatmap1 <- ggplot(proteins_disease, aes(x = BMI, y = Age, fill = protein_1)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = "white") +
  labs(title = "(a)",
       x = "BMI", y = "Age", fill = "No. of\nProteins") +
  facet_grid(. ~ Diseases) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.9, size = 8)) +
  theme(axis.text.y = element_text(angle = 45, hjust = 0.9, size = 8)) + 
  theme(plot.title = element_text(size = 10, face = "bold")) +  # Adjust title text size
  theme(axis.title = element_text(size = 10)) +  # Adjust axis labels text size
  theme(legend.text = element_text(size = 8)) +
  theme(legend.title =element_text(size = 8)) +
  theme(panel.border = element_rect(color = "grey", fill = NA, size = 0.5)) +
  theme(strip.text = element_text(size = 8.4))



# Create heatmap 2
heatmap2 <- ggplot(proteins_no_disease, aes(x = BMI, y = Age, fill = protein_0)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = "white") +
  labs(title = "(b)",
       x = "BMI", y = "Age", fill = "No. of\nProteins") +
  facet_grid(. ~ Diseases) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.9, size = 8)) +
  theme(axis.text.y = element_text(angle = 45, hjust = 0.9, size = 8)) + 
  theme(plot.title = element_text(size = 10, face = "bold")) +  # Adjust title text size
  theme(axis.title = element_text(size = 10)) +  # Adjust axis labels text size
  theme(legend.text = element_text(size = 8)) +
  theme(legend.title = element_text(size = 8)) +
  theme(panel.border = element_rect(color = "grey", fill = NA, linewidth  = 0.5))  +
  theme(strip.text = element_text(size = 8.4))

# Print both heatmaps on the same screen
grid.arrange(heatmap1, heatmap2, nrow = 2)

aggregate(proteins_disease$protein_1, list(proteins_disease$Diseases), sum)
aggregate(proteins_no_disease$protein_0, list(proteins_no_disease$Diseases), sum)



















# Create a heatmap
heatmap <- ggplot(proteins_disease, aes(x = BMI, y = Age, fill = protein_1)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = "white") +
  labs(title = "Significant Proteins per Age, BMI, and Pre-existing Diseases",
       x = "BMI", y = "Age", fill = "Total Proteins") +
  facet_grid(. ~ Diseases) +  # Add Diseases as facet
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.9, size = 8)) +
  theme(axis.text.y = element_text(angle = 45, hjust = 0.9, size = 8))

# Print the heatmap
print(heatmap)


# Load required libraries
library(ggplot2)

# Create fictional data
set.seed(123)  # for reproducibility
age <- sample(20:80, 100, replace = TRUE)
bmi <- sample(18.5:35, 100, replace = TRUE)
diseases <- sample(c("None", "Diabetes", "Hypertension", "Heart disease"), 100, replace = TRUE)
proteins <- sample(100:500, 100, replace = TRUE)




# Create a dataframe
data <- data.frame(Age = age, BMI = bmi, Diseases = diseases, Proteins = proteins)

# Create a heatmap
heatmap <- ggplot(data, aes(x = BMI, y = Age, fill = Proteins)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = "white") +
  labs(title = "Significant Proteins per Age, BMI, and Pre-existing Diseases",
       x = "BMI", y = "Age", fill = "Total Proteins") +
  facet_grid(. ~ Diseases) +  # Add Diseases as facet
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.9, size = 8)) +
  theme(axis.text.y = element_text(angle = 45, hjust = 0.9, size = 8))

# Print the heatmap
print(heatmap)




# Load required libraries
library(ggplot2)
library(gridExtra)

# Create fictional data for heatmap 1
set.seed(123)
age1 <- sample(20:80, 100, replace = TRUE)
bmi1 <- sample(18.5:35, 100, replace = TRUE)
diseases1 <- sample(c("None", "Diabetes", "Hypertension", "Heart disease"), 100, replace = TRUE)
proteins1 <- sample(100:500, 100, replace = TRUE)
data1 <- data.frame(Age = age1, BMI = bmi1, Diseases = diseases1, Proteins = proteins1)

# Create fictional data for heatmap 2
age2 <- sample(20:80, 100, replace = TRUE)
bmi2 <- sample(18.5:35, 100, replace = TRUE)
diseases2 <- sample(c("None", "Diabetes", "Hypertension", "Heart disease"), 100, replace = TRUE)
proteins2 <- sample(100:500, 100, replace = TRUE)
data2 <- data.frame(Age = age2, BMI = bmi2, Diseases = diseases2, Proteins = proteins2)

# Create heatmap 1
heatmap1 <- ggplot(data1, aes(x = BMI, y = Age, fill = Proteins)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = "white") +
  labs(title = "Heatmap 1",
       x = "BMI", y = "Age", fill = "Total Proteins") +
  facet_grid(. ~ Diseases) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.9, size = 8)) +
  theme(axis.text.y = element_text(angle = 45, hjust = 0.9, size = 8)) + 
  theme(plot.title = element_text(size = 12)) +  # Adjust title text size
  theme(axis.title = element_text(size = 10)) +  # Adjust axis labels text size
  theme(legend.text = element_text(size = 8)) +
  theme(legend.title =element_text(size = 8)) +
  theme(panel.border = element_rect(color = "grey", fill = NA, size = 0.5))

# Create heatmap 2
heatmap2 <- ggplot(data2, aes(x = BMI, y = Age, fill = Proteins)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = "white") +
  labs(title = "Heatmap 2",
       x = "BMI", y = "Age", fill = "Total Proteins") +
  facet_grid(. ~ Diseases) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.9, size = 8)) +
  theme(axis.text.y = element_text(angle = 45, hjust = 0.9, size = 8)) + 
  theme(plot.title = element_text(size = 12)) +  # Adjust title text size
  theme(axis.title = element_text(size = 10)) +  # Adjust axis labels text size
  theme(legend.text = element_text(size = 8)) +
  theme(legend.title = element_text(size = 8)) +
  theme(panel.border = element_rect(color = "grey", fill = NA, size = 0.5))

# Print both heatmaps on the same screen
grid.arrange(heatmap1, heatmap2, ncol = 2)



