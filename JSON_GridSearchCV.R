# Install and load the jsonlite package if you haven't already
if (!requireNamespace("jsonlite", quietly = TRUE)) {
  install.packages("jsonlite")
}
library(jsonlite)


# Read the JSON file into a dataframe
dataframe_1stpass <- fromJSON("/Users/marcelo_palermo/PycharmProjects/TeseDoutorado/Training/GridSearchCV/GridSearchResults/CV40/DeepLearning/Model_Proteins_Phase1/cv_results_DeepLearning_ALL_EarlyStopping2_Balanced_Adam_JSON.txt")
dataframe_2ndpass <- fromJSON("/Users/marcelo_palermo/PycharmProjects/TeseDoutorado/Training/GridSearchCV/GridSearchResults/CV40/DeepLearning/Model_Proteins_1stRefinement/cv_results_DeepLearning_ALL_EarlyStopping2_Balanced_Adam_JSON.txt")
dataframe_1stpass = dataframe_2ndpass


# # Loop through all columns and print the first item of each list
# for (col in names(dataframe)) {
#   cat("Column:", col, "\n")
#   cat("First item:", dataframe[[col]][[1]], "\n\n")
# }

# Loop through all columns and print the first item of each list
for (col in names(dataframe_1stpass)) {
  cat("Column:", col, "\n")
  cat("First item:", dataframe_1stpass[[col]][[1]], "\n\n")
}

# Loop through all columns and print the first item of each list
for (col in names(dataframe_2ndpass)) {
  cat("Column:", col, "\n")
  cat("First item:", dataframe_2ndpass[[col]][[1]], "\n\n")
}



GridSearchCV_dataframe_1stpass <- data.frame("Fold" = numeric(),"F1Macro" = numeric())
GridSearchCV_dataframe_2ndpass <- data.frame("Fold" = numeric(),"F1Macro" = numeric())



Fold = 1

for (col in names(dataframe_1stpass)[grep("^split", names(dataframe_1stpass))]) {
  
  cat("Column:", col, "\n")
  cat("First item:", dataframe_1stpass[[col]][[1]], "\n\n")
  new_row <- c(Fold,dataframe_1stpass[[col]][[1]])
  GridSearchCV_dataframe_1stpass <- rbind(GridSearchCV_dataframe_1stpass, new_row)
  Fold = Fold+1
  
}

colnames(GridSearchCV_dataframe_1stpass) = c("Fold","F1Macro")

Fold = 1

for (col in names(dataframe_2ndpass)[grep("^split", names(dataframe_2ndpass))]) {
  
  cat("Column:", col, "\n")
  cat("First item:", dataframe_2ndpass[[col]][[1]], "\n\n")
  new_row <- c(Fold,dataframe_2ndpass[[col]][[1]])
  GridSearchCV_dataframe_2ndpass <- rbind(GridSearchCV_dataframe_2ndpass, new_row)
  Fold = Fold+1
  
}

colnames(GridSearchCV_dataframe_2ndpass) = c("Fold","F1Macro")






library(ggplot2)

# Create a line plot using ggplot2
ggplot(data = GridSearchCV_dataframe_1stpass, aes(x = Fold, y = F1Macro)) +
  geom_line(color = "red") +
  labs(x = "k-fold", y = "F1-macro") +
  ylim(0.84, 0.86) +
  theme_minimal() +
  theme(axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"))



# First, create a column indicating the source dataframe
GridSearchCV_dataframe_1stpass$Source <- "1st pass"
GridSearchCV_dataframe_2ndpass$Source <- "2nd pass"

# Combine the dataframes
combined_dataframe <- rbind(GridSearchCV_dataframe_1stpass, GridSearchCV_dataframe_2ndpass)

# Convert Source to factor for grouping
combined_dataframe$Source <- factor(combined_dataframe$Source)


# Create the plot using ggplot2
ggplot(data = combined_dataframe, aes(x = Fold, y = F1Macro, color = Source, linetype = Source)) +
  geom_line() +
  labs(x = "k-fold", y = "F1-macro") +
  ylim(0.8492, 0.8505) +
  scale_x_continuous(breaks = seq(0, max(combined_dataframe$Fold), by = 5)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.9, size = 9)) +
  theme(axis.text.y = element_text(angle = 45, hjust = 0.9, size = 8)) + 
  theme(axis.text = element_text(size = 10),
        axis.title = element_text(size = 10, face = "bold"),
  legend.title = element_blank(), 
  legend.text = element_text(size = 9),
  legend.position = "top") # Change legend text size




# Define an offset for the second pass line
offset <- 0.00002

# Create the plot using ggplot2
ggplot(data = combined_dataframe, aes(x = Fold, y = F1Macro)) +
  geom_smooth(data = combined_dataframe[combined_dataframe$Source == "1st pass",], aes(color = Source), method = "loess", linetype = "solid", se = FALSE) +
  geom_smooth(data = combined_dataframe[combined_dataframe$Source == "2nd pass",], aes(y = F1Macro + offset, color = Source), method = "loess", linetype = "solid", se = FALSE) +
  labs(x = "k-fold", y = "F1-macro") +
  ylim(0.8493558 - offset, 0.8503469 + offset) +  # Adjusting ylim to accommodate the offset
  scale_x_continuous(breaks = seq(0, max(combined_dataframe$Fold), by = 5)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.9, size = 9.5),
        axis.text.y = element_text(angle = 45, hjust = 0.9, size = 8.5),
        axis.text = element_text(size = 10),
        axis.title = element_text(size = 10, face = "bold"),
        legend.title = element_blank(), 
        legend.text = element_text(size = 9.5),
        legend.position = "top",  # Change legend text size
        panel.grid.major = element_line(linetype = "dotted", color = "darkgray"),  # Set major grid lines to dashed
        panel.grid.minor = element_line(linetype = "dotted", color = "darkgray"),
        panel.border = element_rect(color = "black", fill = NA, size = 0.6))  # Add border around plot area

