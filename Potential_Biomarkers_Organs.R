library(dplyr)
library(gridExtra)
library(gganatogram)
library(dplyr)
library(viridis)
library(ggplot2)
library(stringr)


setwd('/Users/marcelo_palermo/Doutorado/R/Olink/Qualification/')


biomarkers = as.data.frame(
  read.csv(
    '/Users/marcelo_palermo/PycharmProjects/TeseDoutorado/Training/CV40/label_names_potential_biomarkers.txt',
    header = FALSE
  )
)
organs_mapping = read.csv(file = 'Organs_Olink_Insight_with_Initials.csv', sep = ';')

biomarkers$V1 = gsub("_", "- ", biomarkers$V1)
biomarkers$V1 = gsub("CERT", "CERT1", biomarkers$V1)

biomarkers$UniProt.ID <- organs_mapping[match(biomarkers$V1,
                                              organs_mapping$Protein.Name), 2]

biomarkers$Organ <- organs_mapping[match(biomarkers$V1,
                                         organs_mapping$Protein.Name), 6]

biomarkers$Panel <- organs_mapping[match(biomarkers$V1,
                                         organs_mapping$Protein.Name), 4]

biomarkers$Organ = gsub("Tissue enhanced: ", "", biomarkers$Organ)
biomarkers$Organ = gsub("Tissue enriched: ", "", biomarkers$Organ)
biomarkers$Organ = gsub("Group enriched: ", "", biomarkers$Organ)
biomarkers$Organ = gsub("- ", ", ", biomarkers$Organ)
biomarkers$Organ = str_replace(biomarkers$Organ, "^\\w{1}", toupper)




write.csv(biomarkers,
          "Potential_Biomarkers_Organs.csv",
          row.names = TRUE)

filtered_data <- biomarkers %>%
  group_by(Organ) %>%
  filter(n() > 1) %>%
  ungroup()

# Load the necessary library


# Plotting
ggplot(biomarkers, aes(x = Organ)) +
  geom_bar(fill = "lightblue3", color = "transparent") +
  geom_text(
    aes(label = ..count..),
    stat = "count",
    hjust = -0.5,
    color = "black",
    size = 2.5
  ) +
  theme_minimal() +
  coord_flip() +
  # labs(title = "Distribution of Potential Biomarkers by Human Tissue",
  labs(title = NULL,
       x = NULL,
       y = "Frequency") +
  theme(axis.text.y = element_text(size = 8)) +  # Adjust the size of y-axis labels +
  ylim(0, 50)  # Set a fixed maximum scale value for y-axis


# Calculate the frequency of each organ
organ_freq <- biomarkers %>%
  count(Organ) %>%
  arrange(desc(n))

# Order the data frame based on organ frequency
data_ordered <- biomarkers %>%
  mutate(Organ = factor(Organ, levels = rev(organ_freq$Organ))) %>%
  arrange(Organ)

### PLOT ARTICLE
ggplot(data_ordered, aes(x = Organ)) +
  geom_bar(
    fill = "lightblue3",
    color = "transparent",
    width = 0.8,
    height = 0.8
  ) +
  geom_text(
    aes(label = ..count..),
    stat = "count",
    hjust = -0.5,
    color = "black",
    size = 2.5
  ) +
  theme_minimal() +
  coord_flip() +
  # labs(title = "Distribution of Potential Biomarkers by Human Tissue",
  labs(title = NULL,
       x = NULL,
       y = NULL) +
  theme(
    axis.text.y = element_text(size = 8, face = "bold"),
    axis.text.x = element_text(size = 8),
    axis.title.x = element_text(size = 8),
    plot.margin = margin(1, 4, 1, 4, "mm")
  ) +  # Adjust the size of y-axis labels +
  ylim(0, 45)  # Set a fixed maximum scale value for y-axis




# Plotting
ggplot(biomarkers, aes(x = Organ, y = length(Organ), fill = Organ)) +
  geom_violin(trim = FALSE) +
  geom_jitter(
    height = 0,
    width = 0.1,
    alpha = 0.5,
    color = "black"
  ) +  # Add jitter for better visualization
  theme_minimal() +
  coord_flip() +
  labs(title = NULL,
       # x = "Human Tissue",
       x = NULL,
       y = "Frequency") +
  guides(fill = FALSE)  # Hide the legend

write.csv(filtered_data,
          "Potential_Biomarkers_Organs_greater_than_one.csv",
          row.names = TRUE)


# Plotting groups with proteins > 1
ggplot(filtered_data, aes(x = Organ)) +
  geom_bar(fill = "skyblue", color = "skyblue2") +
  geom_text(
    aes(label = ..count..),
    stat = "count",
    hjust = -0.5,
    color = "black",
    size = 3
  ) +
  theme_minimal() +
  coord_flip() +
  labs(title = "Distribution of Potential Biomarkers by Human Tissue",
       x = "Organ",
       y = "Frequency") +
  theme(axis.text.y = element_text(size = 7))  # Adjust the size of y-axis labels


# # Plotting groups with proteins > 1 Top Down
# # Calculate the frequency of each organ
# organ_freq <- filtered_data %>%
#   count(Organ) %>%
#   arrange(desc(n))
#
# # Order the data frame based on organ frequency
# data_ordered <- filtered_data %>%
#   mutate(Organ = factor(Organ, levels = rev(organ_freq$Organ))) %>%
#   arrange(Organ)
#
# ggplot(data_ordered, aes(x = Organ)) +
#   geom_bar(fill = "skyblue", color = "skyblue2") +
#   geom_text(
#     aes(label = ..count..),
#     stat = "count",
#     hjust = -0.5,
#     color = "black",
#     size = 3
#   ) +
#   theme_minimal() +
#   coord_flip() +
#   labs(title = "Distribution of Potential Biomarkers by Human Tissue",
#        x = "Organ",
#        y = "Frequency") +
#   theme(axis.text.y = element_text(size = 7))  # Adjust the size of y-axis labels
#


write.csv(filtered_data[which(filtered_data$Organ == "Low tissue specificity"),],
          "Potential_Biomarkers_LTS_g_than_1.csv",
          row.names = TRUE)

write.csv(filtered_data[which(filtered_data$Organ != "Low tissue specificity"),],
          "Potential_Biomarkers_Organs_g_than_1.csv",
          row.names = TRUE)


write.csv(organ_freq, "Grouped_biomarkers.csv",
          row.names = TRUE)



# Plotting > 1 Topdown

organ_freq$Organ <-
  factor(organ_freq$Organ, levels = organ_freq$Organ[order(organ_freq$n, decreasing = FALSE)])

ggplot(organ_freq, aes(x = n, y = Organ)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = n), hjust = -0.2, size = 2.5) +  # Add text labels for counts
  theme_minimal() +
  labs(title = NULL,
       x = "Frequency",
       y = NULL) +
  theme(axis.text.y = element_text(size = 8)) + # Adjust the size of y-axis labels
  xlim(0, 50)  # Set a fixed maximum scale value for y-axis







# Calculate the midpoint index
midpoint <- nrow(organ_freq) / 2

# Split the data into two halves
organ_freq1 <- slice(organ_freq, 1:midpoint)
organ_freq2 <- slice(organ_freq, (midpoint + 1):nrow(organ_freq))

levels(organ_freq1$Organ)

plot1 <- ggplot(organ_freq1, aes(x = n, y = Organ)) +
  # geom_bar(stat = "identity", fill = "skyblue") +
  geom_bar(stat = "identity",
           fill = ifelse(
             organ_freq1$Organ == levels(organ_freq1$Organ)[45],
             "magenta",
             ifelse(
               organ_freq1$Organ == levels(organ_freq1$Organ)[44],
               "yellow",
               ifelse(
                 organ_freq1$Organ == levels(organ_freq1$Organ)[43],
                 "red",
                 ifelse(
                   organ_freq1$Organ == levels(organ_freq1$Organ)[42],
                   "blue",
                   ifelse(
                     organ_freq1$Organ == levels(organ_freq1$Organ)[41],
                     "darkgreen",
                     ifelse(
                       organ_freq1$Organ == levels(organ_freq1$Organ)[40],
                       "green",
                       ifelse(
                         organ_freq1$Organ == levels(organ_freq1$Organ)[39],
                         "orange",
                         ifelse(
                           organ_freq1$Organ == levels(organ_freq1$Organ)[38],
                           "cyan",
                           ifelse(
                             organ_freq1$Organ == levels(organ_freq1$Organ)[37],
                             "black",
                             "skyblue"
                             
                           )
                           
                         )
                       )
                     )
                   )
                 )
               )
             )
           )) +  # Adjust the width of the bars and set color conditionally
  geom_text(aes(label = n), hjust = -0.2, size = 2.8) +  # Add text labels for counts
  theme_minimal() +
  labs(title = NULL,
       x = NULL,
       y = NULL) +
  theme(axis.text.y = element_text(size = 8.5),
        plot.margin = margin(1, 4, 1, 4, "mm")) + # Adjust the size of y-axis labels
  xlim(0, 50)  # Set a fixed maximum scale value for y-axis

plot2 <- ggplot(organ_freq2, aes(x = n, y = Organ)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  geom_text(aes(label = n), hjust = -0.2, size = 2.8) +  # Add text labels for counts
  theme_minimal() +
  labs(title = NULL,
       x = NULL,
       y = NULL) +
  theme(axis.text.y = element_text(size = 8.5),
        plot.margin = margin(1, 4, 1, 4, "mm")) + # Adjust the size of y-axis labels
  xlim(0, 50)  # Set a fixed maximum scale value for y-axis


organPlot <-
  data.frame(
    organ = c(
      "bone_marrow",
      "brain",
      "small_intestine",
      "liver",
      "skeletal_muscle",
      "testis",
      "lymph_node"
    ),
    
    colour = c(
      "darkgreen",
      "blue",
      "red",
      "yellow",
      "orange",
      "green",
      "magenta"
    ),
    value = c(0, 5, 1, 8, 2, 5, 5),
    stringsAsFactors = F
  )
head(organPlot)

hgMale <-
  gganatogram(
    data = organPlot,
    fillOutline = 'azure2',
    organism = 'human',
    sex = 'male',
    fill = "colour"
  ) +
  labs(title = NULL) +
  theme_void() +
  theme(plot.margin = margin(1, 35, 1, 35, "mm"))

organPlot <-
  data.frame(
    organ = c(
      "bone_marrow",
      "brain",
      "small_intestine",
      "liver",
      "skeletal_muscle",
      "placenta",
      "lymph_node"
    ),
    
    colour = c(
      "darkgreen",
      "blue",
      "red",
      "yellow",
      "orange",
      "black",
      "magenta"
    ),
    value = c(0, 5, 1, 8, 2, 5, 5),
    stringsAsFactors = F
  )
head(organPlot)




hgFemale <-
  gganatogram(
    data = organPlot,
    fillOutline = 'seashell',
    organism = 'human',
    sex = 'female',
    fill = "colour"
  ) +
  labs(title = NULL) +
  theme_void() +
  theme(plot.margin = margin(1, 35, 1, 35, "mm"))

grid.arrange(plot1,
             plot2,
             hgMale,
             hgFemale,
             ncol = 2,
             nrow = 2)

# Save PDF: 9.33" por 7.70" Landscape
# Pre-Visualizacao - Editar, imprimir e escolher PDF > Carta EUA, Landscape
# Inserir a, b, c ao editar no Pre-Visualizacao
# manualmente indicar Retina nos dois
#

ifelse(
  organ_freq1$Organ == levels(organ_freq1$Organ)[45],
  "magenta",
  ifelse(
    organ_freq1$Organ == levels(organ_freq1$Organ)[44],
    "yellow",
    ifelse(
      organ_freq1$Organ == levels(organ_freq1$Organ)[43],
      "red",
      ifelse(
        organ_freq1$Organ == levels(organ_freq1$Organ)[42],
        "blue",
        ifelse(
          organ_freq1$Organ == levels(organ_freq1$Organ)[41],
          "darkgreen",
          ifelse(
            organ_freq1$Organ == levels(organ_freq1$Organ)[40],
            "green",
            ifelse(
              organ_freq1$Organ == levels(organ_freq1$Organ)[39],
              "orange",
              ifelse(
                organ_freq1$Organ == levels(organ_freq1$Organ)[38],
                "cyan",
                "skyblue"
                
              )
            )
          )
        )
      )
    )
  )
)
