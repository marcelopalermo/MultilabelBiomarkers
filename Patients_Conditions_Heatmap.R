getwd()
setwd("/Users/marcelo_palermo/Doutorado/R/Olink/Qualification/")
getwd()
MGH_info = as.data.frame(read.csv('MGH_NPX_dataframe_initial.csv'))
patients_info = as.data.frame(read.csv('MGH_COVID_Clinical_Info.csv'))
patients_info <- patients_info[(patients_info$subject_id %in% unique(MGH_info$subject_id)), ]
patients_info = patients_info[which(patients_info$COVID == 1), ]
patients_info <- patients_info[(patients_info$subject_id %in% unique(MGH_info$subject_id)), ]
patients_info = patients_info[which(patients_info$BMI_cat != 5), ]
patients_info["Total_Diseases"] <- NA
patients_info$Total_Diseases = patients_info$HEART + patients_info$LUNG + patients_info$KIDNEY + patients_info$DIABETES + patients_info$HTN

### GROUP BY AGE AND THEN BMI

library(dplyr)
# Sample dataframe
df <- data.frame(
  Group1 = c("A", "A", "B", "B", "A"),
  Group2 = c("X", "Y", "Y", "X", "Y"),
  Value = c(10, 20, 30, 40, 50)
)

# Group by 'Group1' and 'Group2', and sum the 'Value' column
summarized_df <- df %>%
  group_by(Group1, Group2) %>%
  summarise(Sum_Value = sum(Value))

# Print the summarized dataframe
print(summarized_df)



# Generate all combinations of Age, BMI, and diseases
age <- 1:5
bmi <- 0:4
diseases <- c("HEART","LUNG","KIDNEY","DIABETES","HTN","None")
combinations <- expand.grid(Age = age, BMI = bmi, Diseases = diseases)
# Print the resulting dataframe
combinations['value'] <- NA
print(combinations)


### ANDAR POR CADA COMBINACAO Age, BMI, <doenca>
### Colocar a Soma


# Load dplyr
library(dplyr)
each_disease = unique(combinations$Diseases)
agg_tbl <- NA

######################################################################

column_name = "HEART"
# Group by sum using dplyr - HEART - COLUMN NAME
agg_tbl <- patients_info %>% group_by(Age_cat, BMI_cat) %>% 
  summarise(!!paste0("sum_", column_name) := sum(!!sym(column_name)),
            .groups = 'drop')
agg_tbl
agg_tbl$Diseases = "HEART"

colnames(agg_tbl) = c("Age","BMI","value","Diseases")
result <- combinations %>%
  inner_join(agg_tbl, by = c("Age", "BMI","Diseases")) %>%
  mutate(value = ifelse(is.na(value.x), value.y, value.x)) %>%
  select(-value.x, -value.y)

result_final <- result



######################################################################

column_name = "LUNG"
# Group by sum using dplyr - HEART - COLUMN NAME
agg_tbl <- patients_info %>% group_by(Age_cat, BMI_cat) %>% 
  summarise(!!paste0("sum_", column_name) := sum(!!sym(column_name)),
            .groups = 'drop')
agg_tbl
agg_tbl$Diseases = "LUNG"

colnames(agg_tbl) = c("Age","BMI","value","Diseases")
result <- combinations %>%
  inner_join(agg_tbl, by = c("Age", "BMI","Diseases")) %>%
  mutate(value = ifelse(is.na(value.x), value.y, value.x)) %>%
  select(-value.x, -value.y)


result_final <- rbind(result, result_final)

######################################################################

column_name = "HTN"
# Group by sum using dplyr - HEART - COLUMN NAME
agg_tbl <- patients_info %>% group_by(Age_cat, BMI_cat) %>% 
  summarise(!!paste0("sum_", column_name) := sum(!!sym(column_name)),
            .groups = 'drop')
agg_tbl
agg_tbl$Diseases = "HTN"

colnames(agg_tbl) = c("Age","BMI","value","Diseases")
result <- combinations %>%
  inner_join(agg_tbl, by = c("Age", "BMI","Diseases")) %>%
  mutate(value = ifelse(is.na(value.x), value.y, value.x)) %>%
  select(-value.x, -value.y)

result_final = rbind(result,result_final)



######################################################################

column_name = "KIDNEY"
# Group by sum using dplyr - HEART - COLUMN NAME
agg_tbl <- patients_info %>% group_by(Age_cat, BMI_cat) %>% 
  summarise(!!paste0("sum_", column_name) := sum(!!sym(column_name)),
            .groups = 'drop')
agg_tbl
agg_tbl$Diseases = "KIDNEY"

colnames(agg_tbl) = c("Age","BMI","value","Diseases")
result <- combinations %>%
  inner_join(agg_tbl, by = c("Age", "BMI","Diseases")) %>%
  mutate(value = ifelse(is.na(value.x), value.y, value.x)) %>%
  select(-value.x, -value.y)

result_final = rbind(result_final,result)


######################################################################

column_name = "DIABETES"
# Group by sum using dplyr - HEART - COLUMN NAME
agg_tbl <- patients_info %>% group_by(Age_cat, BMI_cat) %>% 
  summarise(!!paste0("sum_", column_name) := sum(!!sym(column_name)),
            .groups = 'drop')
agg_tbl
agg_tbl$Diseases = "DIABETES"

colnames(agg_tbl) = c("Age","BMI","value","Diseases")
result <- combinations %>%
  inner_join(agg_tbl, by = c("Age", "BMI","Diseases")) %>%
  mutate(value = ifelse(is.na(value.x), value.y, value.x)) %>%
  select(-value.x, -value.y)

result_final = rbind(result_final,result)

result_final <- rbind(result_final, combinations[!rownames(combinations) %in% rownames(result_final), ])

combinations$value <-
  result_final[match(paste(combinations$Age, combinations$BMI, combinations$Diseases), 
               paste(result_final$Age, result_final$BMI, result_final$Diseases)), 4]

## Replace NAs by Zeroes
combinations$value[is.na(combinations$value)] <- 0


## ADD None 
summarized_combinations <- combinations %>%
  group_by(Age, BMI) %>%
  summarise(value = sum(value))
# Print the summarized dataframe
print(summarized_combinations)
summarized_combinations <-
  summarized_combinations[which(summarized_combinations$value == 0),]
summarized_combinations['Diseases'] <- 'None'

summarized_combinations$value = 1

## SUBSTITUIR APENAS ROWS DE combinations pelas de summarized_combinations

for (i in 1:nrow(summarized_combinations)) {
  
  row1 = summarized_combinations[i,]
  print(row1)
  #combinations$value[row1$Age & row1$BMI & row1$Diseases] = row1$value
  combinations$value[combinations$Age == row1$Age & combinations$BMI == row1$BMI & combinations$Diseases == 'None'] = 1
  
  
} 



write.csv(combinations,
          "Patients_Conditions_Heatmap.csv",
          row.names = TRUE)



# Heatmap AGE BMI
# Load required libraries
library(ggplot2)

# Create a heatmap using ggplot2
heatmap <- ggplot(combinations, aes(x = BMI, y = Age, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = "white") +
  labs(title = "Patient Distribution Across Age Groups, BMI Categories, and Diseases",
       x = "BMI Category", y = "Age Group", fill = "Patient Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Print the heatmap
print(heatmap)


### AGE, BMI and DISEASES
library(ggplot2)

combinations$BMI <- factor(combinations$BMI, levels = 0:4, labels = c("Underweight", "Normal", "Overweight", "Obese", "Severely Obese"))
combinations$Age <- factor(combinations$Age, levels = 1:5, labels = c("20-34", "35-49", "50-64", "65-79", "80+"))



# df$Column1 <- ifelse(df$Column1 == "A", "a", 
#                      ifelse(df$Column1 == "B", "b", df$Column1))


combinations$Diseases <- 
  ifelse(combinations$Diseases == "HTN", "Hypertension", ifelse(combinations$Diseases == "HEART", "Heart", ifelse(combinations$Diseases == "DIABETES", "Diabetes", ifelse(combinations$Diseases == "KIDNEY", "Kidney", ifelse(combinations$Diseases == "LUNG", "Lung", ifelse(combinations$Diseases == "None", "No Diseases", combinations$Diseases ))))))





# Create a heatmap using ggplot2 with Diseases as facets
heatmap <- ggplot(combinations, aes(x = BMI, y = Age, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "lightblue", high = "darkblue", na.value = "white") +
  labs(#title = "Patients' Cohort Profile for Pre-Existing Diseases",
       x = "BMI", y = "Age", fill = "Patient Count") +
  facet_grid(. ~ Diseases) +  # Add Diseases as facet
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 0.9, size = 8)) +
  theme(axis.text.y = element_text(angle = 45, hjust = 0.9, size = 8))

# Print the heatmap
print(heatmap)



aggregate(combinations$value, list(combinations$Diseases), FUN=sum)
## Trocar codigo Age por extenso
