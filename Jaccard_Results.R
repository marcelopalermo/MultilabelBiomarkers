

Jaccard = as.data.frame(read.csv('/Users/marcelo_palermo/PycharmProjects/TeseDoutorado/Training/CV40/avg_jaccard_scores.csv', header = FALSE))
# Filled Density Plot

# Adjust margins to include space for the title
# par(mar=c(5, 4, 1, 2) + 0.1)  # Adjust margins to include space for title
par(mar=c(3, 3, 1, 1) + 0.1, cex.axis =0.8, mgp=c(2, 1, 0))  # Adjust margins to include space for title



d <- density(Jaccard$V1)
plot(d, main="", xlab = "Jaccard Index", ylab = "Density")
polygon(d, col="lightblue3", border="lightblue4", density = 50, angle = 135)
grid(col = "darkgray")

# Mark the interval 0.98-1.0 area with a different color
polygon(x = c(d$x[d$x >= 0.98 & d$x <= 1.0], rev(d$x[d$x >= 0.98 & d$x <= 1.0])),
        y = c(d$y[d$x >= 0.98 & d$x <= 1.0], rep(0, sum(d$x >= 0.98 & d$x <= 1.0))),
        col = "red", border = NA, density = 50)


# Highlight 0.98 on x-axis
abline(v = 0.98, col = "darkred", lwd = 1)
abline(v = 1, col = "darkred", lwd = 1)

# Add tick mark at 0.98 on x-axis with smaller font
axis(side = 1, at = 0.98, labels = FALSE) # Hide original label
mtext("0.98", side = 1, line = 1, at = 0.98, col = "darkred", cex = 0.8, padj = -1.3, adj = 1)






# Load the data
Jaccard <- as.data.frame(read.csv('/Users/marcelo_palermo/PycharmProjects/TeseDoutorado/Training/CV40/avg_wasserstein_scores.csv', header = FALSE))

# Adjust margins to include space for the title
par(mar=c(5, 4, 1, 2) + 0.1)

# Create scatter plot
plot(Jaccard$V1, type = "p", col = "blue", main = "", xlab = "X-Axis Label", ylab = "Y-Axis Label")

# Add grid
grid()






###### CHART FOR HAMMING LOSS AND WASSERSTEIN DISTANCE

Jaccard <- as.data.frame(read.csv('/Users/marcelo_palermo/PycharmProjects/TeseDoutorado/Training/CV40/avg_wasserstein_scores.csv', header = FALSE))
Jaccard2 <- as.data.frame(read.csv('/Users/marcelo_palermo/PycharmProjects/TeseDoutorado/Training/CV40/avg_hamming_loss_scores.csv', header = FALSE))

# Adjust margins to include space for the title
par(mar=c(3, 3, 1, 1) + 0.1, cex.axis =0.8, mgp=c(2, 1, 0), font.lab = 2, cex.lab = 0.9)  # Adjust margins to include space for title

# Calculate density for Jaccard
d1 <- density(Jaccard$V1)
d2 <- density(Jaccard2$V1)

# Plot the density for Jaccard
plot(d1,  main="", xlab = "Metric", ylab = "Density", col="darkblue",  ylim = c(0, max(d1$y, d2$y)), xlim = range(c(d1$x, d2$x)))
polygon(d1, col="darkblue", border="darkblue", density = 50)

# Calculate density for Jaccard2


# Plot the density for Jaccard2
lines(d2, col = "lightblue", lty = 1)
polygon(d2, col="lightblue", border="lightblue4", density = 50, angle = 135)
grid(col = "darkgray")

# Add a legend
legend("top", legend=c("Wasserstein Distance ", "Hamming Loss"), col=c("darkblue", "lightblue3"), lty=c(1,1), cex=0.8)








### PLOT JACCARD
Jaccard = as.data.frame(read.csv('/Users/marcelo_palermo/PycharmProjects/TeseDoutorado/Training/CV40/avg_jaccard_scores.csv', header = FALSE))
library(ggplot2)
# Create the plot using ggplot2
a <- ggplot(data = Jaccard, aes(x = seq(1, 1009), y = V1)) +
  geom_smooth(method = "loess", linetype = "solid", se = FALSE) +
  labs(x = "Label", y = "Jaccard Index") +  # Adjust axis labels accordingly
 # ylim(0.0024, 0.002) +  # Adjust ylim according to your data
  scale_x_continuous(breaks = seq(0, 1009, by = 100)) +  # Adjust breaks as needed
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

plot_data <- layer_data(a)



### PLOT HAMMING LOSS
Jaccard <- as.data.frame(read.csv('/Users/marcelo_palermo/PycharmProjects/TeseDoutorado/Training/CV40/avg_hamming_loss_scores.csv', header = FALSE))
Hamming_Loss <- Jaccard
library(ggplot2)
# Create the plot using ggplot2
ggplot(data = Jaccard, aes(x = seq(1, 1009), y = V1)) +
  geom_smooth(method = "loess", linetype = "solid", se = FALSE) +
  labs(x = "Label", y = "Hamming Loss") +  # Adjust axis labels accordingly
  # ylim(0.0024, 0.002) +  # Adjust ylim according to your data
  scale_x_continuous(breaks = seq(0, 1009, by = 100)) +  # Adjust breaks as needed
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


### PLOT WASSERSTEIN
Jaccard <- as.data.frame(read.csv('/Users/marcelo_palermo/PycharmProjects/TeseDoutorado/Training/CV40/avg_wasserstein_scores.csv', header = FALSE))
Wasserstein <- Jaccard
library(ggplot2)
# Create the plot using ggplot2
ggplot(data = Jaccard, aes(x = seq(1, 1009), y = V1)) +
  geom_smooth(method = "loess", linetype = "solid", se = FALSE) +
  labs(x = "Label", y = "Wasserstein Distance") +  # Adjust axis labels accordingly
  # ylim(0.0024, 0.002) +  # Adjust ylim according to your data
  scale_x_continuous(breaks = seq(0, 1009, by = 100)) +  # Adjust breaks as needed
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





#HAMMING LOSS AND WASSERSTEIN
Wasserstein$Label = seq(1, 1009)
Hamming_Loss$Label = seq(1, 1009)

# First, create a column indicating the source dataframe
Wasserstein$Source <- "Wasserstein"
Hamming_Loss$Source <- "Hamming Loss"

# Combine the dataframes
combined_dataframe <- rbind(Wasserstein, Hamming_Loss)

# Convert Source to factor for grouping
combined_dataframe$Source <- factor(combined_dataframe$Source)



# Create the plot using ggplot2
ggplot(data = combined_dataframe, aes(x = Label, y = V1)) +
  geom_smooth(data = combined_dataframe[combined_dataframe$Source == "Wasserstein",], aes(color = Source), method = "loess", linetype = "solid", se = FALSE) +
  geom_smooth(data = combined_dataframe[combined_dataframe$Source == "Hamming Loss",], aes(y = V1, color = Source), method = "loess", linetype = "solid", se = FALSE) +
  labs(x = "Label", y = "Metric") +
  #ylim(0.8493558 - offset, 0.8503469 + offset) +  # Adjusting ylim to accommodate the offset
  scale_x_continuous(breaks = seq(0, max(combined_dataframe$Label), by = 100)) +
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




### PLOT F1-Macro per Label
Jaccard = as.data.frame(read.csv('/Users/marcelo_palermo/PycharmProjects/TeseDoutorado/Training/CV40/avg_f1_scores_label.csv', header = FALSE))
library(ggplot2)
# Create the plot using ggplot2
ggplot(data = Jaccard, aes(x = seq(1, 1009), y = V1)) +
  geom_smooth(method = "loess", linetype = "solid", se = FALSE) +
  labs(x = "Label", y = "F1-macro") +  # Adjust axis labels accordingly
  # ylim(0.0024, 0.002) +  # Adjust ylim according to your data
  scale_x_continuous(breaks = seq(0, 1009, by = 100)) +  # Adjust breaks as needed
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




#JACCARD AND F1-MACRO
Wasserstein = as.data.frame(read.csv('/Users/marcelo_palermo/PycharmProjects/TeseDoutorado/Training/CV40/avg_f1_scores_label.csv', header = FALSE))
Hamming_Loss = as.data.frame(read.csv('/Users/marcelo_palermo/PycharmProjects/TeseDoutorado/Training/CV40/avg_jaccard_scores.csv', header = FALSE))




#HAMMING LOSS AND WASSERSTEIN
Wasserstein$Label = seq(1, 1009)
Hamming_Loss$Label = seq(1, 1009)

# First, create a column indicating the source dataframe
Wasserstein$Source <- "F1-macro"
Hamming_Loss$Source <- "Jaccard"

# Combine the dataframes
combined_dataframe <- rbind(Wasserstein, Hamming_Loss)

# Convert Source to factor for grouping
combined_dataframe$Source <- factor(combined_dataframe$Source)


# Define an offset for the second pass line


# Create the plot using ggplot2
ggplot(data = combined_dataframe, aes(x = Label, y = V1)) +
  geom_smooth(data = combined_dataframe[combined_dataframe$Source == "F1-macro",], aes(color = Source), method = "loess", linetype = "solid", se = FALSE) +
  geom_smooth(data = combined_dataframe[combined_dataframe$Source == "Jaccard",], aes(y = V1 + 0.0005, color = Source), method = "loess", linetype = "solid", se = FALSE) +
  labs(x = "Label", y = "Metric") +
  #ylim(0.845, 0.849+ offset) +  # Adjusting ylim to accommodate the offset
  scale_x_continuous(breaks = seq(0, max(combined_dataframe$Label), by = 100)) +
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








library(ggplot2)

# Read the data
Jaccard <- as.data.frame(read.csv('/Users/marcelo_palermo/PycharmProjects/TeseDoutorado/Training/CV40/avg_wasserstein_scores.csv', header = FALSE))
Jaccard2 <- as.data.frame(read.csv('/Users/marcelo_palermo/PycharmProjects/TeseDoutorado/Training/CV40/avg_hamming_loss_scores.csv', header = FALSE))

# Combine the data into a single data frame
data <- data.frame(Wasserstein = Jaccard$V1, Hamming_Loss = Jaccard2$V1)

# Create the bivariate density plot
ggplot(data, aes(x = Wasserstein, y = Hamming_Loss)) +
  geom_density_2d_filled() +
  scale_fill_brewer() +
  xlim(0,0.005) +
  ylim(0,0.003) +
  labs(x = "Wasserstein Distance", y = "Hamming Loss") +
  theme_minimal() +
  guides(fill=FALSE)


library(ggplot2)

# Read the data
Jaccard <- as.data.frame(read.csv('/Users/marcelo_palermo/PycharmProjects/TeseDoutorado/Training/CV40/avg_wasserstein_scores.csv', header = FALSE))
Jaccard2 <- as.data.frame(read.csv('/Users/marcelo_palermo/PycharmProjects/TeseDoutorado/Training/CV40/avg_hamming_loss_scores.csv', header = FALSE))

# Combine the data into a single data frame
data <- data.frame(Wasserstein = Jaccard$V1, Hamming_Loss = Jaccard2$V1)

# Create the bivariate density plot
ggplot(data, aes(x = Wasserstein, y = Hamming_Loss)) +
  geom_density_2d_filled(alpha = 0.8) +  # Make the plot slightly transparent
  scale_fill_brewer() +
  xlim(0, 0.005) +
  ylim(0, 0.003) +
  labs(x = "Wasserstein Distance", y = "Hamming Loss", title = "Bivariate Density Plot") +
  theme_minimal() +
  theme(panel.grid = element_line(color = "darkslategray", linetype = "dotted")) +  # Add grid lines
  guides(fill = FALSE)  # Remove the legend

