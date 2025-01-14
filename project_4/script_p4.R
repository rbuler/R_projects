library(randomForest)
library(caret)
library(reshape2)

rm(list = ls())

image <- read.csv('satellite_image.csv') # data with not labels for inference only to visualize
labels <- read.csv('labels.csv') # labeled data for training and validation


set.seed(42)
train_index <- createDataPartition(labels$label, p = 0.7, list = FALSE)
train_data <- labels[train_index, ]
val_data <- labels[-train_index, ]

rf <- randomForest(
  x = train_data[, c('band1', 'band2', 'band3', 'band4', 'band5', 'band6', 'band7', 'band8')],
  y = as.factor(train_data$label)
)

predictions <- predict(rf, newdata = val_data[, c('band1', 'band2', 'band3', 'band4', 'band5', 'band6', 'band7', 'band8')])

conf_matrix <- confusionMatrix(predictions, as.factor(val_data$label))
conf_matrix_df <- melt(conf_matrix$table)

p0 <- ggplot(conf_matrix_df, aes(Prediction, Reference, fill = value)) +
  geom_tile() +
  scale_fill_viridis_c() +
  theme_minimal() +
  labs(x = "Predicted", y = "Actual", fill = "Frequency") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 18, face = "bold", hjust = 0.5)) +
  ggtitle("Confusion Matrix for Validation Data") +
  geom_text(aes(label = value), color = "black", size = 3)


image$predictions <- predict(rf, newdata = image[c('band1', 'band2', 'band3', 'band4', 'band5', 'band6', 'band7', 'band8')])

class_colors <- c(
  "grass"  = "#228B22",  # Forest green
  "road1"  = "#A9A9A9",  # Dark gray (asphalt)
  "road2"  = "#696969",  # Dim gray
  "roof1"  = "#B22222",  # Firebrick red
  "roof2"  = "#8B0000",  # Dark red
  "roof3"  = "#CD5C5C",  # Indian red
  "shadow" = "#2F4F4F",  # Dark slate gray
  "soil"   = "#8B4513",  # Saddle brown
  "tree"   = "#32CD32",  # Lime green
  "water"  = "#1E90FF"   # Dodger blue
)



p1 <- ggplot() +
  geom_raster(data = image, aes(x = x, y = y, fill = rgb(band4, band3, band2))) +
  geom_point(data = labels, aes(x = x, y = y, color = as.factor(label)), size = 1) +
  scale_fill_identity() +
  scale_color_manual(values = class_colors) +
  labs(title = "True Color Image with Labels Overlay", x = "X Coordinate", y = "Y Coordinate", color = "Land Cover") +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    
    legend.position = "right",
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  ) +
  coord_fixed()
coord_fixed()



p2 <- ggplot() +
  geom_raster(data = image, aes(x = x, y = y, fill = as.factor(predictions))) + 
  scale_fill_manual(values = class_colors) +  
  labs(
    title = "Predicted Land Cover Map",
    x = "X Coordinate",
    y = "Y Coordinate",
    fill = "Class"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    panel.grid = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    
    plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
    
    legend.position = "right",
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  ) +
  coord_fixed()


print(p0)
# print(p1)
# print(p2)


ggsave("p0_plot.png", plot = p0, width = 8, height = 6)
ggsave("p1_plot.png", plot = p1, width = 8, height = 6)
ggsave("p2_plot.png", plot = p2, width = 8, height = 6)