library(ggplot2)
library(ggcorrplot)
library(caret)

rm(list = ls())
set.seed(42)


data <- read.csv("data.csv")
head(data)
summary(data)

reduced_data <- subset(data, select = -c(dependent, group))
corr_matrix = round(cor(reduced_data), 2)
p1 <- ggcorrplot(corr_matrix, hc.order = TRUE, type = "lower",
           lab = TRUE) +
  theme(panel.grid = element_blank())

train_index <- createDataPartition(data$group, p = 0.5, list = FALSE)
train_data <- data[train_index, ]
valid_data <- data[-train_index, ]


model <- lm(dependent ~ var1 + var2 + var3 + var4 + time, data = train_data)
summary(model)



train_data$predicted <- predict(model, newdata = train_data)
valid_data$predicted <- predict(model, newdata = valid_data)




max_value <- max(
  c(train_data$dependent, valid_data$dependent),
  na.rm = TRUE
)
xlim_range <- c(0, max_value)


max_value <- max(
  c(train_data$predicted, valid_data$predicted),
  na.rm = TRUE
)
ylim_range <- c(0, max_value)


p2 <- ggplot(train_data, aes(x = dependent, y = predicted)) +
  geom_point(size = 1.5, alpha = 0.7, color="purple") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", size = 1, color = "red") +
  facet_wrap(~ group) +
  labs(
    title = "Train - Actual vs Predicted Values (by Group)",
    x = "Actual Values",
    y = "Predicted Values"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    strip.text = element_text(face = "bold", size = 12)
  ) +
  coord_cartesian(xlim = xlim_range, ylim = ylim_range)

p3 <- ggplot(valid_data, aes(x = dependent, y = predicted)) +
  geom_point(size = 1.5, alpha = 0.7, color="purple") +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", size = 1, color = "red") +
  facet_wrap(~ group) +
  labs(
    title = "Validation - Actual vs Predicted Values (by Group)",
    x = "Actual Values",
    y = "Predicted Values"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
    strip.text = element_text(face = "bold", size = 12)
  ) +
 coord_cartesian(xlim = xlim_range, ylim = ylim_range)


print(p1)
print(p2)
print(p3)

ggsave("cor_plot.png", plot = p1, width = 8, height = 6, dpi = 300)
ggsave("reg_plot_train.png", plot = p2, width = 8, height = 6, dpi = 300)
ggsave("reg_plot_val.png", plot = p3, width = 8, height = 6, dpi = 300)

