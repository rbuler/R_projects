library(MASS)
library(car)
library(dplyr)
library(onewaytests)


rm(list = ls())

set.seed(42) # commented for determining minimal sample size
n_features <- 1
mean_benign <- 2
mean_malignant <- 3

# n_samples = 45 (n_total = benign + malignant = 90) is the minimum sample size for which 10 subsequent tests yielded significant differences
# the seed was not set then
n_samples <- 150

benign_data <- mvrnorm(n_samples, mean_benign, 0.5)
malignant_data <- mvrnorm(n_samples, mean_malignant, 0.5)

data <- data.frame(
  group = rep(c("Benign", "Malignant"), each = n_samples),
  radiomic_feature_1 = c(benign_data[, 1], malignant_data[, 1])
)

data <- data %>%
  mutate(radiomic_feature_1 = ifelse(group == "Malignant", 
                                     jitter(radiomic_feature_1, factor = 7500), 
                                     radiomic_feature_1))

results <- data.frame(
    is_normal_benign = NA,
    is_normal_malign = NA,
    is_equal_variance = NA,
    p_value_test = NA,
    test_performed = NA
)

# Shapiro-Wilk test for normality
shapiro_benign <- shapiro.test(data[data$group == "Benign", "radiomic_feature_1"])
shapiro_malignant <- shapiro.test(data[data$group == "Malignant", "radiomic_feature_1"])

# Levene's test for eq var
levene_result <- leveneTest(radiomic_feature_1 ~ group, data)

results$is_normal_benign <- shapiro_benign$p.value > .05
results$is_normal_malign <- shapiro_malignant$p.value > .05
results$is_equal_variance <- levene_result$`Pr(>F)`[1] > .05


if (results$is_normal_benign & results$is_normal_malign & results$is_equal_variance) {
    t_test_result <- t.test(radiomic_feature_1 ~ group, data)
    results$p_value_test <- t_test_result$p.value
    results$test_performed <- 'T-test'
} else if (results$is_normal_benign & results$is_normal_malign & !results$is_equal_variance) {
    welch_results <- welch.test(radiomic_feature_1 ~ group, data)
    results$p_value_test <- welch_results$p.value
    results$test_performed <- 'T-test (Welch\'s)'
} else{
    mann_whitney_result <- wilcox.test(radiomic_feature_1 ~ group, data)
    results$p_value_test <- mann_whitney_result$p.value
    results$test_performed <- 'Mann-Whitney U Test'
}



benign_data <- data[data$group == "Benign", "radiomic_feature_1"]
malignant_data <- data[data$group == "Malignant", "radiomic_feature_1"]

data_combined <- data.frame(
  radiomic_feature_1 = c(benign_data, malignant_data),
  group = c(rep("Benign", length(benign_data)), rep("Malignant", length(malignant_data))))

means <- data_combined %>%
  group_by(group) %>%
  summarise(mean_value = mean(radiomic_feature_1, na.rm = TRUE))



p_value_text <- ifelse(results$p_value_test > .05, 
                       paste("p =", sub("^0\\.", ".", format(results$p_value_test, digits = 2))), 
                       "p < .05")
annotation_text <- paste(results$test_performed, "\n", p_value_text)



p2 <- ggplot(data_combined, aes(x = radiomic_feature_1, fill = group)) +
  geom_density(alpha = 0.5) +
  geom_vline(data = means, aes(xintercept = mean_value, color = group), linetype = "dashed", size = 1) +
  scale_fill_manual(values = c("Benign" = rgb(0, 0, 1, 0.5), "Malignant" = rgb(1, 0, 0, 0.5))) +
  scale_color_manual(values = c("Benign" = "blue", "Malignant" = "red")) +
  labs(
    x = "Radiomic Feature Value",
    y = "Density",
    title = "Data distribution by groups"
  ) +
  theme_light() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 12, face = 'bold'),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    panel.grid = element_blank()
  ) +
  annotate("label", 
           x = max(data_combined$radiomic_feature_1) * 0.7,
           y = max(density(data_combined$radiomic_feature_1)$y) * 1,
           label = annotation_text, 
           size = 4, 
           color = "black", 
           fill = "white", 
           label.size = 0.5, 
           hjust = 0)


print(p2)
print(results)
ggsave("fig2.jpg", plot = p2, width = 8, height = 6)

