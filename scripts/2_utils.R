
packages <- c("DALEX", "datadriftR", "rpart", "randomForest", "ggplot2",
              "gridExtra", "MLmetrics", "foreign", "dplyr","caret")

# missing_packages <- packages[!(packages %in% installed.packages()[,"Package"])]

# if(length(missing_packages)) {
#   install.packages(missing_packages, dependencies = TRUE)
# }

lapply(packages, require, character.only = TRUE)

library(DALEX)
library(datadriftR)
library(rpart)
library(randomForest)
library(ggplot2)
library(gridExtra)
library(MLmetrics)
library(foreign)
library(dplyr)
library(caret)



split_data_simple <- function(data,jj) {
  subset_size <- floor(nrow(data) / jj)

  train_size <- floor(subset_size * 0.8)
  test_size <- subset_size - train_size

  train_set <- data[1:train_size, ]
  test_set <- data[(train_size + 1):(train_size + test_size), ]

  list(total = dim(data)[1] ,train_set = dim(train_set)[1], test_set = dim(test_set)[1])
}

metric_list_cal <- function(profile1,profile2){
  pd <- datadriftR::ProfileDifference$new(method = "pdi", deriv = "gold")
  pd$set_profiles(profile1, profile2)
  result <- pd$calculate_difference()
  pdi_cutoff <- result$distance
  pd <- datadriftR::ProfileDifference$new(method = "L2")
  pd$set_profiles(profile1, profile2)
  result <- pd$calculate_difference()
  l2_cutoff <- result$distance
  pd <- datadriftR::ProfileDifference$new(method = "L2_derivative")
  pd$set_profiles(profile1, profile2)
  result <- pd$calculate_difference()
  l2_der_cutoff <- result$distance

  list(PDI = pdi_cutoff, L2 = l2_cutoff,L2Der = l2_der_cutoff)
}

plot1 <- function(profile1,test,profile2,train,variable){
  ggplot() +
    geom_line(data = profile1, aes(x = x, y = y, color = test), size = 1, linetype = "solid", alpha = 0.7) +
    geom_line(data = profile2, aes(x = x, y = y, color = train), size = 1, linetype = "solid", alpha = 0.7) +
    scale_color_manual(values = c( "#CC79A7",  "#009E73"),
                       labels = c(test,train)) +
    ggtitle("") +
    labs(y = "Average Prediction", x = variable, color = " ") +
    theme_minimal() +
    theme(
      plot.title = element_text(family = "Times New Roman", face = "bold", size = 13),
      plot.subtitle = element_text(family = "Times New Roman", size = 10),
      axis.title = element_text(family = "Times New Roman", size = 10),
      axis.text = element_text(family = "Times New Roman", size = 10),
      legend.title = element_text(family = "Times New Roman", size = 8),
      legend.text = element_text(family = "Times New Roman", size = 10),
      legend.position = "top"
    )
}

save_plot <- function(experiment_result_path, number_of_batch,
                      test_accuracy, pdi_cutoff, l2_der_cutoff, l2_cutoff,
                      p1){
  ggsave(filename = paste0(experiment_result_path,number_of_batch,
                           "-",
                           round(test_accuracy,2),
                           "-",
                           round(pdi_cutoff,2),
                           "-",
                           round(l2_der_cutoff,2),
                           "-",
                           round(l2_cutoff,2)
                           ,".png"), plot = p1, width = 7, height = 4)
}

save_plots <- function(plot_list,
                       experiment_result_path,
                       number_of_batch,accuracy) {
  n <- length(plot_list)
  height <- 6 + (n - 2) * 2 # Height calculation based on number of plots

  pp <- do.call(grid.arrange, c(plot_list, nrow = n))

  ggsave(
    filename = paste0(experiment_result_path, "all_",accuracy,"_" ,number_of_batch, ".png"),
    plot = pp,
    width = 9,
    height = height,
    limitsize = FALSE  # Allow saving of large plots

  )
}

title <- function(acc_temp, pdi_temp, l2_der_temp, l2_temp){
        paste("Accuracy",ifelse(acc_temp == "Cutoff","Cutoff",round(acc_temp,2)),"-",
                "PDI",round(pdi_temp,2),"-",
                "L2_Der",round(l2_der_temp,2),"-",
                "L2",round(l2_temp,2))
}
