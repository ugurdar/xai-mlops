# To run all experiments, please run only this script.
source("scripts/2_utils.R")
source("scripts/exp.R")
source("scripts/exp_friedman.R")

#############################################
##### Elec2 #################################
splits <- c(3,5,7)
batches <- c(10,20,30)
models <- c("rf","logistic","tree")
set.seed(1234)
data <- read.arff(paste0("data/","elec",".arff"))
data$class <- ifelse(data$class =="UP",1,0)
data$class <- as.factor(data$class)
data <- data %>% dplyr::select(nswprice, nswdemand, vicprice, vicdemand, class)
df_results <- data.frame()
count <- 1
for(split in splits){
  results_model <- data.frame()
  for(model in models){
    experiment_result_path <- paste0("results/plots/Elec2/",model,"/",batches[count],"/")
    df2 <- run_experiment(model, data, target_col = "class", split, experiment_result_path)
    write.csv(df_results,file=paste0(experiment_result_path,"results.csv"),row.names = FALSE)
    results_model <- rbind(results_model,df2)
  }
  df_results <- rbind(df_results,results_model)
  count <- count +1
}

write.csv(df_results,file=paste0("results/tables/Elec2.csv"),row.names = FALSE)

#############################################
##### Hyperplane ############################
models <- c("rf","logistic","tree")
set.seed(1234)
data <- read.csv("data/hyperplane07.csv",header = TRUE)
data$target <- as.factor(data$target)
df_results <- data.frame()
count <- 1
for(split in splits){
  results_model <- data.frame()
  for(model in models){
    experiment_result_path <- paste0("results/plots/hyperplane/",model,"/",batches[count],"/")
    df2 <- run_experiment(model, data, target_col = "target", split, experiment_result_path)
    write.csv(df_results,file=paste0(experiment_result_path,"results.csv"),row.names = FALSE)
    results_model <- rbind(results_model,df2)
  }
  df_results <- rbind(df_results,results_model)
  count <- count +1
}
write.csv(df_results,file=paste0("results/tables/hyperplane.csv"),row.names = FALSE)

#############################################
##### NOAA ##################################
models <- c("rf","logistic","tree")
set.seed(1234)
data <- read.arff(paste0("data/","NOAA",".arff"))
df_results <- data.frame()
count <- 1
for(split in splits){
  results_model <- data.frame()
  for(model in models){
    experiment_result_path <- paste0("results/plots/NOAA/",model,"/",batches[count],"/")
    df2 <- run_experiment(model, data, target_col = "class", split, experiment_result_path)
    write.csv(df2,file=paste0(experiment_result_path,"results.csv"),row.names = FALSE)
    results_model <- rbind(results_model,df2)
  }
  df_results <- rbind(df_results,results_model)
  count <- count +1
}
write.csv(df_results,file=paste0("results/tables/NOAA.csv"),row.names = FALSE)

#############################################
#### Ozone ##################################
models <- c("rf","logistic","tree")
set.seed(1234)
data <- read.arff(paste0("data/","ozone",".arff"))
df_results <- data.frame()
count <- 1
for(split in splits){
  results_model <- data.frame()
  for(model in models){
    experiment_result_path <- paste0("results/plots/ozone/",model,"/",batches[count],"/")
    df2 <- run_experiment(model, data, target_col = "Class", split, experiment_result_path)
    write.csv(df2,file=paste0(experiment_result_path,"results.csv"),row.names = FALSE)
    results_model <- rbind(results_model,df2)
  }
  df_results <- rbind(df_results,results_model)
  count <- count +1
}
write.csv(df_results,file=paste0("results/tables/ozone.csv"),row.names = FALSE)

#############################################
##### SEA ###################################
models <- c("rf","logistic","tree")
set.seed(1234)
xx <- read.csv("data/SEA_training_data.csv",header = FALSE)
yy <- read.csv("data/SEA_training_class.csv",header = FALSE)
data <- data.frame(xx, class = as.factor(yy$V1))
df_results <- data.frame()
count <- 1
for(split in splits){
  results_model <- data.frame()
  for(model in models){
    experiment_result_path <- paste0("results/plots/SEA/",model,"/",batches[count],"/")
    df2 <- run_experiment(model, data, target_col = "class", split, experiment_result_path)
    write.csv(df2,file=paste0(experiment_result_path,"results.csv"),row.names = FALSE)
    results_model <- rbind(results_model,df2)
  }
  df_results <- rbind(df_results,results_model)
  count <- count +1
}
write.csv(df_results,file=paste0("results/tables/SEA.csv"),row.names = FALSE)

#############################################
##### Friedman ##############################
source("scripts/exp_friedman.R")
set.seed(1234)
models <- c("rf","linear","tree")
splits <- c(3,5,7)
batches <- c(10,20,30)
data <- read.csv("data/friedman_drift_dataset.csv",
header = TRUE)

df_results <- data.frame()
count <- 1
for(split in splits){
  results_model <- data.frame()
  for(model in models){
    message(model)
    experiment_result_path <- paste0("results/plots/Friedman/",model,"/",batches[count],"/")
    df2 <- run_experiment_friedman(model_choice=model, data, target_col = "target",split, experiment_result_path)
    write.csv(df2,file=paste0(experiment_result_path,"results.csv"),row.names = FALSE)
    results_model <- rbind(results_model,df2)
  }
  df_results <- rbind(df_results,results_model)
  count <- count +1
}

write.csv(df_results,file=paste0("results/tables/Friedman.csv"),row.names = FALSE)
