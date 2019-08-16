#data preparation
comparison_data <- read.csv("")
Sys.setlocale("LC_TIME", "")

comparison_data$created_at <- as.POSIXct(comparison_data$created_at)
comparison_data <- comparison_data[!(comparison_data$event == "bcn" & comparison_data$hashtag == "#Niza"),]
comparison_data <- comparison_data[!(comparison_data$event == "nic" & comparison_data$hashtag == "#Barcelona"),]

levels(comparison_data$support) <- c("event description", "expression of solidarity", "#StopIslam")
levels(comparison_data$Emotion) <- c("Arousal", "Valence")

#generate time variables
bcn_time <- min(comparison_data$created_at[comparison_data$event == "bcn"])
mun_time <- min(comparison_data$created_at[comparison_data$event == "chd"])
nic_time <- min(comparison_data$created_at[comparison_data$event == "nic"])

comparison_data$timeelapsed <- NA
comparison_data$timeelapsed[comparison_data$event == "bcn"] <- difftime(comparison_data$created_at[comparison_data$event == "bcn"], bcn_time, units = "mins")
comparison_data$timeelapsed[comparison_data$event == "chd"] <- difftime(comparison_data$created_at[comparison_data$event == "chd"], mun_time, units = "mins")
comparison_data$timeelapsed[comparison_data$event == "nic"] <- difftime(comparison_data$created_at[comparison_data$event == "nic"], nic_time, units = "mins")
comparison_data$'Minutes after attack'<- comparison_data$timeelapsed

