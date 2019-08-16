library(ggplot2)

#data preparation
comparison_data <- read.csv("C:\\Users\\Tobias\\Studium\\CRIMINA\\bigcomparison\\restricted_full_data.csv")
Sys.setlocale("LC_TIME", "")

comparison_data$created_at <- as.POSIXct(comparison_data$created_at)
comparison_data <- comparison_data[!(comparison_data$event == "bcn" & comparison_data$hashtag == "#Niza"),]
comparison_data <- comparison_data[!(comparison_data$event == "nic" & comparison_data$hashtag == "#Barcelona"),]

levels(comparison_data$support) <- c("event description", "expression of solidarity", "#StopIslam")
levels(comparison_data$Emotion) <- c("Arousal", "Valence")

#hashtag comparison

model <- lm(Emotive_Value ~ hashtag * Emotion, comparison_data)
summary(model)
anova(model)

ggplot(comparison_data, aes(x = hashtag, y = Emotive_Value, col = Emotion)) +
  stat_summary(fun.data = mean_se, geom = "point") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = .7) +
  scale_x_discrete(limits = c("#CharlieHebdo", "#Niza", "#Barcelona", "#JeSuisCharlie", "#PrayForNice", "#PrayForBarcelona", "#StopIslam")) +
  scale_y_continuous(name = "Emotive Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0), axis.title.x = element_blank()) +
  scale_colour_grey(start = .2, end = .7) 

#support comparison

model1 <- lm(Emotive_Value ~ support * Emotion, comparison_data)
summary(model1)
anova(model1)

ggplot(comparison_data, aes(x = support, y = Emotive_Value, col = Emotion)) +
  stat_summary(fun.data = mean_se, geom = "point") +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = .5) +
  scale_x_discrete(limits = c("event description", "expression of solidarity", "#StopIslam")) +
  scale_y_continuous(name = "Emotive Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0), axis.title.x = element_blank())+
  scale_colour_grey(start = .2, end = .7)


#development over time

bcn_time <- min(comparison_data$created_at[comparison_data$event == "bcn"])
mun_time <- min(comparison_data$created_at[comparison_data$event == "chd"])
nic_time <- min(comparison_data$created_at[comparison_data$event == "nic"])

comparison_data$timeelapsed <- NA
comparison_data$timeelapsed[comparison_data$event == "bcn"] <- difftime(comparison_data$created_at[comparison_data$event == "bcn"], bcn_time, units = "mins")
comparison_data$timeelapsed[comparison_data$event == "chd"] <- difftime(comparison_data$created_at[comparison_data$event == "chd"], mun_time, units = "mins")
comparison_data$timeelapsed[comparison_data$event == "nic"] <- difftime(comparison_data$created_at[comparison_data$event == "nic"], nic_time, units = "mins")
comparison_data$'Minutes after attack'<- comparison_data$timeelapsed


modelcombination <- lm(data = comparison_data, formula = Emotive_Value ~ timeelapsed * Emotion * event + hashtag)
summary(modelcombination)
anova(modelcombination)

fmt_dcimals <- function(decimals=0){
  # return a function responpsible for formatting the 
  # axis labels with a given number of decimals 
  function(x) format(round(x, decimals),nsmall = decimals,scientific = FALSE)
}

facet_labeller_top <- function(variable, value) {
  c(
    "", 
    "",
    "",
    "",
    "", 
    "",
    "",
    "",
    ""
  )
}

facet_labeller_bottom <- function(variable, value) {
  c(
    "#Barcelona", 
    "#PrayForBarcelona",
    "#StopIslam",
    "#CharlieHebdo",
    "#JeSuisCharlie", 
    "#StopIslam",
    "#Niza",
    "#PrayForNice",
    "#StopIslam"
  )
}


ggplot(comparison_data, aes(x = timeelapsed/60 , y = Emotive_Value, col = Emotion)) +
  geom_smooth(method = "lm", se = TRUE) +
  facet_wrap(event~hashtag, scales = "free", labeller = labeller(event=as_labeller(facet_labeller_top),hashtag = as_labeller(facet_labeller_bottom))) + 
  theme_minimal() +
  theme(axis.text.x = element_blank()) +
  scale_colour_grey(start = .2, end = .7) +
  scale_y_continuous(name = "Emotive Value", labels = fmt_dcimals(2)) +
  scale_x_continuous(name = "Hours after attack (0-24)") 
  
  
