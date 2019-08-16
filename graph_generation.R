library(ggplot2)

#figure 1
ggplot(comparison_data, aes(x = hashtag, y = Emotive_Value, col = Emotion)) +
  stat_summary(fun.data = mean_se, geom = "point") +
  stat_summary(fun.data = mean_cl_normal, geom = "errorbar", width = .7) +
  scale_x_discrete(limits = c("#CharlieHebdo", "#Niza", "#Barcelona", "#JeSuisCharlie", "#PrayForNice", "#PrayForBarcelona", "#StopIslam")) +
  scale_y_continuous(name = "Emotive Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0), axis.title.x = element_blank()) +
  scale_colour_grey(start = .2, end = .7) 

#figure2
ggplot(comparison_data, aes(x = support, y = Emotive_Value, col = Emotion)) +
  stat_summary(fun.data = mean_se, geom = "point") +
  stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = .5) +
  scale_x_discrete(limits = c("event description", "expression of solidarity", "#StopIslam")) +
  scale_y_continuous(name = "Emotive Value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0), axis.title.x = element_blank())+
  scale_colour_grey(start = .2, end = .7)

#figure3
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



