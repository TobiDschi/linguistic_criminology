#hashtag comparison
model <- lm(Emotive_Value ~ hashtag * Emotion, comparison_data)
summary(model)
anova(model)

#support comparison
model1 <- lm(Emotive_Value ~ support * Emotion, comparison_data)
summary(model1)
anova(model1)

#model for comparion of events
modelcombination <- lm(data = comparison_data, formula = Emotive_Value ~ timeelapsed * Emotion * event + hashtag)
summary(modelcombination)
anova(modelcombination)
