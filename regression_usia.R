################ Regression for usia #############

#### regresi usia ####
mod.fit.usia <- glm(formula = partisipasi.masy ~ usia, family = binomial, data = internet.youth.new)
summary(mod.fit.usia)
M.df <- data.frame(usia = seq(16, 30, 1))
M.df$partisipasi.masy <- predict(mod.fit.usia, newdata = M.df, type="response")
ggplot(M.df, aes(x=usia, y=partisipasi.masy)) + geom_line() +
  ylab("Partisipasi Pemuda") + xlab("Usia") +
  theme_bw()

#Uji Chisq
anova(mod.fit.usia, test = "Chisq")

#Hosmer Lemeshow
HL.usia <- HLTest(obj = mod.fit.usia, g = 6)
HL.usia
