################ Regression After Stepwise #################

#### after stepwise forward regression ####
#stepwise forward
empty.mod <- glm(formula = partisipasi.masy ~ 1, family = binomial(link = logit), data = internet.youth)
full.mod <- glm(formula = partisipasi.masy ~ ., family = binomial(link = logit), data = internet.youth)
forw.sel <- step(object = empty.mod, scope = list(upper = full.mod), direction = "forward",
                 k = log(nrow(internet.youth)), trace = TRUE)

mod.fit.fwd = glm(formula = partisipasi.masy ~ jenis.kelamin + status.perkawinan + daerah + usia, 
                  family = binomial(link=logit),data = internet.youth.new)
summary(mod.fit.fwd)

#Uji Chisq
anova(mod.fit.fwd, test = "Chisq")

#Hosmer Lemeshow
HL.fwd <- HLTest(obj = mod.fit.fwd, g = 6)
HL.fwd
