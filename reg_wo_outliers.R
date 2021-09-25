#Penghapusan Data Berpengaruh
internet.youth.new = anti_join(internet.youth, save.diag)
str(internet.youth.new)

#Model Regresi
mod.fit.pmd <- glm(formula = partisipasi.masy ~ use.internet + daerah + jenis.kelamin + usia + 
                 status.perkawinan + ketaatan + pekerjaan + pendidikan + status.ekonomi + mudah.bergaul,
                 family = binomial(link=logit),data = internet.youth.new)
summary(mod.fit.pmd, digits = 5)
round(summary(mod.fit.pmd)$coefficients, 5)

#Uji Chisq
anova(mod.fit.pmd, test = "Chisq")

#### rasio odds ####
round(exp(mod.fit.pmd$coefficient), digits = 3)
beta.ci.pmd = exp(confint.default(object = mod.fit.pmd, level = 0.95))
round(beta.ci.pmd, digits = 3)

#Hosmer Lemeshow
source("F:/MATEMATIKA/SKRIPSI/R SCRIPT/AllGOFTests.r")
HL.pmd <- HLTest(obj = mod.fit.pmd, g = 6)
#options(digits = 3)
HL.pmd
cbind(HL.pmd$observed, round(HL.pmd$expect, digits = 1))
round(HL.pmd$pear, digits = 1)
