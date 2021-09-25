library(tidyverse)
library(broom)
theme_set(theme_classic())

#Uji linieritas variabel kontinu dengan logit
mod.fit <- glm(formula = partisipasi.masy ~ use.internet + daerah + jenis.kelamin + 
                 usia + status.perkawinan + ketaatan + pekerjaan + pendidikan + status.ekonomi + 
                 kesehatan + mudah.bergaul, family = binomial(link=logit),data = internet.youth)
probabilities <- predict(mod.fit, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "pos", "neg")
head(predicted.classes)

mydata <- internet.youth %>%
  dplyr::select_if(is.numeric) 
predictors <- colnames(mydata)
mydata <- mydata %>%
  mutate(logit = log(probabilities/(1-probabilities))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)

ggplot(mydata, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  labs(x = "Logit", y = "Predictor Value") +
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")

#Cook's Distance
plot(mod.fit, which = 4, id.n = 3)

#Uji Multikolenieritas
car::vif(mod.fit)
#manual
use.internet=as.numeric(internet.youth$use.internet)
daerah=as.numeric(internet.youth$daerah)
jenis.kelamin=as.numeric(internet.youth$jenis.kelamin)
usia=as.numeric(internet.youth$usia)
status.perkawinan=as.numeric(internet.youth$status.perkawinan)
ketaatan=as.numeric(internet.youth$ketaatan)
pekerjaan=as.numeric(internet.youth$pekerjaan)
pendidikan=as.numeric(internet.youth$pendidikan)
status.ekonomi=as.numeric(internet.youth$status.ekonomi)
kesehatan=as.numeric(internet.youth$kesehatan)
mudah.bergaul=as.numeric(internet.youth$mudah.bergaul)
mod.vif = lm(use.internet ~ daerah + jenis.kelamin + 
               usia + status.perkawinan + ketaatan + pekerjaan + pendidikan + status.ekonomi + 
               kesehatan + mudah.bergaul)
rsq.internet = summary(mod.vif)$r.square
vif.internet = 1/(1-rsq.internet)
vif.internet

#Uji Independensi
part.internet = table(internet.youth$partisipasi.masy, internet.youth$use.internet)
part.daerah = table(internet.youth$partisipasi.masy, internet.youth$daerah)
part.jns.kelamin = table(internet.youth$partisipasi.masy, internet.youth$jenis.kelamin)
part.umur = table(internet.youth$partisipasi.masy, internet.youth$usia)
part.st.perkawinan = table(internet.youth$partisipasi.masy, internet.youth$status.perkawinan)
part.ketaatan = table(internet.youth$partisipasi.masy, internet.youth$ketaatan)
part.pekerjaan = table(internet.youth$partisipasi.masy, internet.youth$pekerjaan)
part.pendidikan = table(internet.youth$partisipasi.masy, internet.youth$pendidikan)
part.st.ekonomi = table(internet.youth$partisipasi.masy, internet.youth$status.ekonomi)
part.kesehatan = table(internet.youth$partisipasi.masy, internet.youth$kesehatan)
part.mdh.bergaul = table(internet.youth$partisipasi.masy, internet.youth$mudah.bergaul)

############ fisher test ###########
fisher.test(part.internet)
fisher.test(part.daerah)
fisher.test(part.jns.kelamin)
fisher.test(part.umur, simulate.p.value = TRUE)
fisher.test(part.st.perkawinan)
fisher.test(part.ketaatan)
fisher.test(part.pekerjaan)
fisher.test(part.pendidikan)
fisher.test(part.st.ekonomi)
fisher.test(part.kesehatan)
fisher.test(part.mdh.bergaul)
