############## script regression all variable ##############
setwd("F:/COLLEGE/MATEMATIKA/SKRIPSI/R SCRIPT")
internet.youth = read.csv(file = "F:/COLLEGE/MATEMATIKA/SKRIPSI/R SCRIPT/internet_youth.csv", header = TRUE)

#Model Regresi
mod.fit <- glm(formula = partisipasi.masy ~ use.internet + daerah + jenis.kelamin + 
                 usia + status.perkawinan + ketaatan + pekerjaan + pendidikan + status.ekonomi + 
                 kesehatan + mudah.bergaul, family = binomial(link=logit),data = internet.youth)
summary(mod.fit)

#Uji Chisq
anova(mod.fit, test = "Chisq")

#### rasio odds ####
round(exp(mod.fit$coefficient),3)
beta.ci = exp(confint.default(object = mod.fit, level = 0.95))
round(beta.ci,3)

######## Pengecekan Outliers ###########
pi.hat <- predict(mod.fit, type = "response")
p.res <- residuals(mod.fit, type = "pearson")
s.res <- rstandard(mod.fit, type = "pearson")
lin.pred <- mod.fit$linear.predictors
w.n <- data.frame(internet.youth, pi.hat, p.res, s.res, lin.pred)

# Standardized Pearson residual vs X plot
####penggunaan internet
par(mfrow=c(3,4))
plot(x = w.n$use.internet, y = w.n$s.res, xlab = "Penggunaan Internet", ylab = "Standardized Pearson residuals",
     main = "Standardized residuals vs. \n Penggunaan Internet")
abline(h = c(4, 3, 2, 0, -2, -3), lty = "dotted", col = "blue")
w.n$use.internet = as.numeric(w.n$use.internet)
w.n$use.internet = jitter(w.n$use.internet, factor = 0.2)
smooth.stand <- loess(formula = s.res ~ use.internet, data = w.n)
order.dist <- order(w.n$use.internet)
lines(x = w.n$use.internet[order.dist], y = predict(smooth.stand)[order.dist], lty = "solid", col = "red", lwd = 1)
####daerah tempat tinggal
plot(x = w.n$daerah, y = w.n$s.res, xlab = "Daerah Tempat Tinggal", ylab = "Standardized Pearson residuals",
     main = "Standardized residuals vs. \n Daerah Tempat Tinggal")
abline(h = c(4, 3, 2, 0, -2, -3), lty = "dotted", col = "blue")
w.n$daerah = as.numeric(w.n$daerah)
w.n$daerah = jitter(w.n$daerah, factor = 0.2)
smooth.stand <- loess(formula = s.res ~ daerah, data = w.n)
order.dist <- order(w.n$daerah)
lines(x = w.n$daerah[order.dist], y = predict(smooth.stand)[order.dist], lty = "solid", col = "red", lwd = 1)
####jenis kelamin
plot(x = w.n$jenis.kelamin, y = w.n$s.res, xlab = "Jenis Kelamin", ylab = "Standardized Pearson residuals",
     main = "Standardized residuals vs. \n Jenis Kelamin")
abline(h = c(4, 3, 2, 0, -2, -3), lty = "dotted", col = "blue")
w.n$jenis.kelamin = as.numeric(w.n$jenis.kelamin)
w.n$jenis.kelamin = jitter(w.n$jenis.kelamin, factor = 0.2)
smooth.stand <- loess(formula = s.res ~ jenis.kelamin, data = w.n)
order.dist <- order(w.n$jenis.kelamin)
lines(x = w.n$jenis.kelamin[order.dist], y = predict(smooth.stand)[order.dist], lty = "solid", col = "red", lwd = 1)
####usia
plot(x = w.n$usia, y = w.n$s.res, xlab = "Usia", ylab = "Standardized Pearson residuals",
     main = "Standardized residuals vs. \n Usia")
abline(h = c(4, 3, 2, 0, -2, -3), lty = "dotted", col = "blue")
smooth.stand <- loess(formula = s.res ~ usia, data = w.n)
order.dist <- order(w.n$usia)
lines(x = w.n$usia[order.dist], y = predict(smooth.stand)[order.dist], lty = "solid", col = "red", lwd = 1)
####status perkawinan
plot(x = w.n$status.perkawinan, y = w.n$s.res, xlab = "Status Perkawinan", ylab = "Standardized Pearson residuals",
     main = "Standardized residuals vs. \n Status Perkawinan")
abline(h = c(4, 3, 2, 0, -2, -3), lty = "dotted", col = "blue")
w.n$status.perkawinan = as.numeric(w.n$status.perkawinan)
w.n$status.perkawinan = jitter(w.n$status.perkawinan, factor = 0.2)
smooth.stand <- loess(formula = s.res ~ status.perkawinan, data = w.n)
order.dist <- order(w.n$status.perkawinan)
lines(x = w.n$status.perkawinan[order.dist], y = predict(smooth.stand)[order.dist], lty = "solid", col = "red", lwd = 1)
####ketaatan
plot(x = w.n$ketaatan, y = w.n$s.res, xlab = "Ketaatan", ylab = "Standardized Pearson residuals",
     main = "Standardized residuals vs. \n Ketaatan")
abline(h = c(4, 3, 2, 0, -2, -3), lty = "dotted", col = "blue")
w.n$ketaatan = as.numeric(w.n$ketaatan)
w.n$ketaatan = jitter(w.n$ketaatan, factor = 0.2)
smooth.stand <- loess(formula = s.res ~ ketaatan, data = w.n)
order.dist <- order(w.n$ketaatan)
lines(x = w.n$ketaatan[order.dist], y = predict(smooth.stand)[order.dist], lty = "solid", col = "red", lwd = 1)
####status pekerjaan
plot(x = w.n$pekerjaan, y = w.n$s.res, xlab = "Status Pekerjaan", ylab = "Standardized Pearson residuals",
     main = "Standardized residuals vs. \n Status Pekerjaan")
abline(h = c(4, 3, 2, 0, -2, -3), lty = "dotted", col = "blue")
w.n$pekerjaan = as.numeric(w.n$pekerjaan)
w.n$pekerjaan = jitter(w.n$pekerjaan, factor = 0.2)
smooth.stand <- loess(formula = s.res ~ pekerjaan, data = w.n)
order.dist <- order(w.n$pekerjaan)
lines(x = w.n$pekerjaan[order.dist], y = predict(smooth.stand)[order.dist], lty = "solid", col = "red", lwd = 1)
####pendidikan
plot(x = w.n$pendidikan, y = w.n$s.res, xlab = "Pendidikan", ylab = "Standardized Pearson residuals",
     main = "Standardized residuals vs. \n Pendidikan")
abline(h = c(4, 3, 2, 0, -2, -3), lty = "dotted", col = "blue")
w.n$pendidikan = as.numeric(w.n$pendidikan)
w.n$pendidikan = jitter(w.n$pendidikan, factor = 0.2)
smooth.stand <- loess(formula = s.res ~ pendidikan, data = w.n)
order.dist <- order(w.n$pendidikan)
lines(x = w.n$pendidikan[order.dist], y = predict(smooth.stand)[order.dist], lty = "solid", col = "red", lwd = 1)
####status ekonomi
plot(x = w.n$status.ekonomi, y = w.n$s.res, xlab = "Status Ekonomi", ylab = "Standardized Pearson residuals",
     main = "Standardized residuals vs. \n Status Ekonomi")
abline(h = c(4, 3, 2, 0, -2, -3), lty = "dotted", col = "blue")
w.n$status.ekonomi = as.numeric(w.n$status.ekonomi)
w.n$status.ekonomi = jitter(w.n$status.ekonomi, factor = 0.2)
smooth.stand <- loess(formula = s.res ~ status.ekonomi, data = w.n)
order.dist <- order(w.n$status.ekonomi)
lines(x = w.n$status.ekonomi[order.dist], y = predict(smooth.stand)[order.dist], lty = "solid", col = "red", lwd = 1)
####kesehatan
plot(x = w.n$kesehatan, y = w.n$s.res, xlab = "Tingkat Kesehatan", ylab = "Standardized Pearson residuals",
     main = "Standardized residuals vs. \n Tingkat Kesehatan")
abline(h = c(4, 3, 2, 0, -2, -3), lty = "dotted", col = "blue")
w.n$kesehatan = as.numeric(w.n$kesehatan)
w.n$kesehatan = jitter(w.n$kesehatan, factor = 0.2)
smooth.stand <- loess(formula = s.res ~ kesehatan, data = w.n)
order.dist <- order(w.n$kesehatan)
lines(x = w.n$kesehatan[order.dist], y = predict(smooth.stand)[order.dist], lty = "solid", col = "red", lwd = 1)
####kemudahan bergaul
plot(x = w.n$mudah.bergaul, y = w.n$s.res, xlab = "Kemudahan Bergaul", ylab = "Standardized Pearson residuals",
     main = "Standardized residuals vs. \n Kemudahan Bergaul")
abline(h = c(4, 3, 2, 0, -2, -3), lty = "dotted", col = "blue")
w.n$mudah.bergaul = as.numeric(w.n$mudah.bergaul)
w.n$mudah.bergaul = jitter(w.n$mudah.bergaul, factor = 0.2)
smooth.stand <- loess(formula = s.res ~ mudah.bergaul, data = w.n)
order.dist <- order(w.n$mudah.bergaul)
lines(x = w.n$mudah.bergaul[order.dist], y = predict(smooth.stand)[order.dist], lty = "solid", col = "red", lwd = 1)

par(mfrow=c(1,2))
# Standardized Pearson residual vs pi plot
plot(x = w.n$pi.hat, y = w.n$s.res, xlab = "Estimated probability of success", ylab = "Standardized Pearson 
     residuals",main = "Standardized residuals vs. \n Peluang Sukses", ylim = c(-3,7))
abline(h = c(3, 2, 0, -2, -3), lty = "dotted", col = "blue")
smooth.stand <- loess(formula = s.res ~ pi.hat, data = w.n)
order.pi.hat <- order(w.n$pi.hat)
lines(x = w.n$pi.hat[order.pi.hat], y = predict(smooth.stand)[order.pi.hat], lty = "solid", col = "red", lwd = 1)

# Standardized Pearson residual vs linear predictor plot
plot(x = w.n$lin.pred, y = w.n$s.res, xlab = "Linear predictor", ylab = "Standardized Pearson residuals",
     main = "Standardized residuals vs. \n Linear predictor", ylim = c(-3,7))
abline(h = c(3, 2, 0, -2, -3), lty = "dotted", col = "blue")
smooth.stand <- loess(formula = s.res ~ lin.pred, data = w.n)
order.lin.pred <- order(w.n$lin.pred)
lines(x = w.n$lin.pred[order.lin.pred], y = predict(smooth.stand)[order.lin.pred], lty = "solid", col = "red", lwd = 1)

#Hosmer Lemeshow
source("F:/COLLEGE/MATEMATIKA/SKRIPSI/R SCRIPT/AllGOFTests.r")
HL <- HLTest(obj = mod.fit, g = 10)
HL

#leverage regresi model keseluruhan
options(max.print = 100)
source("F:/COLLEGE/MATEMATIKA/SKRIPSI/R SCRIPT/glmDiagnostics.R")
save.diag <- glmInflDiag(mod.fit = mod.fit, print.output = TRUE, which.plots = 1:2)
