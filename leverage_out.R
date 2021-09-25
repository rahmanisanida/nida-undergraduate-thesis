######### Residual
mod.fit.pmd <- glm(formula = partisipasi.masy ~ use.internet + daerah + jenis.kelamin + 
                           usia + status.perkawinan + ketaatan + pekerjaan + status.ekonomi + mudah.bergaul,
                   family = binomial(link=logit),data = internet.youth.new)
pi.hat1 <- predict(mod.fit.pmd, type = "response")
p.res1 <- residuals(mod.fit.pmd, type = "pearson")
s.res1 <- rstandard(mod.fit.pmd, type = "pearson")
lin.pred1 <- mod.fit.pmd$linear.predictors
w.n1 <- data.frame(internet.youth.new, pi.hat1, p.res1, s.res1, lin.pred1)

par(mfrow=c(3,4))
# Standardized Pearson residual vs X plot
####penggunaan internet
plot(x = w.n1$use.internet, y = w.n1$s.res1, xlab = "Penggunaan Internet", ylab = "Standardized Pearson residuals",
     main = "Standardized residuals vs. \n Penggunaan Internet")
abline(h = c(3, 2, 0, -2, -3), lty = "dotted", col = "blue")
w.n1$use.internet = as.numeric(w.n1$use.internet)
w.n1$use.internet = jitter(w.n1$use.internet, factor = 0.2)
smooth.stand <- loess(formula = s.res1 ~ use.internet, data = w.n1)
order.dist <- order(w.n1$use.internet)
lines(x = w.n1$use.internet[order.dist], y = predict(smooth.stand)[order.dist], lty = "solid", col = "red", lwd = 1)
####daerah tempat tinggal
plot(x = w.n1$daerah, y = w.n1$s.res1, xlab = "Daerah Tempat Tinggal", ylab = "Standardized Pearson residuals",
     main = "Standardized residuals vs. \n Daerah Tempat Tinggal")
abline(h = c(3, 2, 0, -2, -3), lty = "dotted", col = "blue")
w.n1$daerah = as.numeric(w.n1$daerah)
w.n1$daerah = jitter(w.n1$daerah, factor = 0.2)
smooth.stand <- loess(formula = s.res1 ~ daerah, data = w.n1)
order.dist <- order(w.n1$daerah)
lines(x = w.n1$daerah[order.dist], y = predict(smooth.stand)[order.dist], lty = "solid", col = "red", lwd = 1)
####jenis kelamin
plot(x = w.n1$jenis.kelamin, y = w.n1$s.res1, xlab = "Jenis Kelamin", ylab = "Standardized Pearson residuals",
     main = "Standardized residuals vs. \n Jenis Kelamin")
abline(h = c(3, 2, 0, -2, -3), lty = "dotted", col = "blue")
w.n1$jenis.kelamin = as.numeric(w.n1$jenis.kelamin)
w.n1$jenis.kelamin = jitter(w.n1$jenis.kelamin, factor = 0.2)
smooth.stand <- loess(formula = s.res1 ~ jenis.kelamin, data = w.n1)
order.dist <- order(w.n1$jenis.kelamin)
lines(x = w.n1$jenis.kelamin[order.dist], y = predict(smooth.stand)[order.dist], lty = "solid", col = "red", lwd = 1)
####usia
plot(x = w.n1$usia, y = w.n1$s.res1, xlab = "Usia", ylab = "Standardized Pearson residuals",
     main = "Standardized residuals vs. \n Usia")
abline(h = c(3, 2, 0, -2, -3), lty = "dotted", col = "blue")
smooth.stand <- loess(formula = s.res1 ~ usia, data = w.n1)
order.dist <- order(w.n1$usia)
lines(x = w.n1$usia[order.dist], y = predict(smooth.stand)[order.dist], lty = "solid", col = "red", lwd = 1)
####status perkawinan
plot(x = w.n1$status.perkawinan, y = w.n1$s.res1, xlab = "Status Perkawinan", ylab = "Standardized Pearson residuals",
     main = "Standardized residuals vs. \n Status Perkawinan")
abline(h = c(3, 2, 0, -2, -3), lty = "dotted", col = "blue")
w.n1$status.perkawinan = as.numeric(w.n1$status.perkawinan)
w.n1$status.perkawinan = jitter(w.n1$status.perkawinan, factor = 0.2)
smooth.stand <- loess(formula = s.res1 ~ status.perkawinan, data = w.n1)
order.dist <- order(w.n1$status.perkawinan)
lines(x = w.n1$status.perkawinan[order.dist], y = predict(smooth.stand)[order.dist], lty = "solid", col = "red", lwd = 1)
####ketaatan
plot(x = w.n1$ketaatan, y = w.n1$s.res1, xlab = "Ketaatan", ylab = "Standardized Pearson residuals",
     main = "Standardized residuals vs. \n Ketaatan")
abline(h = c(3, 2, 0, -2, -3), lty = "dotted", col = "blue")
w.n1$ketaatan = as.numeric(w.n1$ketaatan)
w.n1$ketaatan = jitter(w.n1$ketaatan, factor = 0.2)
smooth.stand <- loess(formula = s.res1 ~ ketaatan, data = w.n1)
order.dist <- order(w.n1$ketaatan)
lines(x = w.n1$ketaatan[order.dist], y = predict(smooth.stand)[order.dist], lty = "solid", col = "red", lwd = 1)
####status pekerjaan
plot(x = w.n1$pekerjaan, y = w.n1$s.res1, xlab = "Status Pekerjaan", ylab = "Standardized Pearson residuals",
     main = "Standardized residuals vs. \n Status Pekerjaan")
abline(h = c(3, 2, 0, -2, -3), lty = "dotted", col = "blue")
w.n1$pekerjaan = as.numeric(w.n1$pekerjaan)
w.n1$pekerjaan = jitter(w.n1$pekerjaan, factor = 0.2)
smooth.stand <- loess(formula = s.res1 ~ pekerjaan, data = w.n1)
order.dist <- order(w.n1$pekerjaan)
lines(x = w.n1$pekerjaan[order.dist], y = predict(smooth.stand)[order.dist], lty = "solid", col = "red", lwd = 1)
####pendidikan
plot(x = w.n1$pendidikan, y = w.n1$s.res, xlab = "Pendidikan", ylab = "Standardized Pearson residuals",
     main = "Standardized residuals vs. \n Pendidikan")
abline(h = c(4, 3, 2, 0, -2, -3), lty = "dotted", col = "blue")
w.n1$pendidikan = as.numeric(w.n1$pendidikan)
w.n1$pendidikan = jitter(w.n1$pendidikan, factor = 0.2)
smooth.stand <- loess(formula = s.res1 ~ pendidikan, data = w.n1)
order.dist <- order(w.n1$pendidikan)
lines(x = w.n1$pendidikan[order.dist], y = predict(smooth.stand)[order.dist], lty = "solid", col = "red", lwd = 1)
####status ekonomi
plot(x = w.n1$status.ekonomi, y = w.n1$s.res1, xlab = "Status Ekonomi", ylab = "Standardized Pearson residuals",
     main = "Standardized residuals vs. \n Status Ekonomi")
abline(h = c(3, 2, 0, -2, -3), lty = "dotted", col = "blue")
w.n1$status.ekonomi = as.numeric(w.n1$status.ekonomi)
w.n1$status.ekonomi = jitter(w.n1$status.ekonomi, factor = 0.2)
smooth.stand <- loess(formula = s.res1 ~ status.ekonomi, data = w.n1)
order.dist <- order(w.n1$status.ekonomi)
lines(x = w.n1$status.ekonomi[order.dist], y = predict(smooth.stand)[order.dist], lty = "solid", col = "red", lwd = 1)
####kemudahan bergaul
plot(x = w.n1$mudah.bergaul, y = w.n1$s.res1, xlab = "Kemudahan Bergaul", ylab = "Standardized Pearson residuals",
     main = "Standardized residuals vs. \n Kemudahan Bergaul")
abline(h = c(3, 2, 0, -2, -3), lty = "dotted", col = "blue")
w.n1$mudah.bergaul = as.numeric(w.n1$mudah.bergaul)
w.n1$mudah.bergaul = jitter(w.n1$mudah.bergaul, factor = 0.2)
smooth.stand <- loess(formula = s.res1 ~ mudah.bergaul, data = w.n1)
order.dist <- order(w.n1$mudah.bergaul)
lines(x = w.n1$mudah.bergaul[order.dist], y = predict(smooth.stand)[order.dist], lty = "solid", col = "red", lwd = 1)

par(mfrow=c(1,2))
# Standardized Pearson residual vs pi plot
plot(x = w.n1$pi.hat, y = w.n1$s.res1, xlab = "Estimated probability of success", ylab = "Standardized Pearson 
     residuals",main = "Standardized residuals vs. \n Peluang Sukses", ylim = c(-3,7))
abline(h = c(3, 2, 0, -2, -3), lty = "dotted", col = "blue")
smooth.stand <- loess(formula = s.res1 ~ pi.hat1, data = w.n1)
order.pi.hat <- order(w.n1$pi.hat1)
lines(x = w.n1$pi.hat1[order.pi.hat], y = predict(smooth.stand)[order.pi.hat], lty = "solid", col = "red", lwd = 1)

# Standardized Pearson residual vs linear predictor plot
plot(x = w.n1$lin.pred1, y = w.n1$s.res1, xlab = "Linear predictor", ylab = "Standardized Pearson residuals",
     main = "Standardized residuals vs. \n Linear predictor", ylim = c(-3,7))
abline(h = c(3, 2, 0, -2, -3), lty = "dotted", col = "blue")
smooth.stand <- loess(formula = s.res1 ~ lin.pred1, data = w.n1)
order.lin.pred <- order(w.n1$lin.pred1)
lines(x = w.n1$lin.pred1[order.lin.pred], y = predict(smooth.stand)[order.lin.pred], lty = "solid", col = "red", lwd = 1)

#leverage tanpa pengamatan berpengaruh
save.diag.2 <- glmInflDiag(mod.fit = mod.fit.pmd, print.output = TRUE, which.plots = 1:2)
