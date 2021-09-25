############### Statistic Descriptive ################

library(car)
library(ggplot2)
library(glmulti)
library(glmnet)
library(questionr)
library(ggpubr)

internet.youth = read.csv(file = "F:/MATEMATIKA/SKRIPSI/R SCRIPT/internet_youth.csv")

#internet.youth$X = seq(1,dim(internet.youth)[1])
internet.youth = internet.youth[-c(1:3)]
internet.youth$jenis.kelamin = factor(internet.youth$jenis.kelamin, levels = c("Perempuan","Laki-laki"))
internet.youth$pendidikan = factor(internet.youth$pendidikan, levels = c("Di bawah SMA","SMA/Sederajat","Perguruan Tinggi"))
internet.youth$status.perkawinan = factor(internet.youth$status.perkawinan, levels = c("Belum Kawin","Kawin"))
internet.youth$status.ekonomi = factor(internet.youth$status.ekonomi, levels = c("Rendah","Sedang","Tinggi"))
internet.youth$kesehatan = factor(internet.youth$kesehatan, levels = c("Tidak Sehat","Kurang Sehat","Sehat","Sangat Sehat"))
internet.youth$mudah.bergaul = factor(internet.youth$mudah.bergaul, levels = c("Tidak Mudah Bergaul","Mudah Bergaul","Sangat Mudah Bergaul"))
internet.youth$ketaatan = factor(internet.youth$ketaatan, levels = c("Tidak taat","Agak taat","Taat","Sangat taat"))
internet.youth$use.internet = factor(internet.youth$use.internet, levels = c("Tidak Menggunakan Internet", "Menggunakan Internet"))

str(internet.youth)
sapply(internet.youth, levels)

freq(internet.youth$partisipasi.masy)
freq(internet.youth$use.internet)
freq(internet.youth$daerah)
freq(internet.youth$jenis.kelamin)
mean(internet.youth$usia)
sd(internet.youth$usia)
freq(internet.youth$status.perkawinan)
freq(internet.youth$ketaatan)
freq(internet.youth$pekerjaan)
freq(internet.youth$pendidikan)
freq(internet.youth$status.ekonomi)
freq(internet.youth$kesehatan)
freq(internet.youth$mudah.bergaul)


#### plot ####
# menggunakan internet vs partisipasi
ggplot(internet.youth, aes(x = partisipasi.masy, group = use.internet)) + 
  geom_bar(aes(y=..prop.., fill=factor(..x..)), stat = "count") +
  geom_text(aes(label = scales::percent(..prop..), y=..prop..), stat="count", vjust=-.5) +
  scale_fill_brewer(palette = "Paired") +
  labs(x = "Partisipasi Pemuda", y = "Percent", fill = "Partisipasi Pemuda") +
  facet_grid(~use.internet) +
  scale_y_continuous(labels = scales::percent)

# daerah vs partisipasi
ggplot(internet.youth, aes(x = partisipasi.masy, group = daerah)) + 
  geom_bar(aes(y=..prop.., fill=factor(..x..)), stat = "count") +
  geom_text(aes(label = scales::percent(..prop..), y=..prop..), stat="count", vjust=-.5) +
  scale_fill_brewer(palette = "Paired") +
  labs(x = "Partisipasi Pemuda", y = "Percent", fill = "Partisipasi Pemuda") +
  facet_grid(~daerah) +
  scale_y_continuous(labels = scales::percent)

# jenis kelamin vs partisipasi
ggplot(internet.youth, aes(x = partisipasi.masy, group = jenis.kelamin)) + 
  geom_bar(aes(y=..prop.., fill=factor(..x..)), stat = "count") +
  geom_text(aes(label = scales::percent(..prop..), y=..prop..), stat="count", vjust=-.5) +
  scale_fill_brewer(palette = "Paired") +
  labs(x = "Partisipasi Pemuda", y = "Percent", fill = "Partisipasi Pemuda") +
  facet_grid(~jenis.kelamin) +
  scale_y_continuous(labels = scales::percent)

# usia vs partisipasi
ggplot(internet.youth, aes(x = partisipasi.masy, y = usia, fill = partisipasi.masy)) + 
  geom_boxplot() +
  stat_summary(fun.y = mean, col = "yellow", geom = "point", size = 3) +
  scale_fill_brewer(palette = "Paired") +
  labs(x = "Partisipasi Pemuda", y = "Usia", fill = "Partisipasi Pemuda") +
  theme_bw()

# perkawinan vs partisipasi
ggplot(internet.youth, aes(x = partisipasi.masy, group = status.perkawinan)) + 
  geom_bar(aes(y=..prop.., fill=factor(..x..)), stat = "count") +
  geom_text(aes(label = scales::percent(..prop..), y=..prop..), stat="count", vjust=-.5) +
  scale_fill_brewer(palette = "Paired") +
  labs(x = "Partisipasi Pemuda", y = "Percent", fill = "Partisipasi Pemuda") +
  facet_grid(~status.perkawinan) +
  scale_y_continuous(labels = scales::percent)

# Ketaatan vs partisipasi
ggplot(internet.youth, aes(x = partisipasi.masy, group = ketaatan)) + 
  geom_bar(aes(y=..prop.., fill=factor(..x..)), stat = "count") +
  geom_text(aes(label = scales::percent(..prop..), y=..prop..), stat="count", vjust=-.5) +
  scale_fill_brewer(palette = "Paired") +
  labs(x = "Partisipasi Pemuda", y = "Percent", fill = "Partisipasi Pemuda") +
  facet_grid(~ketaatan) +
  scale_y_continuous(labels = scales::percent)

# Pekerjaan vs partisipasi
ggplot(internet.youth, aes(x = partisipasi.masy, group = pekerjaan)) + 
  geom_bar(aes(y=..prop.., fill=factor(..x..)), stat = "count") +
  geom_text(aes(label = scales::percent(..prop..), y=..prop..), stat="count", vjust=-.5) +
  scale_fill_brewer(palette = "Paired") +
  labs(x = "Partisipasi Pemuda", y = "Percent", fill = "Partisipasi Pemuda") +
  facet_grid(~pekerjaan) +
  scale_y_continuous(labels = scales::percent)

# pendidikan vs partisipasi
ggplot(internet.youth, aes(x = partisipasi.masy, group = pendidikan)) + 
  geom_bar(aes(y=..prop.., fill=factor(..x..)), stat = "count") +
  geom_text(aes(label = scales::percent(..prop..), y=..prop..), stat="count", vjust=-.5) +
  scale_fill_brewer(palette = "Paired") +
  labs(x = "Partisipasi Pemuda", y = "Percent", fill = "Partisipasi Pemuda") +
  facet_grid(~pendidikan) +
  scale_y_continuous(labels = scales::percent)

# Status ekonomi vs partisipasi
ggplot(internet.youth, aes(x = partisipasi.masy, group = status.ekonomi)) + 
  geom_bar(aes(y=..prop.., fill=factor(..x..)), stat = "count") +
  geom_text(aes(label = scales::percent(..prop..), y=..prop..), stat="count", vjust=-.5) +
  scale_fill_brewer(palette = "Paired") +
  labs(x = "Partisipasi Pemuda", y = "Percent", fill = "Partisipasi Pemuda") +
  facet_grid(~status.ekonomi) +
  scale_y_continuous(labels = scales::percent)

# Kesehatan vs partisipasi
ggplot(internet.youth, aes(x = partisipasi.masy, group = kesehatan)) + 
  geom_bar(aes(y=..prop.., fill=factor(..x..)), stat = "count") +
  geom_text(aes(label = scales::percent(..prop..), y=..prop..), stat="count", vjust=-.5) +
  scale_fill_brewer(palette = "Paired") +
  labs(x = "Partisipasi Pemuda", y = "Percent", fill = "Partisipasi Pemuda") +
  facet_grid(~kesehatan) +
  scale_y_continuous(labels = scales::percent)

# mudah bergaul vs partisipasi
ggplot(internet.youth, aes(x = partisipasi.masy, group = mudah.bergaul)) + 
  geom_bar(aes(y=..prop.., fill=factor(..x..)), stat = "count") +
  geom_text(aes(label = scales::percent(..prop..), y=..prop..), stat="count", vjust=-.5) +
  scale_fill_brewer(palette = "Paired") +
  labs(x = "Partisipasi Pemuda", y = "Percent", fill = "Partisipasi Pemuda") +
  facet_grid(~mudah.bergaul) +
  scale_y_continuous(labels = scales::percent)

#ggarrange(a,b,c,d,e,f,g,h,i,j,k, ncol = 4, nrow = 3)