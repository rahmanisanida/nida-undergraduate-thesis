# working place
#setwd("F:/COLLEGE/MATEMATIKA/SKRIPSI/R SCRIPT")
#setwd("F:/COLLEGE/MATEMATIKA/SKRIPSI/IFLS-5 DATA")

#package
library(foreign)
library(dplyr)
library(plyr)

#### read dta ####
b3b_pm2 = read.dta(file = "F:/COLLEGE/MATEMATIKA/SKRIPSI/IFLS-5 DATA/hh14_all_dta/b3b_pm2.dta")
b3a_dl1 = read.dta(file = "F:/COLLEGE/MATEMATIKA/SKRIPSI/IFLS-5 DATA/hh14_all_dta/b3a_dl1.dta")
bk_sc1 = read.dta(file = "F:/COLLEGE/MATEMATIKA/SKRIPSI/IFLS-5 DATA/hh14_all_dta/bk_sc1.dta")
bk_ar1 = read.dta(file = "F:/COLLEGE/MATEMATIKA/SKRIPSI/IFLS-5 DATA/hh14_all_dta/bk_ar1.dta")
b3a_sw = read.dta(file = "F:/COLLEGE/MATEMATIKA/SKRIPSI/IFLS-5 DATA/hh14_all_dta/b3a_sw.dta")
b3a_mg1 = read.dta(file = "F:/COLLEGE/MATEMATIKA/SKRIPSI/IFLS-5 DATA/hh14_all_dta/b3a_mg1.dta")
b3a_tr = read.dta(file = "F:/COLLEGE/MATEMATIKA/SKRIPSI/IFLS-5 DATA/hh14_all_dta/b3a_tr.dta")
b3b_kk1 = read.dta(file = "F:/COLLEGE/MATEMATIKA/SKRIPSI/IFLS-5 DATA/hh14_all_dta/b3b_kk1.dta")
b3b_psn = read.dta(file = "F:/COLLEGE/MATEMATIKA/SKRIPSI/IFLS-5 DATA/hh14_all_dta/b3b_psn.dta")


##### partisipasi masyarakat #####
partisipasi.masy = subset(b3b_pm2, pm3type == "N", c(hhid14, pidlink, pm16))
colnames(partisipasi.masy) = c("HHID", "PIDLINK", "partisipasi.masy")
partisipasi.masy$partisipasi.masy = revalue(partisipasi.masy$partisipasi.masy, c("1:Yes" = "Ya","3:No" = "Tidak"))

##### internet #####
internet = data.frame(HHID = b3a_dl1$hhid14, PIDLINK = b3a_dl1$pidlink, use.internet = b3a_dl1$dl03c, stringsAsFactors = FALSE)
internet$use.internet = factor(internet$use.internet)
internet$use.internet = grepl("I", internet$use.internet)
internet$use.internet = factor(internet$use.internet)
internet$use.internet = ifelse(internet$use.internet == TRUE, "Menggunakan Internet", "Tidak Menggunakan Internet")

dat1 = internet %>% left_join(partisipasi.masy)
dat1 = na.omit(dat1)

##### daerah tinggal #####
daerah.tinggal = data.frame(HHID = bk_sc1$hhid14, daerah = bk_sc1$sc05, stringsAsFactors = FALSE)
daerah.tinggal$daerah = revalue(daerah.tinggal$daerah, c("1:Urban" = "Kota","2:Rural" = "Desa"))
dat2 = daerah.tinggal %>% left_join(dat1)
dat2 = na.omit(dat2)

##### individual information #####
data.individu = data.frame(HHID = bk_ar1$hhid14, PIDLINK = bk_ar1$pidlink, jenis.kelamin = bk_ar1$ar07, usia = bk_ar1$ar08a, status.perkawinan = bk_ar1$ar13, 
                           pekerjaan = bk_ar1$ar15a, pendidikan = bk_ar1$ar16, stringsAsFactors = FALSE)
#factor jenis kelamin
data.individu$jenis.kelamin = factor(data.individu$jenis.kelamin)
#usia pemuda:16-30, karang taruna:13-45
data.individu = subset(data.individu, data.individu$usia >= 16 & data.individu$usia <= 30)
data.individu = subset(data.individu, data.individu$status.perkawinan != "8:Don't know" & data.individu$status.perkawinan != "9:Missing")
data.individu$status.perkawinan = mapvalues(data.individu$status.perkawinan, c("1:Unmarried", "2:Married", "3:Separated", "4:Divorced", "5:Widow", "6:Cohabitate"), c("Belum Kawin", rep("Kawin",5)))
data.individu = subset(data.individu, data.individu$pekerjaan != "8:Don't know")
data.individu = subset(data.individu, data.individu$pendidikan != "95:Other" & data.individu$pendidikan != "98:Don't know" & data.individu$pendidikan !="99:Missing" & data.individu$pendidikan !="14:Moslem School (Pesantren)" & data.individu$pendidikan != "1:Unschooled")
data.individu$jenis.kelamin = revalue(data.individu$jenis.kelamin, c("1" = "Laki-laki","3" = "Perempuan"))
##### revalue #####
#pendidikan terakhir
data.individu$pendidikan = mapvalues(data.individu$pendidikan, from = c("11:Education A", "2:Grade school", "72:Madrasah Ibtidaiyah",
                                                                        "3:General jr. high", "4:Vocational jr. high", "12:Education B", "73:Madrasah Tsanawiyah",
                                                                        "5:General sr. high (SLA)", "6:Vocational sr. high (SMK)", "15:Education C", "74:Madrasah Aliyah",
                                                                        "13:Open University", "60:Diploma (D1, D2, D3)", "61:University S1", "62:University S2", "63:University S3"),
                                     to = c(rep("Di bawah SMA", 7), rep("SMA/Sederajat", 4), rep("Perguruan Tinggi", 5)))
data.individu$pekerjaan = revalue(data.individu$pekerjaan, c("1:Yes" = "Ya", "3:No" = "Tidak", "6:< 5 Year" = "Ya"))

dat3 = data.individu %>% left_join(dat2)
dat3 = na.omit(dat3)

#ketaatan beragama
taat.agama = data.frame(HHID = b3a_tr$hhid14, PIDLINK = b3a_tr$pidlink, ketaatan = b3a_tr$tr11, stringsAsFactors = FALSE)
taat.agama = subset(taat.agama, taat.agama$ketaatan != "7:Refuse" & taat.agama$ketaatan != "9:Missing")
taat.agama$ketaatan = mapvalues(taat.agama$ketaatan, from = c("1:Very religious","2:Somewhat religious","3:rather  religious", "4:Not religious"), to = c("Sangat taat", "Taat", "Agak taat", "Tidak taat"))
dat4 = taat.agama %>% left_join(dat3)
dat4 = na.omit(dat4)

##### status ekonomi #####
ekonomi = data.frame(HHID = b3a_sw$hhid14, PIDLINK = b3a_sw$pidlink, status.ekonomi = b3a_sw$sw01, stringsAsFactors = FALSE)
ekonomi$status.ekonomi = factor(ekonomi$status.ekonomi)
ekonomi = subset(ekonomi, ekonomi$status.ekonomi != "8" & ekonomi$status.ekonomi != "9")
ekonomi$status.ekonomi = mapvalues(ekonomi$status.ekonomi, from = c("1", "2", "3", "4", "5", "6"), to = c(rep("Rendah",2), rep("Sedang",2), rep("Tinggi",2)))
dat5 = ekonomi %>% left_join(dat4)
dat5 = na.omit(dat5)

##### tingkat kesehatan #####
tingkat.kesehatan = data.frame(HHID = b3b_kk1$hhid14, PIDLINK = b3b_kk1$pidlink, kesehatan = b3b_kk1$kk02k, stringsAsFactors = FALSE)
tingkat.kesehatan = subset(tingkat.kesehatan, tingkat.kesehatan$kesehatan != "9:MISSING")
tingkat.kesehatan$kesehatan = revalue(tingkat.kesehatan$kesehatan, c("1:Very healthy" = "Sangat Sehat", "2:Somewhat healthy" = "Sehat",
                                                                     "3:Somewhat unhealthy" = "Kurang Sehat", "4:Very unhealthy" = "Tidak Sehat"))
dat6 = tingkat.kesehatan %>% left_join(dat5)
dat6 = na.omit(dat6)

##### mudah bergaul #####
mudah.bergaul = subset(b3b_psn, psntype == 13, c(hhid14, pidlink, psn01))
colnames(mudah.bergaul) = c("HHID", "PIDLINK", "mudah.bergaul")
mudah.bergaul$mudah.bergaul = mapvalues(mudah.bergaul$mudah.bergaul, from = c("1:Disagree strongly","2:Disagree a little","3:Neither agree nor disagree","4:Agree a little","5:Agree Strongly"), to = c(rep("Tidak Mudah Bergaul",2), "Netral", "Mudah Bergaul", "Sangat Mudah Bergaul"))
dat7 = mudah.bergaul %>% left_join(dat6)
dat7 = na.omit(dat7)

##### WRITE CSV #####
write.csv(dat7, file = "F:/COLLEGE/MATEMATIKA/SKRIPSI/ICRIEMS/internet_youth.csv")
#write.csv(dat7, file = "F:/COLLEGE/MATEMATIKA/SKRIPSI/R SCRIPT/internet_youth.csv")
