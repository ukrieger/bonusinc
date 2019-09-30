rm(list = ls())

library(ggplot2)
library(haven)
library(pastecs)
library(patternplot)
library(openxlsx)
library(psych)

male_mz = 31660/62808
female_mz = 31149/62808

# Personen, die 2017 das folgende Alter hatten
age16to29_mz = (1109+1113+ 1105+ 991+ 974+  936+ 907+ 873+ 906+ 916+ 846+ 836+ 811+ 786)/62808   # 1988-2001
age30to39_mz = (1037+ 1012+1072+ 1070+1068+1063+1034+1072+1079+1112)/62808  # 1978-1987
age40to49_mz = (1324+1261+1195+1160+1071+983+977+967+996 + 999)/62808 # 1968-1977
age50to59_mz =(1191+1236+1288+1357+1360+1440+1434+1432+1401+1351)/62808 # 1958-1967
age60plus_mz = (760 + 784+789+592+696+ 784+862+949+977+1007+1022+1009+ 1063+ 1074+ 1121+1169)/ 62808  # - 1957



edu_low_mz =  (16139+789+489+1964)/62808
edu_mid_mz = (15435+3656+2176)/62808
edu_high_mz = (5222+ 16732)/62808

german_mz =54758/62808 
other_mz = 8050/62808 

ledig_mz = (22539)/62808 
verheiratet_mz = (32736+119)/62808 
geschieden_mz = (5268+14)/62808 
verwitwet_mz = (2128+4)/62808  

hh1_mz = 13733/62808 
hh2_mz = 22658/62808 
hh3_mz = 11824/62808 
hh4_mz = (10242+4352)/62808 



##############################  Analysen f?r Teilnahme am ersten Teil, online oder Papier
### alle, die Teil 1 ausf?llen
online1 <- read_dta("Y:/Bearbeitung/Projekte/Rekrutierung_2018/Data/push_to_web_and_bonus/part1_recoded.dta")
datenbank <- read.csv2("//nas.uni-mannheim.de/uni-shares/sfb884/sfb884z/Z1/1 Feldarbeit GIP/11 Rekrutierung TNS/Phase3/Datenbank/Feldstart/stand_final.csv")
online1 <- data.frame(online1)
datenbank$id_l <- datenbank$gip_id
datenbank$online1_date <- as.Date(datenbank$online1_date, "%d.%m.%Y")
datenbank$online2_date <- as.Date(datenbank$online2_date, "%d.%m.%Y")
brutto <- read_dta("Y:/Bearbeitung/Panel_Management/Rekrutierung2018/Gemeindestichprobe/brutto_anonym.dta")
brutto <- data.frame(brutto)
brutto <- subset(brutto, brutto$Stichprobe == 2)
datenbank$exclude <- ifelse(datenbank$gip_id %in% c(brutto$gip_id), 1, 0)
datenbank <- subset(datenbank, datenbank$exclude != 1) # exlude tranche 2


# nur F?lle aus Daten nehmen, die laut Datenbank online 1 abgeschlossen haben
dat <- subset(online1, online1$id_l %in% subset(datenbank, datenbank$online1_status %in% c(2) | datenbank$paper1_status == 1)$gip_id)

dat$group <- dat$hGrp2018
dat$group_new <- dat$group
dat$group_new[dat$group==5] <- 1
dat$group_new[dat$group==6] <- 2
dat$group_new[dat$group==1] <- 5
dat$group_new[dat$group==2] <- 6

dat$group_new <- ordered(dat$group_new,
                     levels = c(4, 2, 1, 3, 5, 6),
                     labels = c("Paper-first", "Concurrent", "Push-to-web", "Online-only", "Bonus 20", "Bonus 50"))

### Tabellen f?r Paper (Table 2)

setwd("Z:/Z1/Projekte/Rekrutierungsexperimente_2018/Paper_Draft/tables")

t_f  <- prop.table(table(dat$female, dat$group_new), 2)*100
t_a1 <- prop.table(table(dat$age16to29, dat$group_new), 2)*100
t_a2 <- prop.table(table(dat$age30to39, dat$group_new), 2)*100
t_a3 <- prop.table(table(dat$age40to49, dat$group_new), 2)*100
t_a4 <- prop.table(table(dat$age50to59, dat$group_new), 2)*100
t_a5 <- prop.table(table(dat$age60plus, dat$group_new), 2)*100
t_e1 <- prop.table(table(dat$edu_low, dat$group_new), 2)*100
t_e2 <- prop.table(table(dat$edu_mid, dat$group_new), 2)*100
t_e3 <- prop.table(table(dat$edu_high, dat$group_new), 2)*100
t_c1 <- table(dat$group_new)


write.xlsx(t_f, 'props_female_1.xlsx')
write.xlsx(t_a1, 'props_a1_1.xlsx')
write.xlsx(t_a2, 'props_a2_1.xlsx')
write.xlsx(t_a3, 'props_a3_1.xlsx')
write.xlsx(t_a4, 'props_a4_1.xlsx')
write.xlsx(t_a5, 'props_a5_1.xlsx')
write.xlsx(t_e1, 'props_edu_low_1.xlsx')
write.xlsx(t_e2, 'props_edu_mid_1.xlsx')
write.xlsx(t_e3, 'props_edu_high_1.xlsx')
write.xlsx(t_c1, 'completion_stage1.xlsx')

summary(table(dat$female, dat$group))
summary(table(dat$age16to29, dat$group))
summary(table(dat$age30to39, dat$group))
summary(table(dat$age40to49, dat$group))
summary(table(dat$age50to59, dat$group))
summary(table(dat$age60plus, dat$group))
summary(table(dat$edu_mid, dat$group))
summary(table(dat$edu_high, dat$group))

by(dat$female, dat$group_new, function(x) sum(is.na(x)))
by(dat$age16to29, dat$group_new, function(x) sum(is.na(x)))
by(dat$edu_low, dat$group_new, function(x) sum(is.na(x)))

t_nf1 <- table(is.na(dat$female), dat$group_new)
t_na1 <- table(is.na(dat$age16to29), dat$group_new)
t_ne1 <- table(is.na(dat$edu_low), dat$group_new)

write.xlsx(t_nf1, 't_nf1.xlsx')
write.xlsx(t_na1, 't_na1.xlsx')
write.xlsx(t_ne1, 't_ne1.xlsx')

stats_f <- by(dat$female, dat$group, stat.desc)
props_f <- c( unlist(stats_f[1])[9], unlist(stats_f[2])[9], unlist(stats_f[3])[9], unlist(stats_f[4])[9], unlist(stats_f[5])[9], unlist(stats_f[6])[9])-female_mz
ses_f <- c(unlist(stats_f[1])[10], unlist(stats_f[2])[10], unlist(stats_f[3])[10],unlist(stats_f[4])[10], unlist(stats_f[5])[10], unlist(stats_f[6])[10])

stats <- by(dat$edu_low, dat$group, stat.desc)
props_e1 <- c( unlist(stats[1])[9], unlist(stats[2])[9], unlist(stats[3])[9], unlist(stats[4])[9], unlist(stats[5])[9], unlist(stats[6])[9])-edu_low_mz
ses_e1 <- c(unlist(stats[1])[10], unlist(stats[2])[10], unlist(stats[3])[10],unlist(stats[4])[10], unlist(stats[5])[10], unlist(stats[6])[10])

stats <- by(dat$edu_mid, dat$group, stat.desc)
props_e2 <- c( unlist(stats[1])[9], unlist(stats[2])[9], unlist(stats[3])[9], unlist(stats[4])[9], unlist(stats[5])[9], unlist(stats[6])[9])-edu_mid_mz
ses_e2 <- c(unlist(stats[1])[10], unlist(stats[2])[10], unlist(stats[3])[10],unlist(stats[4])[10], unlist(stats[5])[10], unlist(stats[6])[10])

stats <- by(dat$edu_high, dat$group, stat.desc)
props_e3 <- c( unlist(stats[1])[9], unlist(stats[2])[9], unlist(stats[3])[9], unlist(stats[4])[9], unlist(stats[5])[9], unlist(stats[6])[9])-edu_high_mz
ses_e3 <- c(unlist(stats[1])[10], unlist(stats[2])[10], unlist(stats[3])[10],unlist(stats[4])[10], unlist(stats[5])[10], unlist(stats[6])[10])

stats <- by(dat$age16to29, dat$group, stat.desc)
props_a1 <- c( unlist(stats[1])[9], unlist(stats[2])[9], unlist(stats[3])[9], unlist(stats[4])[9], unlist(stats[5])[9], unlist(stats[6])[9])-age16to29_mz
ses_a1 <- c(unlist(stats[1])[10], unlist(stats[2])[10], unlist(stats[3])[10],unlist(stats[4])[10], unlist(stats[5])[10], unlist(stats[6])[10])

stats <- by(dat$age30to39, dat$group, stat.desc)
props_a2 <- c( unlist(stats[1])[9], unlist(stats[2])[9], unlist(stats[3])[9], unlist(stats[4])[9], unlist(stats[5])[9], unlist(stats[6])[9])-age30to39_mz
ses_a2 <- c(unlist(stats[1])[10], unlist(stats[2])[10], unlist(stats[3])[10],unlist(stats[4])[10], unlist(stats[5])[10], unlist(stats[6])[10])

stats <- by(dat$age40to49, dat$group, stat.desc)
props_a3 <- c( unlist(stats[1])[9], unlist(stats[2])[9], unlist(stats[3])[9], unlist(stats[4])[9], unlist(stats[5])[9], unlist(stats[6])[9])-age40to49_mz
ses_a3 <- c(unlist(stats[1])[10], unlist(stats[2])[10], unlist(stats[3])[10],unlist(stats[4])[10], unlist(stats[5])[10], unlist(stats[6])[10])

stats <- by(dat$age50to59, dat$group, stat.desc)
props_a4 <- c( unlist(stats[1])[9], unlist(stats[2])[9], unlist(stats[3])[9], unlist(stats[4])[9], unlist(stats[5])[9], unlist(stats[6])[9])-age50to59_mz
ses_a4 <- c(unlist(stats[1])[10], unlist(stats[2])[10], unlist(stats[3])[10],unlist(stats[4])[10], unlist(stats[5])[10], unlist(stats[6])[10])

stats <- by(dat$age60plus, dat$group, stat.desc)
props_a5 <- c( unlist(stats[1])[9], unlist(stats[2])[9], unlist(stats[3])[9], unlist(stats[4])[9], unlist(stats[5])[9], unlist(stats[6])[9])-age60plus_mz
ses_a5 <- c(unlist(stats[1])[10], unlist(stats[2])[10], unlist(stats[3])[10],unlist(stats[4])[10], unlist(stats[5])[10], unlist(stats[6])[10])

rel_f <- props_f/female_mz
rel_e1 <- props_e1/edu_low_mz
rel_e2 <- props_e2/edu_mid_mz
rel_e3 <- props_e3/edu_high_mz
rel_a1 <- props_a1/age16to29_mz
rel_a2 <- props_a2/age30to39_mz
rel_a3 <- props_a3/age40to49_mz
rel_a4 <- props_a4/age50to59_mz
rel_a5 <- props_a5/age60plus_mz

rel.ses.f <- (100/female_mz)*ses_f
rel.ses.e1 <- (100/edu_low_mz)*ses_e1
rel.ses.e2 <- (100/edu_mid_mz)*ses_e2
rel.ses.e3 <- (100/edu_high_mz)*ses_e3
rel.ses.a1 <- (100/age16to29_mz)*ses_a1
rel.ses.a2 <- (100/age30to39_mz)*ses_a2
rel.ses.a3 <- (100/age40to49_mz)*ses_a3
rel.ses.a4 <- (100/age50to59_mz)*ses_a4
rel.ses.a5 <- (100/age60plus_mz)*ses_a5

rel.biases_o1 <- c(rel_f, rel_e1, rel_e2, rel_e3, rel_a1, rel_a2, rel_a3, rel_a4, rel_a5)*100
rel.sds_o1 <- c(rel.ses.f, rel.ses.e1, rel.ses.e2, rel.ses.e3,rel.ses.a1, rel.ses.a2, rel.ses.a3, rel.ses.a4, rel.ses.a5)
rel.low_o1 <- rel.biases_o1 - 1.96*rel.sds_o1
rel.high_o1 <- rel.biases_o1 + 1.96*rel.sds_o1

rel.var_o1 <-  c(rep("female", 6), rep("low education", 6), rep("medium education", 6), rep("high education", 6), 
                      rep("16 to 29 years", 6), rep("30 to 39 years", 6), 
                      rep("40 to 49 years", 6),  rep("50 to 59 years", 6), rep("60 years and older", 6))


 

group <- c("bonus 50 Euro", "bonus 20 Euro", "online-only", "paper-first", "push-to-web", "concurrent")
group_inc <- c("bonus 50 Euro", "bonus 20 Euro", "no bonus", "paper-first", "push-to-web", "concurrent")
ggdat1 <- data.frame(props = c(props_f, props_e1, props_e2, props_e3, props_a1, props_a2, props_a3, props_a4, props_a5), 
                     lows = c(props_f - 1.96*ses_f, props_e1 - 1.96*ses_e1, props_e2 - 1.96*ses_e2, props_e3 - 1.96*ses_e3, 
                               props_a1 - 1.96*ses_a1, props_a2 - 1.96*ses_a2, props_a3 - 1.96*ses_a3, props_a4 - 1.96*ses_a4,  props_a5 - 1.96*ses_a5), 
                     highs = c(props_f + 1.96*ses_f, props_e1 + 1.96*ses_e1, props_e2 + 1.96*ses_e2, props_e3 + 1.96*ses_e3, 
                                props_a1 + 1.96*ses_a1, props_a2 + 1.96*ses_a2, props_a3 + 1.96*ses_a3, props_a4 + 1.96*ses_a4, props_a5 + 1.96*ses_a5), 
                     group = rep(group, 9), 
                    rel.low = rel.low_o1,
                      rel.high = rel.high_o1,
                     rel.bias = rel.biases_o1, 
                     var = c(rep("female", 6), rep("low education", 6), rep("medium education", 6), rep("high education", 6), 
                               rep("16 to 29 years", 6), rep("30 to 39 years", 6), 
                               rep("40 to 49 years", 6),rep("50 to 59 years", 6), rep("60 years and older", 6)))
#ggdat2$ord <- c(rep(1, 6), rep(2, 6), rep(3, 6), rep(4, 6), rep(5, 6), rep(6, 6), rep(7, 6), rep(8, 6))
ggdat1$var2 <- factor(ggdat1$var, levels = c("female", "low education", "medium education", "high education", "16 to 29 years", "30 to 39 years", 
                                             "40 to 49 years", "50 to 59 years","60 years and older"))
ggdat1$group <- factor(ggdat1$group, levels = c(  "bonus 50 Euro", "bonus 20 Euro", "paper-first","online-only", "concurrent","push-to-web" ))



#########################################  Teilnahme an online 2, Rekrutierung
###########################################################################################################
############## ab hier auch Nationalit?t und Familienstand
dat2 <- subset(dat, dat$id_l %in% subset(datenbank, datenbank$online2_status %in% c(2))$id_l) # completes

dat2$group <- dat$group_new


setwd("Z:/Z1/Projekte/Rekrutierungsexperimente_2018/Paper_Draft/tables")

############################################
### Tabellen f?r Paper (Table 4)

t_f <- prop.table(table(dat2$female, dat2$group_new), 2)*100
t_a1 <- prop.table(table(dat2$age16to29, dat2$group_new), 2)*100
t_a2 <- prop.table(table(dat2$age30to39, dat2$group_new), 2)*100
t_a3 <- prop.table(table(dat2$age40to49, dat2$group_new), 2)*100
t_a4 <- prop.table(table(dat2$age50to59, dat2$group_new), 2)*100
t_a5 <- prop.table(table(dat2$age60plus , dat2$group_new), 2)*100
t_e1 <- prop.table(table(dat2$edu_low, dat2$group_new), 2)*100
t_e2 <- prop.table(table(dat2$edu_mid, dat2$group_new), 2)*100
t_e3 <- prop.table(table(dat2$edu_high, dat2$group_new), 2)*100
t_c2 <- table(dat2$group_new)

write.xlsx(t_f, 'props_female_2.xlsx')
write.xlsx(t_a1, 'props_a1_2.xlsx')
write.xlsx(t_a2, 'props_a2_2.xlsx')
write.xlsx(t_a3, 'props_a3_2.xlsx')
write.xlsx(t_a4, 'props_a4_2.xlsx')
write.xlsx(t_a5, 'props_a5_2.xlsx')
write.xlsx(t_e1, 'props_edu_low_2.xlsx')
write.xlsx(t_e2, 'props_edu_mid_2.xlsx')
write.xlsx(t_e3, 'props_edu_high_2.xlsx')
write.xlsx(t_c2, 'completion_stage2.xlsx')


summary(table(dat2$female, dat2$group))
summary(table(dat2$age16to29, dat2$group))
summary(table(dat2$age30to39, dat2$group))
summary(table(dat2$age40to49, dat2$group))
summary(table(dat2$age50to59, dat2$group))
summary(table(dat2$age60plus, dat2$group))
summary(table(dat2$edu_low, dat2$group))
summary(table(dat2$edu_mid, dat2$group))
summary(table(dat2$edu_high, dat2$group))


stats <- by(dat2$female, dat2$group, stat.desc)
props_f <- c( unlist(stats[1])[9], unlist(stats[2])[9], unlist(stats[3])[9], unlist(stats[4])[9], unlist(stats[5])[9], unlist(stats[6])[9])-female_mz
ses_f <- c(unlist(stats[1])[10], unlist(stats[2])[10], unlist(stats[3])[10],unlist(stats[4])[10], unlist(stats[5])[10], unlist(stats[6])[10])

stats <- by(dat2$edu_low, dat2$group, stat.desc)
props_e1 <- c( unlist(stats[1])[9], unlist(stats[2])[9], unlist(stats[3])[9], unlist(stats[4])[9], unlist(stats[5])[9], unlist(stats[6])[9])-edu_low_mz
ses_e1 <- c(unlist(stats[1])[10], unlist(stats[2])[10], unlist(stats[3])[10],unlist(stats[4])[10], unlist(stats[5])[10], unlist(stats[6])[10])

stats <- by(dat2$edu_mid, dat2$group, stat.desc)
props_e2 <- c( unlist(stats[1])[9], unlist(stats[2])[9], unlist(stats[3])[9], unlist(stats[4])[9], unlist(stats[5])[9], unlist(stats[6])[9])-edu_mid_mz
ses_e2 <- c(unlist(stats[1])[10], unlist(stats[2])[10], unlist(stats[3])[10],unlist(stats[4])[10], unlist(stats[5])[10], unlist(stats[6])[10])

stats <- by(dat2$edu_high, dat2$group, stat.desc)
props_e3 <- c( unlist(stats[1])[9], unlist(stats[2])[9], unlist(stats[3])[9], unlist(stats[4])[9], unlist(stats[5])[9], unlist(stats[6])[9])-edu_high_mz
ses_e3 <- c(unlist(stats[1])[10], unlist(stats[2])[10], unlist(stats[3])[10],unlist(stats[4])[10], unlist(stats[5])[10], unlist(stats[6])[10])

stats <- by(dat2$age16to29, dat2$group, stat.desc)
props_a1 <- c( unlist(stats[1])[9], unlist(stats[2])[9], unlist(stats[3])[9], unlist(stats[4])[9], unlist(stats[5])[9], unlist(stats[6])[9])-age16to29_mz
ses_a1 <- c(unlist(stats[1])[10], unlist(stats[2])[10], unlist(stats[3])[10],unlist(stats[4])[10], unlist(stats[5])[10], unlist(stats[6])[10])

stats <- by(dat2$age30to39, dat2$group, stat.desc)
props_a2 <- c( unlist(stats[1])[9], unlist(stats[2])[9], unlist(stats[3])[9], unlist(stats[4])[9], unlist(stats[5])[9], unlist(stats[6])[9])-age30to39_mz
ses_a2 <- c(unlist(stats[1])[10], unlist(stats[2])[10], unlist(stats[3])[10],unlist(stats[4])[10], unlist(stats[5])[10], unlist(stats[6])[10])

stats <- by(dat2$age40to49, dat2$group, stat.desc)
props_a3 <- c( unlist(stats[1])[9], unlist(stats[2])[9], unlist(stats[3])[9], unlist(stats[4])[9], unlist(stats[5])[9], unlist(stats[6])[9])-age40to49_mz
ses_a3 <- c(unlist(stats[1])[10], unlist(stats[2])[10], unlist(stats[3])[10],unlist(stats[4])[10], unlist(stats[5])[10], unlist(stats[6])[10])

stats <- by(dat2$age50to59, dat2$group, stat.desc)
props_a4 <- c( unlist(stats[1])[9], unlist(stats[2])[9], unlist(stats[3])[9], unlist(stats[4])[9], unlist(stats[5])[9], unlist(stats[6])[9])-age50to59_mz
ses_a4 <- c(unlist(stats[1])[10], unlist(stats[2])[10], unlist(stats[3])[10],unlist(stats[4])[10], unlist(stats[5])[10], unlist(stats[6])[10])

stats <- by(dat2$age60plus, dat2$group, stat.desc)
props_a5 <- c( unlist(stats[1])[9], unlist(stats[2])[9], unlist(stats[3])[9], unlist(stats[4])[9], unlist(stats[5])[9], unlist(stats[6])[9])-age60plus_mz
ses_a5 <- c(unlist(stats[1])[10], unlist(stats[2])[10], unlist(stats[3])[10],unlist(stats[4])[10], unlist(stats[5])[10], unlist(stats[6])[10])

# F?r Familienstand und Nationalit?t erg?nzen

### Familienstand, Nationali?t und HH-Gr??e
online2 <- read_dta("Y:/Bearbeitung/Projekte/Rekrutierung_2018/Data/push_to_web_and_bonus/online2_recoded.dta")
online2 <- data.frame(online2)
dato2 <- subset(online2, online2$id_l %in% subset(datenbank, datenbank$online2_status %in% c(2))$id_l) # completes
dato2 <- merge(dato2, dat[, c("id_l", "group")], by.x="id_l", by.y="id_l")

dato2$group_new <- dato2$group
dato2$group_new[dato2$group==5] <- 1
dato2$group_new[dato2$group==6] <- 2
dato2$group_new[dato2$group==1] <- 5
dato2$group_new[dato2$group==2] <- 6

dato2$group_new <- ordered(dato2$group_new,
                         levels = c(4, 2, 1, 3, 5, 6),
                         labels = c("Paper-first", "Concurrent", "Push-to-web", "Online-only", "Bonus 20", "Bonus 50"))


### Tabellen f?r Paper (Table 4-Fortsetzung)
t_m1 <- prop.table(table(dato2$ledig, dato2$group_new), 2)*100
t_m2 <- prop.table(table(dato2$verheiratet, dato2$group_new), 2)*100
t_m3 <- prop.table(table(dato2$geschieden, dato2$group_new), 2)*100
t_m4 <- prop.table(table(dato2$verwitwet, dato2$group_new), 2)*100
t_hh1 <- prop.table(table(dato2$hh1, dato2$group_new), 2)*100
t_hh2 <- prop.table(table(dato2$hh2, dato2$group_new), 2)*100
t_hh3 <- prop.table(table(dato2$hh3, dato2$group_new), 2)*100
t_hh4 <- prop.table(table(dato2$hh4p, dato2$group_new), 2)*100
t_g <- prop.table(table(dato2$german, dato2$group_new), 2)*100
t_o <- prop.table(table(dato2$other, dato2$group_new), 2)*100

summary(table(dato2$ledig, dato2$group))
summary(table(dato2$verheiratet, dato2$group))
summary(table(dato2$geschieden, dato2$group))
summary(table(dato2$verwitwet, dato2$group))
summary(table(dato2$hh1, dato2$group))
summary(table(dato2$hh2, dato2$group))
summary(table(dato2$hh3, dato2$group))
summary(table(dato2$hh4p, dato2$group))
summary(table(dato2$german, dato2$group))
summary(table(dato2$other, dato2$group))

write.xlsx(t_m1, 'props_m1_2.xlsx')
write.xlsx(t_m2, 'props_m2_2.xlsx')
write.xlsx(t_m3, 'props_m3_2.xlsx')
write.xlsx(t_m4, 'props_m4_2.xlsx')
write.xlsx(t_hh1, 'props_hh1_2.xlsx')
write.xlsx(t_hh2, 'props_hh2_2.xlsx')
write.xlsx(t_hh3, 'props_hh3_2.xlsx')
write.xlsx(t_hh4, 'props_hh4_2.xlsx')
write.xlsx(t_g, 'props_g_2.xlsx')
write.xlsx(t_o, 'props_o_2.xlsx')

## NAs

t_nf2 <- table(is.na(dat2$female), dat2$group_new)
t_na2 <- table(is.na(dat2$age16to29), dat2$group_new)
t_ne2 <- table(is.na(dat2$edu_low), dat2$group_new)
t_nm2 <- table(is.na(dato2$ledig), dato2$group_new)
t_nh2 <- table(is.na(dato2$hh1), dato2$group_new)
t_ng2 <- table(is.na(dato2$german), dato2$group_new)

write.xlsx(t_nf2, 't_nf2.xlsx')
write.xlsx(t_na2, 't_na2.xlsx')
write.xlsx(t_ne2, 't_ne2.xlsx')
write.xlsx(t_nm2, 't_nm2.xlsx')
write.xlsx(t_nh2, 't_nh2.xlsx')
write.xlsx(t_ng2, 't_ng2.xlsx')


stats <- by(dato2$ledig, dato2$group, stat.desc)
props_m1 <- c( unlist(stats[1])[9], unlist(stats[2])[9], unlist(stats[3])[9], unlist(stats[4])[9], unlist(stats[5])[9], unlist(stats[6])[9])-ledig_mz
ses_m1 <- c(unlist(stats[1])[10], unlist(stats[2])[10], unlist(stats[3])[10],unlist(stats[4])[10], unlist(stats[5])[10], unlist(stats[6])[10])

stats <- by(dato2$verheiratet, dato2$group, stat.desc)
props_m2 <- c( unlist(stats[1])[9], unlist(stats[2])[9], unlist(stats[3])[9], unlist(stats[4])[9], unlist(stats[5])[9], unlist(stats[6])[9])-verheiratet_mz
ses_m2 <- c(unlist(stats[1])[10], unlist(stats[2])[10], unlist(stats[3])[10],unlist(stats[4])[10], unlist(stats[5])[10], unlist(stats[6])[10])

stats <- by(dato2$geschieden, dato2$group, stat.desc)
props_m3 <- c( unlist(stats[1])[9], unlist(stats[2])[9], unlist(stats[3])[9], unlist(stats[4])[9], unlist(stats[5])[9], unlist(stats[6])[9])-geschieden_mz
ses_m3 <- c(unlist(stats[1])[10], unlist(stats[2])[10], unlist(stats[3])[10],unlist(stats[4])[10], unlist(stats[5])[10], unlist(stats[6])[10])

stats <- by(dato2$verwitwet, dato2$group, stat.desc)
props_m4 <- c( unlist(stats[1])[9], unlist(stats[2])[9], unlist(stats[3])[9], unlist(stats[4])[9], unlist(stats[5])[9], unlist(stats[6])[9])-verwitwet_mz
ses_m4 <- c(unlist(stats[1])[10], unlist(stats[2])[10], unlist(stats[3])[10],unlist(stats[4])[10], unlist(stats[5])[10], unlist(stats[6])[10])

stats <- by(dato2$german, dato2$group, stat.desc)
props_g <- c( unlist(stats[1])[9], unlist(stats[2])[9], unlist(stats[3])[9], unlist(stats[4])[9], unlist(stats[5])[9], unlist(stats[6])[9])-german_mz
ses_g <- c(unlist(stats[1])[10], unlist(stats[2])[10], unlist(stats[3])[10],unlist(stats[4])[10], unlist(stats[5])[10], unlist(stats[6])[10])

stats <- by(dato2$hh1, dato2$group, stat.desc)
props_h1 <- c( unlist(stats[1])[9], unlist(stats[2])[9], unlist(stats[3])[9], unlist(stats[4])[9], unlist(stats[5])[9], unlist(stats[6])[9])-hh1_mz
ses_h1 <- c(unlist(stats[1])[10], unlist(stats[2])[10], unlist(stats[3])[10],unlist(stats[4])[10], unlist(stats[5])[10], unlist(stats[6])[10])

stats <- by(dato2$hh2, dato2$group, stat.desc)
props_h2 <- c( unlist(stats[1])[9], unlist(stats[2])[9], unlist(stats[3])[9], unlist(stats[4])[9], unlist(stats[5])[9], unlist(stats[6])[9])-hh2_mz
ses_h2 <- c(unlist(stats[1])[10], unlist(stats[2])[10], unlist(stats[3])[10],unlist(stats[4])[10], unlist(stats[5])[10], unlist(stats[6])[10])

stats <- by(dato2$hh3, dato2$group, stat.desc)
props_h3 <- c( unlist(stats[1])[9], unlist(stats[2])[9], unlist(stats[3])[9], unlist(stats[4])[9], unlist(stats[5])[9], unlist(stats[6])[9])-hh3_mz
ses_h3 <- c(unlist(stats[1])[10], unlist(stats[2])[10], unlist(stats[3])[10],unlist(stats[4])[10], unlist(stats[5])[10], unlist(stats[6])[10])

stats <- by(dato2$hh4, dato2$group, stat.desc)
props_h4 <- c( unlist(stats[1])[9], unlist(stats[2])[9], unlist(stats[3])[9], unlist(stats[4])[9], unlist(stats[5])[9], unlist(stats[6])[9])-hh4_mz
ses_h4 <- c(unlist(stats[1])[10], unlist(stats[2])[10], unlist(stats[3])[10],unlist(stats[4])[10], unlist(stats[5])[10], unlist(stats[6])[10])

rel_h1 <- props_h1/hh1_mz
rel_h2 <- props_h2/hh2_mz
rel_h3 <- props_h3/hh3_mz
rel_h4 <- props_h4/hh4_mz
rel_g <- props_g/german_mz
rel_f <- props_f/female_mz
rel_e1 <- props_e1/edu_low_mz
rel_e2 <- props_e2/edu_mid_mz
rel_e3 <- props_e3/edu_high_mz
rel_a1 <- props_a1/age16to29_mz
rel_a2 <- props_a2/age30to39_mz
rel_a3 <- props_a3/age40to49_mz
rel_a4 <- props_a4/age50to59_mz
rel_a5 <- props_a5/age60plus_mz
rel_m1 <- props_m1/ledig_mz
rel_m2 <- props_m2/verheiratet_mz
rel_m3 <- props_m3/geschieden_mz
rel_m4 <- props_m4/verwitwet_mz

rel.ses.f <- (100/female_mz)*ses_f
rel.ses.g <- (100/german_mz)*ses_g
rel.ses.e1 <- (100/edu_low_mz)*ses_e1
rel.ses.e2 <- (100/edu_mid_mz)*ses_e2
rel.ses.e3 <- (100/edu_high_mz)*ses_e3
rel.ses.a1 <- (100/age16to29_mz)*ses_a1
rel.ses.a2 <- (100/age30to39_mz)*ses_a2
rel.ses.a3 <- (100/age40to49_mz)*ses_a3
rel.ses.a4 <- (100/age50to59_mz)*ses_a4
rel.ses.a5 <- (100/age60plus_mz)*ses_a5
rel.ses.m1 <- (100/ledig_mz)*ses_m1
rel.ses.m2 <- (100/verheiratet_mz)*ses_m2
rel.ses.m3 <- (100/geschieden_mz)*ses_m3
rel.ses.m4 <- (100/verwitwet_mz)*ses_m4
rel.ses.h1 <- (100/hh1_mz)*ses_h1
rel.ses.h2 <- (100/hh2_mz)*ses_h2
rel.ses.h3 <- (100/hh3_mz)*ses_h3
rel.ses.h4 <- (100/hh4_mz)*ses_h4

rel.biases_o2  <- c(rel_f, rel_e1, rel_e2, rel_e3, rel_a1, rel_a2, rel_a3, rel_a4, rel_a5, rel_m1, rel_m2, rel_m3, rel_m4, 
                    rel_g, rel_h1, rel_h2, rel_h3, rel_h4)*100
rel.sds_o2 <- c(rel.ses.f, rel.ses.e1, rel.ses.e2, rel.ses.e3,rel.ses.a1, rel.ses.a2, rel.ses.a3, rel.ses.a4, rel.ses.a5,
                rel.ses.m1, rel.ses.m2, rel.ses.m3, rel.ses.m4, rel.ses.g, rel.ses.h1, rel.ses.h2, rel.ses.h3, rel.ses.h4)
rel.low_o2 <- rel.biases_o2 - 1.96*rel.sds_o2
rel.high_o2 <- rel.biases_o2 + 1.96*rel.sds_o2


rel.var_o2 = c(rep("female", 6), rep("low education", 6), rep("medium education", 6), rep("high education", 6), 
               rep("16 to 29 years", 6), rep("30 to 39 years", 6), 
               rep("40 to 49 years", 6),  rep("50 to 59 years", 6), rep("60 years and older", 6), 
               rep("single", 6), rep("married", 6), rep("divorced", 6), rep("widowed", 6), 
        rep("german", 6), rep("single household", 6),  rep("2 household members", 6),  rep("3 household members", 6),  rep("4 and more household members", 6))


# F?r Familienstand und Nationalit?t erg?nzen
group_inc <- c("bonus 50 Euro", "bonus 20 Euro", "no bonus", "paper-first", "push-to-web", "concurrent")

ggdat2 <- data.frame(props = c(props_f,props_e1, props_e2, props_e3, props_a1, props_a2, props_a3, props_a4, props_a5, props_m1, props_m2, props_m3, props_m4, props_g, props_h1, props_h2, props_h3, props_h4), 
                     lows = c(props_f - 1.96*ses_f, props_e1 - 1.96*ses_e1, props_e2 - 1.96*ses_e2, props_e3 - 1.96*ses_e3, 
                              props_a1 - 1.96*ses_a1, props_a2 - 1.96*ses_a2, props_a3 - 1.96*ses_a3, props_a4 - 1.96*ses_a4, props_a5 - 1.96*ses_a5, 
                              props_m1 - 1.96*ses_m1,  props_m2 - 1.96*ses_m2, props_m3 - 1.96*ses_m3, 
                              props_m4 - 1.96*ses_m4, props_g - 1.96*ses_g, props_h1 - 1.96*ses_h1,  props_h2 - 1.96*ses_h2, props_h3 - 1.96*ses_h3, 
                              props_h4 - 1.96*ses_h4), 
                     highs = c(props_f + 1.96*ses_f, props_e1 + 1.96*ses_e1, props_e2 + 1.96*ses_e2, props_e3 + 1.96*ses_e3, 
                               props_a1 + 1.96*ses_a1, props_a2 + 1.96*ses_a2, props_a3 + 1.96*ses_a3, props_a4 + 1.96*ses_a4, props_a5 + 1.96*ses_a5,
                               props_m1 + 1.96*ses_m1, props_m2 + 1.96*ses_m2, props_m3 + 1.96*ses_m3, 
                               props_m4 + 1.96*ses_m4, props_g + 1.96*ses_g, props_h1 + 1.96*ses_h1, props_h2 + 1.96*ses_h2, props_h3 + 1.96*ses_h3, 
                               props_h4 + 1.96*ses_h4), 
                     rel.low = rel.low_o2,
                     rel.high = rel.high_o2,
                     rel.bias = rel.biases_o2, 
                     group = rep(group, 18), 
                     group_inc = rep(group_inc, 18),
                     var = c(rep("female", 6), rep("low education", 6), rep("medium education", 6), rep("high education", 6), 
                             rep("16 to 29 years", 6), rep("30 to 39 years", 6), 
                             rep("40 to 49 years", 6),  rep("50 to 59 years", 6), rep("60 years and older", 6), rep("single", 6), rep("married", 6), rep("divorced", 6), rep("widowed", 6), 
                             rep("german", 6), rep("single household", 6),  rep("2 household members", 6),  rep("3 household members", 6),  rep("4 and more household members", 6)))

ggdat2$var2 <- factor(ggdat2$var, levels = c("female", "low education", "medium education", "high education", "16 to 29 years", "30 to 39 years", 
                                             "40 to 49 years", "50 to 59 years","60 years and older", "4 and more household members", "3 household members", "2 household members", "single household",  "married", "widowed", "divorced", "single"))
ggdat2$group <- factor(ggdat2$group, levels = c(  "bonus 50 Euro", "bonus 20 Euro", "paper-first","online-only", "concurrent","push-to-web" ))
ggdat2$group_inc <- factor(ggdat2$group_inc, levels = c(  "bonus 50 Euro", "bonus 20 Euro", "paper-first","no bonus", "concurrent","push-to-web" ))


# setwd("W:/Projekte/Rekrutierungsexperimente_2018/Paper_Draft/graphs")
setwd("Z:/Projekte/Rekrutierungsexperimente_2018/Paper_Draft/graphs")
######################################################################################
# Plot der relativen biases
rel.bias.dat_o1 <- data.frame(bias = abs(rel.biases_o1), var = rel.var_o1, group= rep(group, 9), group_inc = rep(group_inc, 9))
rel.bias.dat_o1$group <- factor(rel.bias.dat_o1$group, levels = c(  "bonus 50 Euro", "bonus 20 Euro","paper-first", "concurrent", "push-to-web", "online-only" ))
rel.bias.dat_o2 <- data.frame(bias = abs(rel.biases_o2), var = rel.var_o2, group=rep(group, 18), group_inc = rep(group_inc, 9))
rel.bias.dat_o2$group <- factor(rel.bias.dat_o2$group, levels = c(  "bonus 50 Euro", "bonus 20 Euro","paper-first", "concurrent", "push-to-web", "online-only" ))

rel.bias.dat_o1$group_inc <- factor(rel.bias.dat_o1$group_inc, levels = c(  "bonus 50 Euro", "bonus 20 Euro", "paper-first","no bonus", "concurrent","push-to-web" ))
rel.bias.dat_o2$group_inc <- factor(rel.bias.dat_o2$group_inc, levels = c(  "bonus 50 Euro", "bonus 20 Euro", "paper-first","no bonus", "concurrent","push-to-web" ))

rel.bias.max1 <- list()
for(i in 1:6){
rel.bias.max1[[i]] <- c(rel.bias.dat_o1[, 1][i], max(rel.bias.dat_o1[, 1][c(i+6, i+12, i+18)]), 
                       max(rel.bias.dat_o1[, 1][c(i+24, i+30, i+36, i+42, i+48)]) )
}

rel.bias.max2 <- list()
for(i in 1:6){
  rel.bias.max2[[i]] <- c(rel.bias.dat_o2[, 1][i], max(rel.bias.dat_o2[, 1][c(i+6, i+12, i+18)]), 
                         max(rel.bias.dat_o2[, 1][c(i+24, i+30, i+36, i+42, i+48)]),
                         max(rel.bias.dat_o2[, 1][c(i+54, i+60, i+66, i+72)]), rel.bias.dat_o2[, 1][i+78],
                         max(rel.bias.dat_o2[, 1][c(i+84, i+90, i+96, i+102)]) )
}

max.dat <- data.frame(rel.bias1 = unlist(rel.bias.max1), rel.bias2 = unlist(rel.bias.max2), 
                      group=c(rep("bonus 50 Euro",6 ), rep("bonus 50 Euro",6 ), rep("online-only",6 ),
                      rep("paper-first",6 ), rep("push-to-web",6 ), rep("concurrent",6 )))

#### Automatic table generation

setwd("Z:/Z1/Projekte/Rekrutierungsexperimente_2018/Paper_Draft/code")

# Part 1

t_aarb1 <- describeBy(rel.bias.dat_o1, rel.bias.dat_o1$group, mat=T, digits=2)

quant_p2w_1 <- subset(rel.bias.dat_o1, group=="push-to-web", select = c(bias, group)) 
quant_cc_1 <- subset(rel.bias.dat_o1, group=="concurrent", select = c(bias, group)) 
quant_oo_1 <- subset(rel.bias.dat_o1, group=="online-only", select = c(bias, group)) 
quant_pf_1 <- subset(rel.bias.dat_o1, group=="paper-first", select = c(bias, group)) 
  

t_quant1_1 <- describe(quant_p2w_1, quant = c(.25,.75))
t_quant2_1 <- describe(quant_cc_1, quant = c(.25,.75))
t_quant3_1 <- describe(quant_oo_1, quant = c(.25,.75))
t_quant4_1 <- describe(quant_pf_1, quant = c(.25,.75))

setwd("Z:/Z1/Projekte/Rekrutierungsexperimente_2018/Paper_Draft/tables")
write.xlsx(t_aarb1, 'aarb_1.xlsx')
write.xlsx(t_quant1_1, 'quant_p2w_1.xlsx')
write.xlsx(t_quant2_1, 'quant_cc_1.xlsx')
write.xlsx(t_quant3_1, 'quant_oo_1.xlsx')
write.xlsx(t_quant4_1, 'quant_pf_1.xlsx')

# Part 2

t_aarb2 <- describeBy(rel.bias.dat_o2, rel.bias.dat_o2$group, mat=T, digits=2)

quant_p2w_2 <- subset(rel.bias.dat_o2, group=="push-to-web", select = c(bias, group)) 
quant_cc_2 <- subset(rel.bias.dat_o2, group=="concurrent", select = c(bias, group)) 
quant_oo_2 <- subset(rel.bias.dat_o2, group=="online-only", select = c(bias, group)) 
quant_pf_2 <- subset(rel.bias.dat_o2, group=="paper-first", select = c(bias, group)) 


t_quant1_2 <- describe(quant_p2w_2, quant = c(.25,.75))
t_quant2_2 <- describe(quant_cc_2, quant = c(.25,.75))
t_quant3_2 <- describe(quant_oo_2, quant = c(.25,.75))
t_quant4_2 <- describe(quant_pf_2, quant = c(.25,.75))



write.xlsx(t_aarb2, 'aarb_2.xlsx')
write.xlsx(t_quant1_2, 'quant_p2w_2.xlsx')
write.xlsx(t_quant2_2, 'quant_cc_2.xlsx')
write.xlsx(t_quant3_2, 'quant_oo_2.xlsx')
write.xlsx(t_quant4_2, 'quant_pf_2.xlsx')


