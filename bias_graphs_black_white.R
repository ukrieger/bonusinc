rm(list = ls())

library(ggplot2)
library(haven)
library(pastecs)
library(patternplot)

male_mz = 31660/62808
female_mz = 31149/62808

# Personen, die 2017 das folgende Alter hatten
age16to29_mz = (1109+1113+ 1105+ 991+ 974+  936+ 907+ 873+ 906+ 916+ 846+ 836+ 811+ 786)/62808   # 1988-2001
age30to39_mz = (1037+ 1012+1072+ 1070+1068+1063+1034+1072+1079+1112)/62808  # 1978-1987
age40to49_mz = (1324+1261+1195+1160+1071+983+977+967+996 + 999)/62808 # 1968-1977
age50to59_mz =(1191+1236+1288+1357+1360+1440+1434+1432+1401+1351)/62808 # 1958-1967
age60plus_mz = (760 + 784+789+592+696+ 784+862+949+977+1007+1022+1009+ 1063+ 1074+ 1121+1169)/ 62808  # - 1957


edu_low_0_mz = (489+1964)/62808
edu_low_1_mz = (16139+789)/62808
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

### Tabellen f?r Paper (Table 2)
prop.table(table(dat$female, dat$group), 2)*100
prop.table(table(dat$age16to29, dat$group), 2)*100
prop.table(table(dat$age30to39, dat$group), 2)*100
prop.table(table(dat$age40to49, dat$group), 2)*100
prop.table(table(dat$age50to59, dat$group), 2)*100
prop.table(table(dat$age60plus, dat$group), 2)*100
prop.table(table(dat$edu_low, dat$group), 2)*100
prop.table(table(dat$edu_low_0, dat$group), 2)*100
prop.table(table(dat$edu_low_1, dat$group), 2)*100
prop.table(table(dat$edu_low, dat$group), 2)*100
prop.table(table(dat$edu_mid, dat$group), 2)*100
prop.table(table(dat$edu_high, dat$group), 2)*100

summary(table(dat$female, dat$group))
summary(table(dat$age16to29, dat$group))
summary(table(dat$age30to39, dat$group))
summary(table(dat$age40to49, dat$group))
summary(table(dat$age50to59, dat$group))
summary(table(dat$age60plus, dat$group))
summary(table(dat$edu_low, dat$group))
summary(table(dat$edu_low_0, dat$group))
summary(table(dat$edu_low_1, dat$group))
summary(table(dat$edu_mid, dat$group))
summary(table(dat$edu_high, dat$group))

by(dat$female, dat$group, function(x) sum(is.na(x)))
by(dat$age16to29, dat$group, function(x) sum(is.na(x)))
by(dat$edu_low, dat$group, function(x) sum(is.na(x)))


stats_f <- by(dat$female, dat$group, stat.desc)
props_f <- c( unlist(stats_f[1])[9], unlist(stats_f[2])[9], unlist(stats_f[3])[9], unlist(stats_f[4])[9], unlist(stats_f[5])[9], unlist(stats_f[6])[9])-female_mz
ses_f <- c(unlist(stats_f[1])[10], unlist(stats_f[2])[10], unlist(stats_f[3])[10],unlist(stats_f[4])[10], unlist(stats_f[5])[10], unlist(stats_f[6])[10])

stats <- by(dat$edu_low_0, dat$group, stat.desc)
props_e1_0 <- c( unlist(stats[1])[9], unlist(stats[2])[9], unlist(stats[3])[9], unlist(stats[4])[9], unlist(stats[5])[9], unlist(stats[6])[9])-edu_low_0_mz
ses_e1_0 <- c(unlist(stats[1])[10], unlist(stats[2])[10], unlist(stats[3])[10],unlist(stats[4])[10], unlist(stats[5])[10], unlist(stats[6])[10])

stats <- by(dat$edu_low_1, dat$group, stat.desc)
props_e1_1 <- c( unlist(stats[1])[9], unlist(stats[2])[9], unlist(stats[3])[9], unlist(stats[4])[9], unlist(stats[5])[9], unlist(stats[6])[9])-edu_low_1_mz
ses_e1_1 <- c(unlist(stats[1])[10], unlist(stats[2])[10], unlist(stats[3])[10],unlist(stats[4])[10], unlist(stats[5])[10], unlist(stats[6])[10])

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

rel_e1_0 <- props_e1_0/edu_low_0_mz
rel_e1_1 <- props_e1_1/edu_low_1_mz
rel_e1 <- props_e1/edu_low_mz
rel_e2 <- props_e2/edu_mid_mz
rel_e3 <- props_e3/edu_high_mz
rel_a1 <- props_a1/age16to29_mz
rel_a2 <- props_a2/age30to39_mz
rel_a3 <- props_a3/age40to49_mz
rel_a4 <- props_a4/age50to59_mz
rel_a5 <- props_a5/age60plus_mz

rel.ses.f <- (100/female_mz)*ses_f
rel.ses.e1_0 <- (100/edu_low_0_mz)*ses_e1_0
rel.ses.e1_1 <- (100/edu_low_1_mz)*ses_e1_1
rel.ses.e1 <- (100/edu_low_mz)*ses_e1
rel.ses.e2 <- (100/edu_mid_mz)*ses_e2
rel.ses.e3 <- (100/edu_high_mz)*ses_e3
rel.ses.a1 <- (100/age16to29_mz)*ses_a1
rel.ses.a2 <- (100/age30to39_mz)*ses_a2
rel.ses.a3 <- (100/age40to49_mz)*ses_a3
rel.ses.a4 <- (100/age50to59_mz)*ses_a4
rel.ses.a5 <- (100/age60plus_mz)*ses_a5

rel.biases_o1 <- c(rel_f, rel_e1_0, rel_e1_1, rel_e2, rel_e3, rel_a1, rel_a2, rel_a3, rel_a4, rel_a5)*100
rel.sds_o1 <- c(rel.ses.f, rel_e1_0, rel_e1_1, rel.ses.e2, rel.ses.e3,rel.ses.a1, rel.ses.a2, rel.ses.a3, rel.ses.a4, rel.ses.a5)
rel.low_o1 <- rel.biases_o1 - 1.96*rel.sds_o1
rel.high_o1 <- rel.biases_o1 + 1.96*rel.sds_o1

rel.var_o1 <-  c(rep("female", 6), rep("low education", 6), rep("low med education", 6), rep("medium education", 6), rep("high education", 6), 
                      rep("16 to 29 years", 6), rep("30 to 39 years", 6), 
                      rep("40 to 49 years", 6),  rep("50 to 59 years", 6), rep("60 years and older", 6))


 

group <- c("bonus 50 Euro", "bonus 20 Euro", "online-only", "paper-first", "push-to-web", "concurrent")
group_inc <- c("bonus 50 Euro", "bonus 20 Euro", "no bonus", "paper-first", "push-to-web", "concurrent")
ggdat1 <- data.frame(props = c(props_f, props_e1_0, props_e1_1, props_e2, props_e3, props_a1, props_a2, props_a3, props_a4, props_a5), 
                     lows = c(props_f - 1.96*ses_f, props_e1_0 - 1.96*ses_e1_0,  props_e1_1 - 1.96*ses_e1_1, props_e2 - 1.96*ses_e2, props_e3 - 1.96*ses_e3, 
                               props_a1 - 1.96*ses_a1, props_a2 - 1.96*ses_a2, props_a3 - 1.96*ses_a3, props_a4 - 1.96*ses_a4,  props_a5 - 1.96*ses_a5), 
                     highs = c(props_f + 1.96*ses_f, props_e1_0 + 1.96*ses_e1_0, props_e1_1 + 1.96*ses_e1_1, props_e2 + 1.96*ses_e2, props_e3 + 1.96*ses_e3, 
                                props_a1 + 1.96*ses_a1, props_a2 + 1.96*ses_a2, props_a3 + 1.96*ses_a3, props_a4 + 1.96*ses_a4, props_a5 + 1.96*ses_a5), 
                     group = rep(group, 10), 
                    rel.low = rel.low_o1,
                      rel.high = rel.high_o1,
                     rel.bias = rel.biases_o1, 
                     var = c(rep("female", 6), rep("low education", 6), rep("low med education", 6), rep("medium education", 6), rep("high education", 6), 
                               rep("16 to 29 years", 6), rep("30 to 39 years", 6), 
                               rep("40 to 49 years", 6),rep("50 to 59 years", 6), rep("60 years and older", 6)))
#ggdat2$ord <- c(rep(1, 6), rep(2, 6), rep(3, 6), rep(4, 6), rep(5, 6), rep(6, 6), rep(7, 6), rep(8, 6))
ggdat1$var2 <- factor(ggdat1$var, levels = c("female", "low education", "low med education", "medium education", "high education", "16 to 29 years", "30 to 39 years", 
                                             "40 to 49 years", "50 to 59 years","60 years and older"))
ggdat1$group <- factor(ggdat1$group, levels = c(  "bonus 50 Euro", "bonus 20 Euro", "paper-first","online-only", "concurrent","push-to-web" ))


#### Graphiken Incentives
# setwd("W:/Projekte/Rekrutierungsexperimente_2018/bonus_incentives/graphs")
setwd("Z:/Projekte/Rekrutierungsexperimente_2018/bonus_incentives/graphs")

jpeg("inc_part1_bias_female_bw_2.jpg", units="in", width=7, height=5, res=300)
ggplot(subset(ggdat1, ggdat1$group %in% c( "bonus 50 Euro", "bonus 20 Euro", "online-only") & var == "female"), aes(y= (rel.bias ) , x=group, ymin = (rel.low ), ymax = (rel.high)))+
  geom_point(aes(colour=group)) +
  # ggtitle("Relative Gender Bias in Recruitment Stage") +
#  theme(plot.title = element_text(hjust = 0.5))+
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  ylab("% relative bias in proportion of females") + 
  xlab("experimental group") +
  scale_color_manual(values=c("black","black", "black")) +
  geom_hline(yintercept=0, linetype="solid") +
  coord_flip()+
  theme(legend.position="none") +
  geom_pointrange(aes(colour=group, linetype=group)) +
  scale_linetype_manual(values=c("solid", "solid", "solid"))
dev.off()

jpeg("inc_part1_bias_age_bw_2.jpg", units="in", width=7, height=5, res=300)
ggplot(subset(ggdat1, ggdat1$group %in% c( "bonus 50 Euro", "bonus 20 Euro", "online-only") & var %in% c("16 to 29 years", "30 to 39 years",
                                                                                                                     "40 to 49 years",  "50 to 59 years","60 years and older")), aes(y= (rel.bias ) , x=group, ymin = (rel.low ), ymax = (rel.high)))+
  geom_point(aes(colour=group)) +
  # ggtitle("Relative Age Bias in Recruitment Stage") +
#  theme(plot.title = element_text(hjust = 0.5))+
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), strip.background = element_rect(fill="white"))+
  ylab("% relative bias in age categories") + 
  xlab("experimental group") +
  facet_grid(rows = vars(var))+
  scale_color_manual(values=c("black","black", "black")) +
  geom_hline(yintercept=0, linetype="solid") +
  coord_flip()+
  facet_wrap(~ var2, labeller = label_wrap_gen(width=10), ncol=1, strip.position = "right")+
  theme(legend.position="none") +
  geom_pointrange(aes(colour=group, linetype=group)) +
  scale_linetype_manual(values=c("solid", "solid", "solid"))
dev.off()

jpeg("inc_part1_bias_education_bw_2.jpg", units="in", width=7, height=5, res=300)
ggplot(subset(ggdat1, ggdat1$group %in% c( "bonus 50 Euro", "bonus 20 Euro", "online-only") & var %in% c("low education", "low med education", "medium education",
                                                                                                                     "high education")), aes(y= (rel.bias ) , x=group, ymin = (rel.low ), ymax = (rel.high)))+
  geom_point(aes(colour=group)) +
  # ggtitle("Relative Education Bias in Recruitment Stage") +
 # theme(plot.title = element_text(hjust = 0.5))+
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), strip.background = element_rect(fill="white"))+
  ylab("% relative bias in education categories") + 
  xlab("experimental group") +
  facet_grid(rows = vars(var2))+
  scale_color_manual(values=c("black","black", "black", "black")) +
  geom_hline(yintercept=0, linetype="solid") +
  coord_flip()+
  theme(legend.position="none") +
  geom_pointrange(aes(colour=group, linetype=group)) +
  scale_linetype_manual(values=c("solid", "solid", "solid",  "solid"))
dev.off()



###########################################################################################################

#########################################  Teilnahme an online 2, Rekrutierung
###########################################################################################################
############## ab hier auch Nationalit?t und Familienstand
dat2 <- subset(dat, dat$id_l %in% subset(datenbank, datenbank$online2_status %in% c(2))$id_l) # completes

dat2$group <- dat2$hGrp2018

############################################
### Tabellen f?r Paper (Table 4)

prop.table(table(dat2$female, dat2$group), 2)*100
prop.table(table(dat2$age16to29, dat2$group), 2)*100
prop.table(table(dat2$age30to39, dat2$group), 2)*100
prop.table(table(dat2$age40to49, dat2$group), 2)*100
prop.table(table(dat2$age50to59, dat2$group), 2)*100
prop.table(table(dat2$age60plus, dat2$group), 2)*100
prop.table(table(dat2$edu_low, dat2$group), 2)*100
prop.table(table(dat2$edu_mid, dat2$group), 2)*100
prop.table(table(dat2$edu_high, dat2$group), 2)*100


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

### Tabellen f?r Paper (Table 4-Fortsetzung)
prop.table(table(dato2$ledig, dato2$group), 2)*100
prop.table(table(dato2$verheiratet, dato2$group), 2)*100
prop.table(table(dato2$geschieden, dato2$group), 2)*100
prop.table(table(dato2$verwitwet, dato2$group), 2)*100
prop.table(table(dato2$hh1, dato2$group), 2)*100
prop.table(table(dato2$hh2, dato2$group), 2)*100
prop.table(table(dato2$hh3, dato2$group), 2)*100
prop.table(table(dato2$hh4p, dato2$group), 2)*100
prop.table(table(dato2$german, dato2$group), 2)*100
prop.table(table(dato2$other, dato2$group), 2)*100

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

## NAs
by(dat2$female, dat2$group, function(x) sum(is.na(x)))
by(dat2$age16to29, dat2$group, function(x) sum(is.na(x)))
by(dat$edu_low, dat2$group, function(x) sum(is.na(x)))
by(dato2$ledig, dato2$group, function(x) sum(is.na(x)))
by(dato2$hh1, dato2$group, function(x) sum(is.na(x)))
by(dato2$german, dato2$group, function(x) sum(is.na(x)))

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

#### Graphiken Incentives , width=7, height=5
# setwd("W:/Projekte/Rekrutierungsexperimente_2018/bonus_incentives/graphs")
setwd("Z:/Projekte/Rekrutierungsexperimente_2018/bonus_incentives/graphs")

jpeg("inc_online2_bias_female_bw_2.jpg", units="in", width=7, height=5, res=300)
ggplot(subset(ggdat2, ggdat2$group_inc %in% c( "bonus 50 Euro", "bonus 20 Euro", "no bonus") & var == "female"), aes(y= (rel.bias ) , x=group, ymin = (rel.low ), ymax = (rel.high)))+
  geom_point(aes(colour=group_inc)) +
  # ggtitle("Gender Bias in Registration Stage") +
  #  theme(plot.title = element_text(hjust = 0.5))+
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  ylab("% relative bias in proportion of females") + 
  xlab("experimental group") +
  scale_color_manual(values=c("black","black", "black")) +
  geom_hline(yintercept=0, linetype="solid") +
  coord_flip()+
  theme(legend.position="none") +
  geom_pointrange(aes(colour=group_inc, linetype=group_inc)) + 
  scale_linetype_manual(values=c("solid", "solid", "solid"))
dev.off()

jpeg("inc_online2_bias_age_bw_2.jpg", units="in", width=7, height=5, res=300)
ggplot(subset(ggdat2, ggdat2$group_inc %in% c( "bonus 50 Euro", "bonus 20 Euro", "no bonus") & var %in% c("16 to 29 years", "30 to 39 years",
                                                                                                          "40 to 49 years", "50 to 59 years", "60 years and older")), aes(y= (rel.bias ) , x=group, ymin = (rel.low ), ymax = (rel.high)))+
  geom_point(aes(colour=group_inc)) +
  # ggtitle("Age Bias in Registration Stage") +
  #  theme(plot.title = element_text(hjust = 0.5))+
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), strip.background = element_rect(fill="white"))+
  ylab("% relative bias in age categories") + 
  xlab("experimental group") +
  facet_grid(rows = vars(var))+
  scale_color_manual(values=c("black","black", "black")) +
  geom_hline(yintercept=0, linetype="solid") +
  coord_flip()+
  facet_wrap(~ var2, labeller = label_wrap_gen(width=10), ncol=1, strip.position = "right")+
  theme(legend.position="none") +
  geom_pointrange(aes(colour=group_inc, linetype=group_inc)) +
  scale_linetype_manual(values=c("solid", "solid", "solid"))
dev.off()

jpeg("inc_online2_bias_education_bw_2.jpg", units="in", width=7, height=5, res=300)
ggplot(subset(ggdat2, ggdat2$group_inc %in% c( "bonus 50 Euro", "bonus 20 Euro", "no bonus") & var %in% c("low education", "medium education",
                                                                                                          "high education")), aes(y= (rel.bias ) , x=group, ymin = (rel.low ), ymax = (rel.high)))+
  geom_point(aes(colour=group_inc)) +
  # ggtitle("Education Bias in Registration Stage") +
  #  theme(plot.title = element_text(hjust = 0.5))+
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), strip.background = element_rect(fill="white"))+
  ylab("% relative bias in education categories") + 
  xlab("experimental group") +
  facet_grid(rows = vars(var2))+
  scale_color_manual(values=c("black","black", "black")) +
  geom_hline(yintercept=0, linetype="solid") +
  coord_flip()+
  theme(legend.position="none") +
  geom_pointrange(aes(colour=group_inc, linetype=group_inc)) +
  scale_linetype_manual(values=c("solid", "solid", "solid"))
dev.off()

jpeg("inc_online2_bias_female_new_2.jpg", units="in", width=7, height=5, res=300)
ggplot(subset(ggdat2, ggdat2$group_inc %in% c( "bonus 50 Euro", "bonus 20 Euro", "no bonus") & var == "female"), aes(y= (rel.bias ) , x=group, ymin = (rel.low ), ymax = (rel.high)))+
  geom_point(aes(colour=group_inc)) +
  # ggtitle("Gender Bias in Registration Stage") +
  #  theme(plot.title = element_text(hjust = 0.5))+
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  ylab("% relative bias in proportion of females") + 
  xlab("experimental group") +
  scale_color_manual(values=c("black","black", "black")) +
  geom_hline(yintercept=0, linetype="solid") +
  coord_flip()+
  theme(legend.position="none") +
  geom_pointrange(aes(colour=group_inc, linetype=group_inc)) +
  scale_linetype_manual(values=c("solid", "solid", "solid"))
dev.off()

jpeg("inc_online2_bias_age_new_2.jpg", units="in", width=7, height=5, res=300)
ggplot(subset(ggdat2, ggdat2$group_inc %in% c( "bonus 50 Euro", "bonus 20 Euro", "no bonus") & var %in% c("16 to 29 years", "30 to 39 years",
                                                                                                          "40 to 49 years",  "50 to 59 years","60 years and older")), aes(y= (rel.bias ) , x=group, ymin = (rel.low ), ymax = (rel.high)))+
  geom_point(aes(colour=group_inc)) +
  # ggtitle("Age Bias in Registration Stage") +
  #  theme(plot.title = element_text(hjust = 0.5))+
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), strip.background = element_rect(fill="white"))+
  ylab("% relative bias in age categories") + 
  xlab("experimental group") +
  facet_grid(rows = vars(var))+
  scale_color_manual(values=c("black","black", "black")) +
  geom_hline(yintercept=0, linetype="solid") +
  coord_flip()+
  facet_wrap(~ var2, labeller = label_wrap_gen(width=10), ncol=1, strip.position = "right")+
  theme(legend.position="none") +
  geom_pointrange(aes(colour=group_inc, linetype=group_inc)) + 
  scale_linetype_manual(values=c("solid", "solid", "solid"))
dev.off()

jpeg("inc_online2_bias_education_new_2.jpg", units="in", width=7, height=5, res=300)
ggplot(subset(ggdat2, ggdat2$group_inc %in% c( "bonus 50 Euro", "bonus 20 Euro", "no bonus") & var %in% c("low education", "medium education",
                                                                                                          "high education")), aes(y= (rel.bias ) , x=group, ymin = (rel.low ), ymax = (rel.high)))+
  geom_point(aes(colour=group_inc)) +
  # ggtitle("Education Bias in Registration Stage") +
  #  theme(plot.title = element_text(hjust = 0.5))+
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), strip.background = element_rect(fill="white"))+
  ylab("% relative bias in education categories") + 
  xlab("experimental group") +
  facet_grid(rows = vars(var2))+
  scale_color_manual(values=c("black","black", "black")) +
  geom_hline(yintercept=0, linetype="solid") +
  coord_flip()+
  theme(legend.position="none") +
  geom_pointrange(aes(colour=group_inc, linetype=group_inc)) +
  scale_linetype_manual(values=c("solid", "solid", "solid"))
dev.off()

# setwd("W:/Projekte/Rekrutierungsexperimente_2018/Paper_Draft/graphs")
setwd("Z:/Projekte/Rekrutierungsexperimente_2018/Paper_Draft/graphs")

jpeg("ptw_bias_marital_online2_bw_2.jpg", units="in", width=7, height=5, res=300)
ggplot(subset(ggdat2, ggdat2$group %in% c( "push-to-web", "online-only", "paper-first",  "concurrent") & var %in% c("single", "married", "widowed",
                                                                                                                    "divorced")), aes(y= (rel.bias ) , x=group, ymin = (rel.low ), ymax = (rel.high)))+
  geom_point(aes(colour=group)) +
  # ggtitle("Marital Status Bias in Registration Stage") +
  #  theme(plot.title = element_text(hjust = 0.5))+
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), strip.background = element_rect(fill="white"))+
  ylab("% relative bias in marital status") + 
  xlab("experimental group") +
  facet_grid(rows = vars(var2))+
  scale_color_manual(values=c("grey70", "grey40",  "grey70" , "grey40")) +
  geom_hline(yintercept=0, linetype="solid") +
  coord_flip()+
  theme(legend.position="none") +
  geom_pointrange(aes(colour=group, linetype=group)) + 
  scale_linetype_manual(values=c("dashed", "dashed", "solid", "solid"))
dev.off()

jpeg("ptw_bias_german_online2_bw_2.jpg", units="in", width=7, height=5, res=300)
ggplot(subset(ggdat2, ggdat2$group %in% c( "push-to-web", "online-only", "paper-first",  "concurrent") & var == "german"), aes(y= (rel.bias ) , x=group, ymin = (rel.low ), ymax = (rel.high)))+
  geom_point(aes(colour=group)) +
  # ggtitle("Nationality Bias in Registration Stage") +
  #  theme(plot.title = element_text(hjust = 0.5))+
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))+
  ylab("% relative bias in proportion of Germans") + 
  xlab("experimental group") +
  scale_color_manual(values=c("grey70", "grey40",  "grey70" , "grey40")) +
  geom_hline(yintercept=0, linetype="solid") +
  coord_flip()+
  theme(legend.position="none") +
  geom_pointrange(aes(colour=group, linetype=group)) +
  scale_linetype_manual(values=c("dashed", "dashed", "solid", "solid"))
dev.off()

jpeg("ptw_bias_hh_size_online2_bw_2.jpg", units="in", width=7, height=5, res=300)
ggplot(subset(ggdat2, ggdat2$group %in% c( "push-to-web", "online-only", "paper-first",  "concurrent") & var %in% c("4 and more household members", "3 household members", "2 household members", "single household")), aes(y= (rel.bias ) , x=group, ymin = (rel.low ), ymax = (rel.high)))+
  geom_point(aes(colour=group)) +
  # ggtitle("Household Size Bias in Registration Stage") +
  #  theme(plot.title = element_text(hjust = 0.5))+
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), strip.background = element_rect(fill="white"))+
  ylab("% relative bias in household size") + 
  xlab("experimental group") +
  facet_grid(rows = vars(var2))+
  scale_color_manual(values=c("grey70", "grey40",  "grey70" , "grey40")) +
  geom_hline(yintercept=0, linetype="solid") +
  coord_flip()+
  facet_wrap(~ var2, labeller = label_wrap_gen(width=10), ncol=1, strip.position = "right")+
  theme(legend.position="none") +
  geom_pointrange(aes(colour=group, linetype=group)) +
  scale_linetype_manual(values=c("dashed", "dashed", "solid", "solid"))
dev.off()

# setwd("W:/Projekte/Rekrutierungsexperimente_2018/bonus_incentives/graphs")
setwd("Z:/Projekte/Rekrutierungsexperimente_2018/bonus_incentives/graphs")

jpeg("inc_bias_marital_online2_bw_2.jpg", units="in", width=7, height=5, res=300)
ggplot(subset(ggdat2, ggdat2$group_inc %in% c( "bonus 50 Euro", "bonus 20 Euro", "no bonus") & var %in% c("single", "married", "widowed",
                                                                                                                    "divorced")), aes(y= (rel.bias ) , x=group, ymin = (rel.low ), ymax = (rel.high)))+
  geom_point(aes(colour=group_inc)) +
  # ggtitle("Marital Status Bias in Registration Stage") +
  #  theme(plot.title = element_text(hjust = 0.5))+
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), strip.background = element_rect(fill="white"))+
  ylab("% relative bias in marital status") + 
  xlab("experimental group") +
  facet_grid(rows = vars(var2))+
  scale_color_manual(values=c("black","black", "black")) +
  geom_hline(yintercept=0, linetype="solid") +
  coord_flip()+
  theme(legend.position="none") +
  geom_pointrange(aes(colour=group_inc, linetype=group_inc)) +
  scale_linetype_manual(values=c("solid", "solid", "solid"))
dev.off()

jpeg("inc_bias_female_german_online2_bw_2.jpg", units="in", width=7, height=5, res=300)
ggplot(subset(ggdat2, ggdat2$group_inc %in% c( "bonus 50 Euro", "bonus 20 Euro", "no bonus") & var %in% c("female", "german")), aes(y= (rel.bias ) , x=group, ymin = (rel.low ), ymax = (rel.high)))+
  geom_point(aes(colour=group_inc)) +
  # ggtitle("Gender and Nationality Bias in Registration Stage") +
  #  theme(plot.title = element_text(hjust = 0.5))+
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), strip.background = element_rect(fill="white"))+
  ylab("% relative bias in gender and nationality") + 
  xlab("experimental group") +
  facet_grid(rows = vars(var))+
  scale_color_manual(values=c("black","black", "black")) +
  geom_hline(yintercept=0, linetype="solid") +
  coord_flip()+
  theme(legend.position="none") +
  geom_pointrange(aes(colour=group_inc, linetype=group_inc)) +
  scale_linetype_manual(values=c("solid", "solid", "solid"))
dev.off()

jpeg("inc_bias_hh_size_online2_bw_2.jpg", units="in", width=7, height=5, res=300)
ggplot(subset(ggdat2, ggdat2$group_inc %in%c( "bonus 50 Euro", "bonus 20 Euro", "no bonus") & var %in% c("4 and more household members", "3 household members", "2 household members", "single household")), aes(y= (rel.bias ) , x=group, ymin = (rel.low ), ymax = (rel.high)))+
  geom_point(aes(colour=group_inc)) +
  # ggtitle("Household Size Bias in Registration Stage") +
  #  theme(plot.title = element_text(hjust = 0.5))+
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), strip.background = element_rect(fill="white"))+
  ylab("% relative bias in household size") + 
  xlab("experimental group") +
  facet_grid(rows = vars(var2))+
  scale_color_manual(values=c("black","black", "black")) +
  geom_hline(yintercept=0, linetype="solid") +
  coord_flip()+
  facet_wrap(~ var2, labeller = label_wrap_gen(width=10), ncol=1, strip.position = "right")+
  theme(legend.position="none") +
  geom_pointrange(aes(colour=group_inc, linetype=group_inc)) + 
  scale_linetype_manual(values=c("solid", "solid", "solid"))
dev.off()



# Plot der relativen biases
rel.bias.dat_o1 <- data.frame(bias = abs(rel.biases_o1), var = rel.var_o1, group= rep(group, 9), group_inc = rep(group_inc, 9))
rel.bias.dat_o1$group <- factor(rel.bias.dat_o1$group, levels = c(  "bonus 50 Euro", "bonus 20 Euro","push-to-web", "concurrent", "online-only", "paper-first" ))
rel.bias.dat_o2 <- data.frame(bias = abs(rel.biases_o2), var = rel.var_o2, group=rep(group, 18), group_inc = rep(group_inc, 9))
rel.bias.dat_o2$group <- factor(rel.bias.dat_o2$group, levels = c(  "bonus 50 Euro", "bonus 20 Euro","push-to-web", "concurrent", "online-only", "paper-first" ))

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

setwd("Z:/Projekte/Rekrutierungsexperimente_2018/bonus_incentives/graphs")

jpeg("inc_rel_biases_online1_2.jpg", units="in", width=7, height=5, res=300)
ggplot(subset(rel.bias.dat_o1, rel.bias.dat_o1$group_inc %in% c( "bonus 50 Euro", "bonus 20 Euro", "no bonus")), aes(x=group, y=bias, fill =group)) + 
  geom_boxplot() +
  scale_y_continuous(name = "absolute relative bias")+
  xlab("experimental group")+
  # ggtitle("Boxplot of Absolute Relative Biases in Recruitment Stage")+
  # scale_fill_brewer(palette=c("darkgreen", "darkred",  "darkblue" , "grey30"))+
  theme_bw() +
  scale_fill_manual(values=c("grey70", "grey70",  "grey70")) +
  theme(legend.position="none") +
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="black", fill="black") +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
  dev.off()

jpeg("inc_rel_biases_online2_2.jpg", units="in", width=7, height=5, res=300)
ggplot(subset(rel.bias.dat_o2, rel.bias.dat_o2$group_inc %in% c( "bonus 50 Euro", "bonus 20 Euro", "no bonus")), aes(x=group, y=bias, fill=group)) + 
  geom_boxplot()+
  scale_y_continuous(name = "absolute relative bias (%)")+
  xlab("experimental group")+
  # ggtitle("Boxplot of Absolute Relative Biases in Registration Stage")+
  theme_bw() +
  scale_fill_manual(values=c("grey70",  "grey70" , "grey70")) +
  theme(legend.position="none") +
  stat_summary(fun.y=mean, geom="point", shape=20, size=5, color="black", fill="black") +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
  dev.off()

#####################################################################################################

# Incentives Paper: Bias over waves

######################################################################################################

# Teilnahme im Core
core <- read_dta("Y:/Bearbeitung/Datenaufbereitung/W37/Main/V1/W37_GIP_Main_Schritt_1.dta")
core <- subset(core, core$hGIP2 == 3)
core <- data.frame(core)
core <- merge(core, online1[, c("id_l", "hGrp2018")], by.x="id_l", by.y="id_l" )
core <- subset(core, core$id_l %in% datenbank$gip_id)

core1 <- subset(online1, online1$id_l %in% core$id_l) # participants
core2 <- subset(online2, online2$id_l %in% core$id_l)
core1$group <- core1$hGrp2018

stats <- by(core1$female, core1$group, stat.desc)
props_f <- c( unlist(stats[1])[9], unlist(stats[2])[9], unlist(stats[3])[9], unlist(stats[4])[9], unlist(stats[5])[9], unlist(stats[6])[9])-female_mz
rel_f <- props_f/female_mz
ses_f <- c(unlist(stats[1])[10], unlist(stats[2])[10], unlist(stats[3])[10],unlist(stats[4])[10], unlist(stats[5])[10], unlist(stats[6])[10])

stats <- by(core1$edu_low, core1$group, stat.desc)
props_e1 <- c( unlist(stats[1])[9], unlist(stats[2])[9], unlist(stats[3])[9], unlist(stats[4])[9], unlist(stats[5])[9], unlist(stats[6])[9])-edu_low_mz
rel_e1 <- props_e1/edu_low_mz
ses_e1 <- c(unlist(stats[1])[10], unlist(stats[2])[10], unlist(stats[3])[10],unlist(stats[4])[10], unlist(stats[5])[10], unlist(stats[6])[10])

stats <- by(core1$edu_mid, core1$group, stat.desc)
props_e2 <- c( unlist(stats[1])[9], unlist(stats[2])[9], unlist(stats[3])[9], unlist(stats[4])[9], unlist(stats[5])[9], unlist(stats[6])[9])-edu_mid_mz
rel_e2 <- props_e2/edu_mid_mz
ses_e2 <- c(unlist(stats[1])[10], unlist(stats[2])[10], unlist(stats[3])[10],unlist(stats[4])[10], unlist(stats[5])[10], unlist(stats[6])[10])

stats <- by(core1$edu_high,core1$group, stat.desc)
props_e3 <- c( unlist(stats[1])[9], unlist(stats[2])[9], unlist(stats[3])[9], unlist(stats[4])[9], unlist(stats[5])[9], unlist(stats[6])[9])-edu_high_mz
rel_e3 <- props_e3/edu_high_mz
ses_e3 <- c(unlist(stats[1])[10], unlist(stats[2])[10], unlist(stats[3])[10],unlist(stats[4])[10], unlist(stats[5])[10], unlist(stats[6])[10])

stats <- by(core1$age16to29, core1$group, stat.desc)
props_a1 <- c( unlist(stats[1])[9], unlist(stats[2])[9], unlist(stats[3])[9], unlist(stats[4])[9], unlist(stats[5])[9], unlist(stats[6])[9])-age16to29_mz
rel_a1 <- props_a1/age16to29_mz
ses_a1 <- c(unlist(stats[1])[10], unlist(stats[2])[10], unlist(stats[3])[10],unlist(stats[4])[10], unlist(stats[5])[10], unlist(stats[6])[10])

stats <- by(core1$age30to39,core1$group, stat.desc)
props_a2 <- c( unlist(stats[1])[9], unlist(stats[2])[9], unlist(stats[3])[9], unlist(stats[4])[9], unlist(stats[5])[9], unlist(stats[6])[9])-age30to39_mz
rel_a2 <- props_a2/age30to39_mz
ses_a2 <- c(unlist(stats[1])[10], unlist(stats[2])[10], unlist(stats[3])[10],unlist(stats[4])[10], unlist(stats[5])[10], unlist(stats[6])[10])

stats <- by(core1$age40to49, core1$group, stat.desc)
props_a3 <- c( unlist(stats[1])[9], unlist(stats[2])[9], unlist(stats[3])[9], unlist(stats[4])[9], unlist(stats[5])[9], unlist(stats[6])[9])-age40to49_mz
rel_a3 <- props_a3/age40to49_mz
ses_a3 <- c(unlist(stats[1])[10], unlist(stats[2])[10], unlist(stats[3])[10],unlist(stats[4])[10], unlist(stats[5])[10], unlist(stats[6])[10])

stats <- by(core1$age50to59, core1$group, stat.desc)
props_a4 <- c( unlist(stats[1])[9], unlist(stats[2])[9], unlist(stats[3])[9], unlist(stats[4])[9], unlist(stats[5])[9], unlist(stats[6])[9])-age50to59_mz
rel_a4 <- props_a4/age50to59_mz
ses_a4 <- c(unlist(stats[1])[10], unlist(stats[2])[10], unlist(stats[3])[10],unlist(stats[4])[10], unlist(stats[5])[10], unlist(stats[6])[10])

stats <- by(core1$age60plus, core1$group, stat.desc)
props_a5 <- c( unlist(stats[1])[9], unlist(stats[2])[9], unlist(stats[3])[9], unlist(stats[4])[9], unlist(stats[5])[9], unlist(stats[6])[9])-age60plus_mz
rel_a5 <- props_a5/age60plus_mz
ses_a5 <- c(unlist(stats[1])[10], unlist(stats[2])[10], unlist(stats[3])[10],unlist(stats[4])[10], unlist(stats[5])[10], unlist(stats[6])[10])

core2 <- merge(core2, core1[, c("id_l", "group")], by.x="id_l", by.y="id_l")

stats <- by(core2$ledig, core2$group, stat.desc)
props_m1 <- c( unlist(stats[1])[9], unlist(stats[2])[9], unlist(stats[3])[9], unlist(stats[4])[9], unlist(stats[5])[9], unlist(stats[6])[9])-ledig_mz
rel_m1 <- props_m1/ledig_mz
ses_m1 <- c(unlist(stats[1])[10], unlist(stats[2])[10], unlist(stats[3])[10],unlist(stats[4])[10], unlist(stats[5])[10], unlist(stats[6])[10])

stats <- by(core2$verheiratet,core2$group, stat.desc)
props_m2 <- c( unlist(stats[1])[9], unlist(stats[2])[9], unlist(stats[3])[9], unlist(stats[4])[9], unlist(stats[5])[9], unlist(stats[6])[9])-verheiratet_mz
rel_m2 <- props_m2/verheiratet_mz
ses_m2 <- c(unlist(stats[1])[10], unlist(stats[2])[10], unlist(stats[3])[10],unlist(stats[4])[10], unlist(stats[5])[10], unlist(stats[6])[10])

stats <- by(core2$geschieden, core2$group, stat.desc)
props_m3 <- c( unlist(stats[1])[9], unlist(stats[2])[9], unlist(stats[3])[9], unlist(stats[4])[9], unlist(stats[5])[9], unlist(stats[6])[9])-geschieden_mz
rel_m3 <- props_m3/geschieden_mz
ses_m3 <- c(unlist(stats[1])[10], unlist(stats[2])[10], unlist(stats[3])[10],unlist(stats[4])[10], unlist(stats[5])[10], unlist(stats[6])[10])

stats <- by(core2$verwitwet, core2$group, stat.desc)
props_m4 <- c( unlist(stats[1])[9], unlist(stats[2])[9], unlist(stats[3])[9], unlist(stats[4])[9], unlist(stats[5])[9], unlist(stats[6])[9])-verwitwet_mz
rel_m4 <- props_m4/verwitwet_mz
ses_m4 <- c(unlist(stats[1])[10], unlist(stats[2])[10], unlist(stats[3])[10],unlist(stats[4])[10], unlist(stats[5])[10], unlist(stats[6])[10])

stats <- by(core2$german, core2$group, stat.desc)
props_g <- c( unlist(stats[1])[9], unlist(stats[2])[9], unlist(stats[3])[9], unlist(stats[4])[9], unlist(stats[5])[9], unlist(stats[6])[9])-german_mz
rel_g <- props_g/german_mz
ses_g <- c(unlist(stats[1])[10], unlist(stats[2])[10], unlist(stats[3])[10],unlist(stats[4])[10], unlist(stats[5])[10], unlist(stats[6])[10])

stats <- by(core2$hh1, core2$group, stat.desc)
props_h1 <- c( unlist(stats[1])[9], unlist(stats[2])[9], unlist(stats[3])[9], unlist(stats[4])[9], unlist(stats[5])[9], unlist(stats[6])[9])-hh1_mz
rel_h1 <- props_h1/hh1_mz
ses_h1 <- c(unlist(stats[1])[10], unlist(stats[2])[10], unlist(stats[3])[10],unlist(stats[4])[10], unlist(stats[5])[10], unlist(stats[6])[10])

stats <- by(core2$hh2, core2$group, stat.desc)
props_h2 <- c( unlist(stats[1])[9], unlist(stats[2])[9], unlist(stats[3])[9], unlist(stats[4])[9], unlist(stats[5])[9], unlist(stats[6])[9])-hh2_mz
rel_h2 <- props_h2/hh2_mz
ses_h2 <- c(unlist(stats[1])[10], unlist(stats[2])[10], unlist(stats[3])[10],unlist(stats[4])[10], unlist(stats[5])[10], unlist(stats[6])[10])

stats <- by(core2$hh3, core2$group, stat.desc)
props_h3 <- c( unlist(stats[1])[9], unlist(stats[2])[9], unlist(stats[3])[9], unlist(stats[4])[9], unlist(stats[5])[9], unlist(stats[6])[9])-hh3_mz
rel_h3 <- props_h3/hh3_mz
ses_h3 <- c(unlist(stats[1])[10], unlist(stats[2])[10], unlist(stats[3])[10],unlist(stats[4])[10], unlist(stats[5])[10], unlist(stats[6])[10])

stats <- by(core2$hh4, core2$group, stat.desc)
props_h4 <- c( unlist(stats[1])[9], unlist(stats[2])[9], unlist(stats[3])[9], unlist(stats[4])[9], unlist(stats[5])[9], unlist(stats[6])[9])-hh4_mz
rel_h4 <- props_h4/hh4_mz
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

rel.biases_core  <- c(rel_f, rel_e1, rel_e2, rel_e3, rel_a1, rel_a2, rel_a3, rel_a4, rel_a5, rel_m1, rel_m2, rel_m3, rel_m4, 
                    rel_g, rel_h1, rel_h2, rel_h3, rel_h4)*100
rel.sds_core <- c(rel.ses.f, rel.ses.e1, rel.ses.e2, rel.ses.e3,rel.ses.a1, rel.ses.a2, rel.ses.a3, rel.ses.a4, rel.ses.a5,
                rel.ses.m1, rel.ses.m2, rel.ses.m3, rel.ses.m4, rel.ses.g, rel.ses.h1, rel.ses.h2, rel.ses.h3, rel.ses.h4)
rel.low_core <- rel.biases_core - 1.96*rel.sds_core
rel.high_core <- rel.biases_core + 1.96*rel.sds_core

ggdat_core <- data.frame(props = c(props_f,props_e1, props_e2, props_e3, props_a1, props_a2, props_a3, props_a4, props_a5, props_m1, props_m2, props_m3, props_m4, props_g, props_h1, props_h2, props_h3, props_h4), 
                     lows = c(props_f - 1.96*ses_f, props_e1 - 1.96*ses_e1, props_e2 - 1.96*ses_e2, props_e3 - 1.96*ses_e3, 
                              props_a1 - 1.96*ses_a1, props_a2 - 1.96*ses_a2, props_a3 - 1.96*ses_a3, props_a4 - 1.96*ses_a4, props_a5 - 1.96*ses_a5, props_m1 - 1.96*ses_m1,  props_m2 - 1.96*ses_m2, props_m3 - 1.96*ses_m3, 
                              props_m4 - 1.96*ses_m4, props_g - 1.96*ses_g, props_h1 - 1.96*ses_h1,  props_h2 - 1.96*ses_h2, props_h3 - 1.96*ses_h3, 
                              props_h4 - 1.96*ses_h4), 
                     highs = c(props_f + 1.96*ses_f, props_e1 + 1.96*ses_e1, props_e2 + 1.96*ses_e2, props_e3 + 1.96*ses_e3, 
                               props_a1 + 1.96*ses_a1, props_a2 + 1.96*ses_a2, props_a3 + 1.96*ses_a3, props_a4 + 1.96*ses_a4, props_a5 + 1.96*ses_a5,props_m1 + 1.96*ses_m1, props_m2 + 1.96*ses_m2, props_m3 + 1.96*ses_m3, 
                               props_m4 + 1.96*ses_m4, props_g + 1.96*ses_g, props_h1 + 1.96*ses_h1, props_h2 + 1.96*ses_h2, props_h3 + 1.96*ses_h3, 
                               props_h4 + 1.96*ses_h4), 
                     group = rep(group, 18), 
                     group_inc = rep(group_inc, 18),
                     rel.low = rel.low_core,
                     rel.high = rel.high_core,
                     rel.bias = rel.biases_core, 
                     var = c(rep("female", 6), rep("low education", 6), rep("medium education", 6), rep("high education", 6), 
                             rep("16 to 29 years", 6), rep("30 to 39 years", 6), 
                             rep("40 to 49 years", 6),  rep("50 to 59 years", 6), rep("60 years and older", 6), rep("single", 6), rep("married", 6), rep("divorced", 6), rep("widowed", 6), 
                             rep("german", 6), rep("single household", 6),  rep("2 household members", 6),  rep("3 household members", 6),  rep("4 and more household members", 6)))

ggdat_core$var2 <- factor(ggdat_core$var, levels = c("female", "low education", "medium education", "high education", "16 to 29 years", "30 to 39 years", 
                                             "40 to 49 years", "50 to 59 years","60 years and older", "4 and more household members", "3 household members", "2 household members", "single household",  "married", "widowed", "divorced", "single"))
ggdat_core$group <- factor(ggdat_core$group, levels = c(  "bonus 50 Euro", "bonus 20 Euro", "paper-first","online-only", "concurrent","push-to-web" ))
ggdat_core$group_inc <- factor(ggdat_core$group_inc, levels = c(  "bonus 50 Euro", "bonus 20 Euro", "paper-first","no bonus", "concurrent","push-to-web" ))



rel.biases_core  <- c(rel_f, rel_e1, rel_e2, rel_e3, rel_a1, rel_a2, rel_a3, rel_a4, rel_a5, rel_m1, rel_m2, rel_m3, rel_m4, 
                    rel_g, rel_h1, rel_h2, rel_h3, rel_h4)*100
rel.var_core = c(rep("female", 6), rep("low education", 6), rep("medium education", 6), rep("high education", 6), 
               rep("16 to 29 years", 6), rep("30 to 39 years", 6), 
               rep("40 to 49 years", 6),  rep("50 to 59 years", 6), rep("60 years and older", 6),rep("female", 6), rep("single", 6), rep("married", 6), rep("divorced", 6), rep("widowed", 6), 
               rep("german", 6), rep("single household", 6),  rep("2 household members", 6),  rep("3 household members", 6),  rep("4 and more household members", 6))


############################# Welle nach dem Core: Personen wurden entweder schon zur Welle 38 eingeladen oder erst 39

aktivitaten <- read_dta("Y:/Bearbeitung/Panel_Management/Aktivitaeten_Protokoll/GIP-1_Archiv-Stata-Files-Aktivitaetenprotokolle/2019-04-11-status.dta")
aktivitaten <- subset(aktivitaten, aktivitaten$GIP1GIP2GIP3 == "GIP3")
aktivitaten <- subset(aktivitaten, aktivitaten$id %in% c(datenbank$gip_id))
aktivitaten$w38_invited <- ifelse(aktivitaten$Welle38Status6811 %in% c("COMPLETED", "wurde eingeladen", "TIMED OUT"), 1, 0)
aktivitaten$w39_invited <- ifelse(aktivitaten$Welle39Status6901 %in% c("COMPLETED", "wurde eingeladen", "TIMED OUT"), 1, 0)
aktivitaten$w40_invited <- ifelse(aktivitaten$Welle40Status6903 %in% c("COMPLETED", "wurde eingeladen", "TIMED OUT"), 1, 0)

aktivitaten$w37_part <- ifelse(aktivitaten$Welle37Status6809 %in% c("COMPLETED", "TIMED OUT"), 1, 0)
aktivitaten$w38_part <- ifelse(aktivitaten$Welle38Status6811 %in% c("COMPLETED", "TIMED OUT"), 1, 0)
aktivitaten$w39_part <- ifelse(aktivitaten$Welle39Status6901 %in% c("COMPLETED",  "TIMED OUT"), 1, 0)
aktivitaten$w40_part <- ifelse(aktivitaten$Welle40Status6903 %in% c("COMPLETED", "TIMED OUT"), 1, 0)

aktivitaten$first_wave <- ifelse(aktivitaten$w39_invited == 1 & aktivitaten$w38_invited == 0, "w39", 
                                 ifelse(aktivitaten$w40_invited == 1 & aktivitaten$w39_invited == 0, "w40", "w38"))
aktivitaten$first_wave_part <- ifelse((aktivitaten$first_wave == "w38" & aktivitaten$w38_part == 1) | 
                                        (aktivitaten$first_wave == "w39" & aktivitaten$w39_part == 1) | 
                                        (aktivitaten$first_wave == "w40" & aktivitaten$w40_part == 1), 1, 0)

first_wave1 <- subset(online1, online1$id_l %in% subset(aktivitaten, aktivitaten$first_wave_part == 1)$id) 
first_wave2 <- subset(online2, online2$id_l %in% subset(aktivitaten, aktivitaten$first_wave_part == 1)$id)
first_wave1$group <- first_wave1$hGrp2018

stats <- by(first_wave1$female, first_wave1$group, stat.desc)
props_f <- c( unlist(stats[1])[9], unlist(stats[2])[9], unlist(stats[3])[9], unlist(stats[4])[9], unlist(stats[5])[9], unlist(stats[6])[9])-female_mz
rel_f <- props_f/female_mz
ses_f <- c(unlist(stats[1])[10], unlist(stats[2])[10], unlist(stats[3])[10],unlist(stats[4])[10], unlist(stats[5])[10], unlist(stats[6])[10])

stats <- by(first_wave1$edu_low, first_wave1$group, stat.desc)
props_e1 <- c( unlist(stats[1])[9], unlist(stats[2])[9], unlist(stats[3])[9], unlist(stats[4])[9], unlist(stats[5])[9], unlist(stats[6])[9])-edu_low_mz
rel_e1 <- props_e1/edu_low_mz
ses_e1 <- c(unlist(stats[1])[10], unlist(stats[2])[10], unlist(stats[3])[10],unlist(stats[4])[10], unlist(stats[5])[10], unlist(stats[6])[10])

stats <- by(first_wave1$edu_mid, first_wave1$group, stat.desc)
props_e2 <- c( unlist(stats[1])[9], unlist(stats[2])[9], unlist(stats[3])[9], unlist(stats[4])[9], unlist(stats[5])[9], unlist(stats[6])[9])-edu_mid_mz
rel_e2 <- props_e2/edu_mid_mz
ses_e2 <- c(unlist(stats[1])[10], unlist(stats[2])[10], unlist(stats[3])[10],unlist(stats[4])[10], unlist(stats[5])[10], unlist(stats[6])[10])

stats <- by(first_wave1$edu_high,first_wave1$group, stat.desc)
props_e3 <- c( unlist(stats[1])[9], unlist(stats[2])[9], unlist(stats[3])[9], unlist(stats[4])[9], unlist(stats[5])[9], unlist(stats[6])[9])-edu_high_mz
rel_e3 <- props_e3/edu_high_mz
ses_e3 <- c(unlist(stats[1])[10], unlist(stats[2])[10], unlist(stats[3])[10],unlist(stats[4])[10], unlist(stats[5])[10], unlist(stats[6])[10])

stats <- by(first_wave1$age16to29, first_wave1$group, stat.desc)
props_a1 <- c( unlist(stats[1])[9], unlist(stats[2])[9], unlist(stats[3])[9], unlist(stats[4])[9], unlist(stats[5])[9], unlist(stats[6])[9])-age16to29_mz
rel_a1 <- props_a1/age16to29_mz
ses_a1 <- c(unlist(stats[1])[10], unlist(stats[2])[10], unlist(stats[3])[10],unlist(stats[4])[10], unlist(stats[5])[10], unlist(stats[6])[10])

stats <- by(first_wave1$age30to39,first_wave1$group, stat.desc)
props_a2 <- c( unlist(stats[1])[9], unlist(stats[2])[9], unlist(stats[3])[9], unlist(stats[4])[9], unlist(stats[5])[9], unlist(stats[6])[9])-age30to39_mz
rel_a2 <- props_a2/age30to39_mz
ses_a2 <- c(unlist(stats[1])[10], unlist(stats[2])[10], unlist(stats[3])[10],unlist(stats[4])[10], unlist(stats[5])[10], unlist(stats[6])[10])

stats <- by(first_wave1$age40to49, first_wave1$group, stat.desc)
props_a3 <- c( unlist(stats[1])[9], unlist(stats[2])[9], unlist(stats[3])[9], unlist(stats[4])[9], unlist(stats[5])[9], unlist(stats[6])[9])-age40to49_mz
rel_a3 <- props_a3/age40to49_mz
ses_a3 <- c(unlist(stats[1])[10], unlist(stats[2])[10], unlist(stats[3])[10],unlist(stats[4])[10], unlist(stats[5])[10], unlist(stats[6])[10])

stats <- by(first_wave1$age50to59, first_wave1$group, stat.desc)
props_a4 <- c( unlist(stats[1])[9], unlist(stats[2])[9], unlist(stats[3])[9], unlist(stats[4])[9], unlist(stats[5])[9], unlist(stats[6])[9])-age50to59_mz
rel_a4 <- props_a4/age50to59_mz
ses_a4 <- c(unlist(stats[1])[10], unlist(stats[2])[10], unlist(stats[3])[10],unlist(stats[4])[10], unlist(stats[5])[10], unlist(stats[6])[10])

stats <- by(first_wave1$age60plus, first_wave1$group, stat.desc)
props_a5 <- c( unlist(stats[1])[9], unlist(stats[2])[9], unlist(stats[3])[9], unlist(stats[4])[9], unlist(stats[5])[9], unlist(stats[6])[9])-age60plus_mz
rel_a5 <- props_a5/age60plus_mz
ses_a5 <- c(unlist(stats[1])[10], unlist(stats[2])[10], unlist(stats[3])[10],unlist(stats[4])[10], unlist(stats[5])[10], unlist(stats[6])[10])

first_wave2 <- merge(first_wave2, first_wave1[, c("id_l", "group")], by.x="id_l", by.y="id_l")

stats <- by(first_wave2$ledig, first_wave2$group, stat.desc)
props_m1 <- c( unlist(stats[1])[9], unlist(stats[2])[9], unlist(stats[3])[9], unlist(stats[4])[9], unlist(stats[5])[9], unlist(stats[6])[9])-ledig_mz
rel_m1 <- props_m1/ledig_mz
ses_m1 <- c(unlist(stats[1])[10], unlist(stats[2])[10], unlist(stats[3])[10],unlist(stats[4])[10], unlist(stats[5])[10], unlist(stats[6])[10])

stats <- by(first_wave2$verheiratet,first_wave2$group, stat.desc)
props_m2 <- c( unlist(stats[1])[9], unlist(stats[2])[9], unlist(stats[3])[9], unlist(stats[4])[9], unlist(stats[5])[9], unlist(stats[6])[9])-verheiratet_mz
rel_m2 <- props_m2/verheiratet_mz
ses_m2 <- c(unlist(stats[1])[10], unlist(stats[2])[10], unlist(stats[3])[10],unlist(stats[4])[10], unlist(stats[5])[10], unlist(stats[6])[10])

stats <- by(first_wave2$geschieden, first_wave2$group, stat.desc)
props_m3 <- c( unlist(stats[1])[9], unlist(stats[2])[9], unlist(stats[3])[9], unlist(stats[4])[9], unlist(stats[5])[9], unlist(stats[6])[9])-geschieden_mz
rel_m3 <- props_m3/geschieden_mz
ses_m3 <- c(unlist(stats[1])[10], unlist(stats[2])[10], unlist(stats[3])[10],unlist(stats[4])[10], unlist(stats[5])[10], unlist(stats[6])[10])

stats <- by(first_wave2$verwitwet, first_wave2$group, stat.desc)
props_m4 <- c( unlist(stats[1])[9], unlist(stats[2])[9], unlist(stats[3])[9], unlist(stats[4])[9], unlist(stats[5])[9], unlist(stats[6])[9])-verwitwet_mz
rel_m4 <- props_m4/verwitwet_mz
ses_m4 <- c(unlist(stats[1])[10], unlist(stats[2])[10], unlist(stats[3])[10],unlist(stats[4])[10], unlist(stats[5])[10], unlist(stats[6])[10])

stats <- by(first_wave2$german, first_wave2$group, stat.desc)
props_g <- c( unlist(stats[1])[9], unlist(stats[2])[9], unlist(stats[3])[9], unlist(stats[4])[9], unlist(stats[5])[9], unlist(stats[6])[9])-german_mz
rel_g <- props_g/german_mz
ses_g <- c(unlist(stats[1])[10], unlist(stats[2])[10], unlist(stats[3])[10],unlist(stats[4])[10], unlist(stats[5])[10], unlist(stats[6])[10])

stats <- by(first_wave2$hh1, first_wave2$group, stat.desc)
props_h1 <- c( unlist(stats[1])[9], unlist(stats[2])[9], unlist(stats[3])[9], unlist(stats[4])[9], unlist(stats[5])[9], unlist(stats[6])[9])-hh1_mz
rel_h1 <- props_h1/hh1_mz
ses_h1 <- c(unlist(stats[1])[10], unlist(stats[2])[10], unlist(stats[3])[10],unlist(stats[4])[10], unlist(stats[5])[10], unlist(stats[6])[10])

stats <- by(first_wave2$hh2, first_wave2$group, stat.desc)
props_h2 <- c( unlist(stats[1])[9], unlist(stats[2])[9], unlist(stats[3])[9], unlist(stats[4])[9], unlist(stats[5])[9], unlist(stats[6])[9])-hh2_mz
rel_h2 <- props_h2/hh2_mz
ses_h2 <- c(unlist(stats[1])[10], unlist(stats[2])[10], unlist(stats[3])[10],unlist(stats[4])[10], unlist(stats[5])[10], unlist(stats[6])[10])

stats <- by(first_wave2$hh3, first_wave2$group, stat.desc)
props_h3 <- c( unlist(stats[1])[9], unlist(stats[2])[9], unlist(stats[3])[9], unlist(stats[4])[9], unlist(stats[5])[9], unlist(stats[6])[9])-hh3_mz
rel_h3 <- props_h3/hh3_mz
ses_h3 <- c(unlist(stats[1])[10], unlist(stats[2])[10], unlist(stats[3])[10],unlist(stats[4])[10], unlist(stats[5])[10], unlist(stats[6])[10])

stats <- by(first_wave2$hh4, first_wave2$group, stat.desc)
props_h4 <- c( unlist(stats[1])[9], unlist(stats[2])[9], unlist(stats[3])[9], unlist(stats[4])[9], unlist(stats[5])[9], unlist(stats[6])[9])-hh4_mz
rel_h4 <- props_h4/hh4_mz
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

rel.biases_first  <- c(rel_f, rel_e1, rel_e2, rel_e3, rel_a1, rel_a2, rel_a3, rel_a4, rel_a5, rel_m1, rel_m2, rel_m3, rel_m4, 
                    rel_g, rel_h1, rel_h2, rel_h3, rel_h4)*100
rel.sds_first <- c(rel.ses.f, rel.ses.e1, rel.ses.e2, rel.ses.e3,rel.ses.a1, rel.ses.a2, rel.ses.a3, rel.ses.a4, rel.ses.a5,
                rel.ses.m1, rel.ses.m2, rel.ses.m3, rel.ses.m4, rel.ses.g, rel.ses.h1, rel.ses.h2, rel.ses.h3, rel.ses.h4)
rel.low_first <- rel.biases_first - 1.96*rel.sds_first
rel.high_first <- rel.biases_first + 1.96*rel.sds_first

rel.biases_first_wave  <- c(rel_f, rel_e1, rel_e2, rel_e3, rel_a1, rel_a2, rel_a3, rel_a4, rel_a5, rel_m1, rel_m2, rel_m3, rel_m4, 
                            rel_g, rel_h1, rel_h2, rel_h3, rel_h4)*100
rel.var_first_wave = c(rep("female", 6), rep("low education", 6), rep("medium education", 6), rep("high education", 6), 
                       rep("16 to 29 years", 6), rep("30 to 39 years", 6), 
                       rep("40 to 49 years", 6),  rep("50 to 59 years", 6), rep("60 years and older", 6),rep("female", 6), rep("single", 6), rep("married", 6), rep("divorced", 6), rep("widowed", 6), 
                       rep("german", 6), rep("single household", 6),  rep("2 household members", 6),  rep("3 household members", 6),  rep("4 and more household members", 6))


ggdat_first_wave <- data.frame(props = c(props_f,props_e1, props_e2, props_e3, props_a1, props_a2, props_a3, props_a4, props_a5, props_m1, props_m2, props_m3, props_m4, props_g, props_h1, props_h2, props_h3, props_h4), 
                         lows = c(props_f - 1.96*ses_f, props_e1 - 1.96*ses_e1, props_e2 - 1.96*ses_e2, props_e3 - 1.96*ses_e3, 
                                  props_a1 - 1.96*ses_a1, props_a2 - 1.96*ses_a2, props_a3 - 1.96*ses_a3, props_a4 - 1.96*ses_a4, props_a5 - 1.96*ses_a5, props_m1 - 1.96*ses_m1,  props_m2 - 1.96*ses_m2, props_m3 - 1.96*ses_m3, 
                                  props_m4 - 1.96*ses_m4, props_g - 1.96*ses_g, props_h1 - 1.96*ses_h1,  props_h2 - 1.96*ses_h2, props_h3 - 1.96*ses_h3, 
                                  props_h4 - 1.96*ses_h4), 
                         highs = c(props_f + 1.96*ses_f, props_e1 + 1.96*ses_e1, props_e2 + 1.96*ses_e2, props_e3 + 1.96*ses_e3, 
                                   props_a1 + 1.96*ses_a1, props_a2 + 1.96*ses_a2, props_a3 + 1.96*ses_a3, props_a4 + 1.96*ses_a4, props_a5 + 1.96*ses_a5,props_m1 + 1.96*ses_m1, props_m2 + 1.96*ses_m2, props_m3 + 1.96*ses_m3, 
                                   props_m4 + 1.96*ses_m4, props_g + 1.96*ses_g, props_h1 + 1.96*ses_h1, props_h2 + 1.96*ses_h2, props_h3 + 1.96*ses_h3, 
                                   props_h4 + 1.96*ses_h4), 
                         rel.low = rel.low_first,
                         rel.high = rel.high_first,
                         rel.bias = rel.biases_first, 
                         group = rep(group, 18), 
                         group_inc = rep(group_inc, 18),
                         var = c(rep("female", 6), rep("low education", 6), rep("medium education", 6), rep("high education", 6), 
                                 rep("16 to 29 years", 6), rep("30 to 39 years", 6), 
                                 rep("40 to 49 years", 6),  rep("50 to 59 years", 6), rep("60 years and older", 6), rep("single", 6), rep("married", 6), rep("divorced", 6), rep("widowed", 6), 
                                 rep("german", 6), rep("single household", 6),  rep("2 household members", 6),  rep("3 household members", 6),  rep("4 and more household members", 6)))

ggdat_first_wave$var2 <- factor(ggdat_first_wave $var, levels = c("female", "low education", "medium education", "high education", "16 to 29 years", "30 to 39 years", 
                                                     "40 to 49 years", "50 to 59 years","60 years and older", "4 and more household members", "3 household members", "2 household members", "single household",  "married", "widowed", "divorced", "single"))
ggdat_first_wave$group <- factor(ggdat_first_wave $group, levels = c(  "bonus 50 Euro", "bonus 20 Euro", "paper-first","online-only", "concurrent","push-to-web" ))
ggdat_first_wave$group_inc <- factor(ggdat_first_wave $group_inc, levels = c(  "bonus 50 Euro", "bonus 20 Euro", "paper-first","no bonus", "concurrent","push-to-web" ))

ggdat2$wave <- "registration"
ggdat2$rel_bias <- abs(rel.biases_o2)
ggdat_core$wave <- "first wave"
ggdat_core$rel_bias <- abs(rel.biases_core)
ggdat_first_wave$wave <- "second wave"
ggdat_first_wave$rel_bias <- abs(rel.biases_first_wave)

plot_dat <- rbind(ggdat2, ggdat_core, ggdat_first_wave)
plot_dat$wave <- factor(plot_dat$wave, levels = c( "registration", "first wave", "second wave"))

####################################################################
# biases im Zeitverlauf
# setwd("W:/Projekte/Rekrutierungsexperimente_2018/bonus_incentives/graphs")
setwd("Z:/Projekte/Rekrutierungsexperimente_2018/bonus_incentives/graphs")

pdf("biases_over_time.pdf")
ggplot(subset(plot_dat, plot_dat$group_inc %in% c( "bonus 50 Euro", "bonus 20 Euro", "no bonus") & var %in% c("4 and more household members", "3 household members", "2 household members", "single household")), aes(y= (rel.bias ) , x=group, ymin = (rel.low ), ymax = (rel.high)))+
  geom_point(aes(colour=group_inc), size=3.5) +
  # ggtitle("Household Size Bias") +
  #  theme(plot.title = element_text(hjust = 0.5))+
  theme_bw() +
  theme(panel.border=element_blank(), plot.title = element_text(hjust = 0.5)) +
  ylab("% relative bias in household size") + 
  xlab("experimental group") +
  facet_grid(rows = vars(var2))+
  scale_color_manual(values=c("lightblue","darkblue", "black")) +
  geom_hline(yintercept=0, linetype="dashed") +
#  coord_flip()+
  facet_wrap(~ var2, labeller = label_wrap_gen(width=10), ncol=1, strip.position = "right")+
  theme(legend.position="bottom") +
  theme(legend.title=element_blank())+
  geom_pointrange(aes(colour=group_inc)) 

ggplot(subset(plot_dat, plot_dat$group_inc %in% c( "bonus 50 Euro", "bonus 20 Euro", "no bonus") & var == "german"), aes(y= (rel.bias ) , x=group, ymin = (rel.low ), ymax = (rel.high)))+
  geom_point(aes(colour=group_inc)) +
  # ggtitle("Nationality Bias in Registration Stage") +
  #  theme(plot.title = element_text(hjust = 0.5))+
  theme_bw() +
  theme(panel.border=element_blank(), plot.title = element_text(hjust = 0.5)) +
  ylab("% relative bias in proportion of Germans") + 
  xlab("experimental group") +
  scale_color_manual(values=c("lightblue","darkblue", "black")) +
  geom_hline(yintercept=0, linetype="solid") +
  coord_flip()+
  theme(legend.position="none") +
  geom_pointrange(aes(colour=group_inc))

ggplot(subset(plot_dat, plot_dat$group_inc %in% c( "bonus 50 Euro", "bonus 20 Euro", "no bonus") & var == "female"), aes(y= (rel.bias ) , x=group, ymin = (rel.low ), ymax = (rel.high)))+
  geom_point(aes(colour=group_inc)) +
  # ggtitle("Gender Bias in Registration Stage") +
  #  theme(plot.title = element_text(hjust = 0.5))+
  theme_bw() +
  theme(panel.border=element_blank(), plot.title = element_text(hjust = 0.5)) +
  ylab("% relative gender bias") + 
  xlab("experimental group") +
  scale_color_manual(values=c("lightblue","darkblue", "black")) +
  geom_hline(yintercept=0, linetype="solid") +
  coord_flip()+
  theme(legend.position="none") +
  geom_pointrange(aes(colour=group_inc))

ggplot(subset(plot_dat, plot_dat$group_inc %in% c( "bonus 50 Euro", "bonus 20 Euro", "no bonus") & var %in% c("single", "married", "widowed",
                                                                                                              "divorced")), aes(y= (rel.bias ) , x=group, ymin = (rel.low ), ymax = (rel.high)))+
  geom_point(aes(colour=group_inc), size=3.5) +
  # ggtitle("Marital Status Bias") +
  #  theme(plot.title = element_text(hjust = 0.5))+
  theme_bw() +
  theme(panel.border=element_blank(), plot.title = element_text(hjust = 0.5)) +
  ylab("% relative bias in marital status") + 
  xlab("experimental group") +
  facet_grid(rows = vars(var))+
  scale_color_manual(values=c("lightblue","darkblue", "black")) +
  geom_hline(yintercept=0, linetype="dashed") +
#  coord_flip()+
  facet_wrap(~ var2, labeller = label_wrap_gen(width=10), ncol=1, strip.position = "right")+
  theme(legend.position="bottom") +
  theme(legend.title=element_blank())+
  geom_pointrange(aes(colour=group_inc)) 

ggplot(subset(plot_dat, plot_dat$group_inc %in% c( "bonus 50 Euro", "bonus 20 Euro", "no bonus") & var %in%c("16 to 29 years", "30 to 39 years",
                                                                                                             "40 to 49 years",  "50 to 59 years","60 years and older")), aes(y= (rel.bias ) , x=group, ymin = (rel.low ), ymax = (rel.high)))+
  geom_point(aes(colour=group_inc), size=3.5) +
  # ggtitle("Age Bias") +
  #  theme(plot.title = element_text(hjust = 0.5))+
  theme_bw() +
  theme(panel.border=element_blank(), plot.title = element_text(hjust = 0.5)) +
  ylab("% relative bias in age categories") + 
  xlab("experimental group") +
  facet_grid(rows = vars(var))+
  scale_color_manual(values=c("lightblue","darkblue", "black")) +
  geom_hline(yintercept=0, linetype="dashed") +
 # coord_flip()+
  facet_wrap(~ var2, labeller = label_wrap_gen(width=10), ncol=1, strip.position = "right")+
  theme(legend.position="bottom") +
  theme(legend.title=element_blank())+
  geom_pointrange(aes(colour=group_inc)) 

ggplot(subset(plot_dat, plot_dat$group_inc %in% c( "bonus 50 Euro", "bonus 20 Euro", "no bonus") & var %in% c("low education", "medium education",
                                                                                                              "high education")), aes(y= props , x=wave, ymin = lows, ymax = highs))+
  geom_point(aes(colour=group_inc), size=3.5) +
  # ggtitle("Education Bias") +
  #  theme(plot.title = element_text(hjust = 0.5))+
  theme_bw() +
  theme(panel.border=element_blank(), plot.title = element_text(hjust = 0.5)) +
  ylab("% relative bias in education categories") + 
  xlab("experimental group") +
  facet_grid(rows = vars(var))+
  scale_color_manual(values=c("lightblue","darkblue", "black")) +
  geom_hline(yintercept=0, linetype="dashed") +
#  coord_flip()+
  theme(legend.position="bottom") +
  theme(legend.title=element_blank())+
  geom_pointrange(aes(colour=group_inc)) 
dev.off()

# absolute biases
pdf("inc_rel_biases_over_waves.pdf")
ggplot(subset(plot_dat, plot_dat$group_inc %in% c( "bonus 50 Euro", "bonus 20 Euro", "no bonus")), aes(wave, rel_bias, fill=group_inc)) + 
  geom_boxplot()+
  scale_y_continuous(name = "absolute relative bias (%)")+
  xlab("wave")+
  # ggtitle("Boxplot of Absolute Relative Biases over Stage Waves")+
  theme_bw() +
  scale_fill_manual(values=c("lightblue","darkblue", "black")) +
  theme(legend.position="bottom") +
  theme(legend.title=element_blank())+
  stat_summary(fun.y=mean, geom="point",position=position_dodge(width=0.75), shape=20, size=5, color="red", show.legend=FALSE) +
  theme(panel.border=element_blank(), plot.title = element_text(hjust = 0.5)) 
dev.off()

########### Teilnahmequoten nach Gruppen
datenbank <- merge(datenbank, aktivitaten[, c("id", "w37_part", "first_wave_part")], by.x="gip_id", by.y="id")
datenbank$online2 <- ifelse(datenbank$online2_status == 2, 1, 0)
datenbank$online2 <- ifelse(is.na(datenbank$online2_status) == T, 0, datenbank$online2)


rr_reg <- prop.table(table(datenbank$online2, datenbank$experimental_group), 2)[ 2, ]
rr_core <- prop.table(table(datenbank$w37_part, datenbank$experimental_group), 2)[ 2, ]
rr_first <- prop.table(table(datenbank$first_wave_part, datenbank$experimental_group), 2)[ 2, ]