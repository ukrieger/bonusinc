rm(list = ls())
library(zoo)
library(ggplot2)


daten <- read.csv2("//nas.uni-mannheim.de/uni-shares/sfb884/sfb884z/Z1/1 Feldarbeit GIP/11 Rekrutierung TNS/Phase3/Datenbank/Feldstart/stand_final.csv")
#setwd("//nas.uni-mannheim.de/uni-shares/sfb884/sfb884z/Z1//1 Feldarbeit GIP/11 Rekrutierung TNS/Phase3/Datenbank/Feldstart/Graphiken")
setwd("W:/Z1/Projekte/Rekrutierungsexperimente_2018/bonus_incentives/graphs")

dat <- subset(daten, daten$invitation_date == "13.09.2018" & daten$reminder1_sent != "13.12.2018")  # nur 1. Versand
dat <- subset(dat, is.na(dat$invitation_returned) == T)

dat$online1_status <- ifelse(is.na(dat$online1_status) == TRUE, 0, dat$online1_status)
dat$online2_status <- ifelse(is.na(dat$online2_status) == TRUE, 0, dat$online2_status)

dat$online1 <- ifelse(dat$online1_status == 2, 1,  0)
dat$online2 <- ifelse(dat$online2_status == 2, 1, 0)



############  Graphiken für Online 2 Status
dat$tag2 <- as.Date(dat$online2_date, "%d.%m.%Y")
dat2 <- subset(dat, dat$online2_status == 2)
participation2_exp_day <- table(dat2$tag2, dat2$experimental_group)

cum1a_2 <- cumsum(participation2_exp_day[, 1])/ table(dat$experimental_group)[1]  
cum1b_2 <- cumsum(participation2_exp_day[, 2])/ table(dat$experimental_group)[2]  
cum2_2 <- cumsum(participation2_exp_day[, 3])/ table(dat$experimental_group)[3]  

x <- sort(unique(dat2$tag2))

plot.dat.gg.incentive <- data.frame(date = rep(x, 3), rr = c(cum1a_2, cum1b_2, cum2_2), 
                          group = c(rep("Bonus 50 Euro", length(x)), rep("Bonus 20 Euro", length(x)), 
                                    rep("No Bonus", length(x))))



pdf("inc_rr_online2.pdf", width=7, height=5)
ggplot(data=plot.dat.gg.incentive, aes(x=date, y=rr, group=group)) +
  ggtitle("Cumulative Response Rates Panel Registration") +
  theme_bw() +
  theme(panel.border=element_blank(), plot.title = element_text(hjust = 0.5)) +
  xlab("date") + ylab("response rate")+
  geom_step(aes(linetype= group, colour=group))+
  geom_point(aes(colour=group)) +
  scale_color_manual(values=c("lightblue","darkblue", "black")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept=as.numeric(plot.dat.gg.incentive$date[17]), linetype="solid", colour = "red") +
  geom_vline(xintercept=as.numeric(plot.dat.gg.incentive$date[20]), linetype="longdash") +
  geom_vline(xintercept=as.numeric(plot.dat.gg.incentive$date[41]), linetype="longdash") +
  geom_text(x=( sort(unique(plot.dat.gg.incentive$date))[109]), y=0.288, label="bonus 20 Euro", adj=0, color="lightblue") +
  geom_text(x=( sort(unique(plot.dat.gg.incentive$date))[109]), y=0.309, label="bonus 50 Euro", adj=0, color="darkblue") +
  geom_text(x=( sort(unique(plot.dat.gg.incentive$date))[109]), y=0.254, label="online only", adj=0, color="black") +
  theme(legend.position="none") +
#  theme(legend.position="bottom") +
  # aes(size=2)+
  theme(legend.background = element_rect(fill="lightgray", size=0.5, linetype="solid", colour ="black"))
dev.off()




############  Graphiken für Online 1 Status
dat$tag1 <- as.Date(dat$online1_date, "%d.%m.%Y")
dat1 <- subset(dat, dat$online1_status == 2)
participation1_exp_day <- table(dat1$tag1, dat1$experimental_group)

cum1a_1 <- cumsum(participation1_exp_day[, 1])/ table(dat$experimental_group)[1]  
cum1b_1 <- cumsum(participation1_exp_day[, 2])/ table(dat$experimental_group)[2]  
cum2_1 <- cumsum(participation1_exp_day[, 3])/ table(dat$experimental_group)[3]  

x <- sort(unique(dat1$tag1))

plot.dat.gg.incentive1 <- data.frame(date = rep(x, 3), rr = c(cum1a_1, cum1b_1, cum2_1), 
                                    group = c(rep("Bonus 50 Euro", length(x)), rep("Bonus 20 Euro", length(x)), 
                                              rep("No Bonus", length(x))))



pdf("inc_rr_online1.pdf", width=7, height=5)
ggplot(data=plot.dat.gg.incentive1, aes(x=date, y=rr, group=group)) +
  ggtitle("Cumulative Response Rates of Recruitment Interview") +
  theme_bw() +
  theme(panel.border=element_blank(), plot.title = element_text(hjust = 0.5)) +
  xlab("date") + ylab("response rate")+
  geom_step(aes(linetype= group, colour=group))+
  geom_point(aes(colour=group)) +
  scale_color_manual(values=c("lightblue","darkblue", "black")) +
  theme(legend.title = element_blank()) +
  geom_vline(xintercept=as.numeric(plot.dat.gg.incentive$date[17]), linetype="solid", colour = "red") +
  geom_vline(xintercept=as.numeric(plot.dat.gg.incentive$date[20]), linetype="longdash") +
  geom_vline(xintercept=as.numeric(plot.dat.gg.incentive$date[41]), linetype="longdash") +
  geom_text(x=( sort(unique(plot.dat.gg.incentive$date))[109]), y=0.323, label="bonus 20 Euro", adj=0, color="lightblue") +
  geom_text(x=( sort(unique(plot.dat.gg.incentive$date))[109]), y=0.347, label="bonus 50 Euro", adj=0, color="darkblue") +
  geom_text(x=( sort(unique(plot.dat.gg.incentive$date))[109]), y=0.292, label="online only", adj=0, color="black") +
  theme(legend.position="none") +
  theme(legend.background = element_rect(fill="lightgray", size=0.5, linetype="solid", colour ="black"))
dev.off()

