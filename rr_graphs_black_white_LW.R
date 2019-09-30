rm(list = ls())

library(zoo)
library(ggplot2)
library(grid)
library(gtable)
library(haven)
library(openxlsx)
library(psych)
library(scales)
library(extrafont)

#dat <- read.csv2("//nas.uni-mannheim.de/uni-shares/sfb884/sfb884z/Z1/1 Feldarbeit GIP/11 Rekrutierung TNS/Phase3/Datenbank/Feldstart/stand_final.csv")
dat <- read.csv2("Z:/Z1/1 Feldarbeit GIP/11 Rekrutierung TNS/Phase3/Datenbank/Feldstart/stand_final.csv", stringsAsFactors = FALSE)

# reduzieren auf alle, deren Einladungsbrief angekommen ist
brutto <- data.frame(brutto)
brutto <- read_dta("Y:/Bearbeitung/Panel_Management/Rekrutierung2018/Gemeindestichprobe/brutto_anonym.dta")
brutto <- subset(brutto, brutto$Stichprobe == 2)
dat$exclude <- ifelse(dat$gip_id %in% c(brutto$gip_id), 1, 0)
dat <- subset(dat, dat$exclude != 1) # exlude tranche 2
dat$exclude <- ifelse(dat$invitation_returned == 1 & is.na(dat$online1_status) == T  & is.na(dat$online2_status) == T & is.na(dat$paper1_status) == T, 1, 0 )
dat$exclude <- ifelse(is.na(dat$exclude) == T, 0, dat$exclude)
dat <- subset(dat, dat$exclude != 1) # exclude if invitation returned to sender


dat$group_new <- dat$experimental_group
dat$group_new[dat$experimental_group=="1a"] <- "5"
dat$group_new[dat$experimental_group=="1b"] <- "6"
dat$group_new[dat$experimental_group=="2"] <- "3"
dat$group_new[dat$experimental_group=="3"] <- "4"
dat$group_new[dat$experimental_group=="4"] <- "1"
dat$group_new[dat$experimental_group=="5"] <- "2"


dat$group_new <- ordered(dat$group_new,
                         levels = c(1, 2, 3, 4, 5, 6),
                         labels = c("Push-to-web", "Concurrent", "Online-only", "Paper-first", "Bonus 20", "Bonus 50"))

setwd("Z:/Z1/Projekte/Rekrutierungsexperimente_2018/Paper_Draft/code")

t_sample <- table(dat$group_new)
write.xlsx(t_sample, 'sample_size.xlsx')

setwd("Z:/Z1/Projekte/Rekrutierungsexperimente_2018/Paper_Draft/graphs")

dat$start_date <- as.Date(dat$invitation_date, "%d.%m.%Y")
dat$online1_date <- as.Date(dat$online1_date, "%d.%m.%Y")
dat$paper1_date <- as.Date(dat$paper1_date, "%d.%m.%Y")
dat$online2_date <- as.Date(dat$online2_date, "%d.%m.%Y")
dat$reminder1_date <- as.Date(dat$reminder1_sent, "%d.%m.%Y")
dat$reminder2_date <- as.Date(dat$reminder2_sent, "%d.%m.%Y")
dat$login_data_date <- as.Date(dat$login_data_sent, "%d.%m.%Y")

# Fall, der vor dem Zustellen des Briefes online mitgemacht haben will, auf 0 setzten (siehe E-Mail von Herrn Rieburg)
dat$online1_status <- ifelse(dat$online1_date < dat$start, 0, dat$online1_status )

dat$online1_participation_date <- dat$online1_date - dat$start_date
dat$online2_participation_date <- dat$online2_date - dat$start_date

# differenziert nach Zeitpunkt der Einladung: 
dat$online2_participation_date_h1 <- ifelse(dat$online1_status !=2 & dat$paper1_status == 1 , dat$online2_date - dat$login_data_date, NA)
dat$online2_participation_date_h2 <- ifelse(dat$online1_status !=2 & dat$paper1_status == 1 & dat$online2_participation_date_h1 < 0 , dat$online2_date - dat$reminder1_date, dat$online2_participation_date_h1)                                          

dat$online2_participation_date_op <- ifelse(dat$online1_status == 2 , dat$online2_participation_date, 
                                            ifelse(dat$online1_status != 2 & dat$paper1_status == 1, dat$online2_participation_date_h2, NA))

dat$paper1_participation_date <- dat$paper1_date - dat$start_date 

dat$reminder1_dur <- dat$reminder1_date - dat$start_date  # erster Reminder nach 21 Tagen f?r alle
dat$reminder2_dur <- dat$reminder2_date - dat$start_date # zweiter Reminder nach 42 Tagen f?r T1 und 49 Tagen f?r T2

dat$online1_status <- ifelse(is.na(dat$online1_status) == TRUE, 0, dat$online1_status)
dat$online2_status <- ifelse(is.na(dat$online2_status) == TRUE, 0, dat$online2_status)
dat$paper1_status <- ifelse(is.na(dat$paper1_status) == TRUE, 0, dat$paper1_status)

dat$online1 <- ifelse(dat$online1_status == 2, 1,  0)
dat$paper1 <- ifelse(dat$paper1_status == 1, 1, 0)
dat$online2 <- ifelse(dat$online2_status == 2, 1, 0)

table(dat$online1_status, dat$online1, exclude = NULL)

dat$participation1 <- ifelse(dat$online1 == 1 | dat$paper1 == 1 , 1, 0) # 22 Personen haben online1 und paper1 ausgef?llt (?berschneidung Papierfragebogen R?ckversand und Online-Reminder oder concurrent)
#by( dat[, c(53, 54)], dat$experimental_group, table)


############  Panel Registration Rates
# graphics in two versions: version 1, participation day as counted as days from fieldstart
dat2 <- subset(dat, dat$online2_status == 2)
participation2_exp_day <- table(dat2$online2_participation_date, dat2$experimental_group)

cum1a_2 <- cumsum(participation2_exp_day[, 1])/ table(dat$experimental_group)[1]  
cum1b_2 <- cumsum(participation2_exp_day[, 2])/ table(dat$experimental_group)[2]  
cum2_2 <- cumsum(participation2_exp_day[, 3])/ table(dat$experimental_group)[3]  
cum3_2 <- cumsum(participation2_exp_day[, 4]) / table(dat$experimental_group)[4] 
cum4_2 <- cumsum(participation2_exp_day[, 5]) / table(dat$experimental_group)[5] 
cum5_2 <- cumsum(participation2_exp_day[, 6]) / table(dat$experimental_group)[6] 

x <- sort(unique(dat2$online2_participation_date))

plot.dat.gg <- data.frame(date = rep(x, 6), rr = c(cum1a_2, cum1b_2, cum2_2, cum3_2, cum4_2, cum5_2), 
        group = c(rep("Bonus 50 Euro", length(x)), rep("Bonus 20 Euro", length(x)), rep("online-only", length(x)),
                  rep("paper-first", length(x)), rep("push-to-web", length(x)), rep("concurrent", length(x))))



participation2_exp_day2 <- table(dat2$online2_participation_date_op, dat2$experimental_group)
  
cum1a_22 <- cumsum(participation2_exp_day2[, 1])/ table(dat$experimental_group)[1]  
cum1b_22 <- cumsum(participation2_exp_day2[, 2])/ table(dat$experimental_group)[2]  
cum2_22 <- cumsum(participation2_exp_day2[, 3])/ table(dat$experimental_group)[3]  
cum3_22 <- cumsum(participation2_exp_day2[, 4]) / table(dat$experimental_group)[4] 
cum4_22 <- cumsum(participation2_exp_day2[, 5]) / table(dat$experimental_group)[5] 
cum5_22<- cumsum(participation2_exp_day2[, 6]) / table(dat$experimental_group)[6] 
  
x <- sort(unique(dat2$online2_participation_date_op))
  
plot.dat.gg <- data.frame(date = rep(x, 6), rr = c(cum1a_22, cum1b_22, cum2_22, cum3_22, cum4_22, cum5_22), 
                            group = c(rep("Bonus 50 Euro", length(x)), rep("Bonus 20 Euro", length(x)), rep("online-only", length(x)),
                                      rep("paper-first", length(x)), rep("push-to-web", length(x)), rep("concurrent", length(x))))



#############################################################################################################################################
jpeg("ptw_rr_online2_days_from_invitation_bw.jpg", units="in", width=8, height=11, res=300)
p <- ggplot(data=subset(plot.dat.gg, plot.dat.gg$group %in% c( "push-to-web", "online-only", "paper-first",  "concurrent")), aes(x=date, y=rr, group=group)) +
  ggtitle("Registration Survey") +
  # theme(plot.title = element_text(hjust = 0.5)) +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line( size = 0.5, colour = "black"), axis.text = element_text(size = 15, color = "black"), 
        axis.title.x = (element_text(size = 20)), axis.title.y = element_text(size = 20))+ xlab("days since invitation") + ylab("response rate")+
  geom_step(aes(linetype= group, colour=group), lwd=0.5)+
  geom_text(x=90, y=0.218, label="concurrent", adj=0.5, color="black", size=6) +     #adjusted size, color and position
  geom_text(x=90, y=0.201, label="paper-first", adj=0.5, color="black", size=6) +
  geom_text(x=90, y=0.237, label="push-to-web", adj=0.5, color="black", size=6) +
  geom_text(x=90, y=0.256, label="online-only", adj=0.5, color="black", size=6) +
  # geom_point(aes(colour=group)) +
  scale_color_manual(values=c("black", "black",  "black" , "black")) +
  scale_linetype_manual(values=c("solid", "solid", "solid", "solid")) +
  theme(legend.position="none") +
  # coord_cartesian(xlim = c(1, length(x) )) +
  # geom_vline(xintercept=as.numeric(plot.dat.gg$date[20]), linetype="longdash") +
  #  geom_vline(xintercept=as.numeric(plot.dat.gg$date[41]), linetype="longdash") +
  geom_vline(xintercept=as.numeric(plot.dat.gg$date[7]), linetype="solid") +
  annotate("text", x=3, y=0.44, label = "r1", size = 6) +
  geom_vline(xintercept=as.numeric(plot.dat.gg$date[14]), linetype="solid") +
  annotate("text", x=10, y=0.44, label = "r2", size = 6) +
  geom_vline(xintercept=as.numeric(plot.dat.gg$date[21]), linetype="solid") + 
  annotate("text", x=17, y=0.44, label = "r3", size = 6) +
  scale_x_continuous(limits = c(0, 100)) + scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,0.44))
  theme(legend.background = element_rect(fill="lightgray", size=0.5, linetype="solid", colour ="black"))
p + theme(plot.title = element_text(hjust = 0.5, size =  20))
  dev.off() 

###############################################################################################################################

############  Recrutiment rates graphs
#### part 1 online


dat1 <- subset(dat, dat$online1_status == 2)

participation1_exp_day <- table(dat1$online1_participation_date, dat1$experimental_group)


cum1a_1 <- cumsum(participation1_exp_day[, 1]) / table(dat$experimental_group)[1]
cum1b_1 <- cumsum(participation1_exp_day[, 2]) / table(dat$experimental_group)[2]
cum2_1 <- cumsum(participation1_exp_day[, 3]) / table(dat$experimental_group)[3]
cum3_1 <- cumsum(participation1_exp_day[, 4]) / table(dat$experimental_group)[4]
cum4_1 <- cumsum(participation1_exp_day[, 5]) / table(dat$experimental_group)[5]
cum5_1 <- cumsum(participation1_exp_day[, 6]) / table(dat$experimental_group)[6]


x <- sort(unique(dat1$online1_participation_date))

plot.dat.gg <- data.frame(date = rep(x, 6), rr = c(cum1a_1, cum1b_1, cum2_1, cum3_1, cum4_1, cum5_1), 
                          group = c(rep("Bonus 50 Euro", length(x)), rep("Bonus 20 Euro", length(x)), rep("online-only", length(x)),
                                    rep("paper-first", length(x)), rep("push-to-web", length(x)), rep("concurrent", length(x))))
online <- data.frame(date = rep(x, 6), rr = c(cum1a_1, cum1b_1, cum2_1, cum3_1, cum4_1, cum5_1), 
                     group = c(rep("Bonus 50 Euro", length(x)), rep("Bonus 20 Euro", length(x)), rep("online-only", length(x)),
                               rep("paper-first", length(x)), rep("push-to-web", length(x)), rep("concurrent", length(x))))


jpeg("ptw_rr_online1_bw.jpg", units="in", width=8, height=9, res=300)
p <- ggplot(data=subset(plot.dat.gg, plot.dat.gg$group %in% c( "push-to-web", "online-only", "paper-first",  "concurrent")), aes(x=date, y=rr, group=group)) +
  ggtitle("Recruitment Survey") +
  # theme(plot.title = element_text(hjust = 0.5))+
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.text = element_text(size = 15, colour = "black") ,axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20))+
  xlab("days since invitation") + ylab("response rate")+
  geom_step(aes(linetype= group, colour=group))+
  # geom_point(aes(colour=group)) +
  geom_text(x=80, y=0.170, label="concurrent", adj=0, color="black", size=6) +
  geom_text(x=80, y=0.061, label="paper-first", adj=0, color="black", size=6) +
  geom_text(x=80, y=0.229, label="push-to-web", adj=0, color="black", size=6) +
  geom_text(x=80, y=0.288, label="online-only", adj=0, color="black", size=6) +
  scale_color_manual(values=c("black", "black",  "black" , "black")) +
  theme(legend.position="none") +
  # coord_cartesian(xlim = c(1, length(x) )) +
  geom_vline(xintercept=as.numeric(plot.dat.gg$date[20]), linetype="solid") +
  annotate("text", x=18, y=0.44, label = "r1", size = 6) +
  geom_vline(xintercept=as.numeric(plot.dat.gg$date[41]), linetype="solid") +
  annotate("text", x=38, y=0.44, label = "r2", size = 6) +
  # geom_vline(xintercept=as.numeric(plot.dat.gg$date[48]), linetype="longdash") +
  scale_linetype_manual(values=c("solid", "solid", "solid", "solid")) +
  scale_x_continuous(limits = c(0, 100))+ scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 0.44)) 
# theme(legend.background = element_rect(fill="lightgray", size=0.5, linetype="solid", colour ="black"))
p + theme(plot.title = element_text(hjust = 0.5, size = 20))
dev.off()


dat1 <- subset(dat, dat$online1_status == 2)

participation1_exp_day <- table(dat1$online1_participation_date, dat1$experimental_group)


cum1a_1 <- cumsum(participation1_exp_day[, 1]) / table(dat$experimental_group)[1]
cum1b_1 <- cumsum(participation1_exp_day[, 2]) / table(dat$experimental_group)[2]
cum2_1 <- cumsum(participation1_exp_day[, 3]) / table(dat$experimental_group)[3]
cum3_1 <- cumsum(participation1_exp_day[, 4]) / table(dat$experimental_group)[4]
cum4_1 <- cumsum(participation1_exp_day[, 5]) / table(dat$experimental_group)[5]
cum5_1 <- cumsum(participation1_exp_day[, 6]) / table(dat$experimental_group)[6]


x <- sort(unique(dat1$online1_participation_date))

plot.dat.gg <- data.frame(date = rep(x, 6), rr = c(cum1a_1, cum1b_1, cum2_1, cum3_1, cum4_1, cum5_1), 
                          group = c(rep("Bonus 50 Euro", length(x)), rep("Bonus 20 Euro", length(x)), rep("online-only", length(x)),
                                    rep("paper-first", length(x)), rep("push-to-web", length(x)), rep("concurrent", length(x))))
online <- data.frame(date = rep(x, 6), rr = c(cum1a_1, cum1b_1, cum2_1, cum3_1, cum4_1, cum5_1), 
                          group = c(rep("Bonus 50 Euro", length(x)), rep("Bonus 20 Euro", length(x)), rep("online-only", length(x)),
                                    rep("paper-first", length(x)), rep("push-to-web", length(x)), rep("concurrent", length(x))))



#  part 1 paper
dat1p <- subset(dat, dat$paper1_status == 1)
participation1p_exp_day <- table(dat1p$paper1_participation_date, dat1p$experimental_group)
cum1a_1p <- cumsum(participation1p_exp_day[, 1]) / table(dat$experimental_group)[1]
cum1b_1p <- cumsum(participation1p_exp_day[, 2]) / table(dat$experimental_group)[2]
cum2_1p <- cumsum(participation1p_exp_day[, 3]) / table(dat$experimental_group)[3]
cum3_1p <- cumsum(participation1p_exp_day[, 4]) / table(dat$experimental_group)[4]
cum4_1p <- cumsum(participation1p_exp_day[, 5]) / table(dat$experimental_group)[5]
cum5_1p <- cumsum(participation1p_exp_day[, 6]) / table(dat$experimental_group)[6]


x <- sort(unique(dat1p$paper1_participation_date) )


plot.dat.gg <- data.frame(date = rep(x, 6), rr = c(cum1a_1p, cum1b_1p, cum2_1p, cum3_1p, cum4_1p, cum5_1p), 
                          group = c(rep("Bonus 50 Euro", length(x)), rep("Bonus 20 Euro", length(x)), rep("online-only", length(x)),
                                    rep("paper-first", length(x)), rep("push-to-web", length(x)), rep("concurrent", length(x))))

paper <- data.frame(date = rep(x, 6), rr = c(cum1a_1p, cum1b_1p, cum2_1p, cum3_1p, cum4_1p, cum5_1p), 
                          group = c(rep("Bonus 50 Euro", length(x)), rep("Bonus 20 Euro", length(x)), rep("online-only", length(x)),
                                    rep("paper-first", length(x)), rep("push-to-web", length(x)), rep("concurrent", length(x))))


# part 1 paper vs online 
names(online)[names(online) == 'rr'] <- 'rr_online'
names(paper)[names(paper) == 'rr'] <- 'rr_paper'

props <- merge(paper, online, by=c("date", "group"))
props$proportion <- props$rr_online/(props$rr_paper+props$rr_online)

##############object not found (props & paper), führt zu Fehler in Grafik (bleibt leer)



# part 1 either mode
dat$part1 <- ifelse(dat$online1 == 1 | dat$paper1 == 1, 1, 0)
datp1 <- subset(dat, dat$part1 == 1)

datp1$part1_participation_date <- (ifelse(is.na(datp1$paper1_participation_date ) == FALSE, datp1$paper1_participation_date , ifelse(is.na(datp1$online1_participation_date ) == FALSE, datp1$online1_participation_date , NA)))

participation1po_exp_day <- table(datp1$part1_participation_date, datp1$experimental_group)

x <- sort(unique(datp1$part1_participation_date)) 

cum1a_1po <- cumsum(participation1po_exp_day[, 1])/ table(dat$experimental_group)[1]
cum1b_1po <- cumsum(participation1po_exp_day[, 2])/ table(dat$experimental_group)[2] 
cum2_1po <- cumsum(participation1po_exp_day[, 3])/ table(dat$experimental_group)[3] 
cum3_1po <- cumsum(participation1po_exp_day[, 4])/ table(dat$experimental_group)[4] 
cum4_1po <- cumsum(participation1po_exp_day[, 5]) / table(dat$experimental_group)[5]
cum5_1po <- cumsum(participation1po_exp_day[, 6]) / table(dat$experimental_group)[6] 



plot.dat.gg <- data.frame(date = rep(x, 6), rr = c(cum1a_1po, cum1b_1po, cum2_1po, cum3_1po, cum4_1po, cum5_1po), 
                          group = c(rep("Bonus 50 Euro", length(x)), rep("Bonus 20 Euro", length(x)), rep("online-only", length(x)),
                                    rep("paper-first", length(x)), rep("push-to-web", length(x)), rep("concurrent", length(x))))


jpeg("ptw_rr_part1_bw.jpg", units="in", width=8, height=11, res=300)
p <- ggplot(data=subset(plot.dat.gg, plot.dat.gg$group %in% c("push-to-web", "online-only", "paper-first",  "concurrent")), aes(x=date, y=rr, group=group)) +
  ggtitle("Recruitment Survey") +
  theme_bw() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"), axis.text = element_text(size = 15, colour = "black"),
        axis.title.x = element_text(size = 20), axis.title.y = element_text(size = 20))+
  xlab("days since invitation") + ylab("response rate")+
  geom_step(aes(linetype= group, colour=group))+
  # geom_point(aes(colour=group)) +
  geom_text(x=80, y=0.429, label="concurrent", adj=0, color="black", size=6) +      #adjusted size & colour
  geom_text(x=80, y=0.395, label="paper-first", adj=0, color="black", size=6) +
  geom_text(x=80, y=0.351, label="push-to-web", adj=0, color="black", size=6) +
  geom_text(x=80, y=0.285, label="online-only", adj=0, color="black", size=6) +
  scale_color_manual(values=c("black", "black",  "black" , "black")) +
  scale_linetype_manual(values=c("solid", "solid", "solid", "solid")) +
  theme(legend.position="none") +
  #  coord_cartesian(xlim = c(1, length(x) )) +
  geom_vline(xintercept=21, linetype="solid") +
  annotate("text", x=18, y=0.45, label = "r1", size = 6) +
  geom_vline(xintercept=42, linetype="solid") +
  annotate("text", x=38, y=0.45, label = "r2", size = 6) +
  scale_x_continuous(limits = c(0, 100)) + scale_y_continuous(labels = scales::percent_format(accuracy = 1)) 
# theme(legend.background = element_rect(fill="lightgray", size=0.5, linetype="solid", colour ="black"))
p + theme(plot.title = element_text(hjust = 0.5, size = 20))
dev.off()

