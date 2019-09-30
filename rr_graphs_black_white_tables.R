rm(list = ls())

library(zoo)
library(ggplot2)
library(grid)
library(gtable)
library(haven)
library(openxlsx)
library(psych)

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
                         levels = c(4, 2, 1, 3, 5, 6),
labels = c("Paper-first", "Concurrent", "Push-to-web", "Online-only", "Bonus 20", "Bonus 50"))

setwd("Z:/Z1/Projekte/Rekrutierungsexperimente_2018/Paper_Draft/tables")

t_sample <- table(dat$group_new)
write.xlsx(t_sample, 'sample_size.xlsx')

setwd("Z:/Z1/Projekte/Rekrutierungsexperimente_2018/Paper_Draft/code")

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

#############################error ab , 4] "subscript out of bounds", führt zu error in plot weil cum3_1p nicht gefunden werden kann
 
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




################## Tabellen im Paper: Response Rates 

dat$reminder1 <- ifelse(dat$invitation_date == "13.09.2018" , "2018-10-04", "2018-12-13")
dat$reminder2 <- ifelse(dat$invitation_date == "13.09.2018" , "2018-10-25", "2019-01-10")

dat$reminder1 <- as.Date.character(dat$reminder1)
dat$reminder2 <- as.Date.character(dat$reminder2)

# Recruitment interview response rates
dat$part1_date <- (ifelse(is.na(dat$paper1_date ) == FALSE, dat$paper1_date , ifelse(is.na(dat$online1_date ) == FALSE, dat$online1_date , NA)))
dat$part1_date <- as.Date.numeric(dat$part1_date)

dat$end = "2019-03-01" 
dat$end <- as.Date.character(dat$end)
dat$part_before_end <- ifelse((dat$part1_date <= dat$end) & dat$part1 == 1, 1, 0 )

dat$part_before_1reminder <- ifelse((dat$part1_date <= dat$reminder1) & dat$part1 == 1, 1, 0 )
dat$part_before_2reminder <- ifelse((dat$part1_date <= dat$reminder2) & dat$part1 == 1, 1, 0 )

round(prop.table(table(dat$part_before_1reminder, dat$experimental_group), 2)[, c(5, 3, 6, 4)], digits=4)*100
round(prop.table(table(dat$part_before_2reminder, dat$experimental_group), 2)[, c(5, 3, 6, 4)], digits=4)*100
round(prop.table(table(dat$part_before_end, dat$experimental_group), 2)[, c(5, 3, 6, 4)], digits=4)*100

prop.table(table(dat$part_before_1reminder))*100
prop.table(table(dat$part_before_2reminder))*100
prop.table(table(dat$part_before_end))*100

### Automatic table generation

setwd("Z:/Z1/Projekte/Rekrutierungsexperimente_2018/Paper_Draft/tables")

t_rr1_r1 <- round(prop.table(table(dat$part_before_1reminder, dat$group_new), 2), digits=4)*100
t_rr1_r2 <- round(prop.table(table(dat$part_before_2reminder, dat$group_new), 2), digits=4)*100
t_rr1_cum <- round(prop.table(table(dat$part_before_end, dat$group_new), 2), digits=4)*100

write.xlsx(t_rr1_r1, 'rr1_r1.xlsx')
write.xlsx(t_rr1_r2, 'rr1_r2.xlsx')
write.xlsx(t_rr1_cum, 'rr1_cum.xlsx')


# Panel registration rates
dat$online2_before_1reminder <- ifelse((dat$online2_date <= dat$reminder1) & dat$online2 == 1, 1, 0 )
dat$online2_before_2reminder <- ifelse((dat$online2_date <= dat$reminder2) & dat$online2 == 1, 1, 0 )
dat$online2_before_end <- ifelse((dat$online2_date <= dat$end) & dat$online2 == 1, 1, 0 )

round(prop.table(table(dat$online2_before_1reminder, dat$experimental_group), 2)[, c(5, 3, 6, 4)], digits=4)*100
round(prop.table(table(dat$online2_before_2reminder, dat$experimental_group), 2)[, c(5, 3, 6, 4)], digits=4)*100
round(prop.table(table(dat$online2_before_end, dat$experimental_group), 2)[, c(5, 3, 6, 4)], digits=4)*100

t_rr2_r1 <- round(prop.table(table(dat$online2_before_1reminder, dat$group_new), 2), digits=4)*100
t_rr2_r2 <- round(prop.table(table(dat$online2_before_2reminder, dat$group_new), 2), digits=4)*100
t_rr2_cum <- round(prop.table(table(dat$online2_before_end, dat$group_new), 2), digits=4)*100
  
write.xlsx(t_rr2_r1, 'rr2_r1.xlsx')
write.xlsx(t_rr2_r2, 'rr2_r2.xlsx')
write.xlsx(t_rr2_cum, 'rr2_cum.xlsx')


prop.table(table(dat$online2_before_1reminder))*100
prop.table(table(dat$online2_before_2reminder))*100
prop.table(table(dat$online2_before_end))*100


################### Zusatzanalysen: in welchem modus machen Leute mit
table(dat$part1)
dat$mode1 <- ifelse(dat$paper1_status == 1, "paper", ifelse(dat$online1_status == 2, "online", 0))

online1_mode_tab <- prop.table(table(subset(dat,dat$mode1 != 0)$mode1, subset(dat,dat$mode1 != 0)$experimental_group), 2)
library(xtable)
xtable(online1_mode_tab*100)

dat$consent <- ifelse(dat$paper1_status == 1 & is.na(dat$paper1_consent) == TRUE, 0, ifelse(dat$paper1_consent == 1 , 1, NA))
paper1_consent_tab <- prop.table(table(dat$consent, dat$experimental_group), 2)
xtable(paper1_consent_tab*100)

online2_mode_tab <- prop.table(table(subset(dat,dat$mode1 != 0 & dat$online2 == 1)$mode1, subset(dat,dat$mode1 != 0 & dat$online2 == 1)$experimental_group), 2)
xtable(online2_mode_tab*100)

dat$registered <- ifelse(dat$online2_status == 2 , 1, 
                        ifelse( (dat$online1_status == 2 | dat$consent == 1) & dat$online2_status != 2, 0, NA))
registration_tab <- prop.table(table(subset(dat,dat$consent == 1)$registered, subset(dat,dat$consent == 1)$experimental_group), 2)
xtable(registration_tab*100)


