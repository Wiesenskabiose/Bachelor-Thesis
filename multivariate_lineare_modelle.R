# Erarbeitung der multivariaten linearen Regression

# NDVI und NDMI Min und Max Werte finden
#Beispielhaft thier für 2020
# niederschlaege wurden wie in Niederschlagsdaten fuer die jeweiligen lags vorbereitet

sub_NDVI_2020 <- subset(boxplot_bin_20, variable == as.Date("2020-06-26")|
                          variable == as.Date("2020-07-31"))
sub_NDVI_2020 <- sub_NDVI_2020[order(sub_NDVI_2020$value, decreasing = TRUE), ]  # Order data descending
max_NDVI_2020 <- Reduce(rbind,                                 # Top N highest values by group
                        by(sub_NDVI_2020,
                           sub_NDVI_2020["variable"],
                           head,
                           n = 50))
sub_NDVI_2020 <- sub_NDVI_2020[order(sub_NDVI_2020$value,  decreasing = F), ]  # Order data descending
min_NDVI_2020 <- Reduce(rbind,                                 # Top N highest values by group
                        by(sub_NDVI_2020,
                           sub_NDVI_2020["variable"],
                           head,
                           n = 50))

# alle extrahierten werte mergen um pixel uebereinstimmungen zu finden
# pixel die in moeglichst allen Jahren vorkommen waehlen
max_NDVI_pixel <- merge(max_NDVI_2018, max_NDVI_2019, by = "cell")
max_NDVI_pixel <- rbind(max_NDVI_pixel, merge(max_NDVI_2020, max_NDVI_2019, by = "cell")) 
max_NDVI_pixel <- rbind(max_NDVI_pixel,merge(max_NDVI_2018, max_NDVI_2020, by = "cell")) 

min_NDVI_pixel <- merge(min_NDVI_2018, min_NDVI_2019, by = "cell")
min_NDVI_pixel <- rbind(min_NDVI_pixel, merge(min_NDVI_2020, min_NDVI_2019, by = "cell"))
min_NDVI_pixel <- rbind(min_NDVI_pixel, merge(min_NDVI_2018, min_NDVI_2020, by = "cell"))


sub_NDMI_2020 <- subset(boxplot_bin_20_NDMI, variable == as.Date("2020-06-26")|
                          variable == as.Date("2020-07-31"))
sub_NDMI_2020 <- sub_NDMI_2020[order(sub_NDMI_2020$value, decreasing = TRUE), ]  # Order data descending
max_NDMI_2020 <- Reduce(rbind,                                 # Top N highest values by group
                        by(sub_NDMI_2020,
                           sub_NDMI_2020["variable"],
                           head,
                           n = 50))
sub_NDMI_2020 <- sub_NDMI_2020[order(sub_NDMI_2020$value,  decreasing = F), ]  # Order data descending
min_NDMI_2020 <- Reduce(rbind,                                 # Top N highest values by group
                        by(sub_NDMI_2020,
                           sub_NDMI_2020["variable"],
                           head,
                           n = 50))

# alle extrahierten werte mergen um pixel uebereinstimmungen zu finden
# pixel die in moeglichst allen Jahren vorkommen waehlen
max_NDMI_pixel <- merge(max_NDMI_2018, max_NDMI_2019, by = "cell")
max_NDMI_pixel <- merge(max_NDMI_2020, max_NDMI_2019, by = "cell") 
max_NDMI_pixel <- rbind(max_NDMI_pixel,merge(max_NDMI_2018, max_NDMI_2020, by = "cell")) 

min_NDMI_pixel <- merge(min_NDMI_2018, min_NDMI_2019, by = "cell")
min_NDMI_pixel <- merge(min_NDMI_2020, min_NDMI_2019, by = "cell")
min_NDMI_pixel <- rbind(min_NDMI_pixel, merge(min_NDMI_2018, min_NDMI_2020, by = "cell"))


# Zellen NDVI min und max extrahieren um ccf zu machen

NDVI_ts1_min <- bp_NDVI_all[bp_NDVI_all$cell == 85289,]
NDVI_ts2_min <- bp_NDVI_all[bp_NDVI_all$cell == 86824,]
NDVI_ts3_min <- bp_NDVI_all[bp_NDVI_all$cell == 80675,]
NDVI_ts4_min <- bp_NDVI_all[bp_NDVI_all$cell == 81187,]
NDVI_ts5_min <- bp_NDVI_all[bp_NDVI_all$cell == 82213,]
NDVI_ts6_min <- bp_NDVI_all[bp_NDVI_all$cell == 82214,]

NDVI_ts1_max <- bp_NDVI_all[bp_NDVI_all$cell == 69458,]
NDVI_ts2_max <- bp_NDVI_all[bp_NDVI_all$cell == 68945,]
NDVI_ts3_max <- bp_NDVI_all[bp_NDVI_all$cell == 69457,]
NDVI_ts4_max <- bp_NDVI_all[bp_NDVI_all$cell == 137861,]
NDVI_ts5_max <- bp_NDVI_all[bp_NDVI_all$cell == 138372,]
NDVI_ts6_max <- bp_NDVI_all[bp_NDVI_all$cell == 136833,]

# dazugehörige NDMI frames machen
# min
df1 <- dplyr::filter(bp_NDMI_all, cell ==85289| cell ==86824| cell ==80675 | cell ==81187 | cell ==82213 | cell ==82214)
df1 <- aggregate(data = df1, value ~ variable, 
                 FUN = "mean", 
                 na.action = na.pass )
# max
df2 <- dplyr::filter(bp_NDMI_all, cell ==69458| cell ==68945| cell ==69457 | cell ==137861 | cell ==138372 | cell ==136833)
df2 <- aggregate(data = df2, value ~ variable, 
                 FUN = "mean", 
                 na.action = na.pass )
df3 <- dplyr::filter(bp_NDVI_all, cell == 134299 | cell ==134301| 
                       cell ==133789| cell ==103080| cell ==103081| 
                       cell ==102568)
df3 <- aggregate(data = df3, value ~ variable, 
                 FUN = "mean", 
                 na.action = na.pass )

# ccf der ersten drei jeweils machen

par(mfrow = c(3,3))

ccf(prec_18$RS, NDVI_ts1_max$value[1:24], na.action = na.pass)
ccf(prec_18$RS, NDVI_ts2_max$value[1:24], na.action = na.pass)
ccf(prec_18$RS, NDVI_ts3_max$value[1:24], na.action = na.pass)
ccf(prec_19$RS, NDVI_ts1_max$value[25:48], na.action = na.pass)
ccf(prec_19$RS, NDVI_ts2_max$value[25:48], na.action = na.pass)
ccf(prec_19$RS, NDVI_ts3_max$value[25:48], na.action = na.pass)
ccf(prec_20$RS, NDVI_ts1_max$value[49:72], na.action = na.pass)
ccf(prec_20$RS, NDVI_ts2_max$value[49:72], na.action = na.pass)
ccf(prec_20$RS, NDVI_ts3_max$value[49:72], na.action = na.pass)

ccf(prec_18$RS, NDVI_ts1_min$value[1:24], na.action = na.pass)
ccf(prec_18$RS, NDVI_ts2_min$value[1:24], na.action = na.pass)
ccf(prec_18$RS, NDVI_ts3_min$value[1:24], na.action = na.pass)
ccf(prec_19$RS, NDVI_ts1_min$value[25:48], na.action = na.pass)
ccf(prec_19$RS, NDVI_ts2_min$value[25:48], na.action = na.pass)
ccf(prec_19$RS, NDVI_ts3_min$value[25:48], na.action = na.pass)
ccf(prec_20$RS, NDVI_ts1_min$value[49:72], na.action = na.pass)
ccf(prec_20$RS, NDVI_ts2_min$value[49:72], na.action = na.pass)
ccf(prec_20$RS, NDVI_ts3_min$value[49:72], na.action = na.pass)

# max_NDVI mit Niederschlag Kovarianz bei lag -2
# bei NDVI min und Niederschlag Kowarianz bei lag 0 


df_ts_NDVI_min <- rbind(NDVI_ts1_min,NDVI_ts2_min,NDVI_ts3_min,NDVI_ts4_min ,
                        NDVI_ts5_min ,NDVI_ts6_min )
df_ts_NDVI_max <- rbind(NDVI_ts1_max,NDVI_ts2_max,NDVI_ts3_max,NDVI_ts4_max ,
                        NDVI_ts5_max ,NDVI_ts6_max )

df_ts_NDVI_min$Jahr <- substring(df_ts_NDVI_min$variable, 1, 4)
df_ts_NDVI_max$Jahr <- substring(df_ts_NDVI_max$variable, 1, 4)

df_agg_ts_NDVI_min <- aggregate(data = df_ts_NDVI_min, value ~ variable+Jahr, 
                                FUN = "mean", 
                                na.action = na.pass, )
df_agg_ts_NDVI_max <- aggregate(data = df_ts_NDVI_max, value ~ variable+Jahr, 
                                FUN = "mean", 
                                na.action = na.pass, )

# das sind die NDVIs an den Zeitpunkten NDMI min
ts1_min_V <- bp_NDVI_all[bp_NDVI_all$cell == 106655,]
ts2_min_V <- bp_NDVI_all[bp_NDVI_all$cell == 106656,]
ts3_min_V <- bp_NDVI_all[bp_NDVI_all$cell == 106145,]
ts4_min_V <- bp_NDVI_all[bp_NDVI_all$cell == 110980,]
ts5_min_V <- bp_NDVI_all[bp_NDVI_all$cell == 106877,]
ts6_min_V <- bp_NDVI_all[bp_NDVI_all$cell == 110981,]

df_NDVI_trocken <- rbind(ts1_min_V, ts2_min_V, ts3_min_V, 
                         ts4_min_V, ts5_min_V, ts6_min_V )
df_NDVI_trocken$Jahr <- substring(df_NDVI_trocken$variable, 1, 4)
df_NDVI_trocken_agg <- aggregate(data = df_NDVI_trocken, value ~ variable+Jahr, FUN = "mean", 
                                 na.action = na.pass, )

ccf(c(prec_18$RS,prec_19$RS,prec_20$RS), df_NDVI_trocken_agg$value, na.action = na.pass)
# lag bei -1

df_NDVI_trocken_agg <- cbind(df_NDVI_trocken_agg, 
                             rbind(prec_18_neu2,prec_19_neu2,prec_20_neu2))

# Zusammenhaenge anschauenumd punkte fuer dummycodierung festzulegen
par(mfrow = c(2,2))
plot( c(prec_18$RS,prec_19$RS,prec_20$RS), df_agg_ts_NDVI_min$value)
plot( c(prec_18_neu$RS,prec_19_neu$RS,prec_20_neu$RS), df_agg_ts_NDVI_max$value)
plot( c(prec_18_neu$RS,prec_19_neu$RS,prec_20_neu$RS), df_agg_min$value)
plot( df_NDVI_trocken_agg$`RS lag -1`, df_NDVI_trocken_agg$value)

# Niederschlaege mit jeweiligen Indices in einen df

df_agg_NDMI_min <- cbind(df_agg_min, rbind(prec_18_neu,prec_19_neu,prec_20_neu))
df_agg_ts_NDVI_min <- cbind(df_agg_ts_NDVI_min, rbind(prec_18,prec_19,prec_20))
df_agg_ts_NDVI_max <- cbind(df_agg_ts_NDVI_max, rbind(prec_18_neu,prec_19_neu,prec_20_neu))

# jetzt dummy fuer jeden df

df_agg_NDMI_min$LR <- recode(df_agg_NDMI_min$RS, "0:8=1; 8.1:54.1=0")
df_agg_NDMI_min$MR <- recode(df_agg_NDMI_min$RS, "8.1:30=1; 0:8=0; 30.1:54.1=0")
df_agg_NDMI_min$SR <- recode(df_agg_NDMI_min$RS, "30.1:54.1=1; 0:30=0")

df_agg_ts_NDVI_min$LR <- recode(df_agg_ts_NDVI_min$RS, "0:8=1; 8.1:54.1=0")
df_agg_ts_NDVI_min$MR <- recode(df_agg_ts_NDVI_min$RS, "8.1:30=1; 0:8=0; 30.1:54.1=0")
df_agg_ts_NDVI_min$SR <- recode(df_agg_ts_NDVI_min$RS, "30.1:54.1=1; 0:30=0")

df_agg_ts_NDVI_max$LR <- recode(df_agg_ts_NDVI_max$RS, "0:8=1; 8.1:54.1=0")
df_agg_ts_NDVI_max$MR <- recode(df_agg_ts_NDVI_max$RS, "8.1:30=1; 0:8=0; 30.1:54.1=0")
df_agg_ts_NDVI_max$SR <- recode(df_agg_ts_NDVI_max$RS, "30.1:54.1=1; 0:30=0")

df_NDVI_trocken_agg$LR1 <- recode(df_NDVI_trocken_agg$`RS lag -1`, "0:8=1; 8.1:54.1=0")
df_NDVI_trocken_agg$MR1 <- recode(df_NDVI_trocken_agg$`RS lag -1`, "8.1:30=1; 0:8=0; 30.1:54.1=0")
df_NDVI_trocken_agg$SR1 <- recode(df_NDVI_trocken_agg$`RS lag -1`, "30.1:54.1=1; 0:30=0")


# NDMI, lag -2

df_agg_NDMI_min$NDVI <- df_NDVI_trocken_agg$value

# jetzt die Kalenderwoche
df_agg_NDMI_min$KW <-lubridate::isoweek(ymd(df_agg_NDMI_min$variable))
par(mfrow = c(2,2))
plot(df_agg_NDMI_min$KW, df_agg_NDMI_min$value) # zsmh schaut nicht linear aus
df_agg_NDMI_min$logKW <- log10(df_agg_NDMI_min$KW)
plot(df_agg_NDMI_min$logKW, df_agg_NDMI_min$value) # besser (wurde nachfolgend fuer alle gemacht)

# col Namen noch schoen machen 
NDMI_min_aggregiert <- df_agg_NDMI_min

colnames(NDMI_min_aggregiert) <- c( "Datum", "Jahr", "NDMI_min", "Datum RS", "RS",
                                    "Kein/ Kaum Regen", "MR",  "SR",  "NDVI", 
                                    "Kalenderwoche",                          
                                    "log_KW", "logKW2")

lm_NDMI_min <- lm(data = NDMI_min_aggregiert, `NDMI_min` ~ `MR` + 
                    `SR` + `log(KW)` + NDVI)
summary(lm_NDMI_min)

# voraussetzungen pruefen

crPlots(lm_NDMI_min, main = "component residual plot to check for linearity")
format(mean(lm_NDMI_min$residuals),scientific=F) # "-0.0000000000000000007777546"
sd(df_agg_NDMI_min$value, na.rm=T) # 0.07929459
par(mfrow = c(2,2))
plot(lm_NDMI_min, which=1)
plot(lm_NDMI_min, which=2)
plot(lm_NDMI_min, which=4)
acf(lm_NDMI_min$residuals)


# jetzt das ganze fuer NDVI machen
# lag -2

# NDMI Werte hinzufuegen

df_agg_ts_NDVI_max$NDMI <- df2$value

# jetzt die Kalenderwoche
df_agg_ts_NDVI_max$KW <-lubridate::isoweek(ymd(df_agg_ts_NDVI_max$variable))
df_agg_ts_NDVI_max$logKW <- log10(df_agg_ts_NDVI_max$KW)
plot(df_agg_ts_NDVI_max$logKW, df_agg_ts_NDVI_max$value)

# col Namen noch schoen machen 
NDVI_max_aggregiert <- df_agg_ts_NDVI_max

colnames(NDVI_max_aggregiert) <- c( "Datum", "Jahr", "NDVI_max", "Datum RS", "RS",
                                    "Kein/ Kaum Regen", "Mittlerer Regen",  "Starker Regen",                         
                                    "logRS", "NDMI", "Kalenderwoche",                          
                                    "log_KW")

lm_NDVI_max <- lm(data = NDVI_max_aggregiert, `NDVI_max` ~ `Mittlerer Regen` + 
                    `Starker Regen` + `log(KW)` + NDMI)
summary(lm_NDVI_max)


# dieses model gleich noch testen

crPlots(lm_NDVI_max, main = "component residual plot to check for linearity")

format(mean(lm_NDVI_max$residuals),scientific=F) # "0.00000000000000000038648"

sd(lm_NDVI_max$value, na.rm=T) #  0.128398

plot(lm_NDVI_max, which=1)
plot(lm_NDVI_max, which=2)
plot(lm_NDVI_max, which=4)
acf(lm_NDVI_max$residuals)


# NDVI trocken bei lag -1

# NDMI Werte hinzufuegen

df_NDVI_trocken_agg$NDMI <- df_agg_min$value

# jetzt die Kalenderwoche
df_NDVI_trocken_agg$KW <-lubridate::isoweek(ymd(df_NDVI_trocken_agg$variable))
df_NDVI_trocken_agg$logKW <- log10(df_NDVI_trocken_agg$KW)
plot(df_NDVI_trocken_agg$logKW, df_NDVI_trocken_agg$value)

# col Namen noch schoen machen 
NDVI_trocken_aggregiert <- df_NDVI_trocken_agg

colnames(NDVI_trocken_aggregiert) <- c( "Datum", "Jahr", "NDVI_trocken", "Datum RS", 
                                        "RS", "Kein/ Kaum Regen", "MR",  
                                        "SR", "logRS",  "NDMI", 
                                        "Kalenderwoche", "log_KW")

lm_NDVI_trocken <- lm(data = NDVI_trocken_aggregiert, `NDVI_trocken` ~ `MR` + 
                        `SR` + `log_KW` + NDMI)
summary(lm_NDVI_trocken)

# dieses model gleich noch testen

crPlots(lm_NDVI_trocken, main = "component residual plot to check for linearity")

format(mean(lm_NDVI_trocken$residuals),scientific=F) # "-0.000000000000000002041439"

sd(lm_NDVI_trocken, na.rm=T) #  0.1164032

plot(lm_NDVI_trocken, which=1)
plot(lm_NDVI_trocken, which=2)
plot(lm_NDVI_trocken, which=4)
acf(lm_NDVI_trocken$residuals)


# NDVI min lag 0

# NDMI Werte hinzufuegen

df_agg_ts_NDVI_min$NDMI <- df1$value
plot( df_agg_ts_NDVI_min$NDMI, df_agg_ts_NDVI_min$value) # schaut aus wie ein linearer zsmh
# da brauch ich also nichts mehr machen, bombig

# jetzt die Kalenderwoche
df_agg_ts_NDVI_min$KW <-lubridate::isoweek(ymd(df_agg_ts_NDVI_min$variable))
df_agg_ts_NDVI_min$logKW <- log10(df_agg_ts_NDVI_min$KW)
plot(df_agg_ts_NDVI_min$logKW, df_agg_ts_NDVI_min$value)

# col Namen noch schoen machen 
NDVI_min_aggregiert <-df_agg_ts_NDVI_min

colnames(NDVI_min_aggregiert) <- c( "Datum", "Jahr", "NDVI_min", "Datum RS", 
                                    "RS", "Kein/ Kaum Regen", "MR",  
                                    "SR", "logRS",  "NDMI", 
                                    "Kalenderwoche", "log_KW")

lm_NDVI_min <- lm(data = NDVI_min_aggregiert, `NDVI_min` ~ `MR` + 
                    `SR` + `log_KW` + NDMI)
summary(lm_NDVI_min)

# pruefungen

crPlots(lm_NDVI_min, main = "component residual plot to check for linearity")

format(mean(lm_NDVI_min$residuals),scientific=F) # "-0.000000000000000002041439"

sd(df_agg_ts_NDVI_min$value, na.rm=T) #  0.1164032

plot(lm_NDVI_min, which=1)
plot(lm_NDVI_min, which=2)
plot(lm_NDVI_min, which=4)
acf(lm_NDVI_min$residuals)

# NDMI max lag bei 0 
# schaut man sich die Scatterplots an sieht das nicht nach einem
#linearen Zusammenhang aus

df_max_NDMI <- rbind(ts1_max, ts2_max, ts3_max, ts4_max, ts5_max, 
                     ts6_max)
df_NDMI_max_agg <- aggregate(data = df_max_NDMI, value ~ variable, FUN = "mean", 
                             na.action = na.pass, )
ccf( c(prec_18$RS,prec_19$RS,prec_20$RS), df_NDMI_max_agg$value, na.action = na.pass)

# Regen bei lag hinzufuegen

df_NDMI_max_agg <- cbind(df_NDMI_max_agg, df_agg_ts_NDVI_min[,4:8])

# NDVI Werte hinzufuegen

df_NDMI_max_agg$NDVI <- df3$value
plot( df_NDMI_max_agg$NDVI, df_NDMI_max_agg$value) 

# jetzt die Kalenderwoche
df_NDMI_max_agg$KW <-lubridate::isoweek(ymd(df_NDMI_max_agg$variable))
df_NDMI_max_agg$logKW <- log10(df_NDMI_max_agg$KW)
plot(df_NDMI_max_agg$logKW, df_NDMI_max_agg$value)


# fuer stargazer abbildung in r markdown

stargazer(lm_NDMI_min, lm_NDVI_trocken, lm_NDVI_min, lm_NDVI_max, allign = TRUE,
          type = "html", out = "fit_lm.html")