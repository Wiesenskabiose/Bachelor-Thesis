#Stationsniederschlagsdaten DWD
#Station Vechta

setwd("YourPath")

st_vechta <- read.csv("produkt_nieder_tag_19310101_20201231_05200.txt",
                      header = TRUE, sep = ";", dec = ".")
st_vechta <- as.POSIXct(as.numeric(as.character(st_vechta$MESS_DATUM)),origin = "%Y-%m-%d")
st_vechta_18 <- as.data.frame(st_vechta[31138:31321,])
squi <- seq(as.POSIXct("01-04-2018", "%d-%m-%Y", tz = "UTC"), as.POSIXct("01-10-2018", "%d-%m-%Y", tz = "UTC"), 
            by = "day", format = "%d-%m-%Y")
st_vechta_18$MESS_DATUM <- squi


st_vechta_19 <- as.data.frame(st_vechta[31503:31686,])
squi <- seq(as.POSIXct("01-04-2019", "%d-%m-%Y", tz = "UTC"), as.POSIXct("01-10-2019", "%d-%m-%Y", tz = "UTC"), 
            by = "day", format = "%d-%m-%Y")
st_vechta_19$MESS_DATUM <- squi

st_vechta_20 <- as.data.frame(st_vechta[31869:32052,])
squi <- seq(as.POSIXct("01-04-2020", "%d-%m-%Y", tz = "UTC"), as.POSIXct("01-10-2020", "%d-%m-%Y", tz = "UTC"), 
            by = "day", format = "%d-%m-%Y")
st_vechta_20$MESS_DATUM <- squi

# Niederschlag zu 7 Tage Periode agreggieren

prec_20 <- aggregate(RS ~ cut(MESS_DATUM, "7 days"), st_vechta_20[11:178,], sum)
prec_19 <- aggregate(RS ~ cut(MESS_DATUM, "7 days"), st_vechta_19[9:176,], sum)
prec_18 <- aggregate(RS ~ cut(MESS_DATUM, "7 days"), st_vechta_18[9:176,], sum)


# Gesamtmegen des Niederschlages fuer einen von 
# Mitte April bis Ende September

sum(st_vechta$RS[30418:30585]) # 2016: 318
sum(st_vechta$RS[30783:30950]) # 2017: 386.9
sum(st_vechta_18$RS[9:176]) # 2018: 280.2
sum(st_vechta_19$RS[9:176]) # 2019: 248.4
sum(st_vechta_20$RS[11:178]) # 2020: 270.1