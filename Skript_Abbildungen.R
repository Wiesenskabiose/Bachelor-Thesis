
# weiterfuehrendes

# Darstellungen der Boxplots 
# Trennung der UG Namen fuer Darstellung mit ggplot

boxplot_bin_18_NDMI
colnames(boxplot_bin_18_NDMI) <- c("cell", "ID", "variable", "value", "UG_NDMI")
boxplot_bin_18_NDMI$UG_NDMI[boxplot_bin_18_NDMI$UG_NDMI == "Barnstorfer Moor"] <- "NDMI B"
boxplot_bin_18_NDMI$UG_NDMI[boxplot_bin_18_NDMI$UG_NDMI == "Vechtaer Moor"] <- "NDMI V"
boxplot_bin_18_NDMI$UG_NDMI[boxplot_bin_18_NDMI$UG_NDMI == "Goldenstedter Moor"] <- "NDMI G"

boxplot_bin_19_NDMI
colnames(boxplot_bin_19_NDMI) <- c("cell", "ID", "variable", "value", "UG_NDMI")
boxplot_bin_19_NDMI$UG_NDMI[boxplot_bin_19_NDMI$UG_NDMI == "Barnstorfer Moor"] <- "NDMI B"
boxplot_bin_19_NDMI$UG_NDMI[boxplot_bin_19_NDMI$UG_NDMI == "Vechtaer Moor"] <- "NDMI V"
boxplot_bin_19_NDMI$UG_NDMI[boxplot_bin_19_NDMI$UG_NDMI == "Goldenstedter Moor"] <- "NDMI G"

boxplot_bin_20_NDMI
colnames(boxplot_bin_20_NDMI) <- c("cell", "ID", "variable", "value", "UG_NDMI")
boxplot_bin_20_NDMI$UG_NDMI[boxplot_bin_20_NDMI$UG_NDMI == "Barnstorfer Moor"] <- "NDMI B"
boxplot_bin_20_NDMI$UG_NDMI[boxplot_bin_20_NDMI$UG_NDMI == "Vechtaer Moor"] <- "NDMI V"
boxplot_bin_20_NDMI$UG_NDMI[boxplot_bin_20_NDMI$UG_NDMI == "Goldenstedter Moor"] <- "NDMI G"


b1 <- ggplot()+
  geom_boxplot(data = boxplot_bin_18_NDMI, aes(y= value, x= variable, 
                                 fill = factor(UG_NDMI), 
                                 group = interaction(variable, UG_NDMI ) ))+
  geom_boxplot(data = boxplot_bin_18_NDVI, aes(y= value, x= variable, 
                                          fill = factor(UG), 
                                          group = interaction(variable, UG ) ))+
  theme(axis.text.x = element_text(angle=45, hjust = 1), axis.title.x=element_blank())+
  ylab("NDVI- und NDMI-Werte")+theme( legend.position = "none")+
  scale_fill_manual(values = c("yellow", "darkgoldenrod1","darkorchid3", "slateblue3", 
                               "mediumpurple1", "lemonchiffon"))+
  scale_y_continuous(breaks = seq(-0.3, 0.9, 0.3))+
  theme(axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11))
b2 <- ggplot()+
  geom_boxplot(data = boxplot_bin_19_NDMI, aes(y= value, x= variable, 
                                  fill = factor(UG_NDMI), 
                                  group = interaction(variable, UG_NDMI ) ))+
  geom_boxplot(data = boxplot_bin_19_NDVI, aes(y= value, x= variable, 
                                          fill = factor(UG), 
                                          group = interaction(variable, UG ) ))+
  theme(axis.text.x = element_text(angle=45, hjust = 1), axis.title.x=element_blank())+
  ylab("NDVI-/NDMI-Werte")+theme( legend.position = "none")+
  scale_fill_manual(values = c("yellow", "darkgoldenrod1","darkorchid3", "slateblue3", 
                               "mediumpurple1", "lemonchiffon"))+
  scale_y_continuous(breaks = seq(-0.3, 0.9, 0.3))+
  theme(axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11))
b3 <- ggplot()+
  geom_boxplot(data = boxplot_bin_20_NDMI, aes(y= value, x= variable, 
                                  fill = factor(UG_NDMI), 
                                  group = interaction(variable, UG_NDMI ) ))+
  geom_boxplot(data = boxplot_bin_20_NDVI, aes(y= value, x= variable, 
                                          fill = factor(UG), 
                                          group = interaction(variable, UG ) ))+
  theme(axis.text.x = element_text(angle=45, hjust = 1))+
  xlab("Datum")+ ylab("NDVI-/NDMI-Werte")+theme( legend.position = "none")+
  scale_fill_manual(values = c("yellow", "darkgoldenrod1","darkorchid3", "slateblue3", 
                               "mediumpurple1", "lemonchiffon")) +
  scale_y_continuous(breaks = seq(-0.3, 0.9, 0.3))     +
  theme(axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 11),
        axis.text.y = element_text(size = 11))      

b1/b2/b3



#Abbildung des Niederschlages NDVI und NDMI gemeinsam
ggplot()+
  geom_segment(prec_neu, mapping = aes(x =MESS_DATUM, y = RS),
               color = "skyblue", yend = -0.2, xend= prec_neu$MESS_DATUM,
               size = 1)+
  geom_line(ts_NDVI[!is.na(ts_NDVI$value),], mapping = aes(y = value, x = variable, 
                                                           group = UG), color = "darkgoldenrod1", size = 1.5)+
  geom_line(ts_NDMI[!is.na(ts_NDMI$value),], mapping = aes(y = value, x = variable, 
                                                           group = UG), color = "slateblue3", size = 1.5)+
  geom_point(ts_NDMI, mapping = aes(y = value, x = variable, 
                                    shape = UG), size =2.5)+
  geom_point(ts_NDVI, mapping = aes(y = value, x = variable, 
                                    shape = UG), size = 2.5)+
  scale_y_continuous(sec.axis = sec_axis(trans = trans, breaks = seq(0,55,5)))+
  scale_x_break(breaks = c(as.POSIXct("2018-10-01"),as.POSIXct("2019-04-01")))+
  scale_x_break(breaks = c(as.POSIXct("2019-10-01"),as.POSIXct("2020-04-01")))+
  scale_x_datetime(labels = date_format("%b-%y"), date_minor_breaks = "1 month")+
  labs(x = "Datum", y = "NDVI und NDMI", legend = element_blank(), 
       title = "Zeitreihe des NDVI, NDMI und Niederschlages von April bis Oktober in den Jahren 2018, 2019 und 2020")+
  scale_shape_manual(values = c(17,19,15), name = NULL)+
  theme_light()+
  theme(plot.title = element_text(size=22, hjust = 0.3),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        legend.text = element_text(size = 18))+
  guides(shape = guide_legend(override.aes = list(size = 4)))


# Abbildung der Scatterplots NDVI gegen NDMI pro Jahr und Teilmoor

h1 <- ggplot(data = b18[b18$UG_NDVI== "Vechtaer Moor",], mapping = aes(x= value_NDVI, y = value_NDMI))+
  geom_hex(bins = 45)+
  guides(fill = guide_colourbar(label = FALSE,
                                ticks = FALSE))+
  xlab("NDVI-Werte")+ ylab("NDMI-Werte")+
  scale_fill_viridis_c(option = "mako")+
  geom_smooth(method='lm', formula= y~x, se = FALSE, color = "coral2")
h2 <- ggplot(data = b18[b18$UG_NDVI== "Barnstorfer Moor",], mapping = aes(x= value_NDVI, y = value_NDMI))+
  geom_hex(bins = 45)+
  guides(fill = guide_colourbar(label = FALSE,
                                ticks = FALSE))+
  xlab("NDVI-Werte")+ ylab("NDMI-Werte")+
  scale_fill_viridis_c(option = "mako")
h3 <- ggplot(data = b18[b18$UG_NDVI== "Goldenstedter Moor",], mapping = aes(x= value_NDVI, y = value_NDMI))+
  geom_hex(bins = 45)+
  guides(fill = guide_colourbar(label = FALSE,
                                ticks = FALSE))+
  xlab("NDVI-Werte")+ ylab("NDMI-Werte")+
  scale_fill_viridis_c(option = "mako")
h4 <- ggplot(data = b19[b19$UG_NDVI== "Vechtaer Moor",], mapping = aes(x= value_NDVI, y = value_NDMI))+
  geom_hex(bins = 45)+
  guides(fill = guide_colourbar(label = FALSE,
                                ticks = FALSE))+
  xlab("NDVI-Werte")+ ylab("NDMI-Werte")+
  scale_fill_viridis_c(option = "mako")
h5 <- ggplot(data = b19[b19$UG_NDVI== "Barnstorfer Moor",], mapping = aes(x= value_NDVI, y = value_NDMI))+
  geom_hex(bins = 45)+
  guides(fill = guide_colourbar(label = FALSE,
                                ticks = FALSE))+
  xlab("NDVI-Werte")+ ylab("NDMI-Werte")+
  scale_fill_viridis_c(option = "mako")
h6 <- ggplot(data = b19[b19$UG_NDVI== "Goldenstedter Moor",], mapping = aes(x= value_NDVI, y = value_NDMI))+
  geom_hex(bins = 45)+
  guides(fill = guide_colourbar(label = FALSE,
                                ticks = FALSE))+
  xlab("NDVI-Werte")+ ylab("NDMI-Werte")+
  scale_fill_viridis_c(option = "mako")
h7 <- ggplot(data = b20[b20$UG_NDVI== "Vechtaer Moor",], mapping = aes(x= value_NDVI, y = value_NDMI))+
  geom_hex(bins = 45)+
  guides(fill = guide_colourbar(label = FALSE,
                                ticks = FALSE))+
  xlab("NDVI-Werte")+ ylab("NDMI-Werte")+
  scale_fill_viridis_c(option = "mako")
h8 <- ggplot(data = b20[b20$UG_NDVI== "Barnstorfer Moor",], mapping = aes(x= value_NDVI, y = value_NDMI))+
  geom_hex(bins = 45)+
  guides(fill = guide_colourbar(label = FALSE,
                                ticks = FALSE))+
  xlab("NDVI-Werte")+ ylab("NDMI-Werte")+
  scale_fill_viridis_c(option = "mako")
h9 <- ggplot(data = b20[b20$UG_NDVI== "Goldenstedter Moor",], mapping = aes(x= value_NDVI, y = value_NDMI))+
  geom_hex(bins = 45)+
  guides(fill = guide_colourbar(label = FALSE,
                                ticks = FALSE))+
  xlab("NDVI-Werte")+ ylab("NDMI-Werte")+
  scale_fill_viridis_c(option = "mako")

ggarrange(h1, h2, h3, h4, h5, h6, h7, h8, h9, 
          labels = c("2018 Vechtaer Moor", "2018 Barnstorfer Moor", "2018 Goldenstedter Moor",
                     "2019 Vechtaer Moor", "2019 Barnstorfer Moor", "2019 Goldenstedter Moor",
                     "2020 Vechtaer Moor", "2020 Barnstorfer Moor", "2020 Goldenstedter Moor"),
          ncol = 3, nrow = 3,
          font.label = list(size = 10), hjust = c(-0.5), vjust = c(2))

# QQplot fuer alle Daten

par(mfrow = c(2,3))
a <- ggqqplot(vV_melt_20$value, y = "NDVI", title = "Vechtaer Moor, Jahr 2020")
b <- ggqqplot(oV_melt_20$value, y = "NDVI", title = "Barnstorfer Moor, Jahr 2020")
c <- ggqqplot(gV_melt_20$value, y = "NDVI", title = "Goldenstedter Moor, Jahr 2020")
d <- ggqqplot(vM_melt_20$value, y = "NDMI", title = "Vechtaer Moor, Jahr 2020")
e <- ggqqplot(oM_melt_20$value, y = "NDMI", title = "Barnstorfer Moor, Jahr 2020")
f <- ggqqplot(gM_melt_20$value, y = "NDMI", title = "Goldenstedter Moor, Jahr 2020")

ggarrange(a, b, c, d, e, f, ncol = 3, nrow = 2)

a <- ggqqplot(vV_melt_19$value, y = "NDVI", title = "Vechtaer Moor, Jahr 2019")
b <- ggqqplot(oV_melt_19$value, y = "NDVI", title = "Barnstorfer Moor, Jahr 2019")
c <- ggqqplot(gV_melt_19$value, y = "NDVI", title = "Goldenstedter Moor, Jahr 2019")
d <- ggqqplot(vM_melt_19$value, y = "NDMI", title = "Vechtaer Moor, Jahr 2019")
e <- ggqqplot(oM_melt_19$value, y = "NDMI", title = "Barnstorfer Moor, Jahr 2019")
f <- ggqqplot(gM_melt_19$value, y = "NDMI", title = "Goldenstedter Moor, Jahr 2019")

ggarrange(a, b, c, d, e, f, ncol = 3, nrow = 2)

a <- ggqqplot(vV_melt_18$value, y = "NDVI", title = "Vechtaer Moor, Jahr 2018")
b <- ggqqplot(oV_melt_18$value, y = "NDVI", title = "Barnstorfer Moor, Jahr 2018")
c <- ggqqplot(gV_melt_18$value, y = "NDVI", title = "Goldenstedter Moor, Jahr 2018")
d <- ggqqplot(vM_melt_18$value, y = "NDMI", title = "Vechtaer Moor, Jahr 2018")
e <- ggqqplot(oM_melt_18$value, y = "NDMI", title = "Barnstorfer Moor, Jahr 2018")
f <- ggqqplot(gM_melt_18$value, y = "NDMI", title = "Goldenstedter Moor, Jahr 2018")

ggarrange(a, b, c, d, e, f, ncol = 3, nrow = 2)
