
## getting the Dataframes ready, Beispielhaft duer 2020

# create Rasterstacks for NDMI and NDVI each
NDVI_20 <- stack(list.files(path = "YourPath", pattern = "NDVI*", full.names = T))
NDMI_20 <- stack(list.files(path = "YourPath", pattern = "NDMI*", full.names = T))

# shape from the Grosses Moor bei Barnstorf from OpenStreetMap
shape <- shapefile("GrossesMoor.shp")

# mask Rasterstacks to the shape of AOI
NDVI.masked <- mask(x = NDVI, mask = shape)
NDMI.masked <- mask(NDMI, shape)

#view data in order to figure out the polygon areas for dataframes
mapview::mapview(NDVI.masked[[1]])+mapview::mapview(NDMI.masked[[1]], map.types="Esri.WorldImagery", na.color="#00000000")
mapview::mapview(NDVI.masked[[16]])+mapview::mapview(NDMI.masked[[16]], map.types="Esri.WorldImagery", na.color="#00000000")

# converting some rasters into dataframes for plotting them nicely

# example for NDVI
NDVI_pts <- rasterToPoints(NDVI.masked[[16]], spatial = TRUE)
NDVI_df <- data.frame(NDVI_pts)
ggplot() +
  geom_raster(data = NDVI_df, aes(x =x, y=y, fill = NDVI2020.07.31)) +
  ggtitle("NDVI 31.07.2020")+
  scale_fill_viridis(option = "viridis", "NDVI")
ggsave(filename = "NDVI 18092020", device = "png")

# example for NDMI
NDMI_pts <- rasterToPoints(NDMI.masked[[23]], spatial = TRUE)
NDMI_df <- data.frame(NDMI_pts)
ggplot() +
  geom_raster(data = NDVI_df, aes(x =x, y=y, fill = NDVI_df)) +
  ggtitle("NDMI 18.09.2020")+
  scale_fill_viridis(option = "magma", direction = -1, "NDMI")
ggsave(filename = "NDMI 18092020", device = "png")


# decided for polygon areas
# designed polygons in QGIS and exported as ESRI Shapefile

# load shapefiles and transform them to crs from RasterStack
osten <- shapefile("oestliche Flaechen.shp")
osten <- spTransform(osten, "+proj=utm +zone=32 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
goldenstedt <- shapefile("Goldenstedter Moor.shp")
goldenstedt <- spTransform(goldenstedt, "+proj=utm +zone=32 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")
vechta <- shapefile("Vechtaer Moor.shp")
vechta <- spTransform(vechta, "+proj=utm +zone=32 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs")

# function for setting NAN values from Raster to NA in the dataframe
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

# extract 
vechta_NDVI_2020 <- raster::extract(NDVI_2020, vechta, method = "simple",df = TRUE) 
vechta_NDVI_2020[is.nan(vechta_NDVI_2020)] <- NA
osten_NDVI_2020 <- raster::extract(NDVI_2020, osten, method = "simple", df = TRUE)
osten_NDVI_2020[is.nan(osten_NDVI_2020)] <- NA
goldenstedt_NDVI_2020 <- raster::extract(NDVI_2020, goldenstedt, method = "simple", df = TRUE)
goldenstedt_NDVI_2020[is.nan(goldenstedt_NDVI_2020)] <- NA

vechta_NDMI_2020 <- raster::extract(NDMI_2020, vechta, method = "simple", df = TRUE)
vechta_NDMI_2020[is.nan(vechta_NDMI_2020)] <- NA
osten_NDMI_2020 <- raster::extract(NDMI_2020, osten, method = "simple", df = TRUE)
osten_NDMI_2020[is.nan(osten_NDMI_2020)] <- NA
goldenstedt_NDMI_2020 <- raster::extract(NDMI_2020, goldenstedt, method = "simple", df = TRUE)
goldenstedt_NDMI_2020[is.nan(goldenstedt_NDMI_2020)] <- NA


#NDVI
vechta_NDVI_2020$NDVI2020.05.22 <- NA
vechta_NDVI_2020$NDVI2020.06.05 <- NA
vechta_NDVI_2020$NDVI2020.07.17[which(vechta_NDVI_2020$ID == 2|3|4)] <- NA
vechta_NDVI_2020$NDVI2020.07.24 <- NA
vechta_NDVI_2020$NDVI2020.08.14 <- NA
vechta_NDVI_2020$NDVI2020.09.04 <- NA

osten_NDVI_2020$NDVI2020.05.22[which(osten_NDVI_2020$ID ==3)] <- NA
osten_NDVI_2020$NDVI2020.06.05 <- NA
osten_NDVI_2020$NDVI2020.07.03[which(osten_NDVI_2020$ID ==1 | 2)] <- NA
osten_NDVI_2020$NDVI2020.07.17 <- NA
osten_NDVI_2020$NDVI2020.07.24[which(osten_NDVI_2020$ID ==1)] <- NA
osten_NDVI_2020$NDVI2020.08.14 <- NA
osten_NDVI_2020$NDVI2020.08.21 <- NA

goldenstedt_NDVI_2020$NDVI2020.06.05 <- NA
goldenstedt_NDVI_2020$NDVI2020.07.10[which(goldenstedt_NDVI_2020$ID ==1|4|5)] <- NA
goldenstedt_NDVI_2020$NDVI2020.07.17[which(goldenstedt_NDVI_2020$ID ==1|4)] <- NA
goldenstedt_NDVI_2020$NDVI2020.07.24[which(goldenstedt_NDVI_2020$ID ==1)] <- NA
goldenstedt_NDVI_2020$NDVI2020.08.14 <- NA
goldenstedt_NDVI_2020$NDVI2020.08.21 <- NA


colnames(vechta_NDVI_2020) <- c("ID", "17.04.2020", "24.04.2020", "01.05.2020", "08.05.2020",
                                "15.05.2020", "22.05.2020", "29.05.2020", "05.06.2020", "12.06.2020", "19.06.2020",
                                "26.06.2020", "03.07.2020", "10.07.2020", "17.07.2020", "24.07.2020", "31.07.2020",
                                "07.08.2020", "14.08.2020", "21.08.2020", "28.08.2020", "04.09.2020", "11.09.2020",
                                "18.09.2020", "25.09.2020")
colnames(osten_NDVI_2020) <- c("ID", "17.04.2020", "24.04.2020", "01.05.2020", "08.05.2020",
                               "15.05.2020", "22.05.2020", "29.05.2020", "05.06.2020", "12.06.2020", "19.06.2020",
                               "26.06.2020", "03.07.2020", "10.07.2020", "17.07.2020", "24.07.2020", "31.07.2020",
                               "07.08.2020", "14.08.2020", "21.08.2020", "28.08.2020", "04.09.2020", "11.09.2020",
                               "18.09.2020", "25.09.2020")
colnames(goldenstedt_NDVI_2020) <- c("ID", "17.04.2020", "24.04.2020", "01.05.2020", "08.05.2020",
                                     "15.05.2020", "22.05.2020", "29.05.2020", "05.06.2020", "12.06.2020", "19.06.2020",
                                     "26.06.2020", "03.07.2020", "10.07.2020", "17.07.2020", "24.07.2020", "31.07.2020",
                                     "07.08.2020", "14.08.2020", "21.08.2020", "28.08.2020", "04.09.2020", "11.09.2020",
                                     "18.09.2020", "25.09.2020")

#NDMI
vechta_NDMI_2020$NDMI2020.05.22 <- NA
vechta_NDMI_2020$NDMI2020.06.05 <- NA
vechta_NDMI_2020$NDMI2020.07.24 <- NA
vechta_NDMI_2020$NDMI2020.08.14 <- NA
vechta_NDMI_2020$NDMI2020.09.04 <- NA

osten_NDMI_2020$NDMI2020.05.22[which(osten_NDMI_2020$ID ==3)] <- NA
osten_NDMI_2020$NDMI2020.06.05 <- NA
osten_NDMI_2020$NDMI2020.07.03[which(osten_NDMI_2020$ID ==1|2)] <- NA
osten_NDMI_2020$NDMI2020.07.17 <- NA
osten_NDMI_2020$NDMI2020.07.24[which(osten_NDMI_2020$ID ==1)] <- NA
osten_NDMI_2020$NDMI2020.08.14 <- NA
osten_NDMI_2020$NDMI2020.08.21 <- NA

goldenstedt_NDMI_2020$NDMI2020.06.05 <- NA
goldenstedt_NDMI_2020$NDMI2020.07.10[which(goldenstedt_NDMI_2020$ID ==1|4|5)] <- NA
goldenstedt_NDMI_2020$NDMI2020.07.17[which(goldenstedt_NDMI_2020$ID ==1|4)] <- NA
goldenstedt_NDMI_2020$NDMI2020.07.24[which(goldenstedt_NDMI_2020$ID ==1)] <- NA
goldenstedt_NDMI_2020$NDMI2020.08.14 <- NA
goldenstedt_NDMI_2020$NDMI2020.08.21 <- NA

colnames(vechta_NDMI_2020) <- c("ID", "17.04.2020", "24.04.2020", "01.05.2020", "08.05.2020",
                                "15.05.2020", "22.05.2020", "29.05.2020", "05.06.2020", "12.06.2020", "19.06.2020",
                                "26.06.2020", "03.07.2020", "10.07.2020", "17.07.2020", "24.07.2020", "31.07.2020",
                                "07.08.2020", "14.08.2020", "21.08.2020", "28.08.2020", "04.09.2020", "11.09.2020",
                                "18.09.2020", "25.09.2020")
colnames(osten_NDMI_2020) <- c("ID", "17.04.2020", "24.04.2020", "01.05.2020", "08.05.2020",
                               "15.05.2020", "22.05.2020", "29.05.2020", "05.06.2020", "12.06.2020", "19.06.2020",
                               "26.06.2020", "03.07.2020", "10.07.2020", "17.07.2020", "24.07.2020", "31.07.2020",
                               "07.08.2020", "14.08.2020", "21.08.2020", "28.08.2020", "04.09.2020", "11.09.2020",
                               "18.09.2020", "25.09.2020")
colnames(goldenstedt_NDMI_2020) <- c("ID", "17.04.2020", "24.04.2020", "01.05.2020", "08.05.2020",
                                     "15.05.2020", "22.05.2020", "29.05.2020", "05.06.2020", "12.06.2020", "19.06.2020",
                                     "26.06.2020", "03.07.2020", "10.07.2020", "17.07.2020", "24.07.2020", "31.07.2020",
                                     "07.08.2020", "14.08.2020", "21.08.2020", "28.08.2020", "04.09.2020", "11.09.2020",
                                     "18.09.2020", "25.09.2020")


vV_melt_20 <- melt(vechta_NDVI_2020, id.vars = c("ID") )
oV_melt_20 <- melt(osten_NDVI_2020, id.vars = c("ID") )
gV_melt_20 <- melt(goldenstedt_NDVI_2020, id.vars = c("ID") )

vV_melt_20$variable <- as.POSIXct( x = vV_melt_20$variable, "%d.%m.%Y", tz = "UTC")
oV_melt_20$variable <- as.POSIXct( x = oV_melt_20$variable, "%d.%m.%Y", tz = "UTC")
gV_melt_20$variable <- as.POSIXct( x = gV_melt_20$variable, "%d.%m.%Y", tz = "UTC")

vM_melt_20 <- melt(vechta_NDMI_2020, id.vars = c("ID") )
oM_melt_20 <- melt(osten_NDMI_2020, id.vars = c("ID") )
gM_melt_20 <- melt(goldenstedt_NDMI_2020, id.vars = c("ID") )

vM_melt_20$variable <- as.POSIXct( x = vM_melt_20$variable, "%d.%m.%Y", tz = "UTC")
oM_melt_20$variable <- as.POSIXct( x = oM_melt_20$variable, "%d.%m.%Y", tz = "UTC")
gM_melt_20$variable <- as.POSIXct( x = gM_melt_20$variable, "%d.%m.%Y", tz = "UTC")


ggplot()+
  geom_boxplot(data = oV_melt_20, aes(y= value, x= variable, fill = factor(ID), group = factor(variable) ))+
  theme(axis.text.x = element_text(angle=90))+
  ggtitle("NDVI-Werte des Jahres 2020 von fünf Standorten im Teil Drebbersches Moor")+
  xlab("Datum")+ ylab("NDVI") + labs(fill = "Standorte")

## alle Orte (NDVI&NDMI) in einem Plot mit unterschiedlichen Formen 

vV_stat_20 <- summarySE(vV_melt_20, measurevar = "value", groupvars = "variable", na.rm = T)
vV_stat_20$UG <- "Vechtaer Moor"
oV_stat_20 <- summarySE(oV_melt_20, measurevar = "value", groupvars = "variable", na.rm = T)
oV_stat_20$UG <- "Barnstorfer Moor"
gV_stat_20 <- summarySE(gV_melt_20, measurevar = "value", groupvars = "variable", na.rm = T)
gV_stat_20$UG <- "Goldenstedter Moor"
NDVI_2020_bind <- rbind(vV_stat_20, oV_stat_20, gV_stat_20)


vM_stat_20 <- summarySE(vM_melt_20, measurevar = "value", groupvars = "variable")
vM_stat_20$UG <- "Vechtaer Moor"
oM_stat_20 <- summarySE(oM_melt_20, measurevar = "value", groupvars = "variable")
oM_stat_20$UG <- "Barnstorfer Moor"
gM_stat_20 <- summarySE(gM_melt_20, measurevar = "value", groupvars = "variable")
gM_stat_20$UG <- "Goldenstedter Moor"
NDMI_2020_bind <- rbind(vM_stat_20, oM_stat_20, gM_stat_20)

# Darstellung der Means pro Jahr, getrennte Teilmoore
ggplot()+
  geom_line(NDVI_2020_bind[!is.na(NDVI_2020_bind$value),], mapping = aes(x = variable, 
                                                                         y = value, group = UG ))+
  theme(axis.text.x = element_text(angle=90))+
  geom_point(NDVI_2020_bind, mapping = aes(x = variable, y = value,  colour = factor(UG) ))+
  geom_line(NDMI_2020_bind[!is.na(NDMI_2020_bind$value),], mapping = aes(x = variable, 
                                                                         y = value, group = UG ))+
  geom_point(NDMI_2020_bind, mapping = aes(x = variable, y = value,  colour = factor(UG) ))

# erstellen von boxplots mit gesammelten standorten (alle Daten)

vV_bind20 <-vV_melt_20
oV_bind20 <- oV_melt_20
gV_bind20 <- gV_melt_20

vV_bind20$UG <- "Vechtaer Moor"
oV_bind20$UG <- "Barnstorfer Moor"
gV_bind20$UG <- "Goldenstedter Moor"

boxplot_bin_20_NDVI <- rbind(vV_bind20, oV_bind20, gV_bind20)

ggplot()+
  geom_boxplot(data = boxplot_bin_20_NDVI, aes(y= value, x= as.character(variable), fill = factor(UG) ))+
  theme(axis.text.x = element_text(angle=90))+
  ggtitle("NDVI-Werte des Jahres 2020")+
  xlab("Datum")+ ylab("NDVI")+theme(legend.title = element_blank())+
  scale_fill_manual(values = c("darkgoldenrod1", "yellow2", "lightgoldenrod2"))


vM_bind20 <-vM_melt_20
oM_bind20 <- oM_melt_20
gM_bind20 <- gM_melt_20

vM_bind20$UG <- "Vechtaer Moor"
oM_bind20$UG <- "Barnstorfer Moor"
gM_bind20$UG <- "Goldenstedter Moor"

boxplot_bin_20_NDMI <- rbind(vM_bind20, oM_bind20, gM_bind20)

ggplot()+
  geom_boxplot(data = boxplot_bin_20_NDMI, aes(y= value, x= as.character(variable), fill = factor(UG) ))+
  theme(axis.text.x = element_text(angle=90))+
  ggtitle("NDMI-Werte des Jahres 2020")+
  xlab("Datum")+ ylab("NDVI")+theme(legend.title = element_blank())+
  scale_fill_manual(values = c("purple3", "slateblue3", "mediumpurple2"))
