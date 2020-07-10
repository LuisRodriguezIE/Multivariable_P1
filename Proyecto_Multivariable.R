#------------------------------ ###### ---------------------------------|
#    -- ================================================================|
#    -- Author:		    <Luis Rodriguez>           									      |
#    -- Organization: <ITAM>                                            |
#    -- Create date:  <17/06/2020>										                  |
#    -- Last modification date:  <17/06/2020>							              |
#    -- Description:	<Proyecto Modulo Multivariable>                   |
#    -- Data Source:  <https://www.kaggle.com/rajyellow46/wine-quality> |
#    -- ================================================================|
#------------------------------ ###### ---------------------------------|

########                 Librerias, Descripción ########  

library(sf)              # Simple Features for R.
library(dplyr)           # Grammar of Data Manipulation.
library(tidyr)           # Easily Tidy Data with 'spread()' and 'gather()'. 
library(sp)              # Classes and Methods for Spatial Data.
library(raster)          # Geographic Data Analysis and Modeling.
library(rgeos)           # Interface to Geometry Engine.
library(rgbif)           # Interface to the Global 'Biodiversity' Information Facility API.
library(viridis)         # Default Color Maps from 'matplotlib'.
library(gridExtra)       # Miscellaneous Functions for "Grid" Graphics.
library(rasterVis)       # Visualization Methods for Raster Data.
library(caTools)         # Tools: moving window statistics, GIF, Base64, ROC. 
library(corrplot)        # Visualization of a Correlation Matrix.
library(dygraphs)        # Interactive Time Series.
library(data.table)	     # Data information handle.
library(directlabels)    # Direct Labels for Multicolor Plots.
library(forecast)        # Forecasting Functions for Time Series and Linear Models.
library(factoextra)      # Extract and Visualize the Results of Multivariate Data Analyses.
library(GGally)          # Extension to ggplot.
library(ggcorrplot)      # Visualization of Correlation Matrix using ggplot.
library(ggrepel)	       # Automatically Position Non-Overlapping Text Labels with 'ggplot2.
library(gmapsdistance)   # Distance and Travel Time Between Two Points from Google Maps.
library(geosphere)       # Compute various aspects of distance.
library(ggplot2)         # Create Elegant Data Visualisations Using the Grammar of Graphics.
library(ggfortify)       # Draw Some Popular Packages, Time Series.
library(lubridate)       # Dates and times made easy with lubridate.
library(plyr)            # The split-apply-combine paradigm for R.
library(psych)           # Toolbox for personality, psychometric theory and experimental psychology.
library(skimr)           # Provides an alternatuve to the default summary function within R.
library(xts)             # Test For Time-Series Object.
library(zoo)             # Infrastructure for Regular and Irregular Time Series.
library(ggthemes)        # This package contains extra themes for ggplot.
library(fpp2)            # Library for Book Forecasting: principles and practice.
library(RODBC)           # Open connections to ODBC databases.
library(sqldf)           # Manipulate R Data Frames Using SQL.
library(shiny)           # Web Application Framework for R
library(shinythemes)     # Themes for Shiny
library(leaflet)         # Create Interactive Web Maps with the JavaScript 'Leaflet'.

################## DEFINIR RUTA DE ARCHIVOS ##################

DirectorioDatos <- "C:/Users/rodrigl2/OneDrive - Brambles/Desktop/Luis/ITAM/Multivariable"
setwd(DirectorioDatos)

################## EXPLICACIÓN DATOS ##################

# The two datasets are related to red and white variants of the Portuguese "Vinho Verde" wine. 
# The reference [Cortez et al., 2009]. Due to privacy and logistic issues, only physicochemical (inputs) 
# and sensory (the output) variables are available (e.g. there is no data about grape types, 
#                                                   wine brand, wine selling price, etc.).

################## CARGAR ARCHIVOS DE DATOS ##################

VinosCalidad <- read.csv('VinosCalidad.csv')
VinosCalidad$Calidad <- factor(VinosCalidad$Calidad, levels = c("Excelente", "Muy Bueno", "Bueno", 
                                                                "Distinguido", "Aceptable", 
                                                                "Regular", "Deficiente"))
################## DIAGRAMA DE CORRELACIÓN ##################

# pairs.panels(VinosCalidad, scale = F)

# summary(VinosCalidad)

################## ANALISIS DE DATOS ##################

VinosDescribe <- VinosCalidad%>%
  dplyr::group_by(Tipo, Calidad)%>%
  dplyr::summarise(Num.Registros = n())

# View(VinosDescribe)

# VinosCalidad <- VinosCalidad%>%
#  dplyr::filter(Calidad == 'Muy Bueno')

################## DIAGRAMA DE CAJA ##################

VinosBox <- VinosCalidad%>%
  dplyr::select(-c(Calidad, Calidad.Num))

VinosBox1 <- VinosBox%>%
  dplyr::select(c(Tipo, Acidez.Fija, Acidez.Volatil, Acido.Citrico, Azucar.Residual, Cloruro))

VinosBox2 <- VinosBox%>%
  dplyr::select(c(Tipo, Dioxido.Sulfuro.Libre, Dioxido.Sulfuro.Total, Densidad, pH, Sulfato, Alcohol))

VinosBox1[,2:ncol(VinosBox1)] <- scale(VinosBox1[,2:ncol(VinosBox1)], center = T, scale = T)
VinosBox2[,2:ncol(VinosBox2)] <- scale(VinosBox2[,2:ncol(VinosBox2)], center = T, scale = T)

VinosBox1 <- gather(VinosBox1, key = 'Variable', value = 'Valor', Acidez.Fija:Cloruro)
VinosBox2 <- gather(VinosBox2, key = 'Variable', value = 'Valor', Dioxido.Sulfuro.Libre:Alcohol)

# View(VinosBox1)
# View(VinosBox2)

ggplot(VinosBox1, aes(x = Variable, y = Valor, fill = Tipo))+
  theme_bw()+
  geom_boxplot(position=position_dodge(0.8))

ggplot(VinosBox2, aes(x = Variable, y = Valor, fill = Tipo))+
  theme_bw()+
  geom_boxplot(position=position_dodge(0.8))

################## DIAGRAMA DE CAJA NORMALIZADA ##################

normalize <- function(x) {
  return ((x - min(x, na.rm = T)) / (max(x, na.rm = T) - min(x, na.rm = T)))
}

# VinosBoxN1 <- VinosBox%>%
#  dplyr::select(c(Tipo, Acidez.Fija, Acidez.Volatil, Acido.Citrico, Azucar.Residual, Cloruro))
# VinosBoxN2 <- VinosBox%>%
#  dplyr::select(c(Tipo, Dioxido.Sulfuro.Libre, Dioxido.Sulfuro.Total, Densidad, pH, Sulfato, Alcohol))

VinosBoxN1 <- VinosBox%>%
  dplyr::select(c(Tipo, Acidez.Fija, Acidez.Volatil, Cloruro, Densidad, pH, Sulfato))
VinosBoxN2 <- VinosBox%>%
  dplyr::select(c(Tipo, Alcohol, Dioxido.Sulfuro.Libre, Dioxido.Sulfuro.Total, Acido.Citrico, Azucar.Residual))

VinosBoxN1[,2:ncol(VinosBoxN1)] <- sapply(VinosBoxN1[,2:ncol(VinosBoxN1)], normalize)
VinosBoxN2[,2:ncol(VinosBoxN2)] <- sapply(VinosBoxN2[,2:ncol(VinosBoxN2)], normalize)

VinosBoxN1 <- gather(VinosBoxN1, key = 'Variable', value = 'Valor', Acidez.Fija:Sulfato)
VinosBoxN2 <- gather(VinosBoxN2, key = 'Variable', value = 'Valor', Alcohol:Azucar.Residual)

ggplot(VinosBoxN1, aes(x = Variable, y = Valor, fill = Tipo))+
  theme_bw()+
  ylab("Valor Normalizado")+
  geom_boxplot(position=position_dodge(0.8))

ggplot(VinosBoxN2, aes(x = Variable, y = Valor, fill = Tipo))+
  theme_bw()+
  ylab("Valor Normalizado")+
  geom_boxplot(position=position_dodge(0.8))

################## ANALISIS DESCRIPTIVO DE VARIABLES ##################

VinoNumerico <- VinosCalidad%>%
  dplyr::select(-c(Tipo, Calidad, Calidad.Num)) # 

# Vector de medias o de desviaciones estandar.

sapply(VinoNumerico, mean, na.rm = T)
sapply(VinoNumerico, sd, na.rm = T)

# Obtener el vector de medias de una matriz.

colMeans(VinoNumerico, na.rm = T)

# Matriz de varianzas y covarianzas.

cov(VinoNumerico, use = "na.or.complete")
cor(VinoNumerico, use = "na.or.complete")

ggplot(data = VinosCalidad, aes(x = Alcohol, y = Azucar.Residual, col = Calidad))+
  geom_point()+
  ylim(0,25)+
  theme_bw()+
  scale_color_manual(breaks = c("Excelente", "Muy Bueno", "Bueno", 
                                "Distinguido", "Aceptable", 
                                "Regular", "Deficiente"),
                     values=c("red", "darkmagenta", "darkgreen",
                              "navyblue", "lightskyblue", "gray78", "gray85"))
#  scale_color_brewer(palette="Set1")

ggplot(data = VinosCalidad, aes(x = Alcohol, y = Densidad, col = Calidad))+
  geom_point()+
  ylim(0.985,1.005)+
  theme_bw()+
  scale_color_manual(breaks = c("Excelente", "Muy Bueno", "Bueno", 
                                "Distinguido", "Aceptable", 
                                "Regular", "Deficiente"),
                     values=c("red", "darkmagenta", "darkgreen",
                              "navyblue", "lightskyblue", "gray78", "gray85"))

ggplot(data = VinosCalidad, aes(x = Azucar.Residual, y = Densidad, col = Calidad, shape = Tipo))+
  geom_point()+
  ylim(0.985,1.005)+
  xlim(0,25)+
  theme_bw()+
  scale_color_manual(breaks = c("Excelente", "Muy Bueno", "Bueno", 
                                "Distinguido", "Aceptable", 
                                "Regular", "Deficiente"),
                     values=c("red", "darkmagenta", "darkgreen",
                              "navyblue", "lightskyblue", "gray78", "gray85"))


VinoNumericoM  <- cor(VinoNumerico, use = "na.or.complete")

col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
corrplot(VinoNumericoM, method = "color", col = col(200),  
         type = "upper", order = "FPC", 
         addCoef.col = "black", # Add coefficient of correlation
         tl.col = "black", tl.srt = 30, #Text label color and rotation
         # Combine with significance
          sig.level = 0.01, insig = "blank", 
         # hide correlation coefficient on the principal diagonal
         diag=FALSE,
#         title = "Matriz de Correlación Vinos",
         tl.cex = 0.8,
         number.cex = 0.8)

################## ANALISIS DE COMPONENTES PRINCIPALES ##################

VinoNumericoC <- VinoNumerico[complete.cases(VinoNumerico), ]
VinoNumericoC <- scale(VinoNumericoC, center = T, scale = T)

VinosACP <- prcomp(VinoNumericoC, center = TRUE, scale. = TRUE)

VinosACP


summary(VinosACP)


# sumvar <- as.data.frame()
# class(sumvar)


#write.csv(sumvar, "sumvar.csv", row.names = F)

round(VinosACP$rotation,2)
VinosACP$x


screeplot(VinosACP, type="lines", main="Visualización de ACP")
biplot(VinosACP)

resultACP <- VinosACP
numCol <- 2

CP1 <- resultACP$x[,1]/resultACP$sdev[1]
CP2 <- resultACP$x[,2]/resultACP$sdev[2]

plot(CP1, CP2, xlab="eje x",ylab="eje y", xlim = c(-3,4), ylim = c(-4,3), col = VinosCalidad$Tipo)
text(CP1,CP2, labels=row.names(VinoNumericoC),cex=0.5)

corr1<-resultACP$rotation[,1]*resultACP$sdev[1]
corr2<-resultACP$rotation[,2]*resultACP$sdev[2]

plot(corr1, corr2, xlim=c(0,1), type="n")
arrows(0,0, corr1, corr2, length=0.1)
text(corr1+0.02,corr2+0.01, labels=abbreviate(colnames(VinoNumericoC)),
     cex=1.2)





