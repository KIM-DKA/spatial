
 
???Busan 데이터로 실습결과를 재현해보고 자신의 해석을 덧붙여서 제출하라
(Global Moran's I 통계량, LISA 지도, Getis and Ord's Local G, Geary C, Mantel test and kulldorff clustering까지 6개)
-코드만 제출하지 말것
-개인 데이터 사용 및 재현도 가능


### library -----------------------------

install.packages('spdep')

library(rgdal)
library(maptools)
library(sp)
library(ggplot2)
library(dplyr)
library(sf)
library(spdep)
library(tmap)

### Working dir -----------------------------

setwd('C:/Users/rlaem/Desktop/student/SNU/geoda/report5/SSTE_w08_datafile/Busan')
busan_tm <- rgdal::readOGR("bnd_dong_21_2014.shp")
summary(busan_tm)

# data for merge
sep <- read.table("busan_cardio_mortality14.csv", header = T, sep = ",") 

##merge##
busan_tm@data <- merge(busan_tm@data, 
                       sep[ , c("adm_dr_cd", "dep4", "dep", "obs", "exp", "smr")], 
                       by.x="adm_dr_cd", by.y="adm_dr_cd")

names(busan_tm@data); str(busan_tm@data)
busandata=busan_tm@data

# TM coordinate

sp::proj4string(busan_tm) <- sp::CRS("+proj=tmerc +lat_0=38 +lon_0=127.5 
                             +k=0.9996 +x_0=1000000 +y_0=2000000 
                             +ellps=GRS80 +units=m +no_defs") 

# Transform Longitude-lattitude coordinate
busan <- sp::spTransform(busan_tm, CRS=sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs") ) 


# spatial linking structure
poly.nlist<- spdep::poly2nb(busan)
plot(busan, col="white", axes=T, cex.axis=.75, bg="lightskyblue", border="slategrey")
plot(poly.nlist, coordinates(busan), col = "red", pch = ".", add=T);grid()

# spatial weight
listw <- spdep::nb2listw(poly.nlist)   
# if there is an island
listw <- spdep::nb2listw(poly.nlist, zero.policy=TRUE)

# Moran test for global spatial autocorrelation
moran.test(busan$smr, listw, zero.policy=T)

# Monte Carlo Moran Test
bperm <-moran.mc(busan$smr, listw, nsim=999 ,zero.policy=T)
bperm

# Moran's I plot
spdep::moran.plot(busan$smr, listw = listw, zero.policy=T)

# Geary C test under randomization
geary.test(busan@data$smr, listw, zero.policy = T)

# Geary-c test using obs/exp
#DCluster::gearyc.test(obs ~ offset(log(exp)), data=busan, 
#                      model="poisson", R=999, applyto="SMR", 
#                      listw=listw, 
#                      n=length(poly.nlist), 
#                      n1=length(poly.nlist)-1,
#                      S0=spdep::Szero(listw),
#                      zero.policy=T)

# Global G test (binary weights recommended ) 
#col.W <- spdep::nb2listw(poly.nlist, zero.policy=TRUE, style="B")
globalG.test(busan$smr, listw = listw, zero.policy=T)

# Mantel permutation test 
sp.mantel.mc(busan$smr, listw = listw, nsim = 99, zero.policy=T)


############################################################################
# Spatial cluster detection

# Local Morans'I (LISA)
busan@data$lisa=spdep::localmoran(busan@data$smr, listw)[,1]
# p-value
busan@data$lisa_p=spdep::localmoran(busan@data$smr, listw)[,5]

# LISA map
#library(tmap)
#   tm_shape(busan) +
#    tm_fill("smr",title="Local moran statistic") + 
#    tm_borders() +
#    tm_layout(legend.outside = TRUE, legend.outside.position = "left",
#              main.title = " SMR for Busan (Local Moran's I)", 
#              title.size = 0.5,main.title.position="left")

# LISA cluster pattern
lisa=spdep::localmoran(busan@data$smr, listw)
quadrant = vector(mode="numeric",length=nrow(lisa))
# centers the variable of interest around its mean
m.smr = busan$smr - mean(busan$smr)     
# centers the local Moran's around the mean
m.lisa <- lisa[,1] - mean(lisa[,1])    
# significance threshold
signif <- 0.1 
# builds a data quadrant
quadrant[m.smr >0 & m.lisa>0] <- 4  
quadrant[m.smr <0 & m.lisa<0] <- 1      
quadrant[m.smr <0 & m.lisa>0] <- 2
quadrant[m.smr >0 & m.lisa<0] <- 3
quadrant[lisa[,5]>signif] <- 0

# plot in r
brks <- c(0,1,2,3,4)
colors <- c("white","blue",rgb(0,0,1,alpha=0.4),rgb(1,0,0,alpha=0.4),"red")
plot(busan, border="lightgray",col=colors[findInterval(quadrant, brks,all.inside=FALSE)])
legend("bottomleft",legend=c("insignificant","low-low","low-high","high-low","high-high"),
       fill=colors,bty="n",cex=0.6)
title("LISA cluster map")
box()

# Getis and Ord's G
busan@data$lg = spdep::localG(busan@data$smr, listw)
GISTools::choropleth(busan, busan$lg, shading=lg_shade, axes=T);  
lg_shade = GISTools::auto.shading(c(busan$lg, -busan$lg), 
                                  cols= rev(RColorBrewer::brewer.pal(5,"RdBu" )) )
GISTools::choro.legend(129.2, 35.15, lg_shade, fmt="%6.2f",cex=0.5 )
title("SMR for Busan (Getis and Ord's local G)") 

# same picture using tmap
tm_shape(busan) + 
  tm_fill("lg", breaks = c(-4.5, -0.99, -0.32, 0.32, 0.99, 5), 
          midpoint = 0, n=5, palette =lg_shade$cols) + 
  tm_borders(alpha=.5)+
  tm_layout(legend.outside = TRUE, legend.outside.position = "right",
            main.title = "SMR for Busan (Getis and Ord's local G)", 
            main.title.size = 1.0,main.title.position="left")


# Kulldorff -Nagawala Clustering
x = coordinates(busan_tm)[,1]
y = coordinates(busan_tm)[,2]

busan_1 = as(busan_tm,"data.frame")[, c("obs", "exp")]
busan_1 = cbind(busan_1, x=x, y=y)
colnames(busan_1) = c("Observed", "Expected", "x", "y")


mle <- DCluster::calculate.mle(busan_1, model="poisson")
knresults <- DCluster::opgam(data=as(busan_1, "data.frame"), thegrid=busan_1[,c("x","y")],
                             alpha=0.05, iscluster= DCluster::kn.iscluster, 
                             fractpop=0.15, R=99, model="poisson", mle=mle)
knresults

# Drawing scan circles
# library(plotrix)
library(plotrix)
plot(busan_tm, density=10, col="grey", axes=T, cex.axis=.75, xlab="Easting", ylab="Northing")
for(i in 1: 50){
  plotrix::draw.circle(knresults$x[i],knresults$y[i], 
                       knresults$size[i]*100,nv=10, 
                       border="red", col=NA, lty=2,lwd=0.5)
}

points(knresults$x, knresults$y, col="red")
title(main=c("Kulldorff-Nagawala Clustering"))



