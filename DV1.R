library(ggplot2) # Data visualization
library(lubridate)
library(dplyr)
library(tidyr)
library(corrplot)
library(leaflet)
library(scatterplot3d)
library(plotly)
library(TSstudio)
df<- read.csv("C:/Users/HP/Desktop/padhai/DV/JCOMP/datasets/all_month.csv", header=TRUE, sep=',', stringsAsFactors = FALSE)
# View(df)
glimpse(df)
df[is.na(df)] <- 0
df$date <- as.Date(substring(df$time,1,10),"%Y-%m-%d")
earthquakeCor <- df[,c("latitude","longitude","depth","mag", "nst","gap",
                       "dmin","rms","horizontalError","depthError","magError","magNst")]
correlations <- cor(earthquakeCor)
p <- corrplot(correlations, method="circle")

df %>% group_by(magNst) %>% 
  summarize(avg = mean(mag), min=min(mag), max=max(mag), events = length(mag)) %>% 
  ungroup() -> dfm
head(dfm)
glimpse(dfm)
tail(dfm)

ggplot(df, aes(x=date, y=mag)) + geom_point() + geom_line(linetype="dotted",color="blue") + theme_bw() + 
  labs(title="Time series for all month US Dataset", x="Date", y="Magnitude")

dfmag <- df %>% filter(mag >= 5) 
ggplot(dfmag, aes(x=date, y=mag)) + geom_point() + geom_line(linetype="solid",color="blue") + theme_bw() + 
  labs(title="Time series for large (magnitude > 5)", 
       x="Date", y="Magnitude")


dat_ts <- ts(dfmag[,c("mag")])
ts_plot(dat_ts)
dat_ts <- ts(dfmag[,c("latitude")])
ts_plot(dat_ts)
dat_ts <- ts(dfmag[,c("longitude")])
ts_plot(dat_ts)


boxplot(df$mag, main="Boxplot for magnitude", ylab="Magnitude")

boxplot(df$depth, main="Boxplot for depth", ylab="Depth")

colors = c("red", "blue", "yellow", "green", "magenta",
                "lightblue", "grey", "lightgreen", "darkblue", "cyan")
boxplot(mag ~ magSource, data=df, col=colors, xlim=c(0.5,10), ylim=c(1,8), 
                        ylab="Magnitude", main="Magnitude, grouped by magnitude source")

colors = c("red", "blue", "yellow", "green", "magenta",
                "lightblue", "grey", "lightgreen", "darkblue", "cyan")
boxplot(mag ~ magType, data=df, col=colors, xlim=c(0.5,10), ylim=c(1,8),
                        ylab = "Magnitude", main="Magnitude grouped by magnitude type")
attach(df)
scatterplot3d(longitude,latitude,mag, pch=16, highlight.3d=TRUE,
              type="h",
              main="Earthquakes \nMagnitude vs. position",
              xlab="Longitude",
              ylab="Latitude",
              zlab="Magnitude [Richter]")
dfmag$date <- as.integer(strftime(dfmag$time, "%d"))
dfmag$date
# create column indicating point color
dfmag$pcolor <- "darkgreen" #all other than Bucharest or US
dfmag$pcolor[dfmag$locationSource=="buc"] <- "red"
dfmag$pcolor[dfmag$locationSource=="us"] <- "blue"
with(dfmag, {
s3d <- scatterplot3d(date, depth, mag,        # x y and z axis
     color=pcolor, pch=19,        # circle color indicates location Source
     type="h", lty.hplot=2,       # lines to the horizontal plane
     scale.y=.95,                 # scale y axis (reduce by 25%)
     main="Major earthquakes (magnitude > 5)",
     xlab="Date",
     ylab="Depth [Km]",
     zlab="Magnitude [Richter]")
s3d.coords <- s3d$xyz.convert(date, depth, mag)
text(s3d.coords$x, s3d.coords$y,     # x and y coordinates
     labels=place,       # text to plot
     pos=4, cex=.5)                  # shrink text 50% and place on right side of points)
# add the legend
legend("topleft", inset=.05,      # location and inset
       bty="n", cex=.75,              # suppress legend box, shrink text 50%
       title="Location source",
       c("gcmt", "US", "other"), fill=c("red", "blue", "darkgreen"))
})

with(dfmag, {
  s3d <- scatterplot3d(date, depth, mag,        # x y and z axis
                       color=pcolor, pch=19,        # circle color indicates location Source
                       type="h", lty.hplot=2,       # lines to the horizontal plane
                       scale.y=.75,                 # scale y axis (reduce by 25%)
                       main="Major earthquakes (magnitude > 5)",
                       xlab="Date",
                       ylab="Depth [Km]",
                       zlab="Magnitude [Richter]")
  s3d.coords <- s3d$xyz.convert(date, depth, mag)
  text(s3d.coords$x, s3d.coords$y,     # x and y coordinates
       labels=magType,       # text to plot
       pos=4, cex=.5)                  # shrink text 50% and place on right side of points)
  # add the legend
  legend("topleft", inset=.05,      # location and inset
         bty="n", cex=.75,              # suppress legend box, shrink text 50%
         title="Location source",
         c("gcmt", "US", "other"), fill=c("red", "blue", "darkgreen"))
})