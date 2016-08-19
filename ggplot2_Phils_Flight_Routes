##an exercise mapping all flights in the Philippines
##data was obtained from the http://openflights.org/data.html
##the latest data was obtained last 2012 per the website's claim

library(RColorBrewer)
library(RCurl)
library(ggmap)
library(ggplot2)
library(dplyr)
library(ggrepel)

# to load the windows fonts to be used
library(extrafont)
windowsFonts(gothic = windowsFont("century gothic"))

#===========================================================================================#
#===========================================================================================#
# THIS THE DATA PREPARATION PART #


#load the data from the website -- http://openflights.org/data.html
url <- getURL('https://raw.githubusercontent.com/jpatokal/openflights/master/data/airports.dat')
airport <- read.csv(text = url, header = FALSE)
str(airport)

View(airport)

#load the data from the website -- http://openflights.org/data.html
url2 <- getURL('https://raw.githubusercontent.com/jpatokal/openflights/master/data/routes.dat')
flight <- read.csv(text = url2, header = FALSE)
str(flight)


#change column names to match data frames
names(flight)[3] <- 'source'
names(flight)[5] <- 'destination'

names(airport)[5] <- 'source'
names(airport)

#for airport, include only country, IATA/FAA, lat and long
airport2 <- airport[,c(5,4,7,8)]
View(airport2)

#for flights, include only the source and destination
flight2 <- flight[,c('source', 'destination')]
View(flight2)




#join 2 tables to get the latitude and longitudes of the source
data1 = merge(flight2, airport2, by.x = 'source', by.y = 'source')
View(data1)

#join data1 and airport to get lat and long of the destination
data2 = merge(data1, airport2, by.x = 'destination', by.y = 'source')
View(data2)

names(data2)
#"destination" "source"      "V7.x"        "V8.x"        "V7.y"        "V8.y"   

##filter the data to get the philippine flights only

data2 <- data2 %>% filter(V4.y == 'Philippines') %>% filter(V4.x == 'Philippines')

#===========================================================================================#
#===========================================================================================#
# THIS THE DATA VISUALIZATION PART #

#chart the flights

f1 <- ggplot(data2, aes(x = V7.x, y = V8.x)) + 
  geom_segment(aes(x = V7.x, y = V8.x, xend = V7.y, yend = V8.y), size = 0.0005, alpha = 0.005, color = 'white')


f2 <- f1 + ggtitle('Philippine Flights')

f3 <- f2 +  theme_minimal() + theme(text = element_text(color = "white"),
                                    plot.title = element_text(family = 'gothic', size = 15, vjust = 1, hjust = -0.1),
                                    plot.margin = unit(c(1,1,1,1), "cm"),
                                    legend.position = 'none',
                                    axis.text = element_blank(),
                                    axis.title.x = element_blank(),
                                    axis.title.y = element_blank(),
                                    axis.ticks.y = element_blank(), # element_blank() is how we remove elements
                                    axis.line = element_blank(),
                                    axis.line.y = element_blank(),
                                    
                                    plot.background = element_rect(fill = 'black', colour = 'black'),
                                    
                                    panel.background = element_rect(fill = 'black', colour = 'black'),
                                    panel.grid.major = element_blank(),
                                    panel.grid.major.x = element_blank())
f3
