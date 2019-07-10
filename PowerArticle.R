#Analysis of the article found here:
# http://www.latimes.com/projects/la-fi-electricity-capacity/

library(ggplot2)
library(grid)
library(tidyr)


capacity = read.csv("annual-capacity-california.csv")
customers = read.csv("annual-customers-california.csv")
price = read.csv("annual-price-california-vs-usa.csv")
revenue = read.csv("annual-revenue-california.csv")
sales = read.csv("annual-sales-california.csv")
caliPlants = read.csv("plants-california.csv")
plants = read.csv("plants.csv")

##################################################################################################
#Make first Figure
##################################################################################################
#Figure 1:
#The main jist of the article is that CA has too much power generating capacity.
#The article compares Power use to power generation and gives a figure to support its hypothesis.
#Lets examine that figure ("California's electricity usage" on the left, "energy supply" on the right)

#1 - I get that the author is talking about rates (increase/decrease), so only the slope matters
#2 - There are however, things I *personally* dislike about this figure:
#2A - The two figures are given with two different units and different scales on the Y-axis 
#2B - No explanation of how (its actually apples and oranges, so it is not possible) to go 
#           from megaWatts to megaWatt-hours is given
#2C - The graph on the left is actually more compressed in the vertical direction -
#          it takes up less screen space vertically then the graph on the right
#2D - The figure on the left plots data that only uses ~61% of the vertical axis (61/100) while
#           the figure on the right uses ~83% of the vertical axis (25/30)
#2E - This makes the figures look much more "convincing" unless you really look at the details
#2F - Again, while factually accurate, this leaves me somewhat in discontent
#2G - Also, I can't tell how much of the electricity used in CA is actually generated in CA.
#       This info might be in the other data, I will check for it, 
#       But in a use-production comparison, I was hoping for it here

#Lets recreate those two figures on the axis with the same "compression"
#Also, there are two given capacities.
#      in the article, summer capacity was used. It trails nameplate capacity in every year
#      Kudos to the author for selecting the more "difficult" dataset to work with

#Recreate the first graphic - similar to the article
#this is total usage, not just residential usage of electrictiy
colnames(capacity)[7] <- "nominal_Capacity"
colnames(capacity)[8] <- "summer_Capacity"

Fig1a <-   ggplot() +
  geom_line(data = sales, aes(x = Year, y = (Total/1000000)), size =2, color = "orange") +
  ylab("megaWatt-hours") +
  ggtitle("Total Power Usage") +
  geom_vline(xintercept = 2008, color = "red",  linetype = "longdash") +
  coord_cartesian(ylim=(c(200,300))) +
  scale_y_continuous(breaks = c(200,250,300)) +
  annotate("text", label = "Recession Begins", 
           color = "red", size = 4, 
           x= 2007.9, y = 300, 
           hjust =1, vjust = 0)

#this is summer capactiy, not nameplate capacity.  
Fig1b <-   ggplot() + 
  geom_line(data = capacity, aes(x = Year, y = (summer_Capacity/1000)), size =2, color = "orange") +
  ylab("megaWatts") +
  ggtitle("Total CA Power Generation") +
  geom_vline(xintercept = 2008, color = "red", linetype = "longdash") +
  coord_cartesian(ylim=(c(50,80))) +
  scale_y_continuous(breaks = c(50,60,70,80)) +
  annotate("text", label = "Recession Begins", 
           color = "red", size = 4, 
           x= 2007.9, y = 80, hjust =1, vjust=0)

# grid.newpage()
# grid.draw(cbind(ggplotGrob(Fig1a), ggplotGrob(Fig1b), size = "last"))
#Note that they look exponential in nature.  Summer is slightly but consistently lower.
###END Fig 1


#Alternative Figure 1 presentation
#Standardize the data to the 1990 values 
#(can also standardize to smallest value [(min(Total) instead of Total[1]] if you like)
Alt_Fig1a <-   ggplot() +
  geom_line(data = sales, aes(x = Year, y = (Total/Total[1])), size =2, color = "orange") +
  ylab("Relative Power Usage") +
  ggtitle("Relative Power Usage") +
  geom_vline(xintercept = 2008, color = "red",  linetype = "longdash") +
  coord_cartesian(ylim=(c(0.9,1.5))) +
  scale_y_continuous(breaks = c(1.0,1.1,1.2,1.3,1.4)) +
  annotate("text", label = "Recession Begins", 
            color = "red", size = 4, 
            x= 2007.9, y = 1.4, 
            hjust =1, vjust = 0)

#this is summer capactiy, not nameplate capacity.  
Alt_Fig1b <-   ggplot() + 
  geom_line(data = capacity, aes(x = Year, y = (summer_Capacity/summer_Capacity[1])), size =2, color = "orange") +
  ylab("Relative Power Supply") +
  ggtitle("Relative CA Power Generation") +
  geom_vline(xintercept = 2008, color = "red", linetype = "longdash") +
  coord_cartesian(ylim=(c(0.9,1.5))) +
  scale_y_continuous(breaks = c(1.0,1.1,1.2,1.3,1.4)) +
  annotate("text", label = "Recession Begins", 
           color = "red", size = 4, 
           x= 2007.9, y = 1.4, 
           hjust =1, vjust = 0)

# grid.newpage()
# grid.draw(cbind(ggplotGrob(Alt_Fig1a), ggplotGrob(Alt_Fig1b), size = "last"))

###END ALT_Figure 1

grid.newpage()
grid.draw(rbind((cbind(ggplotGrob(Fig1a), ggplotGrob(Fig1b), size = "last")),
                (cbind(ggplotGrob(Alt_Fig1a), ggplotGrob(Alt_Fig1b), size = "last")),
                size = "last"
                ))
#Article points 1 and 2 shown in figure:
# 1 -demand for electricity has decreased since 2008
#2 - while generation has continued to increase

#From the alt figure, combine them into one graph
Alt_Fig1 <-   ggplot() +
  geom_line(data = sales, aes(x = Year, y = (Total/Total[1])), size =2, color = "orange") +
  geom_line(data = capacity, aes(x = Year, y = (summer_Capacity/summer_Capacity[1])), size =2, color = "deepskyblue3") +
  ylab("Relative to 1990 levels") +
  ggtitle("CA Power Usage vs Generation") +
  geom_vline(xintercept = 2008, color = "red",  linetype = "longdash") +
  coord_cartesian(ylim=(c(0.9,1.5))) +
  scale_y_continuous(breaks = c(1.0,1.1,1.2,1.3,1.4)) +
  annotate("text", label = "Recession Begins", 
           color = "red", size = 4, 
           x= 2007.9, y = 1.4, 
           hjust =1, vjust = 0)

Alt_Fig1 +   
  geom_point(data = sales[sales$Year == c(2008,2015), ],
             aes(x = sales[sales$Year == c(2008,2015),"Year"], 
                 y = (sales[sales$Year == c(2008,2015),"Total"]/sales$Total[1])), 
             size =2) +  #usage (for orange line) 
  geom_text(data = sales[sales$Year == c(2008,2015), ],
            aes(x = sales[sales$Year == c(2008,2015),"Year"], 
                y = (sales[sales$Year == c(2008,2015),"Total"]/sales$Total[1])),
            label = paste("Usage = ",
                          round(sales[sales$Year == c(2008,2015),"Total"]/sales$Total[1], 
                                digits = 2)
                          ),
            nudge_y = 0.025, nudge_x = -1.0, color = "orange") + #For orange points
  
    geom_point(data = capacity[capacity$Year == c(2008,2015), ],
             aes(x = capacity[capacity$Year == c(2008,2015),"Year"], 
                 y = (capacity[capacity$Year == c(2008,2015),"summer_Capacity"]/capacity$summer_Capacity[1])), 
             size =2) + #Generation for blue line
  geom_text(data = capacity[capacity$Year == c(2008,2015), ],
            aes(x = capacity[capacity$Year == c(2008,2015),"Year"], 
                y = (capacity[capacity$Year == c(2008,2015),"summer_Capacity"]/capacity$summer_Capacity[1])),
            label = paste(" Generation = ",
                          round(capacity[capacity$Year == c(2008,2015),"summer_Capacity"]/capacity$summer_Capacity[1], 
                                digits = 2)
                          ),
            nudge_y = -0.025, nudge_x = -1.5, color = "darkblue")
  

   #The usage points labels

##################################################################################################
#First Figure Complete
##################################################################################################



##################################################################################################
#Make Second Figure
##################################################################################################
ggplot() + 
  geom_line(data = price, 
            aes(x = Year, 
                y = Total..2015.dollars.,
                color = State), 
             size = 2) +
  
  geom_point(data = price[price$Year == c(2008,2015), ], 
             aes(x = price[price$Year == c("2008","2015") ,"Year"],
                 y = price[price$Year == c("2008","2015") , "Total..2015.dollars."]), size = 2) +  #The final (2015) points
  
  geom_text(data = price[price$Year == c(2008,2015), ], 
            aes(x = price[price$Year == c("2008","2015") ,"Year"],
                y = price[price$Year == c("2008","2015") , "Total..2015.dollars."]),
            label = round(price[price$Year == c("2008","2015") , "Total..2015.dollars."], digits = 2),
            nudge_y = -0.5, nudge_x = -1.0) +    #The final (2015) points labels
  
  xlab("Year") +
  ylab("Price (cents)") +
  
  geom_vline(xintercept = 2008, color = "red", linetype = "longdash") +
  coord_cartesian(ylim=(c(9,17))) +
  scale_y_continuous(breaks = seq(9,17,2)) +
  annotate("text", label = "Recession Begins", 
           color = "red", size = 4, 
           x= 2007.9, y = 16.5, 
           hjust =1, vjust = 0) +
  scale_color_manual(name = NULL, values = c("orange", "deepskyblue3"))   #No legend title
 
##Article Point 3 shown in graph:
#Using 2008 as the reference point, CA paid 28% more than the rest of the country then,
# now (2015), CA pays 48% more than 2008 

#Article Point 4 shown in graph:
# CA pays more than 12% now than in 2008 compared to the US which pays ~3% less.

#Note: no idea why my 2008 line does not match up the that in the figure from the article.
#Thankfully, the numbers in the calculations do match up

##################################################################################################
#END Second Figure
##################################################################################################

#Point 5 the author made is that CA uses 2.6% less electricity since 2008 (see Fig 1),
#but now they pay 6.8 billion more than in 2008 - can be seen from revenue dataset 2015-2008.
#someting like this is to be expected from the aforementioned 12% rate increase in CA however.
#so no graph

#Point 6 - some regulator claimed that new plants are to replace old ones
#It would be nice to check if this is true, 
#but we can't test this since we don't have the year the plants opened.


##################################################################################################
#Article points 7 shown in figure below:
# - by 2020, CA will produce 21% more electricity than it needs
# To demonstrate this, we will examine the predicted levels of electricity use and generation 
##################################################################################################

#Assume all used electricity is produced in-state
#companies, and also we don't know what the current excess capacity is





