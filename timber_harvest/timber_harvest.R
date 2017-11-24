## BC timber harvest indicator plots

library(readr)
library(dplyr) #data prep
library(reshape2) #for restructuring data table
library(ggplot2)  #for plotting
library(extrafont) #Verdana font
library(envreportutils) #soe theme
library(rphylopic) #for conifer image, package available from GitHub only
library(curl) #required by rphylopic function

## @knitr pre

## DATA
## load timber harvest data from FLNRO
harvest <- read_csv("~/soe_data/forests/timber_harvest/2017/bc_timber_harvest.csv")

forecast <- read_csv("~/soe_data/forests/timber_harvest/2017/bc_timber_supply_forecast.csv")

##font selection
chart_font_web <- "Verdana"

## @knitr harvest

## harvest plot

## restructuring the harvest csv table for plotting
harvest_long <- melt(harvest, id.vars = "Year", 
                variable.name = "harvest", value.name = "millions_m3")

#remove units and underscores from names
harvest_long$harvest <- gsub("_millions_m3", "", harvest_long$harvest)
harvest_long$harvest <- gsub("_", " ", harvest_long$harvest)

## total harvest dataframe for total line
total_harvest <- harvest_long %>% 
  filter(harvest == "Total harvest")

## component harvest dataframe for stacked chart
harvest_comp <- harvest_long %>% 
  filter(harvest == "Harvest regulated by AACs" | harvest == "Harvest not regulated by AACs")

## plot order
harvest.order <- c("Harvest not regulated by AACs", "Harvest regulated by AACs")
harvest_comp$harvest <- factor(harvest_comp$harvest, levels = harvest.order)

## chart colours
pal <- c("#99cc00", "#339966")
names(pal) <- harvest.order

## add tree image
conifer <- image_data("f86235e3-f437-4630-9e77-73732b9bcf41", size = "512")[[1]]

## stacked area chart
harvest.plot <- ggplot(harvest_comp, aes(x = Year, y = millions_m3)) +
  geom_area(data = harvest_comp, aes(x = Year, y = millions_m3, fill = harvest), alpha = 0.7) +
  scale_fill_manual(values = pal, guide = guide_legend(title = "", label.position = "top"),
                    labels = c("Harvest Not Regulated\nby Allowable Annual Cut", "Harvest Regulated\nby Allowable Annual Cut")) +
  geom_line(data = total_harvest, aes(x = Year, y = millions_m3, colour = "Total Timber\nHarvest"), alpha = 0.7, size = 1.3) +
  scale_color_manual(values = ("Total Timber Volume\nHarvested" = "black"),
                     guide = guide_legend(order=1, title = "")) +
  xlab("Year") +
  ylab(expression(paste("Timber Volume", " ", "(", "millions", " ", m^3, ")"))) +
  scale_x_continuous(limits = c(1910, 2015), breaks=seq(1915, 2015, 10), expand = c(0,0)) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 10), expand=c(0, 0)) +
  theme_soe() +
  theme(panel.grid.major.x = element_blank(),
        legend.text = element_text(size = 14),
        axis.title = element_text(size=16),
        axis.text = element_text(size=12),
        legend.position = c(.23,.61), 
        legend.direction = "vertical",
        legend.background = element_rect(fill = "NA"),
        plot.margin = unit(c(10,10,5,5),"mm")) +
  add_phylopic(conifer, alpha = .7, color = "grey30", ysize = 30, x = 2005, y = 20)

plot(harvest.plot)


## @knitr aac

## AAC plot

## total AAC dataframe for total line
total_aac <- harvest_long %>% 
  filter(harvest == "Total sum of AACs") %>%
  filter(Year > "1944")

## total harvest dataframe for total line from 1945
total_aac_short <- harvest_comp %>% 
  filter(harvest == "Harvest regulated by AACs") %>%
  filter(Year > "1944")

# colour for aac plot area
pal2 <- c("#339966")

## aac plot
aac.plot <- ggplot(total_aac_short, aes(x = Year, y = millions_m3)) +
  geom_area(data = total_aac_short, aes(x = Year, y = millions_m3, fill = harvest), alpha = 0.7) +
  scale_fill_manual(values = pal2, guide = guide_legend(title = "", label.position = "top"),
                    labels = c("Harvest Regulated\nby Allowable Annual Cut")) +
  geom_line(data = total_aac, aes(x = Year, y = millions_m3, colour = "Total Allowable\nAnnual Cut"), alpha = 0.7, size = 1.3) +
  scale_color_manual(values = ("Total Allowable Annual Cut" = "red"),
                     guide = guide_legend(order=1, title = "")) +
  xlab("Year") +
  ylab(expression(paste("Timber Volume", " ", "(", "millions", " ", m^3, ")"))) +
  scale_x_continuous(limits = c(1945, 2015), breaks=seq(1950, 2015, 5), expand = c(0,0)) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 10), expand=c(0, 0)) +
  theme_soe() +
  theme(panel.grid.major.x = element_blank(),
        legend.text = element_text(size = 14),
        axis.title = element_text(size=16),
        axis.text = element_text(size=12),
        legend.position = c(.14,.86), 
        legend.direction = "vertical",
        legend.background = element_rect(fill = "NA"),
        plot.margin = unit(c(10,10,5,5),"mm")) +
  add_phylopic(conifer, alpha = .7, color = "grey30", ysize = 30, x = 2008, y = 20)
plot(aac.plot)

## @knitr forecast

## restructuring the csv table for plotting
forecast_long <- melt(forecast, id.vars = "Year", 
                     variable.name = "harvest", value.name = "m3_per_year")

forecast_mut <- forecast_long %>% 
  mutate(volume = round(m3_per_year/1000000, digits=0))


## chart colours
pal3 <- c("black","blue", "#005a32")

## forecast plot
forecast.plot <- ggplot(forecast_mut, aes(x = Year, y = volume, colour = harvest)) +
  geom_line(alpha = 0.7, size = 1.4, show.legend = FALSE) +
  scale_color_manual(values = pal3) +
  xlab("Year") +
  ylab(expression(paste("Timber Harvest", " ", "(", m^3,"/year", " ", " * million", ")"))) +
  scale_x_continuous(limits = c(2010, 2100), breaks=seq(2010, 2100, 10), expand = c(0,0)) +
  scale_y_continuous(limits = c(0, 100), breaks = seq(10, 100, 10), expand=c(0, 0)) +
  theme_soe() +
  theme(panel.grid.major.x = element_blank(),
        axis.title = element_text(size=16),
        axis.text = element_text(size=12),
        plot.margin = unit(c(10,10,5,5),"mm")) +
   annotate("text", label = "British Columbia", x = 2091, y = 67,
            size = 6, colour = "black",  family = chart_font_web) +
  annotate("text", label = "Coast", x = 2091, y = 20,
           size = 6, colour = "blue",  family = chart_font_web) +
  annotate("text", label = "Interior", x = 2091, y = 50,
           size = 6, colour = "#005a32",  family = chart_font_web)
plot(forecast.plot)
  
## @knitr stop

## save plots in SVG format
dir.create('out', showWarnings = FALSE)

svg_px("./out/harvest_chart.svg", width = 836, height = 489) 
plot(harvest.plot)
dev.off()

svg_px("./out/aac_chart.svg", width = 836, height = 489)
plot(aac.plot)
dev.off()

svg_px("./out/forecast_chart.svg", width = 836, height = 489)
plot(forecast.plot)
dev.off()

# png(filename = "./out/harvest_chart.png", width = 836, height = 489, units = "px", type = "cairo-png")
# plot(harvest.plot)
# dev.off()
# 
# png(filename = "./out/aac_chart.png", width = 836, height = 489, units = "px", type = "cairo-png")
# plot(aac.plot)
# dev.off()
# 
# png(filename = "./out/forecast_chart.png", width = 836, height = 489, units = "px", type = "cairo-png")
# plot(forecast.plot)
# dev.off()

