## Silviculture indicator plots

#loading libraries for script
library(readr) #read CSV file
library(magrittr) #pipe %>% 
library(dplyr) #data cleaning
library(reshape2) #wide to long df format
library(ggplot2) #plotting
library(RColorBrewer) #colour palette
library(extrafont) #Verdana font
library(grid) #for plots
library(envreportutils) #for soe_theme(), package from GitHub

#do not turn all strings in csv files into factors
options(stringsAsFactors=FALSE)

################################################################################
## Read in the data files
################################################################################

# Silvicultural systems
silsystems <- read.csv("Z:/forests/silviculture/2017/silviculture_systems.csv", strip.white=TRUE)

# Disturbances and Reforestation
dist.refor <- read.csv("Z:/forests/silviculture/2017/disturbance_and_reforestation.csv", strip.white=TRUE)

# Silvicultural Treatments
treatments <- read.csv("Z:/forests/silviculture/2017/silviculture_treatments.csv", strip.white=TRUE)

# Gains
gains <- read.csv("Z:/forests/silviculture/2017/timber_volume_gains.csv", strip.white=TRUE)

chart_font_web <- "Verdana"


###################################
# SILVICULTUTRE SYSTEMS
##################################

## @knitr silviculture

# Sum harvested systems
silsystems$Total <- silsystems$Clearcutting_with_reserves_ha + silsystems$Clearcutting_ha + silsystems$Partial_cutting_ha

# using reshape to melt the data 
silsystems.long <- melt(silsystems, id.vars="Fiscal_Year",
                        variable.name="System", value.name="Hectares")

#remove _ha and Fiscal from names
silsystems.long$System <- gsub("_ha", "", silsystems.long$System)
colnames(silsystems.long)[colnames(silsystems.long)=="Fiscal_Year"] <- "Year"

# total dataframe
silsystems.total <- silsystems.long %>% 
  filter(System == "Total") %>% 
  mutate(Area = round(Hectares/1000, digits=0))

# systems dataframe
silsystems.data <- silsystems.long %>% 
  filter(System != "Total") %>% 
  mutate(Area = round(Hectares/1000, digits=0))

#creating a list for order 'cause using it a lot
system.order <- c("Partial_cutting" ,"Clearcutting_with_reserves",
                  "Clearcutting")

#ordering factor
silsystems.data$System <- factor(silsystems.data$System, levels=system.order)

system.order.no <- length(system.order)+1

#creating colour palette for graphs
# 2014 palette
#system.pal <- brewer.pal(system.order.no, "Set1")

#SoE suggested Yellow-Green palette
system.pal <- c("#00441b","#41ab5d","#d9f0a3", "#f7fcf5")

#SoF suggested Blue-Green palette
#system.pal <- c("#e0f3bd","#a8ddb5","#43a2ca", "#f7fcf5")

names(system.pal) <- system.order


#SYSTEMS PLOT - plotting systems over time by sector using stacked area chart - large
silsystems.stack <- ggplot(data=silsystems.data,
                           aes(x = Year, y = Area, fill = System)) + 
  geom_area(aes(fill=System), size=.2, alpha=.7) + 
  xlab ("Year") + ylab ("Area (Hectares*1000)") +
  ggtitle ("Silvicultural Systems") +
  theme_soe() +
  scale_y_continuous(limits = c(0, 300), breaks=seq(0, 300, 30), 
                     expand=c(0,0)) +
  scale_x_continuous(limits = c(1987, 2015), breaks=seq(1988, 2015, 3), expand = c(0,0)) + #for large graph
 #  scale_x_continuous(limits = c(1987, 2015), breaks=seq(1987, 2015, 4), expand = c(0,0)) + #for small graph
  scale_fill_manual(name = "System", values = system.pal,
                    breaks = system.order) +
  geom_line(data=silsystems.total, aes(x = Year, y = Area),
            colour = "black", size = 1.3, show.legend = FALSE) +
  annotate("text", label = "Clearcutting", x = 1994, y = 75,
           size = 4, family = chart_font_web) +
  annotate("text", label = "Partial\nCutting", x = 1996, y = 240,
           size = 4, family = chart_font_web) +
  annotate("segment", x = 1996, xend = 1997.5, y = 220, yend = 178) +
  annotate("text", label = "Total Area\nHarvested", x = 2009, y = 255,
           size = 4, family = chart_font_web) +
  annotate("segment", x = 2009.5, xend = 2010.5, y = 236, yend = 200) +
  annotate("text", label = "Clearcutting with\n Reserves", x = 2008, y = 110,
           size = 4, family = chart_font_web) +
  theme(legend.position = "none", plot.title = element_text(size = 12, hjust = .5),
        panel.grid.major.x = (element_blank()), plot.margin = unit(c(5,5,5,5),"mm"))
plot(silsystems.stack)


###################################
# DISTURBANCES and REFORESTATION
##################################

## @knitr dist-refor

# drop columns
dist.refor <- dist.refor %>% 
  select(Fiscal_Year, Harvested_ha, Natural_Disturbance_ha,
Total_Disturbance_ha, Reforestation_ha)

# change column name
colnames(dist.refor)[colnames(dist.refor)=="Fiscal_Year"] <- "Year"

# using reshape to melt the data
dist.refor.long <- melt(dist.refor, id.vars="Year",
                        variable.name="Category", value.name="Hectares")

#remove _ha 
dist.refor.long$Category <- gsub("_ha", "", dist.refor.long$Category)

# total disturbance & refor dataframe
total.dist <- dist.refor.long %>% 
  filter(Category == "Total_Disturbance" | Category == "Reforestation") %>% 
  mutate(Area = round(Hectares/1000, digits=0))

# dist types dataframe
dist.data <- dist.refor.long %>% 
  filter(Category == "Harvested" | Category == "Natural_Disturbance") %>% 
  mutate(Area = round(Hectares/1000, digits=0))

#ordering factor
dist.data.order <- c( "Natural_Disturbance", "Harvested")
dist.data$Category <- factor(dist.data$Category, levels=dist.data.order)

#making colour palette for stacked geom

#SoE palette
dfPalette <- c("#41ab5d", "#d9f0a3")


#SoF suggested palette
#dfPalette <- c("#a6bddb", "#2b8cbe")


#DISTURBANCE PLOT - plotting disturbances and reforestation over time
dist.refor.plot <- ggplot(data=dist.data, aes(x = Year, y = Area, group = Category)) +
  geom_area(aes(fill=Category), size=.2, alpha=.7) + 
  geom_line(data=total.dist, aes(x = Year, y = Area, group = Category,
                                 colour = Category), size = 1.3) +
  xlab ("Year") + ylab ("Area (Hectares*1000)") +
  ggtitle ("Disturbances and Reforestation") +
  scale_y_continuous(limits = c(0,300), breaks=seq(0, 300, 30),
                     expand=c(0,0)) +
  scale_fill_manual(name = "Category", drop = FALSE, values = dfPalette,
                    breaks = dist.data.order, guide = FALSE) +
  scale_colour_manual(name = NULL, drop = FALSE, label = c("Reforestation", "Total Disturbance"),
                      values = c("#006d2c", "black")) +
  scale_x_continuous(limits = c(1987, 2015), breaks=seq(1988, 2015, 3), expand=c(0,0)) + # for large graph
 #  scale_x_continuous(limits = c(1987, 2015), breaks=seq(1987, 2015, 4), expand=c(0,0)) + # for small graph
   annotate("text", label = "Natural Disturbance",
            x = 2004, y = 140, size = 4, family = chart_font_web) +
   annotate("segment", x = 2007.5, xend = 2009, y = 152, yend = 170) +
   annotate("text", label = "Harvested",
            x = 1997, y = 87, size = 4, family = chart_font_web) +
  theme_soe() +
  theme(legend.position = c(.2,.96), legend.direction = "vertical",
        legend.background = element_rect(fill = "NA"),
        legend.text = element_text(size =11, family = chart_font_web),
        plot.title = element_text(size = 12,  hjust = .5),
        panel.grid.major.x = (element_blank()), plot.margin = unit(c(5,5,5,5),"mm"))
plot(dist.refor.plot)


###################################
# SILVICULTURAL TREATMENTS
##################################

## @knitr treat

# change column name
colnames(treatments)[colnames(treatments)=="Activity.Year"] <- "Fiscal_Year"

# using reshape to melt the data
treatments.long <- melt(treatments, id.vars="Fiscal_Year",
                        variable.name="Treatment", value.name="Hectares")

# removing underscores and ha from treatment names
treatments.long$Treatment <- gsub("_ha", "", treatments.long$Treatment)
treatments.long$Treatment <- gsub("_", " ", treatments.long$Treatment)

# creating total treatment category and rounding area
treatments.long.plot <- treatments.long %>%
  group_by(Fiscal_Year) %>%
  summarize(Hectares = sum(Hectares, na.rm=TRUE)) %>%
  mutate(Treatment = "Total") %>%
  rbind(treatments.long) %>%
  mutate(Area = round(Hectares/1000, digits=2))

# plot order
treat.sys.order <- c("Fertilizing", "Pruning", "Spacing", "Select Seed", "FFT Planting", "Total")
treatments.long.plot$Treatment <- factor(treatments.long.plot$Treatment, levels=treat.sys.order)

#removing NA rows
treatments.filtered <- treatments.long.plot[complete.cases(treatments.long.plot),]

#making colour palette
system.order.no <- length(treat.sys.order)+1
#tmPalette <- c("#e41a1c","#ff7f00", "#377eb8", "#4daf4a", "#984ea3", "#000000")
tmPalette <- c("#ff7f00","#662506", "#2171b5", "#41ab5d", "#00441b", "#000000")

#TREATMENTS PLOT - plotting treatments over time
treatment.plot <- ggplot(data=treatments.filtered, aes(x = Fiscal_Year,
                                                       y = Area, group=Treatment,
                                                       colour=Treatment)) + 
  geom_line(size = 1.3) + 
  xlab ("Year") + ylab ("Area (Hectares*1000)") +
  ggtitle ("Incremental Silviculture") + 
  scale_y_continuous(limits = c(0,200), breaks=seq(0, 200, 20),
                     expand = c(0,0)) +
  scale_x_continuous(limits = c(1987, 2015), breaks=seq(1988, 2015, 3), expand=c(0,0)) + # for small graph
  # scale_x_continuous(limits = c(1987, 2015), breaks=seq(1987, 2015, 4), expand=c(0,0)) + # for large graph
  scale_colour_manual(values = tmPalette, name = NULL) +
  guides(colour = guide_legend(reverse=TRUE)) +
  # annotate("text", label = "Total area treated", x = 2003.8, y = 145,
  #          size = 3, family = chart_font_web) +
  # annotate("segment", x = 2004, xend = 2007, y = 141, yend = 117) +
  # annotate("text", label = "Select Seed", x = 2003, y = 50,
  #          size = 3, family = chart_font_web) +
  # annotate("segment", x = 2002, xend = 2003, y = 60.5, yend = 53) +
  # annotate("text", label = "Spacing", x = 1993, y = 44,
  #          size = 3, colour = "black",  family = chart_font_web) +
  # annotate("segment", x = 1993, xend = 1994, y = 41, yend = 31.5) +
  # annotate("text", label = "Fertilizing", x = 2004, y = 27,
  #          size = 3, colour = "black",  family = chart_font_web) +
  # annotate("segment", x = 2004, xend = 2005.5, y = 23, yend = 13) +
  # annotate("text", label = "Pruning", x = 1996, y = 18,
  #          size = 3, family = chart_font_web) +
  # annotate("segment", x = 1996, xend = 1997, y = 15, yend = 7.5) +
  # annotate("text", label = "Rehabilitation\nplanting", x = 2010.5, y = 36.5,
  #          size = 3, family = chart_font_web) +
  # annotate("segment", x = 2011, xend = 2013.5, y = 29, yend = 14.5) +
  theme_soe() +
  theme(legend.position = c(.3,.70), legend.direction = "vertical",
        legend.background = element_rect(fill = "NA"),
        legend.text = element_text(size =11, family = chart_font_web),
        plot.title = element_text(size = 12,  hjust = .5),
        panel.grid.major.x = (element_blank()), plot.margin = unit(c(5,5,5,5),"mm"))
plot(treatment.plot)


###################################
# TIMBER VOLUME GAINS
##################################

## @knitr gains

# using reshape to melt the data 
gains.long <- melt(gains, id.vars="Fiscal_Year",
                   variable.name="Treatment", value.name="Volume_gain_m3_per_ha")

# removed underscores and ha etc
gains.long$Treatment <- gsub("_m3.ha", "", gains.long$Treatment)
gains.long$Treatment <- gsub("_", " ", gains.long$Treatment)

# Order of treatments to be displayed (bottom-up) in stacked area chart
treatment.order <- c("FFT Planting Volume", "Select Seed Volume", "Spacing Volume", "Fertilization Volume")

gains.long$Treatment <- factor(gains.long$Treatment, levels = treatment.order)

## removing NA rows and creating new area columns
gains.filtered <- gains.long[complete.cases(gains.long),] %>%
  mutate(Volume = round(Volume_gain_m3_per_ha/1000, digits = 3))

#subsetting total values for line overlay on stacked area chart
gains.total <- gains.filtered %>%
  group_by(Fiscal_Year) %>%
  summarise(Volume = sum(Volume)) %>%
  mutate(Treatment = "Total")

treatment.order.no <- length(treatment.order)+1

#creating colour palette for graphs
#treatment.pal <- brewer.pal(treatment.order.no, "Set1")
treatment.pal <- c("#00441b","#41ab5d", "#2171b5","#ff7f00", "#000000")
names(treatment.pal) <- treatment.order


#GAINS PLOTS - plotting systems over time by sector using stacked area chart
gains.stack <- ggplot(data=gains.filtered, aes(x = Fiscal_Year,
                                               y = Volume, fill = Treatment)) + 
  geom_area(aes(fill=Treatment), size=.2, alpha=.7) + 
  xlab("Year") +  ylab(expression(paste("Volume ","(",m^3, "*1000)"))) +
  ggtitle ("Timber Volume Gains from Incremental Silviculture") +
  scale_y_continuous(limits = c(0, 12000), breaks=seq(0, 12000, 1200),
                     expand=c(0,0)) +
  scale_x_continuous(limits = c(1987, 2015), breaks=seq(1988, 2015, 3), expand=c(0,0)) + # for large graph
  # scale_x_continuous(limits = c(1987, 2015), breaks=seq(1987, 2015, 4), expand=c(0,0)) + # for small graph
  scale_fill_manual(name = NULL, values = treatment.pal,
                    breaks = treatment.order) +
  geom_line(data=gains.total, aes(x = Fiscal_Year, y = Volume),
            colour = "black", size = 1.3) +
   annotate("text", label = "Total timber volume gain expected\n 65 years after treatment",
            x = 2003, y = 10200, size = 4, family = chart_font_web) +
   annotate("segment", x = 2003, xend = 2010, y = 9300, yend = 6200) +
  # annotate("text", label = "Fertilizing", x = 2006, y =5.8,
  #          size = 3, family = chart_font_web) +
  # annotate("segment", x = 2006.2, xend = 2008.3, y = 5.5, yend = 4.6) +
  # annotate("text", label = "Spacing", x = 1993, y = 0.4,
  #          size = 3, family = chart_font_web) +
  # annotate("text", label = "Select seed", x = 2010, y = 3.2,
  #          size = 3, family = chart_font_web) +
  # annotate("text", label = "Rehabilitation\nPlanting", x = 2011, y = 1,
  #          size = 3, family = chart_font_web) +
  theme_soe() +
  theme(legend.position = c(.35,.5), legend.direction = "vertical",
        legend.background = element_rect(fill = "NA"),
        legend.text = element_text(size =11, family = chart_font_web),
        plot.title = element_text(size = 12,  hjust = .5),
        panel.grid.major.x = (element_blank()), plot.margin = unit(c(5,5,5,5),"mm"))
plot(gains.stack)

# Set plot dimensions:
lg_w.px <- 836
lg_h.px <- 574
sm_w.px <- 455
sm_h.px <- 315

lg_dpi <- 100
sm_dpi <- 72

ggsave("graphs/systems_small.png", plot = silsystems.stack, type = "cairo-png", 
       width = sm_w.px / sm_dpi, height = sm_h.px / sm_dpi, units="in", 
       dpi=sm_dpi)

ggsave("graphs/systems_large.png", plot = silsystems.stack, type="cairo-png",  
       width = lg_w.px / lg_dpi, height = lg_h.px / lg_dpi, units="in", 
       dpi=lg_dpi)

ggsave("graphs/dist_refor_small.png", plot = dist.refor.plot, type = "cairo-png", 
       width = sm_w.px / sm_dpi, height = sm_h.px / sm_dpi, units="in", 
       dpi=sm_dpi)

ggsave("graphs/dist_refor_large.png", plot = dist.refor.plot, type="cairo-png",  
       width = lg_w.px / lg_dpi, height = lg_h.px / lg_dpi, units="in", 
       dpi=lg_dpi)

ggsave("graphs/treatment_small.png", plot = treatment.plot, type = "cairo-png", 
       width = sm_w.px / sm_dpi, height = sm_h.px / sm_dpi, units="in", 
       dpi=sm_dpi)

ggsave("graphs/treatment_large.png", plot = treatment.plot, type="cairo-png",  
       width = lg_w.px / lg_dpi, height = lg_h.px / lg_dpi, units="in", 
       dpi=lg_dpi)

ggsave("graphs/gains_small.png", plot = gains.stack, type = "cairo-png", 
       width = sm_w.px / sm_dpi, height = sm_h.px / sm_dpi, units="in", 
       dpi=sm_dpi)

ggsave("graphs/gains_large.png", plot = gains.stack, type="cairo-png",  
       width = lg_w.px / lg_dpi, height = lg_h.px / lg_dpi, units="in", 
       dpi=lg_dpi)