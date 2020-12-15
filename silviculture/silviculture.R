## Silviculture indicator plots

## Loading packages for script
library(readr) #read CSV file
library(dplyr) #data cleaning
library(tidyr) #wide to long df format
library(ggplot2) #plotting
library(envreportutils) #for soe_theme(), package from GitHub
library(here)
# library(extrafont) #Verdana font

here::i_am("silviculture/silviculture.R")

## Set chart font
chart_font_web <- "Verdana"

## @knitr pre

## Read in CSV files from the BC Data Catalogue (data licence: Open Government Licence-British Columbia)


# silsystems <- read_csv("https://catalogue.data.gov.bc.ca/dataset/b3369823-d130-4e8d-bcca-55c8749fab40/resource/d5d56912-5389-467d-9d90-5732df4df9c6/download/silviculturesystems.csv")
# dist.refor <- read_csv("https://catalogue.data.gov.bc.ca/dataset/b3369823-d130-4e8d-bcca-55c8749fab40/resource/38fc6c11-4930-4e44-a755-5f75c46fa46e/download/disturbanceandreforestation.csv")
# treatments <- read_csv("https://catalogue.data.gov.bc.ca/dataset/b3369823-d130-4e8d-bcca-55c8749fab40/resource/33055c65-ac86-4612-a4e8-bda7e1973830/download/silviculturetreatments.csv")
# gains <- read_csv("https://catalogue.data.gov.bc.ca/dataset/b3369823-d130-4e8d-bcca-55c8749fab40/resource/005df3ca-e6be-4c11-996d-fbea182e280d/download/timbervolumegains.csv")
silsystems <- read_csv(here("data/silviculture_systems.csv"))
dist.refor <- read_csv(here("data/disturbance_and_reforestation.csv"))
treatments <- read_csv(here("data/silviculture_treatments.csv"))
gains <- read_csv(here("data/timber_volume_gains.csv"))



## @knitr silviculture

## sum harvest categories, reshape from wide to long format,
## rename Year column and remove _ha from System names
silsystems.long <- silsystems %>% 
  rowwise() %>% 
  mutate(Total = sum(Clearcutting_with_reserves_ha, Clearcutting_ha, Partial_cutting_ha, na.rm=TRUE)) %>% 
  gather(key = System, value = Hectares, Clearcutting_with_reserves_ha,
         Clearcutting_ha, Partial_cutting_ha, Total) %>% 
  rename(Year = Fiscal_Year) %>%
  mutate(System = gsub("_ha", "", System))

## total dataframe
silsystems.total <- silsystems.long %>% 
  filter(System == "Total") %>% 
  mutate(Area = round(Hectares/1000, digits=0))

## systems dataframe
silsystems.data <- silsystems.long %>% 
  filter(System != "Total") %>% 
  mutate(Area = round(Hectares/1000, digits=0))

## creating a list for ordering factors
system.order <- c("Partial_cutting" ,"Clearcutting_with_reserves",
                  "Clearcutting")

## ordering factor for plotting
silsystems.data$System <- factor(silsystems.data$System, levels=system.order)
system.order.no <- length(system.order)+1

## Yellow-Green colour palette
system.pal <- c("#00441b","#41ab5d","#d9f0a3", "#f7fcf5")
names(system.pal) <- system.order

## Silvicultural Systems Plot (systems over time by sector using stacked area chart)
silsystems.stack <- ggplot(data=silsystems.data,
                           aes(x = Year, y = Area, fill = System)) + 
  geom_area(aes(fill=System), size=.2, alpha=.7) + 
  xlab ("Year") + ylab ("Area (Hectares*1000)") +
#  ggtitle ("Silvicultural Systems") +
  theme_soe() +
  scale_y_continuous(limits = c(0, 300), breaks=seq(0, 300, 30), 
                     expand=c(0,0)) +
  scale_x_continuous(limits = c(1987, 2019), breaks=seq(1987, 2019, 4), expand = c(0,0)) + 
  scale_fill_manual(name = "System", values = system.pal,
                    breaks = system.order) +
  geom_line(data=silsystems.total, aes(x = Year, y = Area),
            colour = "black", size = 1.3, show.legend = FALSE) +
  annotate("text", label = "Clearcutting", x = 1994, y = 75,
           size = 5, family = chart_font_web) +
  annotate("text", label = "Partial\nCutting", x = 1996, y = 240,
           size = 5, family = chart_font_web) +
  annotate("segment", x = 1996, xend = 1997.5, y = 220, yend = 176) +
  annotate("text", label = "Total Area\nHarvested", x = 2009, y = 255,
           size = 5, family = chart_font_web) +
  annotate("segment", x = 2009.5, xend = 2010.5, y = 236, yend = 202) +
  annotate("text", label = "Clearcutting with\n Reserves", x = 2008, y = 110,
           size = 5, family = chart_font_web) +
  theme(legend.position = "none",
 #       plot.title = element_text(size = 12, hjust = .5),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        panel.grid.major.x = (element_blank()),
        plot.margin = unit(c(5,10,5,5),"mm"))
plot(silsystems.stack)


## @knitr dist-refor

## drop colomes, change names, reshape df wide to long, remove -ha from Category name 
dist.refor.long <- dist.refor %>%
  select(Fiscal_Year, Harvested_ha, Natural_Disturbance_ha,
         Total_Disturbance_ha, Reforestation_ha) %>% 
  rename(Year = Fiscal_Year) %>%
  gather(key = Category, value = Hectares, Harvested_ha, Natural_Disturbance_ha,
         Total_Disturbance_ha, Reforestation_ha) %>%
  mutate(Category = gsub("_ha", "", Category))
  
## total disturbance & refor dataframe
total.dist <- dist.refor.long %>% 
  filter(Category == "Total_Disturbance" | Category == "Reforestation") %>% 
  mutate(Area = round(Hectares/1000, digits=0))

## disturbance types dataframe
dist.data <- dist.refor.long %>% 
  filter(Category == "Harvested" | Category == "Natural_Disturbance") %>% 
  mutate(Area = round(Hectares/1000, digits=0))

## make and order factors for plotting
dist.data.order <- c("Natural_Disturbance", "Harvested")
dist.data$Category <- factor(dist.data$Category, levels=dist.data.order)

## colour palette for stacked geom
dfPalette <- c("#41ab5d", "#d9f0a3")

## disturbance plot (plotting disturbances and reforestation over time)
dist.refor.plot <- ggplot(data=dist.data, aes(x = Year, y = Area, group = Category)) +
  geom_area(aes(fill=Category), size=.2, alpha=.7) + 
  geom_line(data=total.dist, aes(x = Year, y = Area, group = Category,
                                 colour = Category), size = 1.3) +
  xlab ("Year") + ylab ("Area (Hectares*1000)") +
 # ggtitle ("Disturbances and Reforestation") +
  scale_y_continuous(limits = c(0,330), breaks=seq(0, 330, 30),
                     expand=c(0,0)) +
  scale_fill_manual(name = "Category", drop = FALSE, values = dfPalette,
                    breaks = dist.data.order, guide = FALSE) +
  scale_colour_manual(name = NULL, drop = FALSE, label = c("Reforestation", "Total Disturbance"),
                      values = c("#006d2c", "black")) +
  scale_x_continuous(limits = c(1987, 2019), breaks=seq(1987, 2019, 4), expand=c(0,0)) +
   annotate("text", label = "Natural Disturbance",
            x = 2004, y = 140, size = 5, family = chart_font_web) +
   annotate("segment", x = 2007.5, xend = 2009, y = 152, yend = 170) +
   annotate("text", label = "Harvested",
            x = 1997, y = 87, size = 5, family = chart_font_web) +
  theme_soe() +
  theme(legend.position = c(.2,.96),
        legend.direction = "vertical",
        legend.background = element_rect(fill = "NA"),
        legend.text = element_text(size =14, family = chart_font_web),
 #       plot.title = element_text(size = 12,  hjust = .5),
        panel.grid.major.x = (element_blank()),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        plot.margin = unit(c(5,10,5,5),"mm"))
plot(dist.refor.plot)


## @knitr treat

## rename year column, total category, change df from wide to long format, remove _ha from Treatment names,
## create area column and filter our NAs
treatments.long <- treatments %>% 
  rename(Fiscal_Year = `Activity Year`) %>%
  rowwise() %>% 
  mutate(Total = sum(Pruning_ha, Spacing_ha, Fertilizing_ha, FFT_Planting_ha, Select_Seed_ha, na.rm=TRUE)) %>% 
  gather(key = Treatment, value = Hectares, Pruning_ha, Spacing_ha,     
         Fertilizing_ha, FFT_Planting_ha, Select_Seed_ha, Total) %>%
  mutate(Treatment = gsub("_ha", "", Treatment)) %>% 
  mutate(Treatment = gsub("_", " ", Treatment)) %>% 
  mutate(Area = round(Hectares/1000, digits=2)) %>% 
  mutate(Treatment = case_when(
    Treatment == "FFT Planting" ~ "Planting", 
    Treatment == "Fertilizing" ~ "Areal Fertilizing",
    TRUE ~ Treatment
  )) %>% 
  na.omit

## make and order factors for plotting
treat.sys.order <- c("Areal Fertilizing", "Pruning", "Spacing", "Select Seed", "Planting", "Total")
treatments.long$Treatment <- factor(treatments.long$Treatment, levels=treat.sys.order)

## colour palette
system.order.no <- length(treat.sys.order)+1
tmPalette <- c("#ff7f00","#662506", "#2171b5", "#41ab5d", "#00441b", "#000000")

## treatments plot (plotting treatments over time)
treatment.plot <- ggplot(data=treatments.long,
                         aes(x = Fiscal_Year, y = Area, group=Treatment, colour=Treatment)) + 
  geom_line(size = 1.3) + 
  xlab ("Year") + ylab ("Area (Hectares*1000)") +
#  ggtitle ("Incremental Silviculture") + 
  scale_y_continuous(limits = c(0,200), breaks=seq(0, 200, 20),
                     expand = c(0,0)) +
  scale_x_continuous(limits = c(1987, 2018), breaks=seq(1988, 2018, 3), expand=c(0,0)) + 
  scale_colour_manual(values = tmPalette, name = NULL) +
  guides(colour = guide_legend(reverse=TRUE)) +
  theme_soe() +
  theme(legend.position = c(.3,.70),
        legend.direction = "vertical",
        legend.background = element_rect(fill = "NA"),
        legend.text = element_text(size =14, family = chart_font_web),
 #       plot.title = element_text(size = 12,  hjust = .5),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        panel.grid.major.x = (element_blank()),
        plot.margin = unit(c(5,10,5,5),"mm"))
plot(treatment.plot)


## @knitr gains

## reshape df from wide to long, remove _ and units from Treatment names, create Volume column, drop NAs
gains.long <-  gains %>% 
  rowwise() %>% 
  mutate(Total = sum(`FFT_Planting_Volume_m3/ha`, `Select_Seed_Volume_m3/ha`,
                     `Spacing_Volume_m3/ha`, `Fertilization_Volume_m3/ha`, na.rm=TRUE)) %>% 
  gather(key = Treatment, value = Volume_gain_m3_per_ha,
         `FFT_Planting_Volume_m3/ha`, `Select_Seed_Volume_m3/ha`,
         `Spacing_Volume_m3/ha`, `Fertilization_Volume_m3/ha`) %>%
  mutate(Treatment = gsub("_m3.ha", "", Treatment)) %>% 
  mutate(Treatment = gsub("_", " ", Treatment)) %>% 
  mutate(Volume = round(Volume_gain_m3_per_ha/1000, digits = 3)) %>% 
  mutate(Treatment = case_when(
    Treatment == "FFT Planting Volume" ~ "Planting Volume", 
    Treatment == "Fertilization Volume" ~ "Areal Fertilization Volume",
    TRUE ~ Treatment
  )) %>% 
  na.omit

## a total values df for line overlay on stacked area chart
gains.total <- gains.long %>%
  group_by(Fiscal_Year) %>%
  summarise(Volume = sum(Volume, na.rm=TRUE)) %>%
  mutate(Treatment = "Total")

## order of treatments to be displayed (bottom-up) in stacked area chart
treatment.order <- c("Planting Volume", "Select Seed Volume", "Spacing Volume", "Areal Fertilization Volume")
gains.long$Treatment <- factor(gains.long$Treatment, levels = treatment.order)

## creating colour palette for graphs
treatment.order.no <- length(treatment.order)+1
treatment.pal <- c("#00441b","#41ab5d", "#2171b5","#ff7f00", "#000000")
names(treatment.pal) <- treatment.order

## gains plot (plotting systems over time by sector using stacked area chart)
gains.stack <- ggplot(data=gains.long, aes(x = Fiscal_Year, y = Volume, fill = Treatment)) + 
  geom_area(aes(fill=Treatment), size=.2, alpha=.7) + 
  xlab("Year") +  ylab(expression(paste("Volume", " ","(",m^3, "*1000)"))) +
#  ggtitle ("Timber Volume Gains from Incremental Silviculture") +
  scale_y_continuous(limits = c(0, 10000), breaks=seq(0, 10000, 1000),
                     expand=c(0,0)) +
  scale_x_continuous(limits = c(1987, 2019), breaks=seq(1987, 2019, 4), expand=c(0,0)) +
  scale_fill_manual(name = NULL, values = treatment.pal,
                    breaks = treatment.order) +
  geom_line(data=gains.total, aes(x = Fiscal_Year, y = Volume),
            colour = "black", size = 1.3) +
   annotate("text", label = "Total timber volume gain expected\n 65 years after treatment",
            x = 2003, y = 8000, size = 5, family = chart_font_web) +
   annotate("segment", x = 2003, xend = 2010.5, y = 7500, yend = 6000) +
  theme_soe() +
  theme(legend.position = c(.35,.5),
        legend.direction = "vertical",
        legend.background = element_rect(fill = "NA"),
        legend.text = element_text(size =14, family = chart_font_web),
  #      plot.title = element_text(size = 12,  hjust = .5),
        panel.grid.major.x = (element_blank()),
        axis.title = element_text(size = 16),
        axis.text = element_text(size = 12),
        plot.margin = unit(c(5,10,5,5),"mm"))
plot(gains.stack)

## @knitr end

## Print plots to SVG for web
svg_px(file = "./out/systems.svg", width = 836, height = 489)
plot(silsystems.stack)
dev.off()

svg_px(file = "./out/dist_refor.svg", width = 836, height = 489)
plot(dist.refor.plot)
dev.off()

svg_px(file = "./out/treatment.svg", width = 836, height = 489)
plot(treatment.plot)
dev.off()

svg_px(file = "./out/gains.svg", width = 836, height = 489)
plot(gains.stack)
dev.off()

# ## Print plots to PNG (for retina quality) for web
# png_retina(filename = "./out/systems.png", width = 500, units = "px", type = "cairo-png")
# plot(silsystems.stack)
# dev.off()
# 
# png_retina(filename = "./out/dist_refor.png", width = 500, units = "px", type = "cairo-png")
# plot(dist.refor.plot)
# dev.off()
# 
# png_retina(filename = "./out/treatment.png", width = 500, units = "px", type = "cairo-png")
# plot(treatment.plot)
# dev.off()
# 
# png_retina(filename = "./out/gains.png", width = 500, units = "px", type = "cairo-png")
# plot(gains.stack)
# dev.off()
