## Tree Seed Use in BC indicator plots 

library(readr) #read in CSV files
library(dplyr) #data prep
library(reshape2) #for restructuring data table
library(bcmaps) #get bc boundary and nrs boundaries, package from GitHub
library(ggplot2)  #for plotting
library(extrafont) #Verdana font
library(envreportutils) #soe theme & svg_px(), package from GitHub
library(rphylopic) #for conifer image, package from GitHub
library(scales) #for pretty_breaks()
library(rmapshaper) # for intersect and simplify functions
library(sf) # mapping

here::i_am("tree_seed_use/tree_seed_use.R")

##font selection
chart_font_web <- "Verdana"

## @knitr pre

## DATA
## load tree seed use results data from the BC Data Catalogue (data licence: Open Government Licence-British Columbia)

bc_forest <- read_csv("https://catalogue.data.gov.bc.ca/dataset/54ec827b-3b9a-4fea-8d9b-d8c006e5b9cc/resource/9e329a4d-1648-4c64-bb86-2cebba2517a2/download/bcregen.csv")
district_forest <- read_csv("https://catalogue.data.gov.bc.ca/dataset/54ec827b-3b9a-4fea-8d9b-d8c006e5b9cc/resource/a9f93154-5c3a-4752-bc34-cb3cbaff45c2/download/districtregen.csv")
# bc_forest <- read_csv(here("data/bc_regen.csv"))
# district_forest <- read_csv(here("data/district_regen.csv"))


theme_map <- function() {
  theme_bw() +     
    theme(axis.title = element_blank(), 
          axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank(),
          panel.border = element_blank(), 
          plot.margin = unit(c(5, 0, 5, 0), "mm"), 
          strip.background = element_blank(), 
          strip.text = element_text(vjust = 0, hjust = 0.4, size = rel(0.9))
    )  
}


## @knitr bc_regen

## BC forest regeneration by tree seed use stacked area plot

# rename Year column
colnames(bc_forest)[colnames(bc_forest)=="Fiscal_Yr"] <- "Year"
bc_forest$planted_total <- bc_forest$Planted_Natural_Stand_Superior_ha + bc_forest$Planted_Orchard_ha + bc_forest$Planted_Natural_Stand_NonSuperior_ha
bc_forest$reforest_total <- bc_forest$planted_total + bc_forest$Natural_Regeneration_ha
bc_forest$ss_total <- bc_forest$Planted_Natural_Stand_Superior_ha + bc_forest$Planted_Orchard_ha
bc_forest$ss_perc <- round((bc_forest$ss_total/bc_forest$planted_total)*100, digits = 0)

## restructuring the csv table for plotting
bc_long <- melt(bc_forest, id.vars = "Year", 
                variable.name = "seed", value.name = "hectares")

#remove _ha and underscores from names
bc_long$seed <- gsub("_ha", "", bc_long$seed)
bc_long$seed <- gsub("_", " ", bc_long$seed)

## creating data frame for total line
bc_total <- bc_long %>% 
  filter(seed == "reforest total") %>% 
  mutate(area = round(hectares/1000, digits=0))

## creating data frame for seed stacked geom
bc_seed <- bc_long %>% 
  filter(seed != "reforest total") %>% 
  filter (seed != "planted total") %>% 
  filter (seed != "ss total") %>% 
  filter (seed != "ss perc") %>% 
  mutate(area = round(hectares/1000, digits=0))

## arranging the order of the species to be plotted
seed.order <- c("Planted Orchard", "Planted Natural Stand Superior",
                "Planted Natural Stand NonSuperior", "Natural Regeneration")

## reordering the variables for plotting
bc_seed$seed <- factor(bc_seed$seed, levels = seed.order)

## matching colour name with variable names
## colours and HEX codes from http://colorbrewer2.org/
pal <- c("#99cc00", "#339966", "#ccffcc", "#ffff99")
names(pal) <- seed.order

## add tree image
conifer <- image_data("f86235e3-f437-4630-9e77-73732b9bcf41", size = "512")[[1]]

## stacked area plot
forest_regen <- ggplot(bc_seed, aes(x = Year, y = area)) +
  geom_area(data = bc_seed, aes(x = Year, y = area, fill = seed), alpha = 0.7) +
  scale_fill_manual(values = pal, breaks = seed.order, guide = guide_legend(title = "", label.position = "top"),
                    labels = c("Planted, Orchard\nSeed", "Planted, Natural Stand\nSuperior Seed", "Planted, Natural Stand\nNon-Superior Seed", "Natural\nRegeneration")) +
  geom_line(data = bc_total, aes(x = Year, y = area, colour = "Total Area\nReforested"), alpha = 0.7, size = 1.3) +
  scale_color_manual(values = ("Total Area\nReforested" = "black"),
                     guide = guide_legend(order=1, title = "")) +
  xlab("Year") +
  ylab("Area Reforested (Hectares*1000)") +
  labs(caption = "\n**Note: Data for the 2013-2019 period is incomplete pending\nreporting of planting and natural regeneration field surveys") +
  scale_x_continuous(limits = c(1987, 2019), breaks=seq(1987, 2019, 4), expand = c(0,0)) +
  scale_y_continuous(limits = c(0, 300), breaks = seq(0, 300, 30), expand=c(0, 0)) +
  theme_soe() +
  theme(panel.grid.major.x = element_blank(),
        legend.text = element_text(size = 14),
        legend.text.align = 0,
        axis.title = element_text(size=16),
        axis.text = element_text(size=12),
        plot.margin = unit(c(5,5,5,5),"mm"),
        legend.background = element_rect(colour = "white"),
        plot.caption = element_text(size = 12)) + 
  add_phylopic(conifer, alpha = .7, color = "grey30", ysize = 100, x = 1992, y = 140)
plot(forest_regen)


## @knitr map_regen

## Change in Proportion of select seed use by District MAP using district_forest

## substituting NAs with 0
district_forest[is.na(district_forest)] <- 0

## change negative numbers to zero in Nat Regen column
district_forest$Natural_Regeneration_ha[district_forest$Natural_Regeneration_ha < 0] <- 0

## create derived total columns
district_forest$planted_total <- district_forest$B_Plus_Superior_ha + district_forest$A_Orchard_ha + district_forest$B_NonSuperior_ha
#district_forest$regen_total <- district_forest$planted_total + district_forest$Natural_Regeneration_ha
district_forest$selectseed_total <- district_forest$B_Plus_Superior_ha + district_forest$A_Orchard_ha
district_forest$prop_ss <- round(((district_forest$selectseed_total/district_forest$planted_total)*100), digits = 0)

## Remove region and individual seed type columns
district_seed <- subset(district_forest, select = -c(ROLLUP_REGION_CODE, Natural_Regeneration_ha,
                                                      B_Plus_Superior_ha, A_Orchard_ha, B_NonSuperior_ha))

bc_ss <- district_seed %>% 
  group_by(Year) %>% 
  summarize(across(ends_with("total"), sum)) %>% 
  mutate(prop_ss = selectseed_total / planted_total)

## subsetting data for facet map plot
yrs <- round(seq(min(district_seed$Year), max(district_seed$Year), length.out = 6))
seed_change <- district_seed %>% 
  filter(district_seed$Year %in% yrs)

## NRS District Map
nrdis <- nr_districts()
bc <- bc_bound()
nrs_dis_map <- ms_clip(nrdis, bc) ## both layers in bcmaps
nrs_dis_map <- ms_simplify(nrs_dis_map, keep = 0.02)
#plot(nrs_dis_map)

## JOIN MAP AND TABULAR DATA
## joining shapefile with data table
# seed_map <- fortify(nrs_dis_map, region = "ORG_UNIT") #converts spatial polygon to a dataframe
seed_map <- left_join(nrs_dis_map, seed_change, by = c("ORG_UNIT" = "ORG_UNIT_CODE"))

## plotting the facet comparison map
seed_map_plot <- ggplot(seed_map) +
  geom_sf(aes(fill = prop_ss), colour = "grey60", size = 0.3) +
  facet_wrap(~Year, ncol = 3) +
  scale_fill_continuous(name = "Percentage of\nForest Planted\nUsing Select Seed (%)", low = "#ffffcc", 
                        high = "#006837", na.value = "grey81", 
                        breaks = pretty_breaks(6), 
                        guide = guide_colourbar(draw.llim = TRUE, 
                                                draw.ulim = TRUE)) +
  theme_map() +
   theme(legend.title = element_text(face = "bold", size = 14),
         legend.text = element_text(size = 14),
         strip.text.x = element_text(size = 14)) 
plot(seed_map_plot)

## @knitr stop

##### SAVE PLOTS for WEB PAGE
dir.create('out', showWarnings = FALSE)

## save BC regeneration plots as SVG
svg_px(file = "./out/forest_regen_chart.svg", width = 836, height = 489)
plot(forest_regen)
dev.off()

## save facet seed planting change map
png_retina(filename = "./out/district_seed_change_map.png", width = 900, units = "px", type = "cairo-png")
plot(seed_map_plot)
dev.off()

# ## save BC regeneration plots as PNG
# png(filename = "./out/forest_regen_chart.png", width = 836, height = 489, units = "px", type = "cairo-png")
# plot(forest_regen)
# dev.off()

# ## save facet seed planting change map
# png(filename = "./out/district_seed_change_map.png", width = 900, units = "px", type = "cairo-png")
# plot(seed_map_plot)
# dev.off()
