### Author: Joao Macosso 42255
### repos: github/macosso



# load Library

library(tidyverse)
library(WDI)
library(rnaturalearth)
library(cowplot)
library(sf)
library(ggmap)
library(leaflet)
library(treemapify)
library(ggrepel)
library(scales)
library(RColorBrewer)

options(ggrepel.max.overlaps = Inf) # This just allow labeling items over others


rm(list = ls()) ## Clear all the variables, careful

# install.packages(rstudioapi)
# This sets sets the working directory to the path of this source code
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# Note the Dataset is extracted from world bank world development indicators
# Through the package WDI
# I have saved the data in rdata, just in case there might be an error reading the file

# indicators <- c("NY.GDP.MKTP.CD",
#                 "NY.GDP.PCAP.CD",
#                 "BX.GSR.GNFS.CD",
#                 "SE.PRM.TENR",
#                 "SE.PRM.UNER.ZS",
#                 "SE.PRM.UNER.FE.ZS",
#                 "SE.PRM.UNER.MA.ZS",
#                 "NY.GDP.PCAP.PP.CD",
#                 "SE.XPD.CTOT.ZS",
#                 "SE.TER.CUAT.BA.FE.ZS",
#                 "SP.DYN.LE00.IN",
#                 "SP.DYN.LE00.MA.IN",
#                 "SP.DYN.LE00.FE.IN",
#                 "SI.POV.GINI",
#                 "SI.DST.10TH.10",
#                 "SL.TLF.ADVN.ZS",
#                 "NE.TRD.GNFS.ZS")
# 
# WDIDATASET <- WDI(indicator = indicators,
#                 country = "all", start=1990, end=2020,extra=T) %>%
#   dplyr::select(-c(capital,longitude,latitude))
# 
# save.image("data/WDI.RData")
load('data/WDI.RData')



WDIDATASET <- rename(WDIDATASET, 
                   GDP = NY.GDP.MKTP.CD, 
                   GDP_p_Cap = NY.GDP.PCAP.CD,
                   GDP_p_Cap_PPP = NY.GDP.PCAP.PP.CD,
                   Export = BX.GSR.GNFS.CD,
                   Prim_Enrol = SE.PRM.TENR,
                   Child_out_School = SE.PRM.UNER.ZS,
                   Child_out_School_female = SE.PRM.UNER.FE.ZS,
                   Child_out_School_male = SE.PRM.UNER.MA.ZS,
                   Ed_Expend = SE.XPD.CTOT.ZS,
                   Ed_Attain_25 = SE.TER.CUAT.BA.FE.ZS,
                   Life_exp = SP.DYN.LE00.IN,
                   Life_exp_Male = SP.DYN.LE00.MA.IN,
                   Life_exp_Female =SP.DYN.LE00.FE.IN,
                   GINI_Index = SI.POV.GINI,
                   Income_Held_by_10 = SI.DST.10TH.10,
                   Advanced_ed_labor =SL.TLF.ADVN.ZS,
                   Trade = NE.TRD.GNFS.ZS)


WDI_Data <- WDIDATASET
# List of World Bank regions -----------------------
WB_regions <- unique(WDI_Data$region)[!unique(WDI_Data$region) %in% c("Aggregates", NA)]


WDI_Data_Aggregates <- WDIDATASET %>%
  filter(country %in% WB_regions & (is.na(region) | region == "Aggregates"))


WDI_Data <- WDI_Data %>%
  filter(!is.na(region) & region != "Aggregates")

WDI_Data <- WDI_Data %>%
  mutate(income = factor(income, levels = c("High income", "Upper middle income",
                                          "Lower middle income", "Low income")))
head(WDI_Data_Aggregates)
unique(WDI_Data_Aggregates$country)


ggplot(WDI_Data_Aggregates, aes(y = GDP_p_Cap, x = year, col = country)) +
  geom_line()


world <- ne_countries(scale = "medium", returnclass = "sf")


WDI_Data %>% dplyr::filter(!is.na(Child_out_School)) %>%
  group_by(iso2c, country, iso3c,
           region,income, lending) %>%
  arrange(year) %>%
  summarise(Child_out_School = last(Child_out_School),
            year = last(year),
            GDP_p_Cap_PPP = last(GDP_p_Cap_PPP)) %>%
  ungroup() -> Child_out_school_df


colnames(world)
world <- world %>%
  left_join(Child_out_school_df %>% 
              dplyr::select(-country), by = c("iso_a2" = "iso2c"))

world$name_long[is.na(world$Child_out_School)]

format_sep <- function(x) format(x, big.mark = ' ')

# Children Out of School --------------------------------
## World -----------------------
(world_map<- ggplot(data = world)  +
  coord_sf(expand = FALSE) +
  geom_sf(aes(fill =  Child_out_School)) +
  geom_rect(xmin = -18, xmax = 50, ymin = -35, ymax = 25, 
            fill = NA, colour = "black", linewidth = 1) +
   labs(title = "Children out of School") +
  scale_fill_viridis_c(option = "H", trans = "sqrt",
                       direction = 1,
                       begin = 0.40,
                       breaks = seq(0, 30, by=5),
                       labels = paste0(seq(0, 30, by=5), "%"),
                       guide = guide_colorbar(barwidth = 30, barheight = 1), 
                       name = "Children Out of School") +
  theme(legend.position = 'none',
        panel.background = element_rect(fill = "azure"),
        panel.border = element_rect(fill = NA),
        plot.title = element_text(hjust = 0.5)))

## Sub-Saharan Africa
(SSA_Map <- ggplot(data = world) +
  geom_sf(aes(fill = Child_out_School)) +
  annotate(geom = "text", x = -1, y = -12, 
           label = "Sub-Saharan \n Africa", 
           fontface = "italic", 
           color = "grey22", size = 3) +
  coord_sf(xlim = c(-18, 50), ylim = c(-35, 25), expand = FALSE) +
  scale_fill_viridis_c(option = "H", trans = "sqrt",
                       direction = 1,
                       begin = 0.40,
                       breaks = seq(0, 30, by=10),
                       labels = paste(seq(0, 30, by=10), "%"), 
                       name = "Children Out of School") +
  theme(legend.position = "right", axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        panel.background = element_rect(fill = "azure"), 
        panel.border = element_rect(fill = NA)))

## Combine World and Sub Saharan
(plot_grid(world_map, SSA_Map,
                      nrow = 2, rel_widths = c(1.5, 1)) -> Child_out_school_plt )

ggsave("Children Out of Education.png",
       plot = Child_out_school_plt,
       width = 15.02,
       height = 8.85,
       path = "./Plots")

## Rank countries by Children out of school ------------------
par(mar=c(3,4,2,2))
display.brewer.all()

Child_out_school_df %>% filter(year == 2020) %>%
  top_n(-10, Child_out_School) %>%
  arrange(Child_out_School) %>%
  ggplot(aes( y = reorder(country, -Child_out_School), 
         x = Child_out_School/100, fill = income)) +
  geom_bar(stat="identity") + 
  geom_text(aes(label = paste(round(Child_out_School,2),"%")),
            position = position_stack(vjust = 1.05), size = 3) +
  scale_x_continuous(labels = scales::percent) +
  theme_cowplot() +
  theme(axis.title.y = element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  labs(title = "Countries with the Lowest Percentage of Children out of school",
        x = "Percentage of Children out of School",
        color = "Income Group") -> Children_Out_of_Education_lowest_Countries

ggsave("Children Out of Education_lowest_Countries.png",
       plot = Children_Out_of_Education_lowest_Countries,
       width = 7.02,
       height = 7.85,
       path = "./Plots")

## Relationhip Between Children out of School and GDP per Capita ----------
ggplot(Child_out_school_df %>% filter(income != "Not classified"), aes(y = GDP_p_Cap_PPP, x = Child_out_School, col = region)) +
  geom_point(alpha = 0.5) +
  scale_x_continuous(breaks = seq(0, 70, by = 10),
                     labels = paste0(format(seq(0, 70, by = 10), decimal.mark = "."),"%")) +
  scale_y_continuous(breaks = seq(0, 17e4, by = 2e4),
                     labels = paste0(format(seq(0, 16e4, by = 2e4), decimal.mark = "."), "$")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(y = "GDP per Capita PP",
       x = "Children out of School",
       title = "Relationship between Children out of School and GDP per Capita",
       fill = "Region") -> Child_out_educ_GDPcap

Child_out_educ_GDPcap_main <- Child_out_educ_GDPcap +
  geom_rect(xmin = 0, xmax = 90, ymin = 0, ymax = 10000, 
            fill = NA, colour = "black", linewidth = 0.5) +
  theme(legend.position = 'none',
        axis.title.x = element_blank(),
        axis.title.y = element_blank())

(Child_out_educ_GDPcap_out <- Child_out_educ_GDPcap +
    scale_y_continuous(limits = c(0, 1e4),
                       breaks = seq(0, 1e4, by = 2e3),
                       labels = paste0(format(seq(0, 1e4, by = 2e3), 
                                              decimal.mark = "."), "$")) +
    theme(plot.title = element_blank()))



Child_Outof_Education <- plot_grid(Child_out_educ_GDPcap_main,
                                   Child_out_educ_GDPcap_out ,
                                   nrow = 2, rel_widths = c(2.3, 1.3))

ggsave("Children Out of Education Scatter.png",
       plot = Child_Outof_Education,
       width = 9.02,
       height = 8.85,
       path = "./Plots")


# Trade --------------
## Largest Exporters ------------------
(ggplot(WDI_Data %>% dplyr::filter(year == 2020), aes(area = Export, fill = region, label = country)) +
  geom_treemap(alpha = 0.7) +
  geom_treemap_text(fontface = "italic", colour = "black", place = "centre",
                    grow = F) + 
  labs(title = "Largest Exporters of Goods and Services in 2020", fill = "Region") +
  theme(legend.position = 'bottom', 
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_brewer(palette = "Set3") -> top_Exporters_plt)

ggsave("Largest Exporters.png",
       plot = top_Exporters_plt,
       width = 9.02,
       height = 4.85,
       path = "./Plots")


## Trade Openess ------
unique(WDI_Data$region)

WDI_Data %>% dplyr::filter(year == 2020 & !is.na(income)) %>%
  mutate(myLabel = case_when(Trade > 150 ~ iso3c,
                             TRUE ~ ""))%>%
  ggplot(aes(x = Trade/100, y = ..count../sum(..count..), col = region))  +
  facet_wrap(.~region) +
  stat_bin(bins = 30, geom = 'step', size = 1 ,
           data = WDI_Data %>% dplyr::filter(region == "Europe & Central Asia", 
                                year == 2020 & !is.na(income))) +
  stat_bin(bins = 30, geom = 'step', size = 1 ,
           data = WDI_Data %>% dplyr::filter(region == "Middle East & North Africa",
                                year == 2020 & !is.na(income))) +
  stat_bin(bins = 30, geom = 'step', size = 1 ,
           data = WDI_Data %>% dplyr::filter(region == "South Asia" ,
                                year == 2020 & !is.na(income))) +
  stat_bin(bins = 30, geom = 'step', size = 1 ,
           data = WDI_Data %>% dplyr::filter(region == "Latin America & Caribbean",
                                year == 2020 & !is.na(income))) +
  stat_bin(bins = 30, geom = 'step', size = 1 ,
           data = WDI_Data %>% dplyr::filter(region == "Sub-Saharan Africa",
                                year == 2020 & !is.na(income))) +
  stat_bin(bins = 30, geom = 'step', size = 1 ,
           data = WDI_Data %>% dplyr::filter(region == "East Asia & Pacific",
                                year == 2020 & !is.na(income))) +
  stat_bin(bins = 30, geom = 'step', size = 1 ,
           data = WDI_Data %>% dplyr::filter(region == "East Asia & Pacific",
                                year == 2020 & !is.na(income))) +
  stat_bin(bins = 30, geom = 'step', size = 1 ,
           data = WDI_Data %>% dplyr::filter(region ==  "North America",
                                year == 2020 & !is.na(income))) +
  labs(y = 'Percentage share of the whole sample',
       x = "Trade Openness (Import + Export)/GDP",
       title = "Trade Openess Destribution Accross Regions (2020")+
  
  scale_y_continuous(n.breaks = 7, labels = scales::percent) +
  scale_x_continuous(n.breaks = 7, labels = scales::percent) +
  theme_classic() +
  theme(legend.position = c(.75,0.15),
        plot.title = element_text(hjust = 0.5)) +
  guides(col =guide_legend(ncol=2)) -> Trade_Openess_dist
         

ggsave("Trade Openess dist.png",
       plot = Trade_Openess_dist,
       width = 9.02,
       height = 4.85,
       path = "./Plots")



# Top 10 Richest Countries in the World -----------------
Ranked_GDP_p_Capita <- WDI_Data %>% 
  dplyr::filter(between(year, 2005, 2020)) %>%
  group_by(year) %>%
  top_n(10, GDP_p_Cap_PPP) %>%
  mutate(GDP_p_Cap_Rank = rank(-GDP_p_Cap_PPP),
         year = as.factor(year)) %>%
  ungroup()

ggplot(data = Ranked_GDP_p_Capita, aes(x = year, y = GDP_p_Cap_Rank, group = country)) +
  geom_line(aes(color = region,), size = 1) +
  geom_point(aes(color = region), size = 1.3, shape = 21, fill = 'white') +
  geom_text(data = Ranked_GDP_p_Capita %>% dplyr::filter(year == 2005, GDP_p_Cap_Rank <= 10),
            aes(label = country, x = year) , hjust = "outward", color = "#888888", size = 4) + 
  geom_text(data = Ranked_GDP_p_Capita %>% dplyr::filter(year == 2020, GDP_p_Cap_Rank <= 10),
            aes(label = country, x = year) , hjust = "outward", color = "#888888", size = 4) +
  scale_x_discrete(expand = c(.2, .2)) +
  scale_y_reverse(breaks = seq(1, 15)) +
  labs(title = "Top 10 Richest countries in the in the world (Nominal GDP per Capita PPP)", x = "Year", y = "Rank") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        legend.position = 'bottom', 
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) -> RichestCountries_plt

ggsave("Richest Countries.png",
       plot = RichestCountries_plt,
       width = 10.02,
       height = 4.85,
       path = "./Plots")








# Higher Education Attainment ----------------------------

WDI_Data %>% dplyr::filter(!is.na(Ed_Attain_25)) %>%
  group_by(iso2c, country, iso3c,
           region,income, lending) %>%
  arrange(year) %>%
  summarise(Ed_Attain_25 = last(Ed_Attain_25),
            year = last(year),
            GDP_p_Cap_PPP = last(GDP_p_Cap_PPP)) %>%
  ungroup() -> Ed_Attain_25_df


world2 <- ne_countries(scale = "medium", returnclass = "sf")
colnames(world2)
world2 <- world2 %>%
  left_join(Ed_Attain_25_df %>% 
              dplyr::select(-country), by = c("iso_a2" = "iso2c"))
  

(world_map_2<- ggplot(data = world2)  +
    coord_sf(expand = FALSE) +
    geom_sf(aes(fill =  Ed_Attain_25)) +
    geom_rect(xmin = -18, xmax = 50, ymin = -35, ymax = 25, 
              fill = NA, colour = "black", linewidth = 1) +
    labs(title = "Higher Education Attainment at Age 25") +
    scale_fill_viridis_c(option = "H", trans = "sqrt",
                         direction = -1,
                         begin = 0.40,
                         breaks = seq(0, 30, by=5),
                         labels = paste0(seq(0, 30, by=5), "%"),
                         guide = guide_colorbar(barwidth = 30, barheight = 1), 
                         name = "Higher Education Attainment") +
    theme(legend.position = 'none',
          panel.background = element_rect(fill = "azure"),
          panel.border = element_rect(fill = NA),
          plot.title = element_text(hjust = 0.5)))

(SSA_Map_2 <- ggplot(data = world2) +
    geom_sf(aes(fill = Ed_Attain_25)) +
    annotate(geom = "text", x = -5, y = -12, 
             label = "Sub-Saharan \n Africa", 
             fontface = "italic", 
             color = "grey22", size = 3) +
    coord_sf(xlim = c(-18, 50), ylim = c(-35, 25), expand = FALSE) +
    scale_fill_viridis_c(option = "H", trans = "sqrt",
                         direction = -1,
                         begin = 0.40,
                         breaks = seq(0, 30, by=10),
                         labels = paste(seq(0, 30, by=10), "%"), 
                         name = "Higher Education Attainment Rate ") +
    theme(legend.position = "right", axis.title.x = element_blank(), 
          axis.title.y = element_blank(), panel.background = element_rect(fill = "azure"), 
          panel.border = element_rect(fill = NA)))


(plot_grid(world_map_2, SSA_Map_2,
           nrow = 2, rel_widths = c(1.1, 1)) -> ed_attainment )

ggsave("Ed_Attainmant_map.png",
       plot = ed_attainment,
       width = 9.02,
       height = 6.85,
       path = "./Plots")

## Relationhip Between Education attainment and GDP per Capita -------------
ggplot(Ed_Attain_25_df, aes(y = GDP_p_Cap_PPP, x = Ed_Attain_25, col = region)) +
  geom_point(alpha = 0.7) +
  scale_x_continuous(breaks = seq(0, 70, by = 10),
                     labels = paste0(format(seq(0, 70, by = 10), decimal.mark = "."),"%")) +
  scale_y_continuous(breaks = seq(0, 17e4, by = 2e4),
                     labels = paste0(format(seq(0, 16e4, by = 2e4), decimal.mark = "."), "$")) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(y = "GDP per Capita PPP",
       x = "Higher Education Attainment",
       title = "Relationship between Education Attainment and GDP per Capita",
       fill = "Region") -> HigherEducationGdPCap_plt

ggsave("Higher Eduction Vs GDP per Capita.png",
       plot = HigherEducationGdPCap_plt,
       width = 9.02,
       height = 4.85,
       path = "./Plots")

## Labor with Advanced qualification ----------

(WDI_Data %>%  dplyr::filter(between(year, 2016, 2020)) %>%
  mutate(myLabel = case_when(Advanced_ed_labor < 50 ~ iso3c,
                             Advanced_ed_labor > 90 ~ iso3c,
                             country == "Poland" ~ iso3c, # to label Poland aswell
                             GDP_p_Cap_PPP > 100000 ~ iso3c,
                             TRUE ~ "")) %>%
  ggplot(aes(y = GDP_p_Cap_PPP, x = Advanced_ed_labor/100,
             color = region, shape = factor(year))) +
  geom_point(size = 1) +
  geom_text_repel(aes(label = myLabel), size = 3) + 
  labs(title = "Labor with Advanced qualification and GDP per Capita",
       x = "Labor with Advanced qualification ",
       y = "Nominal GDP per Capita (PPP)",
       shape = "Year",
       color = "Region")  + 
  scale_y_continuous(n.breaks =10, labels= unit_format(unit = "$")) +
  scale_x_continuous(n.breaks =10, labels = scales::percent) +
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5),
        legend.position = "bottom") +
  guides(color = guide_legend(ncol=2),
         shape = guide_legend(ncol=2)) -> Advanced_ed_labor_plot)

ggsave("Labor with Advanced qualification.png",
       plot = Advanced_ed_labor_plot,
       width = 7.02,
       height = 7.85,
       path = "./Plots") 

#Life Expectancy --------------

## Boxplot --------------------------
ggplot(WDI_Data, aes(x = Ed_Attain_25, y = Life_exp, col = region)) +
  geom_point()


WDI_Data %>% dplyr::filter(income != "Not classified" & between(year,2016, 2020)) %>%
  ggplot(aes(y = Life_exp, fill = income)) +
  geom_boxplot() +
  facet_wrap(.~year, ncol = 2) +
  theme(legend.position = c(.75,0.2),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_discrete(name = "Income Group") +
  labs(title = "Life Expectancy Accross Income Groups",
       y = "Life Expectancy")  -> Life_Expectancy_Boxplot


ggsave("Life Expectancy Boxplot.png",
       plot = Life_Expectancy_Boxplot,
       width = 9.02,
       height = 4.85,
       path = "./Plots")


## Life Expectancy and GDP Per Capita ----- 
(WDI_Data %>% dplyr::filter(income != "Not classified" & between(year,2016, 2020)) %>%
  ggplot(aes(y = Life_exp, x = GDP_p_Cap_PPP, 
             shape = income, col = region)) +
  geom_point(size = 1.5) +
  labs(title = "Life Expectancy and GDP Per Capita (PPP)",
       y = "Life Expectancy",
       x = "Nominal GDP per Capita (PPP)",
       shape = "Income",
       color = "Region")  + 
  scale_x_continuous(n.breaks =10, labels= unit_format(unit = "$")) +
  theme_classic()+
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_rect(xmin = 0, xmax = 7500, ymin = 70, ymax = 77, 
             fill = NA, colour = "black", linewidth = 1) +
  annotate(geom = "text", x = 0, y = 80, 
            label = "low income \n high life", 
            fontface = "italic", 
            color = "grey22", size = 2)  -> Expectancy_GDP) 

ggsave("Life Expectancy and GDP.png",
       plot = Expectancy_GDP,
       width = 7.02,
       height = 7.85,
       path = "./Plots")


(WDI_Data %>% dplyr::filter(income != "Not classified" & between(year,2016, 2020)) %>%
    ggplot(aes(y = Life_exp, x = GDP_p_Cap_PPP, 
               shape = factor(year), color = region)) +
    geom_point(size = 1.5) +
    scale_y_continuous(limits = c(70, 77)) +
    scale_x_continuous(limits = c(2000, 7500), n.breaks =10, labels= unit_format(unit = "$")) +
    labs(title = "Low Income but Relatively High Expectancy",
         y = "Life Expectancy",
         x = "Nominal GDP per Capita (PPP)",
         shape = "Year",
         color = "Region")  + 
    theme_classic()  + 
    geom_label_repel(aes(label = iso3c),
                   box.padding   = 0.35, 
                   point.padding = 0.5,
                   segment.color = 'grey50') +
    theme(plot.title = element_text(hjust = 0.5)) -> Expectancy_GDP_case)
  
  

ggsave("Life Expectancy and GDP_case.png",
       plot = Expectancy_GDP_case,
       width = 7.02,
       height = 7.85,
       path = "./Plots")

# GINI Index

# Top 10 Richest Countries in the World by GDP per Capita Purchasing Power Parity
Ranked_GINI <- WDI_Data %>% 
  dplyr::filter(between(year, 2010, 2020)) %>%
  group_by(year) %>%
  top_n(10, GINI_Index) %>%
  mutate(GINI_Index_rank = rank(-GINI_Index),
         year = as.factor(year),
         Latin_America = ifelse(region == "Latin America & Caribbean", TRUE, FALSE)) %>%
  ungroup()

ggplot(data = Ranked_GINI, aes(x = year, y = GINI_Index_rank, group = country)) +
  geom_line(aes(color = region), size = 1.0) +
  geom_point(aes(color = region), size = 1.3, shape = 21, fill = 'white') +
  geom_text(data = Ranked_GINI %>% dplyr::filter(year == 2010, GINI_Index_rank <= 10),
            aes(label = country, x = year) , hjust = "outward", color = "#888888", size = 4) + 
  geom_text(data = Ranked_GINI %>% dplyr::filter(year == 2020, GINI_Index_rank <= 10),
            aes(label = country, x = year) , hjust = "outward", color = "#888888", size = 4) +
  scale_x_discrete(expand = c(.2, .2)) +
  scale_y_reverse(breaks = seq(1, 15)) +
  scale_fill_discrete(name = "Region") +
  labs(title = "The Most Unequal Countries by GINI Index", x = "Year", y = "Rank") +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks = element_blank(),
        legend.position = 'bottom', 
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90, 
                                   vjust = 0.5, 
                                   hjust=1)) -> GINI_plt


ggsave("Most Unequal Countries.png",
       plot = GINI_plt,
       width = 10.02,
       height = 4.85,
       path = "./Plots")

WDI_Data %>% dplyr::filter(income != "Not classified" & between(year,2016, 2020)) %>%
  ggplot(aes(y = GINI_Index, fill = region)) +
  geom_boxplot() +
  facet_wrap(.~year, ncol = 2) +
  theme(legend.position = c(.75,0.2),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        plot.title = element_text(hjust = 0.5)) +
  scale_fill_discrete(name = "Region") +
  labs(title = "GINI Index Accross Regions",
       y = "GINI Index") +
  guides(fill=guide_legend(ncol=2))-> GiniIndex_BoxPlot



ggsave("GINI Index Accross Regions_boxPlot.png",
       plot = GiniIndex_BoxPlot,
       width = 9.02,
       height = 4.85,
       path = "./Plots")

