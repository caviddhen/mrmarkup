MagplottingScripts <- function(BAUgdx, POLgdx){

setwd("/p/projects/magpie/users/davidch/ICPdata_cluster/plots")
library(mrmarkup)
library(brms)
BAUgdx <-  "/p/projects/magpie/users/davidch/magpie_versions/develop/magpie/output/markupDefPov_2023-01-18_15.56.21/fulldata.gdx"
POLgdx <-  "/p/projects/magpie/users/davidch/magpie_versions/develop/magpie/output/markupPolPov_2023-01-18_15.58.16/fulldata.gdx"

setConfig(forcecache=T)
BAUm <- FoodExpMagpieMarkup(gdx = BAUgdx, level = "iso", type = "consumer", prodAggr = FALSE, afterShock = TRUE,
                                povmodel = FALSE, validYi = FALSE)

BAUm <- BAUm  %>% mutate(scen = "BAU")
                               
POL <- FoodExpMagpieMarkup(gdx = POLgdx, level = "iso", type = "consumer", prodAggr = FALSE, afterShock = TRUE,
                                povmodel = FALSE, validYi = FALSE)

POLm <- POL  %>% mutate(scen = "POL")

markups <- rbind(BAUm, POLm)  %>% 
          select(iso3c, year, scen, k, prodPrice, caterPrice, noCaterPrice) %>% 
         pivot_longer(c(prodPrice, caterPrice, noCaterPrice), names_to = "Price Type")  %>% 
         rename("Price" = "value")

def2020 <- filter(markups,year == 2020) %>%
             rename( "pr20" = "Price"  ) %>%
         select(iso3c, scen, k, pr20, `Price Type`)

markups <- inner_join(markups, def2020) %>%
         mutate(PriceIndex = Price/pr20) %>%
       select(!pr20)


markups$`Price Type` <- factor(markups$`Price Type`,
                               levels = c("prodPrice", "caterPrice", "noCaterPrice"))


# reg <- "EUR"
#
### global price plot
gdppc <- calcOutput("GDPpc", aggregate = FALSE)[,,"gdppc_SSP2"]  %>% 
        as.data.frame(rev = 2)  %>% 
        rename("gdppc" = ".value")
iG <- gdppc %>% filter(year == 2020) %>%
  mutate(incomeG =  case_when(
    gdppc <= 1006 ~ "LIC",
    gdppc > 1006 & gdppc <= 3956 ~ "LMIC",
    gdppc > 3956 & gdppc <= 12235 ~ "UMIC",
    gdppc > 12235 ~ "HIC")) %>%
  select(iso3c, incomeG)
iG$incomeG <- factor(iG$incomeG, levels = c("HIC", "UMIC","LMIC", "LIC"))

markups$`Price Type` <- factor(markups$`Price Type`,
                                  levels = c("caterPrice", "noCaterPrice", "prodPrice"))

kBH <- read.csv(system.file("extdata",mapping="mapMAgPIELEM.csv",
                              package = "mrmarkup"))  %>%
  rename("BHName" = prod)

                                  
kBH$BHName <- factor(kBH$BHName,
                               levels = c("Bread and cereals", "Meat", "Milk products", "Eggs",
                                          "Vegetables", "Fruit", "Processed"))
kBH <- mutate(kBH,
              t = case_when(
                BHName %in% c("Bread and cereals",
                               "Vegetables", "Fruit",
                                "Processed") ~ "Plant-Based",
                BHName %in% c("Meat", "Milk products", "Eggs") ~
                  "Livestock Products"
              ))

consb <- Kcal(gdx = BAUgdx, level = "iso",
           calibrated = TRUE, after_shock = TRUE, 
           products = "kfo", product_aggr = FALSE,
           per_capita = FALSE) * 365  # This is in MILLION KCAL! 
consb <- add_dimension(consb, dim = 3.2, add = "scen", nm = "BAU")

consp <-  Kcal(gdx = POLgdx, level = "iso",
           calibrated = TRUE, after_shock = TRUE, 
           products = "kfo", product_aggr = FALSE,
           per_capita = FALSE) * 365  # This is in MILLION KCAL! 
consp <- add_dimension(consp, dim = 3.2, add = "scen", nm = "POL")

cons <- mbind(consb, consp)

cons <- cons %>% #from FoodExpMagpieMarkup
  as.data.frame(rev=2)  %>%
  rename("foodD" = ".value", "iso3c" = "iso",
         "k" = "kfo", "year" = "t")


markupsGlo <- inner_join(markups, iG) %>%
   inner_join(cons) %>%
   inner_join(kBH) %>%
   group_by(year, incomeG, BHName, `Price Type`, scen) %>%
   summarise(Price = weighted.mean(Price, w = foodD))


markupsGloProd <- inner_join(markups, iG) %>%
  inner_join(cons) %>%
  inner_join(kBH) %>%
  group_by(year, `Price Type`, BHName, scen) %>%
  summarise(Price = weighted.mean(Price, w = foodD))

markupsGloG <- inner_join(markups, iG) %>%
  inner_join(cons) %>%
  inner_join(kBH) %>%
  group_by(year, `Price Type`, scen) %>%
  summarise(Price = weighted.mean(Price, w = foodD)) %>%
  mutate(t = "Total")

markupsGloIG <- inner_join(markups, iG) %>%
  inner_join(cons) %>%
  inner_join(kBH) %>%
  group_by(year, incomeG, `Price Type`, scen) %>%
  summarise(Price = weighted.mean(Price, w = foodD)) %>%
  mutate(t = "Total")

markupsGlo3 <- inner_join(markups, iG) %>%
  inner_join(cons) %>%
  inner_join(kBH) %>%
  group_by(year, `Price Type`, t, scen) %>%
  summarise(Price = weighted.mean(Price, w = foodD))

markupsGlo3 <- rbind(markupsGlo3, markupsGloG) %>%
             mutate(t = factor(t, levels = c("Plant-Based","Livestock Products",
                                              "Total")))

## BAU plot for all 3 aggregations

ggplot(filter(markupsGlo,
              scen %in% c("BAU"), year %in% seq(2020, 2050, 5)),
       aes(x = year, y = Price, color = `Price Type`))+
  geom_line(lwd = 1.4)+
  facet_wrap(incomeG~ BHName, scales = "free", nrow = 4) +
  ggtitle(paste(" BAU")) +
  ylab("Price $USD05/kg")+
  scale_color_manual( values = c("#1E5B3E", "#348C62",  "#54D598")) +
  theme_bw(base_size = 10)

ggplot(filter(markupsGloProd,
              scen %in% c("BAU"), year %in% seq(2020, 2050, 5)),
       aes(x = year, y = Price, color = `Price Type`))+
  geom_line(lwd = 1.4)+
 facet_wrap(~ BHName, scales = "fixed", nrow = 2) +
  ggtitle(paste(" BAU")) +
  ylab("Price $USD/kg")+
  scale_color_manual(labels = c("Consumer Price FAFH", "Consumer Price FAH", "Prod Price" ),
                    values = c("#1E5B3E", "#348C62",  "#54D598")) +
  theme_bw(base_size = 14)

ggplot(filter(markupsGloIG,
              scen %in% c("BAU"), year %in% seq(2020, 2050, 5)),
       aes(x = year, y = Price, color = `Price Type`))+
  geom_line(lwd = 1.4)+
  facet_wrap(~incomeG, scales = "fixed", nrow = 1) +
  ggtitle(paste("BAU")) +
  ylab("Price $USD05/kg")+
  scale_color_manual(labels = c("Consumer Price FAFH", "Consumer Price FAH", "Prod Price" ),
                     values = c("#1E5B3E", "#348C62",  "#54D598")) +
  theme_bw(base_size = 16)


ggplot(filter(markupsGlo3,
              scen %in% c("BAU"), year %in% seq(2020, 2050, 5)),
       aes(x = year, y = Price, color = `Price Type`))+
  geom_line(lwd = 1.4)+
  facet_wrap(~ t, scales = "fixed", nrow = 1) +
  ggtitle(paste(" BAU")) +
  ylab("Price $USD/kg")+
  scale_color_manual(labels = c("Consumer Price FAFH", "Consumer Price FAH", "Prod Price" ),
                     values = c("#1E5B3E", "#348C62",  "#54D598")) +
  theme_bw(base_size = 14)


ggplot(filter(markupsGloG,
              scen %in% c("BAU"), year %in% seq(2020, 2050, 5)),
       aes(x = year, y = Price, color = `Price Type`))+
  geom_line(lwd = 1.4)+
  #facet_wrap(~ BHName, scales = "free", nrow = 2) +
  ggtitle(paste(" BAU")) +
  ylab("Price $USD/kg")+
  scale_color_manual(labels = c("Consumer Price FAFH", "Consumer Price FAH", "Prod Price" ),
                     values = c("#1E5B3E", "#348C62",  "#54D598")) +
  theme_bw(base_size = 18)



## POL plot for all 3 aggregations

ggplot(filter(markupsGlo,
              scen %in% c("POL"), year %in% seq(2020, 2050, 5)),
       aes(x = year, y = Price, color = `Price Type`))+
  geom_line(lwd = 1.4)+
  facet_wrap(incomeG~ BHName, scales = "fixed", nrow = 4) +
  ggtitle(paste("POL")) +
  ylab("Price $USD/kg")+
  scale_color_manual(labels = c("Consumer Price FAFH", "Consumer Price FAH", "Prod Price" ),
                     values = c("#1E5B3E", "#348C62",  "#54D598")) +
  theme_bw(base_size = 10)


ggplot(filter(markupsGloIG,
              scen %in% c("POL"), year %in% seq(2020, 2050, 5)),
       aes(x = year, y = Price, color = `Price Type`))+
  geom_line(lwd = 1.4)+
  facet_wrap(~incomeG, scales = "fixed", nrow = 4) +
  ggtitle(paste("POL")) +
  ylab("Price $USD/kg")+
  scale_color_manual(labels = c("Consumer Price FAFH", "Consumer Price FAH", "Prod Price" ),
                     values = c("#1E5B3E", "#348C62",  "#54D598")) +
  theme_bw(base_size = 10)

ggplot(filter(markupsGloProd,
              scen %in% c("POL"), year %in% seq(2020, 2050, 5)),
       aes(x = year, y = Price, color = `Price Type`))+
  geom_line(lwd = 1.4)+
  facet_wrap(~ BHName, scales = "fixed", nrow = 2) +
  ggtitle(paste("POL")) +
  ylab("Price $USD/kg")+
  scale_color_manual(labels = c("Consumer Price FAFH", "Consumer Price FAH", "Prod Price" ),
                     values = c("#1E5B3E", "#348C62",  "#54D598")) +
  theme_bw(base_size = 14)


ggplot(filter(markupsGloG,
              scen %in% c("POL"), year %in% seq(2020, 2050, 5)),
       aes(x = year, y = Price, color = `Price Type`))+
  geom_line(lwd = 1.4)+
  #facet_wrap(~ BHName, scales = "free", nrow = 2) +
  ggtitle(paste("POL")) +
  ylab("Price $USD/kg")+
  scale_color_manual(labels = c("Consumer Price FAFH", "Consumer Price FAH", "Prod Price" ),
                     values = c("#1E5B3E", "#348C62",  "#54D598")) +
  theme_bw(base_size = 18)

### facet Scen and BAU GLO prices
out <- ggplot(filter(markupsGloG, year %in% seq(2020, 2050, 5)),
       aes(x = year, y = Price, color = `Price Type`))+
  geom_line(lwd = 1.4)+
  facet_wrap(~ scen, scales = "fixed", nrow = 1) +
  ggtitle(paste("Global Average Food Prices \nBaseline and Climate Mitigation Scenarios")) +
  ylab("Price $USD/kg")+
  scale_color_manual(labels = c("Consumer Price FAFH", "Consumer Price FAH", "Prod Price" ),
                     values = c("#1E5B3E", "#348C62",  "#54D598")) +
  theme_bw(base_size = 24)

ggsave(out, file = "./Fig3GloPrices.png", width = 20, height = 10)
ggsave(out, file = "./Fig3GloPrices.pdf", width = 20, height = 10)


### make relative to BAU

markupsRatioGlo <- markupsGlo %>%
         pivot_wider(names_from = scen, values_from = Price) %>%
         mutate(ratio = POL/BAU)

markupsRatioGloProd <- markupsGloProd %>%
  pivot_wider(names_from = scen, values_from = Price) %>%
  mutate(ratio = POL/BAU)

markupsRatioGloG <- markupsGloG %>%
  pivot_wider(names_from = scen, values_from = Price) %>%
  mutate(ratio = POL/BAU)

markupsRatioGloIG <- markupsGloIG %>%
  pivot_wider(names_from = scen, values_from = Price) %>%
  mutate(ratio = POL/BAU)

markupsRatioGlo3 <- markupsGlo3 %>%
  pivot_wider(names_from = scen, values_from = Price) %>%
  mutate(ratio = POL/BAU)


### plots comparing BAU and POL
ggplot(filter(markupsRatioGlo, year %in% seq(2020,2050,5)),
       aes(x = year, y = ratio, color = `Price Type`))+
  geom_line(lwd = 1.4)+
  facet_wrap(incomeG ~ BHName, scales = "free", nrow = 4) +
  ggtitle(paste("POL:BAU Price Ratio")) +
  scale_color_manual(values = c( "#54D598", "#1E5B3E", "#348C62"))+
  theme_bw(base_size = 11)

ggplot(filter(markupsRatioGloProd, year %in% seq(2020,2050,5)),
       aes(x = year, y = ratio, color = `Price Type`))+
  geom_line(lwd = 1.4)+
  facet_wrap( ~ BHName, scales = "free", nrow =2) +
  ggtitle(paste("POL:BAU Price Ratio")) +
  scale_color_manual(values = c( "#54D598", "#1E5B3E", "#348C62"),
                     guide = guide_legend(reverse = TRUE) )+
  theme_bw(base_size = 11)

b <- ggplot(filter(markupsRatioGloIG, year %in% seq(2020,2050,5)),
       aes(x = year, y = ratio, color = `Price Type`))+
  geom_line(lwd = 1.4)+
  facet_wrap( ~ incomeG, scales = "free", nrow = 1) +
  ggtitle(paste("b) POL to BAU Price Ratio by Income Group")) +
  scale_color_manual(values = c(  "#1E5B3E","#348C62", "#54D598"))+
  theme_bw(base_size = 18)

a <- ggplot(filter(markupsRatioGlo3, year %in% seq(2020,2050,5)),
       aes(x = year, y = ratio, color = `Price Type`))+
  geom_line(lwd = 1.4)+
  facet_wrap( ~ t, scales = "free", nrow = 1) +
  ggtitle(paste("a) POL to BAU Price Ratio by Product ")) +
  scale_color_manual(values = c(  "#1E5B3E","#348C62", "#54D598"))+
  theme_bw(base_size = 18)

library(gridExtra)
out1 <- grid.arrange(a,b)
ggsave(out1, file = "./Fig4POLBAURatio.pdf", height = 12, width = 10)
ggsave(out1, file = "./Fig4POLBAURatio.png", height = 12, width = 10)

ggplot(filter(markupsRatioGloG, year %in% seq(2020,2050,5)),
       aes(x = year, y = ratio, color = `Price Type`))+
  geom_line(lwd = 1.4)+
  #facet_wrap( ~ BHName, scales = "free", nrow = 4) +
  ggtitle(paste("POL:BAU Price Ratio")) +
  scale_color_manual(values = c(  "#1E5B3E","#348C62", "#54D598"))+
  theme_bw(base_size = 11)




#use FAO consumption

### plots comparing BAU and POL
ggplot(filter(markups, iso3c == iso,
              k %in% prods,
              scen %in% c("BAU", "POL"),
              `Price Type` == "noCaterPrice"),
       aes(x = year, y = Price, color = scen))+
  geom_line(lwd = 1.4)+
  facet_wrap(~k, scales = "free") +
  ggtitle(paste(iso, " Consumer FAH Prices")) +
  scale_color_manual(labels = c("BAU", "POL"),
                     values = c( "#5AD2D8", "#995AD8"),
                     guide = guide_legend(reverse = TRUE) ) +
  theme_bw(base_size = 18)

### plots comparing BAU and POL
ggplot(filter(markups, iso3c == iso,
              k %in% prods,
              scen %in% c("BAU", "POL"),
              `Price Type` == "CaterPrice"),
       aes(x = year, y = Price, color = scen))+
  geom_line(lwd = 1.4)+
  facet_wrap(~k, scales = "free") +
  ggtitle(paste(iso, " Consumer FAFH Prices")) +
  scale_color_manual(labels = c("BAU", "POL"),
                     values = c( "#5AD2D8", "#995AD8"),
                     guide = guide_legend(reverse = TRUE) ) +
  theme_bw(base_size = 18)
}