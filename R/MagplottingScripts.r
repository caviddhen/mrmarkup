
def2020 <- filter(markups,year == 2020) %>%
             dplyr::rename( "pr20" = "Price"  ) %>%
         select(iso3c, k, pr20, `Price Type`)

markups1 <- inner_join(markups, def2020) %>%
         mutate(PriceIndex = Price/pr20) %>%
       select(!pr20)


markups1$`Price Type` <- factor(markups1$`Price Type`,
                               levels = c("prodPrice", "CaterPrice", "noCaterPrice"))


# reg <- "EUR"
#

### global price plot
iG <- gdppc %>% filter(year == 2020) %>%
  mutate(incomeG =  case_when(
    gdppc <= 1006 ~ "LIC",
    gdppc > 1006 & gdppc <= 3956 ~ "LMIC",
    gdppc > 3956 & gdppc <= 12235 ~ "UMIC",
    gdppc > 12235 ~ "HIC")) %>%
  select(iso3c, incomeG)


iG$incomeG <- factor(iG$incomeG, levels = c("HIC", "UMIC","LMIC", "LIC"))
markups$`Price Type` <- factor(markups$`Price Type`,
                                  levels = c("CaterPrice", "noCaterPrice", "prodPrice"))
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

cons <- cons[,,kfo] %>% #from FoodExpMagpieMarkup
  as.data.frame(rev=2)  %>%
  rename("foodD" = ".value", "iso3c" = "i",
         "k" = "kall", "year" = "t", "scen" = "new")


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
  ylab("Price $USD/ton")+
  scale_color_manual(labels = c("Consumer Price FAFH", "Consumer Price FAH", "Prod Price" ),
                     values = c("#1E5B3E", "#348C62",  "#54D598")) +
  theme_bw(base_size = 10)

ggplot(filter(markupsGloProd,
              scen %in% c("BAU"), year %in% seq(2020, 2050, 5)),
       aes(x = year, y = Price, color = `Price Type`))+
  geom_line(lwd = 1.4)+
 facet_wrap(~ BHName, scales = "free", nrow = 2) +
  ggtitle(paste(" BAU")) +
  ylab("Price $USD/ton")+
  scale_color_manual(labels = c("Consumer Price FAFH", "Consumer Price FAH", "Prod Price" ),
                    values = c("#1E5B3E", "#348C62",  "#54D598")) +
  theme_bw(base_size = 14)

ggplot(filter(markupsGloIG,
              scen %in% c("BAU"), year %in% seq(2020, 2050, 5)),
       aes(x = year, y = Price, color = `Price Type`))+
  geom_line(lwd = 1.4)+
  facet_wrap(~incomeG, scales = "free", nrow = 1) +
  ggtitle(paste("BAU")) +
  ylab("Price $USD/ton")+
  scale_color_manual(labels = c("Consumer Price FAFH", "Consumer Price FAH", "Prod Price" ),
                     values = c("#1E5B3E", "#348C62",  "#54D598")) +
  theme_bw(base_size = 16)


ggplot(filter(markupsGlo3,
              scen %in% c("BAU"), year %in% seq(2020, 2050, 5)),
       aes(x = year, y = Price, color = `Price Type`))+
  geom_line(lwd = 1.4)+
  facet_wrap(~ t, scales = "free", nrow = 1) +
  ggtitle(paste(" BAU")) +
  ylab("Price $USD/ton")+
  scale_color_manual(labels = c("Consumer Price FAFH", "Consumer Price FAH", "Prod Price" ),
                     values = c("#1E5B3E", "#348C62",  "#54D598")) +
  theme_bw(base_size = 14)


ggplot(filter(markupsGloG,
              scen %in% c("BAU"), year %in% seq(2020, 2050, 5)),
       aes(x = year, y = Price, color = `Price Type`))+
  geom_line(lwd = 1.4)+
  #facet_wrap(~ BHName, scales = "free", nrow = 2) +
  ggtitle(paste(" BAU")) +
  ylab("Price $USD/ton")+
  scale_color_manual(labels = c("Consumer Price FAFH", "Consumer Price FAH", "Prod Price" ),
                     values = c("#1E5B3E", "#348C62",  "#54D598")) +
  theme_bw(base_size = 18)



## POL plot for all 3 aggregations

ggplot(filter(markupsGlo,
              scen %in% c("POL"), year %in% seq(2020, 2050, 5)),
       aes(x = year, y = Price, color = `Price Type`))+
  geom_line(lwd = 1.4)+
  facet_wrap(incomeG~ BHName, scales = "free", nrow = 4) +
  ggtitle(paste("POL")) +
  ylab("Price $USD/ton")+
  scale_color_manual(labels = c("Consumer Price FAFH", "Consumer Price FAH", "Prod Price" ),
                     values = c("#1E5B3E", "#348C62",  "#54D598")) +
  theme_bw(base_size = 10)

ggplot(filter(markupsGloProd,
              scen %in% c("POL"), year %in% seq(2020, 2050, 5)),
       aes(x = year, y = Price, color = `Price Type`))+
  geom_line(lwd = 1.4)+
  facet_wrap(~ BHName, scales = "free", nrow = 2) +
  ggtitle(paste("POL")) +
  ylab("Price $USD/ton")+
  scale_color_manual(labels = c("Consumer Price FAFH", "Consumer Price FAH", "Prod Price" ),
                     values = c("#1E5B3E", "#348C62",  "#54D598")) +
  theme_bw(base_size = 14)


ggplot(filter(markupsGloG,
              scen %in% c("POL"), year %in% seq(2020, 2050, 5)),
       aes(x = year, y = Price, color = `Price Type`))+
  geom_line(lwd = 1.4)+
  #facet_wrap(~ BHName, scales = "free", nrow = 2) +
  ggtitle(paste("POL")) +
  ylab("Price $USD/ton")+
  scale_color_manual(labels = c("Consumer Price FAFH", "Consumer Price FAH", "Prod Price" ),
                     values = c("#1E5B3E", "#348C62",  "#54D598")) +
  theme_bw(base_size = 18)


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
  scale_color_manual(labels = c("Prod Price", "Consumer Price FAH", "Consumer Price FAFH" ),
                     values = c( "#54D598", "#1E5B3E", "#348C62"))+
  theme_bw(base_size = 11)

ggplot(filter(markupsRatioGloProd, year %in% seq(2020,2050,5)),
       aes(x = year, y = ratio, color = `Price Type`))+
  geom_line(lwd = 1.4)+
  facet_wrap( ~ BHName, scales = "free", nrow =2) +
  ggtitle(paste("POL:BAU Price Ratio")) +
  scale_color_manual(labels = c("Prod Price", "Consumer Price FAH", "Consumer Price FAFH" ),
                     values = c( "#54D598", "#1E5B3E", "#348C62"),
                     guide = guide_legend(reverse = TRUE) )+
  theme_bw(base_size = 11)

ggplot(filter(markupsRatioGloIG, year %in% seq(2020,2050,5)),
       aes(x = year, y = ratio, color = `Price Type`))+
  geom_line(lwd = 1.4)+
  facet_wrap( ~ incomeG, scales = "free", nrow = 2) +
  ggtitle(paste("POL:BAU Price Ratio")) +
  scale_color_manual(labels = c( "Consumer Price FAFH", "Consumer Price FAH", "Prod Price"),
                     values = c(  "#1E5B3E","#348C62", "#54D598"))+
  theme_bw(base_size = 18)

ggplot(filter(markupsRatioGlo3, year %in% seq(2020,2050,5)),
       aes(x = year, y = ratio, color = `Price Type`))+
  geom_line(lwd = 1.4)+
  facet_wrap( ~ t, scales = "free", nrow = 2) +
  ggtitle(paste("POL:BAU Price Ratio")) +
  scale_color_manual(labels = c( "Consumer Price FAFH", "Consumer Price FAH", "Prod Price"),
                     values = c(  "#1E5B3E","#348C62", "#54D598"))+
  theme_bw(base_size = 18)



ggplot(filter(markupsRatioGloG, year %in% seq(2020,2050,5)),
       aes(x = year, y = ratio, color = `Price Type`))+
  geom_line(lwd = 1.4)+
  #facet_wrap( ~ BHName, scales = "free", nrow = 4) +
  ggtitle(paste("POL:BAU Price Ratio")) +
  scale_color_manual(labels = c( "Consumer Price FAFH", "Consumer Price FAH", "Prod Price"),
                     values = c(  "#1E5B3E","#348C62", "#54D598"))+
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
