#' @title FoodExpMagpieMarkup
#' @description calculate markups
#' @param magclass TRUE for magclass object equivalent to magpie4 FoodExpenditure output
#' @param gdx gdx file as input for mag base prices
#' @import dplyr tidyr
#' @importFrom magclass as.data.frame
#' @importFrom magpiesets findset
#' @export
#'
#' @return dataframe or magclass object of consumer food expenditures
#' @author David M Chen

FoodExpMagpieMarkup <- function(gdx, level = "reg", magclass = FALSE) {

kfo <- findset("kfo")

kBH <- read.csv(system.file("extdata",mapping="mapMAgPIELEM.csv",
                              package = "mrmarkup"))  %>%
  rename("BHName" = prod)

mapping <- readGDX(gdx, "i_to_iso")  %>% 
           rename("iso3c" = "iso")


attr <- calcOutput("Attributes", aggregate = F)[,,"wm"] #convert prices to wm for markup
wm <- attr   %>% magclass::as.data.frame(rev = 2)  %>%
  rename("k" = "products", "wm" = ".value")  %>%
  select(k, wm)

prpr <- prices(gdx, type = "consumer", level = "reg")
prpr <- collapseNames((prpr / attr[,,"wm"]))
prpr <- time_interpolate(prpr, interpolated_year = c(2010:2017), integrate_interpolated_years = TRUE)
prpr <- add_columns(prpr, addnm = "Vegetables", dim = 3.1, fill = NA)
prpr[,,"Vegetables"] <- prpr[,,"others"]

prpr <- toolAggregate(prpr, rel = mapping,
                      from = "i", to = "iso3c",
                      weight = NULL)

### replace price for oils and sugars (which already processed) with weighted average price of
#primary products that go into the secondary products
ksd <- findset("ksd")

cons <- demand(gdx)
cons <- gdxAggregate(gdx = gdx, cons, weight = 'Intake', to = "iso")[,,"food"]
cons <- collapseNames(cons)
kfo <- c(findset("kfo"), "Vegetables")
cons <- add_columns(collapseNames(cons), addnm = "Vegetables")
#load input vegetable data to get a the latest split

consKmag <- readFBSnew(level = "prim")
VegShr <- consKmag[,,"Vegetables"]/collapseNames((consKmag[,,"others"]+consKmag[,,"Vegetables"]))
VegShr <- toolCountryFill(VegShr, fill = 0.58) #mean value
othShr <- (1-VegShr)
cons[,,"Vegetables"] <- setYears(VegShr[,2019,], NULL) * cons[,,"others"]
cons[,,"others"] <- setYears(othShr[,2019,], NULL) * cons[,,"others"]


attr <- add_columns(attr, addnm = "Vegetables", dim = 3.2)
attr[,,"Vegetables"] <- attr[,,"others"]
cons <- cons * attr[,,"wm"] %>%
  collapseNames()

proc_shr <- calcOutput("Processing_shares", aggregate = F)
proc_shr <- time_interpolate(proc_shr[getRegions(prpr),,], interpolated_year = getYears(prpr),
                             integrate_interpolated_years = T)
proc_shr <- proc_shr[,getYears(cons),]

cvn_fct <- calcOutput("Processing_conversion_factors", aggregate = F)
cvn_fct <- time_interpolate(cvn_fct, interpolated_year = getYears(prpr),
                            integrate_interpolated_years = T)[,getYears(prpr),]
cvn_fct <- cvn_fct[,getYears(cons),]

proc <- cons[,,intersect(ksd, getNames(cons, dim = 1))][,,"alcohol", inv = T]
proc_oils <- collapseNames((proc[,,"oils"] /
                              dimSums(cvn_fct[,,c("milling", "extracting")][,,"oils"],dim=3.1) *
                              proc_shr[,,"oils"] ))
proc_oils[is.na(proc_oils)] <- 0
proc_oils <- time_interpolate(proc_oils, interpolated_year = getYears(prpr),
                               integrate_interpolated_years = T)[,getYears(prpr),]


proc_sugar <- collapseNames((proc[,,"sugar"] /
                               dimSums(cvn_fct[,,"refining"][,,list("ItemCodeItem" = "sugar")],dim=3.1) *
                               proc_shr[,,list("ItemCodeItem" = "sugar")] ))
proc_sugar[is.na(proc_sugar)] <- 0
proc_sugar <- time_interpolate(proc_sugar, interpolated_year = getYears(prpr),
                               integrate_interpolated_years = T)[,getYears(prpr),]

citems <- intersect(getNames(proc_sugar, dim =1), getNames(prpr, dim =1))

sugmap <- data.frame(sugar = rep("sugar", length(citems)), pr = citems)
prpr_sug <- toolAggregate(prpr[,,citems], rel = sugmap,
                   from = "pr", to = "sugar",
                   weight = proc_sugar[,,citems],
                   dim = 3.1)

citems <- intersect(getNames(proc_oils, dim =1), getNames(prpr, dim = 1))
oilmap <- data.frame(oils = rep("oils", length(citems)), pr = citems)
prpr_oils <- toolAggregate(prpr[,,citems], rel = oilmap,
                          from = "pr", to = "oils",
                          weight = proc_oils[,,citems],
                          dim = 3.1)

prpr[,,"sugar"] <- prpr_sug
prpr[,,"oils"] <- prpr_oils

prpr<- prpr[,,kfo]


prpr <- prpr %>%
  collapseNames() %>%
    as.data.frame(rev = 2)  %>% 
  rename("iso3c" = "i", "year" = t, "k" = kcr, "value" = ".value") %>%
  inner_join(kBH) %>%
  GDPuc::convertGDP(unit_in = "constant 2005 US$MER",
                   unit_out = "constant 2017 US$MER",
                   replace_NAs = "no_conversion")


##### get markup regression coefs #####
coefs <- regressMarkups()

#remove alcohol
prpr <- filter(prpr, k != "alcohol")

magCoefs <- kBH  %>%
  rename("prod" = "BHName")  %>% 
    inner_join(coefs) 

gdppc <- income(gdx, level = "iso")

gdppc <- gdppc %>%
  as.data.frame(rev = 2)  %>%
  rename("iso3c" = "iso", "year" = "t_all", "gdppc" = ".value") %>%
  select(iso3c, year, gdppc)


markupPr <- inner_join(prpr, gdppc)  %>%
  inner_join(magCoefs)  %>%
mutate(markupCater = a*(b^log(gdppc, base = 10)),
         CaterPrice = value + markupCater)  %>% 
select(!c(a, b, markupCater))  %>% 
pivot_wider(names_from = cater, values_from = CaterPrice) %>% 
  rename("prodPrice" = value, "caterPrice" = cater, "noCaterPrice" = noCater)

markups <-  markupPr %>%
  pivot_longer(cols = c(prodPrice, noCaterPrice, caterPrice),
               names_to = "Price Type", values_to = "Price") %>%
  select(iso3c, year, k,  `Price Type`, Price)


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

cons <- cons[,,kfo] %>%
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


cons <- cons[,,kfo] %>% #from FoodExpMagpieMarkup
  as.data.frame(rev=2)  %>%
  rename("foodD" = ".value", "iso3c" = "i",
         "k" = "kall", "year" = "t")



magExp <- inner_join(cons,
                     markups)  %>%
  inner_join(gdppc) %>%
  mutate(AFHshr =  (6.367e-06*gdppc + 3.492e-02 )) %>%      ### from kcal_fafh
  pivot_wider(names_from =  c(`Price Type`),
              values_from = Price) %>%
  mutate(fahExp = foodD * (1-AFHshr) * (noCaterPrice),
         fafhExp = foodD * AFHshr * (caterPrice),
         farmAHexp = foodD *(1-AFHshr) * prodPrice,
         farmAFHexp = foodD *AFHshr * prodPrice,
         farmAHshr = farmAHexp/fahExp,
         farmAFHshr = farmAFHexp/fafhExp)%>%
  select(iso3c, year, k, foodD, gdppc, prodPrice, caterPrice, noCaterPrice, fahExp, fafhExp, farmAHexp, farmAFHexp, farmAHshr, farmAFHshr)


magExpMeanK <- magExp %>%
  group_by(iso3c, year) %>%
  summarise(fahExp = sum(fahExp),
            fafhExp = sum(fafhExp),
            farmAHexp = sum(farmAHexp),
            farmAFHexp = sum(farmAFHexp)) %>%
  mutate(totExp = fahExp + fafhExp,
         totfarmExp = farmAHexp + farmAFHexp,
         farmShrAH =  farmAHexp / fahExp,  # get farm shares
         farmShrAFH = farmAFHexp/fafhExp,
         farmShrTot = totfarmExp/totExp) %>%
  mutate(across(c(ends_with("Exp")),  ~ . / !!1e9 ),
         totalFoodExp = fahExp + fafhExp) # get total food exp in billions


yi4 <- read_xlsx(system.file("extdata",mapping="YiSourceFig4.xlsx",
                                    package = "mrmarkup"), skip = 1) %>%
  pivot_longer(cols = c(2:last_col()), names_to = "year", values_to = "YifarmAHshr") %>%
  mutate(year = as.numeric(year)) %>%
  filter(!is.na(year)) %>%
  group_by(Country, year) %>%
  summarise(YifarmAHshr = mean(YifarmAHshr, na.rm =T)) %>%
  ungroup()
yi4$iso3c <- toolCountry2isocode(yi4$Country, mapping = c("Korea, Rep." = "KOR"))

compyi4 <-  select(magExpMeanK, iso3c, year,farmShrAH) %>%
  inner_join( select(yi4, iso3c, year, YifarmAHshr)) %>%
  pivot_longer(cols = c(farmShrAH, YifarmAHshr),
               names_to = "source", values_to = "farmAHShr")

ggplot(filter(compyi4),
       aes(x = year, y = farmAHShr, colour = source)) +
  geom_line()+
  facet_wrap(~iso3c) +
  #  ylim(c(0.15, 0.45)) +
  theme_bw(base_size = 20) +
  ggtitle("Farm Share of At Home Food Expenditures \n based on MAgPIE prices")


yi3 <- read_xlsx(system.file("extdata",mapping="YiSourceFig3.xlsx",
                                    package = "mrmarkup"), skip = 4) %>%
  rename("year" = Year, "USDAfarmShrTot" = `Farm value share of expenditures`,
         "YifarmShrTot" = `New farm share series`,
  ) %>%
  select(year, USDAfarmShrTot, YifarmShrTot) %>%
  filter(!is.na(YifarmShrTot))
yi3$USDAfarmShrTot[is.na(yi3$USDAfarmShrTot)]  <- 0

yi3 <- yi3 %>%
  mutate(year = as.numeric(year),
         USDAfarmShrTot = as.numeric(USDAfarmShrTot),
         YifarmShrTot = YifarmShrTot/100,
         USDAfarmShrTot = USDAfarmShrTot/100)

yi3[which(yi3$USDAfarmShrTot==0),"USDAfarmShrTot"] <- NA


mkYi3 <- filter(magExpMeanK, iso3c == "USA") %>%
  select(year, farmShrTot) %>%
  as.magpie() %>%
  time_interpolate(interpolated_year = c(1990:2020), integrate_interpolated_years = TRUE) %>%
  as.data.frame(rev = 2) %>%
  pivot_wider(names_from = "data", values_from = ".value")

compyi3 <- mkYi3 %>%
  right_join(yi3) %>%
  pivot_longer(cols = c(farmShrTot, USDAfarmShrTot, YifarmShrTot),
               names_to = "source", values_to = "farmShr")

ggplot(compyi3, aes(x = year, y = farmShr, colour = source)) +
  geom_line() +
  #ylim(c(0.10, 0.30)) +
  theme_bw(base_size = 20) +
  ggtitle("Farm share of US food expenditures  \n based on MAgPIE prices")



