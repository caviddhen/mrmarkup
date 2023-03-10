#' @title FoodExpMagpieMarkup
#' @description calculate markups
#' @param level reg or regglo
#' @param type "consumer" (marked up) or "producer" price output 
#' @param gdx gdx file as input for mag base prices
#' @param afterShock TRUE or FALSE for kcal consumed 
#' @param povmodel TRUE outputs data in aggregated magclass format for poverty model
#' @param prodAggr TRUE for aggregating across alll products
#' @param validYi plot validation to Yi 2021
#' @import dplyr tidyr
#' @importFrom magclass as.data.frame
#' @importFrom magpiesets findset
#' @importFrom gdx readGDX
#' @import brms
#' @export
#'
#' @return dataframe or magclass object of consumer food expenditures
#' @author David M Chen

FoodExpMagpieMarkup <- function(gdx, level = "reg", type = "consumer", prodAggr = FALSE, afterShock = FALSE,
                                povmodel = FALSE, validYi = FALSE) {
#gdx <-  "/p/projects/magpie/users/davidch/magpie_versions/develop/magpie/output/markupDefPov_2023-01-18_15.56.21/fulldata.gdx"

# NOTE THIS ONE IS IN 2005 USDMER

kfo <- findset("kfo")
kBH <- read.csv(system.file("extdata",mapping="mapMAgPIELEM.csv",
                              package = "mrmarkup"))  %>%
  rename("BHName" = prod)

mapping <- readGDX(gdx = gdx, "i_to_iso")  %>% 
           rename("iso3c" = "iso")


attr <- calcOutput("Attributes", aggregate = F)[,,"wm"] # t wm / t dm convert prices from dm to wm for markup
wm <- attr   %>% magclass::as.data.frame(rev = 2)  %>%
  rename("k" = "products", "wm" = ".value")  %>%
  select(k, wm)

nutr <- readGDX(gdx, "f15_nutrition_attributes")[,,"kcal"]  #mio kcal / t DM convert prices from kcal to dm

prpr <- FoodDemandModuleConsumerPrices(gdx) # $/kcal
prpr <- collapseNames((prpr / attr[,,"wm"][,,getItems(prpr, dim = 3)] * nutr[,getYears(prpr),getItems(prpr, dim = 3)] * 1e6)) # prpr in $/twmM
prpr <- time_interpolate(prpr, interpolated_year = c(2010:2017), integrate_interpolated_years = TRUE)
prpr <- add_columns(prpr, addnm = "Vegetables", dim = 3.1, fill = NA)
prpr[,,"Vegetables"] <- prpr[,,"others"]

### replace price for oils and sugars (which already processed) with weighted average price of
#primary products that go into the secondary products
ksd <- findset("ksd")

#cons <- demand(gdx)
#cons <- gdxAggregate(gdx = gdx, cons, weight = 'Intake', to = "iso")[,,"food"]
#cons <- collapseNames(cons)
#kfo <- c(findset("kfo"), "Vegetables")
#cons <- add_columns(collapseNames(cons), addnm = "Vegetables")
#load input vegetable data to get a the latest split

consKmag <- readFBSnew(level = "prim")

cons <- Kcal(gdx = gdx, level = "iso",
           calibrated = TRUE, after_shock = afterShock, 
           products = "kfo", product_aggr = FALSE,
           per_capita = FALSE) * 365  # This is in MILLION KCAL! 

cons <- collapseNames(cons)
kfo <- c(findset("kfo"), "Vegetables")
cons <- add_columns(collapseNames(cons), addnm = "Vegetables")
#load input vegetable data to get a the latest split

VegShr <- consKmag[,,"Vegetables"]/collapseNames((consKmag[,,"others"]+consKmag[,,"Vegetables"]))
VegShr <- toolCountryFill(VegShr, fill = 0.58) #mean value
othShr <- (1-VegShr)
cons[,,"Vegetables"] <- setYears(VegShr[,2019,], NULL) * cons[,,"others"]
cons[,,"others"] <- setYears(othShr[,2019,], NULL) * cons[,,"others"]


attr <- add_columns(attr, addnm = "Vegetables", dim = 3.2)
attr[,,"Vegetables"] <- attr[,,"others"]
#cons <- cons * attr[,,"wm"][,,getItems(cons, dim =3)] %>%
#  collapseNames()

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
  rename("iso3c" = "iso", "year" = t, "k" = kfo, "value" = ".value") %>%
  inner_join(kBH) 

##### get markup regression coefs #####
#coefs <- regressMarkups()

load("/p/projects/magpie/users/davidch/ICPdata_cluster/brmshiermodel.Rda")

#remove alcohol
prpr <- filter(prpr, k != "alcohol")

#magCoefs <- kBH  %>%
#  rename("prod" = "BHName")  %>% 
#    inner_join(coefs) 

gdppc <- income(gdx, level = "iso")

gdppc <- gdppc %>%
  as.data.frame(rev = 2)  %>%
  rename("iso3c" = "iso", "year" = "t_all", "gdppc" = ".value") %>%
  select(iso3c, year, gdppc)

nutr <- as.data.frame(nutr, rev= 2)  %>% 
  rename("year" = "t_all", "k" = kall, "kcal" = ".value") %>%
  select(year,k, kcal)

attr <- as.data.frame(attr, rev = 2)  %>% 
      rename( "k" = "products", "wm" = ".value") %>%
      select(k, wm)

load("/p/projects/magpie/users/davidch/ICPdata_cluster/fittedDF.Rda")                              
head(fits)

head(prpr)

 prpr <- prpr  %>% 
               GDPuc::convertGDP(unit_in = "constant 2005 US$MER",
                   unit_out = "constant 2017 US$MER",
                   replace_NAs = "no_conversion")  %>% 
        rename("prodPrice" = value)  %>% 
        inner_join(fits)   %>% 
        select(!value)

markupPr <- prpr  %>% 
                   mutate(consPrice = prodPrice + Estimate)  %>% 
   select(iso3c, year, k, prodPrice, BHName,  Cater, gdppc, prodPrice, consPrice)  %>% 
             pivot_wider(names_from = Cater, values_from = consPrice)    %>% 
  rename( "caterPrice" = Cater, "noCaterPrice" = noCater)  



# markupPr <- inner_join(prpr, gdppc)  %>%
#  inner_join(magCoefs)  %>%
#  inner_join(nutr)  %>% 
#  inner_join(attr)  %>%
#  rename("prodPrice" = value)  %>% 
#mutate(markupCater = a*(b^log(gdppc, base = 10)),
#       markupCater = markupCater * wm / kcal / 1e6) %>% #get to tdm, to $/kcal from  miokcal/tdm 
#rename("value" = "markupCater")  %>% 
#  GDPuc::convertGDP(unit_in = "constant 2017 US$MER",
#                   unit_out = "constant 2005 US$MER",
#                   replace_NAs = "no_conversion")  %>% 
#       rename("markupCater" = value)  %>% 
#mutate(CaterPrice = prodPrice + markupCater)  %>% 
#select(!c(a, b, markupCater))  %>% 
#pivot_wider(names_from = cater, values_from = CaterPrice) %>% 
#  rename( "caterPrice" = cater, "noCaterPrice" = noCater)

markups <-  markupPr %>%
  pivot_longer(cols = c(prodPrice, noCaterPrice, caterPrice),
               names_to = "Price Type", values_to = "Price") %>%
  select(iso3c, year, k,  `Price Type`, Price) 

## plotting scripts started here

cons <- cons[,,kfo] %>% #from FoodExpMagpieMarkup
  as.data.frame(rev=2)  %>% 
  rename("foodD" = ".value", "iso3c" = "iso",
         "k" = "kfo", "year" = "t")

AFHshr <- regressFAFH()

magExp <- inner_join(cons,
                     markups)  %>%
  inner_join(AFHshr) %>%      ### from kcal_fafh
  pivot_wider(names_from =  c(`Price Type`),
              values_from = Price) %>%
  mutate(foodD = foodD * 1e6, #convert to kcal from Mio. kcal
         fahExp = foodD * (1-AFHshr) * (noCaterPrice),
         fafhExp = foodD * AFHshr * (caterPrice),
         farmAHexp = foodD *(1-AFHshr) * prodPrice,
         farmAFHexp = foodD *AFHshr * prodPrice,
         farmAHshr = farmAHexp/fahExp,
         farmAFHshr = farmAFHexp/fafhExp)%>% 
    mutate(across(c(ends_with("Exp")),  ~ . / !!1e9 ),
         totalFoodExp = fahExp + fafhExp)  %>%  # get total food exp in billions
  select(iso3c, year, k, foodD, gdp, prodPrice, caterPrice, noCaterPrice, fahExp, fafhExp, totalFoodExp, farmAHexp, farmAFHexp, farmAHshr, farmAFHshr)


if(prodAggr) {
magExp<- magExp %>%
  group_by(iso3c, year) %>%
  summarise(fahExp = sum(fahExp),
            fafhExp = sum(fafhExp),
            farmAHexp = sum(farmAHexp),
            farmAFHexp = sum(farmAFHexp),
            totExp = sum(totalFoodExp)) %>%
  mutate(totfarmExp = farmAHexp + farmAFHexp,
         farmShrAH =  farmAHexp / fahExp,  # get farm shares
         farmShrAFH = farmAFHexp/fafhExp,
         farmShrTot = totfarmExp/totExp)
}

if(validYi){

yi4 <- read_xlsx(system.file("extdata",mapping="YiSourceFig4.xlsx",
                                    package = "mrmarkup"), skip = 1) %>%
  pivot_longer(cols = c(2:last_col()), names_to = "year", values_to = "YifarmAHshr") %>%
  mutate(year = as.numeric(year)) %>%
  filter(!is.na(year)) %>%
  group_by(Country, year) %>%
  summarise(YifarmAHshr = mean(YifarmAHshr, na.rm =T)) %>%
  ungroup()
yi4$iso3c <- toolCountry2isocode(yi4$Country, mapping = c("Korea, Rep." = "KOR"))

compyi4 <-  select(magExp, iso3c, year,farmShrAH) %>%
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


mkYi3 <- filter(magExp, iso3c == "USA") %>%
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

} 

out <- magExp

if(povmodel) {
pop <- population(gdx, level = "iso")

  if (type == "producer") {
    exp <- "totfarmExp"
  } else if (type == "consumer") {
    exp <- "totExp"
  }
mag <- as.magpie(magExp[,c("iso3c", "year", exp)], tidy = TRUE)

if(level == "reg") {
mag <- toolAggregate(mag, rel = mapping,  from = "iso3c", to = "i",
                      dim = 1)
pop <- toolAggregate(pop, rel = mapping,  from = "iso3c", to = "i",
                      dim = 1)
out <- mag/pop*1e3 # in billion dollars/million ppl to dollar/capita

} else if (level == "regglo") {
mag <- toolAggregate(mag, rel = mapping,  from = "iso3c", to = "i",
                      dim = 1)
pop <- toolAggregate(pop, rel = mapping,  from = "iso3c", to = "i",
                      dim = 1)
 glom <- dimSums(mag, dim = 1)
 glop <- dimSums(pop, dim = 1)
 getItems(glom, dim = 1) <- "GLO"
 getItems(glop, dim = 1) <- "GLO"
 mag <- mbind(mag, glom)
 pop <- mbind(pop, glop)
 out <- mag/pop*1e3 # in billion dollars/million ppl to dollar/capita

} else if (level == "iso"){
out <- mag/pop*1e3 # in billion dollars/million ppl to dollar/capita

}
getNames(out) <- NULL
}

return(out)

}
