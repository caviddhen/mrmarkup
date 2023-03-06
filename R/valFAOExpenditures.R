#' @title valFAOExpenditures
#' @description uses regression markups to calculate consumer price markups
#' @import tidyr dplyr mrcommons
#' @importFrom magpiesets findset
#' @import madrat
#' @export
#'
#' @return dataframe of FAO FBS
#' @author David M Chen
#' @importFrom readxl read_xlsx
#' @importFrom ggrepel geom_text_repel

valFAOExpenditures <- function(plot = TRUE){

kBH <- read.csv(system.file("extdata",mapping="mapMAgPIELEM.csv",
                                           package = "mrmarkup")) %>%
  rename("BHName" = prod)

h12 <- toolGetMapping("h12.csv", type = "regional") %>%
  rename("iso3c" = "CountryCode", "i" = "RegionCode") %>%
  select(iso3c, i)

FAOp <- readFAOPrices(level = "prim", fillPrices = TRUE)
#make 0 prices NA to fill
FAOp[FAOp == 0] <- NA

iso <- getItems(FAOp, dim = 1)
items <- getItems(FAOp, dim = 3)
avg <- new.magpie(cells_and_regions = iso,
                  years = NULL,
                  names = items)
for (i in iso ){
  for (t in items){
    avg[i,,t] <- dimSums(FAOp[i,,t], dim = 2, na.rm = T)/length(which(!is.na(FAOp[i,,t])))

    FAOp[i,,t] <- ifelse(is.na(FAOp[i,,t]), avg[i,,t], FAOp[i,,t])

  }
}

#then fill with regional average
for (i in getNames(FAOp)){
  FAOp[,,i] <- toolFillWithRegionAvg(FAOp[,,i])
}

#then fill with global FAOini price
#
pinit <- calcOutput("IniFoodPrice", aggregate = FALSE, datasource = "FAO")
attr <- calcOutput("Attributes", aggregate = FALSE)
pinit <- pinit/attr[,,"wm"][,,getItems(pinit, dim = 3)] %>%
  collapseNames()
#load("C:/PIK/ICPdata/pinitWM.Rda")

#pinit <- pinit * 1.23 # inflate

pinit <- add_columns(pinit, addnm = "Vegetables", dim = 3.1, fill = 0)
FAOp <- FAOp[,,"remaining", inv=T]
citems <- intersect(getNames(pinit), getNames(FAOp))

for (i in citems)  {
  FAOp[,,i] <- ifelse(is.na(FAOp[,,i]), pinit[,,i], FAOp[,,i])
}

prpr <- FAOp %>%  as.data.frame(rev = 2) %>%
  rename( "iso3c"= ISO, "year" = Year, "k" = ItemCodeItem, "value" = ".value") %>%
  inner_join(h12)   %>%
  inner_join(kBH) %>%
  rename("magPrice" = "value")

#remove alcohol
prpr <- filter(prpr, k != "alcohol")


##### get markup regression coefs #####


coefs <- coefs  %>%   
#regressMarkups() %>%
  rename("BHName" = "prod")

magCoefs <- kBH  %>%
  inner_join(coefs)


#
gdppcppp <- calcOutput("GDPpc", aggregate = F)
#load("C:/PIK/ICPdata/gdppcppp.Rda")

gdppc <- time_interpolate(gdppcppp[,,"gdppc_SSP2"], interpolated_year = c(2010:2030),
                          integrate_interpolated_year = TRUE) %>%
  as.data.frame(rev = 2)  %>%
  rename("gdppc" = ".value")  %>%
  select(iso3c, year, gdppc)

#attr <- calcOutput("Attributes", aggregate = F)[,,"wm"] #convert markup to dm for magpie
#wm <- attr   %>% as.data.frame(rev = 2)  %>%
#  rename("k" = "products", "wm" = ".value")  %>%
#  select(k, wm)


markupPrC <- inner_join(prpr, gdppc)  %>% 
               mutate(Cater = "Cater") %>% 
          mutate(loggdp = log(gdppc, base = 10))
markupPrNC <- inner_join(prpr, gdppc)  %>% 
              mutate(Cater = "noCater")  %>% 
          mutate(loggdp = log(gdppc, base = 10))


### load normal model!!
load("/p/projects/magpie/users/davidch/ICPdata_cluster/brm")

#split into 2 to save RAM
markupPrC <- cbind(markupPrC,  fitted(brmM , markupPrC))
markupPrNC <- cbind(markupPrNC,  fitted(brmSmooth1, markupPrNC))

save(markupPrC, file = "/p/projects/magpie/users/davidch/markupPrC.Rda")
load( "/p/projects/magpie/users/davidch/markupPrC.Rda")
markupPr
markupPr <- rbind(markupPrC, markupPrNC)  %>% 
              rename("markup" = Estimate)  %>% 
           mutate(consPrice = magPrice + markup,
                   consPrice025 = magPrice + `Q2.5`,
                   consPrice975 = magPrice + `Q97.5` ) %>%
  select(!c(markup, `Est.Error`, `Q2.5`, `Q97.5`)) %>%
  pivot_longer(names_to = "Prices", cols = c(consPrice, 
                                         consPrice025, consPrice975))  %>% 
  pivot_wider(names_from = c(Cater, Prices), values_from = value) %>% 
  pivot_longer(cols = c(magPrice,Cater_consPrice, Cater_consPrice025, Cater_consPrice975,
                       noCater_consPrice, noCater_consPrice025, noCater_consPrice975),
             names_to = "Price Type", values_to = "Price")


  #pivot_longer(cols = c(magPrice, noCater, cater),
  #            names_to = "Price Type", values_to = "Price")


#  #rename("k" = kcr)  %>%
 # inner_join(magCoefs)  %>%
  ## inner_join(wm) %>%
  #mutate(markup = a*(b^log(gdppc, base = 10)),
  #       # markupCater = markupCater * wm,  #convert to dry matter
  #       consPrice = magPrice + markup) %>%
  #select(!c(a,b, markup)) %>%
  #pivot_wider(names_from = cater, values_from = consPrice) %>%
  #pivot_longer(cols = c(magPrice, noCater, cater),
   #            names_to = "Price Type", values_to = "Price")



#use FAO consumption

consKmag <- readFBSnew(level = "prim")
cons <- consKmag %>%
  as.data.frame(rev=2)  %>%
  rename("foodD" = ".value", "iso3c" = "Area",
         "k" = "prod", "year" = "Year")


# reg <- "EUR"
#
pr <- "others"
iso <- "IND"

plotp <- markupPr %>%
        inner_join(cons) %>%
        group_by(iso3c, year, BHName, `Price Type`) %>%
         summarise(`Average Price` = weighted.mean(Price, w = foodD)/1000)

plotp1 <- separate(plotp, `Price Type`, into = c("Price Type", "ybd"))
plotp1[which(plotp1[,"ybd"] == "consPrice"), "ybd"] <- "y"
plotp1[which(plotp1[,"ybd"] == "consPrice025"), "ybd"] <- "ylo"
plotp1[which(plotp1[,"ybd"] == "consPrice975"), "ybd"] <- "yhi"

plotp1[which(is.na(plotp1[,"ybd"])), "ybd"] <- "y"

plotp1 <- plotp1  %>% pivot_wider(names_from = "ybd", values_from = "Average Price")  %>% 
   mutate(ylo = case_when(
                        is.na(ylo) ~ y,
                        !is.na(ylo) ~ ylo),
          yhi = case_when(
                        is.na(yhi) ~ y,
                         !is.na(yhi) ~ yhi))
                    
plotp1[which(plotp1[,"Price Type"] == "Cater"), "Price Type"] <- "Food-Away-From-Home"
plotp1[which(plotp1[,"Price Type"] == "magPrice"), "Price Type"] <- "Producer"
plotp1[which(plotp1[,"Price Type"] == "noCater"), "Price Type"] <- "Food-At-Home"

iso <- "IND"

ind <- ggplot(filter(plotp1, iso3c == iso)) + 
 geom_ribbon(aes(x= year , ymin = ylo, ymax = yhi, fill = `Price Type`), alpha=0.2) +
  geom_line(aes(x = year, y = y, color = `Price Type`), lwd = 1.4) +
  facet_wrap(~BHName, scales = "free") +
  ggtitle(paste("Food Prices India")) +
  labs(x = "Year", y = "USD$17/kg") +
  theme_classic(base_size = 14) +
  expand_limits(y = 0) +
  scale_x_continuous(breaks = ~ axisTicks(., log = FALSE))


ind

iso <- "USA"
usa <- ggplot(filter(plotp1, iso3c == iso)) + 
 geom_ribbon(aes(x= year , ymin = ylo, ymax = yhi, fill = `Price Type`), alpha=0.2) +
  geom_line(aes(x = year, y = y, color = `Price Type`), lwd = 1.4) +
  facet_wrap(~BHName, scales = "free") +
  ggtitle(paste("Food Prices USA")) +
  labs(x = "Year", y = "USD$17/kg") +
  theme_classic(base_size = 14) +
  expand_limits(y = 0) +
  scale_x_continuous(breaks = ~ axisTicks(., log = FALSE))

usa
library(gridExtra)
indusa <- grid.arrange(ind, usa, nrow = 2)
ggsave(indusa, height = 16, width = 12, filename = "/p/projects/magpie/users/davidch/ICPdata_cluster/plots/indusa.png")
ggsave(indusa, height = 16, width = 12, filename = "/p/projects/magpie/users/davidch/ICPdata_cluster/plots/indusa.pdf")



AFH <- regressFAFH(weight = TRUE)

magExp <- inner_join(cons, markupPr)  %>%
  inner_join(AFH) %>%      ### from kcal_fafh
  pivot_wider(names_from = `Price Type`,
              values_from = Price) %>% 
  mutate(fahExp = foodD * (1-AFHshr) * (noCater_consPrice),   #FAH
       fahExpL = foodD * (1-AFHshr) * (noCater_consPrice025),
       fahExpH = foodD * (1-AFHshr) * (noCater_consPrice975),
         fafhExp = foodD * AFHshr * (Cater_consPrice),   # FAFH 
        fafhExpL = foodD * (AFHshr) * (Cater_consPrice025),
       fafhExpH = foodD * (AFHshr) * (Cater_consPrice975),
         farmAHexp = foodD *(1-AFHshr) * magPrice, #farm Exp
         farmAFHexp = foodD *AFHshr * magPrice,
         farmAHshr = farmAHexp/fahExp,   #farm share of AH
         farmAHshrL = farmAHexp/fahExpL,
         farmAHshrH = farmAHexp/fahExpH,
         farmAFHshr = farmAFHexp/fafhExp, #farm share of AFH
        farmAFHshrL = farmAFHexp/fafhExpL,
         farmAFHshrH = farmAFHexp/fafhExpH) %>%
  select(iso3c, year, k, foodD, gdppc, fahExp, fahExpL, fahExpH,
                                       fafhExp,  fafhExpL, fafhExpH,
                farmAHexp, farmAFHexp,
                                farmAHshr, farmAHshrL, farmAHshrH,
                                farmAFHshr, farmAFHshrL, farmAFHshrH )


magExpMeanK <- magExp %>%
  group_by(iso3c, year) %>%
  summarise(fahExp = sum(fahExp),
              fahExpL = sum(fahExpL),
              fahExpH = sum(fahExpH),
            fafhExp = sum(fafhExp),
           fafhExpL = sum(fafhExpL),
          fafhExpH = sum(fafhExpH),

           farmAHexp = sum(farmAHexp),
            farmAFHexp = sum(farmAFHexp)) %>%
  mutate(totExp = fahExp + fafhExp,
        totExpL = fahExpL + fafhExpL,
        totExpH = fahExpH + fafhExpH,
         totfarmExp = farmAHexp + farmAFHexp,
         farmShrAH =  farmAHexp / fahExp,  # get farm shares
          farmShrAHL =  farmAHexp / fahExpL, 
 farmShrAHH =  farmAHexp / fahExpH, 
         farmShrAFH = farmAFHexp/fafhExp,
     farmShrAFHL = farmAFHexp/fafhExpL,
          farmShrAFHH = farmAFHexp/fafhExpH,
         farmShrTot = totfarmExp/totExp, 
        farmShrTotL = totfarmExp/totExpL, 
         farmShrTotH = totfarmExp/totExpH) %>%
  mutate(across(c(ends_with("Exp")),  ~ . / !!1e9 ),
         totalFoodExp = fahExp + fafhExp, 
         totalFoodExpL = fahExpL + fafhExpH,
         totalFoodExpH = fahExpL + fafhExpH,
         ) # get total food exp in billions


# make map of farm share 
pop <- calcOutput("Population", aggregate = FALSE)[,,"pop_SSP2"]
pop <- time_interpolate(pop, interpolated_year = unique(magExpMeanK$year), integrate_interpolated_year = TRUE)

mag <- as.magpie(magExpMeanK[,c("iso3c", "year", "farmShrTot")], tidy = TRUE)
library(rworldmap)
library(luplot)
map <- plotcountrymap(mag[,2019, ],  numCats = 5,
            catMethod = "pretty", colourPalette = c('#f1eef6','#bdc9e1','#74a9cf','#2b8cbe','#045a8d'),
            mapTitle = "Farm share of Food Dollar \n All Food Expenditures", 
            addLegend = FALSE)
 do.call(addMapLegend, c(map, 
                       legendLabels = "all"))

ggsave(map, file = "/p/projects/magpie/users/davidch/ICPdata_cluster/plots/plotmap.pdf", height = 30, width = 24)


#write.csv(magExpMeanKvalid, file="noweightReg_GDPconv.csv")

yi4 <-  read_xlsx(system.file("extdata",mapping="YiSourceFig4.xlsx",
                                    package = "mrmarkup"), skip = 1) %>%
  pivot_longer(cols = c(2:last_col()), names_to = "year", values_to = "YifarmAHshr") %>%
  mutate(year = as.numeric(year)) %>%
  filter(!is.na(year)) %>%
  group_by(Country, year) %>%
  summarise(YifarmAHshr = mean(YifarmAHshr, na.rm =T)) %>%
  ungroup()
yi4$iso3c <- toolCountry2isocode(yi4$Country, mapping = c("Korea, Rep." = "KOR") )

compyi4 <-  select(magExpMeanK, iso3c, year, farmShrAH) %>%
  inner_join( select(yi4, iso3c, year, YifarmAHshr)) %>%
  pivot_longer(cols = c(farmShrAH, YifarmAHshr),
               names_to = "source", values_to = "farmAHShr")


compyi4L <-  select(magExpMeanK, iso3c, year, farmShrAHL) %>%
  inner_join( select(yi4, iso3c, year, YifarmAHshr)) %>%
   rename("farmShrAH" = "farmShrAHL")  %>% 
  pivot_longer(cols = c(farmShrAH, YifarmAHshr),
               names_to = "source", values_to = "farmAHShrL")

compyi4H <-  select(magExpMeanK, iso3c, year, farmShrAHH) %>%
  inner_join( select(yi4, iso3c, year, YifarmAHshr)) %>%
         rename( "farmShrAH" = "farmShrAHH")  %>% 
  pivot_longer(cols = c(farmShrAH, YifarmAHshr),
               names_to = "source", values_to = "farmAHShrH")

compyi4 <- inner_join(compyi4, compyi4L)  %>% 
          inner_join(compyi4H)


a <- ggplot(compyi4) +
   geom_ribbon(aes(x= year , ymin = farmAHShrL, ymax = farmAHShrH, fill = source), alpha=0.2) +
  geom_line( aes(x = year, y = farmAHShr, colour = source), size = 1.5)+
  facet_wrap(~iso3c) +
  #  ylim(c(0.15, 0.45)) +
  theme_bw(base_size = 20) +
  ggtitle("b) Farm Share of Food-At-Home Expenditures")
a
yi3 <-  read_xlsx(system.file("extdata",mapping="YiSourceFig3.xlsx",
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

compyi3 <- filter(magExpMeanK, iso3c == "USA") %>%
  select(year, farmShrTot) %>%
  right_join(yi3) %>%
  pivot_longer(cols = c(farmShrTot, USDAfarmShrTot, YifarmShrTot),
               names_to = "source", values_to = "farmShr")


compyi3L <- filter(magExpMeanK, iso3c == "USA") %>%
  select(year, farmShrTotL) %>%
  right_join(yi3) %>%
  rename( "farmShrTot" = "farmShrTotL")  %>% 
pivot_longer(cols = c(farmShrTot, USDAfarmShrTot, YifarmShrTot),
               names_to = "source", values_to = "farmShrL")


compyi3H <- filter(magExpMeanK, iso3c == "USA") %>%
  select(year, farmShrTotH) %>%
  right_join(yi3) %>% 
    rename( "farmShrTot" = "farmShrTotH")  %>% 
  pivot_longer(cols = c(farmShrTot, USDAfarmShrTot, YifarmShrTot),
               names_to = "source", values_to = "farmShrH")

compyi3 <- inner_join(compyi3, compyi3L)  %>% 
          inner_join(compyi3H)

b <- ggplot(compyi3) +
   geom_ribbon(aes(x= year , ymin = farmShrL, ymax = farmShrH, fill = source), alpha=0.2) +
  geom_line( aes(x = year, y = farmShr, colour = source), size = 2)+
  #ylim(c(0.10, 0.30)) +
  theme_bw(base_size = 22) +
  ggtitle("a) Farm share of US food expenditures")
b
  library(gridExtra)
out <- grid.arrange(b,a)
ggsave(out, height =  24, width = 16, file = "/p/projects/magpie/users/davidch/ICPdata_cluster/plots/Fig2.png")
ggsave(out, height =  24, width = 16, file = "/p/projects/magpie/users/davidch/ICPdata_cluster/plots/Fig2.pdf")

}

