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
coefs <- regressMarkups() %>%
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


markupPr <- inner_join(prpr, gdppc)  %>%
  #rename("k" = kcr)  %>%
  inner_join(magCoefs)  %>%
  # inner_join(wm) %>%
  mutate(markup = a*(b^log(gdppc, base = 10)),
         # markupCater = markupCater * wm,  #convert to dry matter
         consPrice = magPrice + markup) %>%
  select(!c(a,b, markup)) %>%
  pivot_wider(names_from = cater, values_from = consPrice) %>%
  pivot_longer(cols = c(magPrice, noCater, cater),
               names_to = "Price Type", values_to = "Price")



# reg <- "EUR"
#
pr <- "others"
iso <- "IND"
ggplot(filter(markupPr, iso3c == iso, k %in% c("tece", "livst_rum","oils","sugar","Vegetables", "others", "livst_pig", "livst_chick")),
       aes(x = year, y = Price, colour = `Price Type`))+
  geom_line(lwd = 1.4)+
  facet_wrap(~k) +
  ggtitle(paste(iso))



#use FAO consumption



consKmag <- readFBSnew(level = "prim")
cons <- consKmag %>%
  as.data.frame(rev=2)  %>%
  rename("foodD" = ".value", "iso3c" = "Area",
         "k" = "prod", "year" = "Year")

AFH <- regressFAFH(weight = TRUE)

magExp <- inner_join(cons, markupPr)  %>%
  inner_join(AFH) %>%      ### from kcal_fafh
  pivot_wider(names_from = `Price Type`,
              values_from = Price) %>%
  mutate(fahExp = foodD * (1-AFHshr) * (noCater),
         fafhExp = foodD * AFHshr * (cater),
         farmAHexp = foodD *(1-AFHshr) * magPrice,
         farmAFHexp = foodD *AFHshr * magPrice,
         farmAHshr = farmAHexp/fahExp,
         farmAFHshr = farmAFHexp/fafhExp)%>%
  select(iso3c, year, k, foodD, gdppc, fahExp, fafhExp,
         farmAHexp, farmAFHexp, farmAHshr, farmAFHshr)


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


magExpMeanKvalid <- magExpMeanK %>%
  filter(year == "2015", iso3c %in% c("CHN", "USA", "IND", "BRA")) %>%
  relocate(farmAHexp, .after = fahExp) %>%
  relocate(farmShrAH, .after = farmAHexp)  %>%
  relocate(farmAFHexp, .after = fafhExp) %>%
  relocate(farmShrAFH, .after = farmAFHexp) %>%
  select(-totalFoodExp)

magExpMeanKvalid[c(3:ncol(magExpMeanKvalid))] <- round(magExpMeanKvalid[c(3:ncol(magExpMeanKvalid))], digits = 3)



# make map of farm share 
pop <- calcOutput("Population", aggregate = FALSE)[,,"pop_SSP2"]
pop <- time_interpolate(pop, interpolated_year = unique(magExpMeanK$year), integrate_interpolated_year = TRUE)

mag <- as.magpie(magExpMeanK[,c("iso3c", "year", "farmShrTot")], tidy = TRUE)
#library(rworldmap)
#library(luplot)
#plotmap(mag[,2019,])
#t <- plotcountrymap(mag[,2019, ],  numCats = 5,
#            catMethod = "pretty", colourPalette = c('#f1eef6','#bdc9e1','#74a9cf','#2b8cbe','#045a8d'),
#            mapTitle = "Farm share of Food Dollar \n All Food Expenditures", 
#            addLegend = FALSE)
# do.call(addMapLegend, c(t, 
#                       legendLabels = "all"))

#save(mag, file = "/p/projects/landuse/users/davidch/ICPdata/plotme.Rds")


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

a <- ggplot(compyi4, aes(x = year, y = farmAHShr, colour = source)) +
  geom_line(size = 1.5)+
  facet_wrap(~iso3c) +
  #  ylim(c(0.15, 0.45)) +
  theme_bw(base_size = 20) +
  ggtitle("a) Farm Share of Food-At-Home Expenditures")

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

b <- ggplot(compyi3, aes(x = year, y = farmShr, colour = source)) +
  geom_line(size = 2) +
  #ylim(c(0.10, 0.30)) +
  theme_bw(base_size = 22) +
  ggtitle("b) Farm share of US food expenditures")

#  library(gridExtra)
#out <- grid.arrange(a, b)
#ggsave(out, height =  24, width = 16, file = "/p/projects/landuse/users/davidch/ICPdata/Fig2.png")
#ggsave(out, height =  24, width = 16, file = "/p/projects/landuse/users/davidch/ICPdata/Fig2.pdf")

}

