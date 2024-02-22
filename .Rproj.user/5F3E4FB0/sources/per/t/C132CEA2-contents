# second analysis of fair share, with 2022 May updated cerc dataset

## --- getting the data ready --- ##

library(ggplot2)
library(magrittr)
library(dplyr)
library(forcats)
library(moments)
library(officer)
library(rvg)

# read csv
df <- read.csv(file="data/cerc_all_output_1694574828.csv") # import dataset excl. lulucf
print(df)

df.lulucf <- read.csv(file="data/cerc_all_output_1699941907_LULUCF.csv") # import dataset incl. lulucf [disabled on web api]
print(df.lulucf)

# slice dataframe to fit
df.sliced <- df %>% slice(-(1:29))
names(df.sliced) <- df.sliced[1,]
df.sliced <- df.sliced[-1,]

df.lulucf.sliced <- df.lulucf %>% slice(-(1:29))
names(df.lulucf.sliced) <- df.lulucf.sliced[1,]
df.lulucf.sliced <- df.lulucf.sliced[-1,]

# extract for MYS, IDN, THA, and USA
fs.countries = c("MYS","IDN","THA","USA")
df.fs <- df.sliced %>%  filter(iso3 %in% fs.countries)
df.lulucf.fs <- df.lulucf.sliced %>%  filter(iso3 %in% fs.countries)

# assign correct object type to columns
df.fs[,c(3:21)] <- sapply(df.fs[c(3:21)], as.numeric)
str(df.fs)

df.lulucf.fs[,c(3:21)] <- sapply(df.lulucf.fs[c(3:21)], as.numeric)
str(df.lulucf.fs)

## ------------------ ##
##  --- NDC --- ## 
###** IDN
###* BAU scenario - 2869 MtCO2e 
###* 31.89% unconditional
###* 43.20% conditional

###** THA
###* BAU scenario - 555 MtCO2e
###* 30% unconditional below BAU scenario
###* 40% conditional

###** USA
###* 2005 levels net emissions - 6635 MtCO2e
###* 50 -52% reduction below 2005 net emissions levels

###** MYS [source: [NC](https://unfccc.int/sites/default/files/resource/Malaysia%20NC3%20BUR2_final%20high%20res.pdf), BUR4]
###* ttl emissions 2005 - 250.044
###* ttl BAU emissions 2030 - 530.29108 (549.53451) # (incl. LULUCF emissions only no removal)
###*  net emissions 2005 - 52.967
###*  net BAU emissions 2030 - 302.88559
###*  GDP 2005 (cst 2015) - 729.82
###*  GDP 2030 - 2143.55
###*  45% (EI2005 - EI2030)

## ---  NDC - fair share comparisons --- ##

#### compute NDC - emissions allocation

NDC.uncon.idn = 2869*(1-.3189)
NDC.con.idn = 2869*(1-.4320)
NDC.uncon.tha = 555*(1-.3)
NDC.con.tha = 555*(1-.4)
NDC.lower.US = 6635*(1-.5)
NDC.upper.US = 6635*(1-.52)

#### compute NDC - emissions reduction

NDC.rdxn.uncon.idn = 2869*.3189
NDC.rdxn.con.idn = 2869*.4320
NDC.rdxn.uncon.tha = 555*.3
NDC.rdxn.con.tha = 555*.4
NDC.rdxn.lower.US = 6635*.5
NDC.rdxn.upper.US = 6635*.52

#### MYS emissions intensity-based NDC calc

# set up CAGR and EI functions #

CAGR_FV <- function(CAGR, present_value, yrs) {
  final_value = present_value * (CAGR / 100 + 1)^yrs
  return(final_value)
}

EI_Ef <- function(Ei, delta) {
  # Ef = emissions intensity at final year, 2030
  # Ei = emissions intensity at initial year, 2005
  # delta = difference between compared final and initial year, 0.45
  Ef = Ei*(delta + 1)
  return(Ef)
}

## find MYS NDC emissions level

GDP2005<- 729.82
GDP2030 <- CAGR_FV(4.5, 1507.31, 8) # find GDP2030 based on WEO forecast 4.5% growth rate

BUR4.emi2005 <- 250.044 

EI2005 <- BUR4.emi2005/GDP2005
EI2030 <- EI_Ef(EI2005, -0.45) # find emissions intensity based on known 2005 figures

BUR4.emi2030 <- EI2030*GDP2030 # find absolute 2030 emissions

NDC.MYS <- BUR4.emi2030 # NDC 2030 emissions level is based on EI 45% lower than 2005  

BUR4.netemi2005 <- 52.967
NC3.netemi2030 <- 302.89

EI2005.lulucf <- BUR4.netemi2005/GDP2005 # find emissions intensity based on known 2005 netted figures
EI2030.lulucf <- EI_Ef(EI2005.lulucf, -0.45) # find emissions intensity based on known 2005 netted figures
EI2030.lulucf.con <- EI_Ef(EI2005.lulucf, -0.35)

BUR4.netemi2030 <- EI2030.lulucf*GDP2030 # find absolute 2030 emissions
BUR4.netemi2030.con <- EI2030.lulucf.con*GDP2030 # find absolute 2030 emissions


NDC.MYS.lulucf <- BUR4.netemi2030 # NDC 2030 emissions level is based on EI 45% lower than 2005
NDC.MYS.lulucf.con <- BUR4.netemi2030.con # NDC 2030 emissions level is based on EI 35% lower than 2005  


#### compute NDC - emissions reduction for MYS

NDC.rdxn.MYS <- 530.29108 - BUR4.emi2030 # BUR BAU
NDC.rdxn.MYS.lulucf <- 302.88559 - BUR4.netemi2030
NDC.rdxn.MYS.lulucf.con <- 302.88559 - BUR4.netemi2030.con


## ------------------ ##

## --- create comparison table --- ##

df.compare <- df.fs

## --- add NDC column for all countries --- ##
# create base and net-CO2 emissions line [fossil CO2 +- LULUCF removals]

df.compare <- df.compare %>% mutate(base_emissions_mtco2e = fossil_CO2_MtCO2+NonCO2_MtCO2e)
df.compare <- df.compare %>% mutate(net_emissions_mtco2e = fossil_CO2_MtCO2+NonCO2_MtCO2e+LULUCF_MtCO2)

# add NDC value to new column

df.compare <- df.compare %>% 
  mutate(NDCuncon_emissions_target_mtco2e = 
           case_when(
                      iso3 == "IDN" & year == 2030 ~ NDC.uncon.idn,
                      iso3 == "THA" & year == 2030 ~ NDC.uncon.tha,
                      iso3 == "USA" & year == 2030 ~ NDC.lower.US,
                      iso3 == "MYS" & year == 2030 ~ NDC.MYS,
                      .default = NA)
         )
df.compare <- df.compare %>% 
  mutate(NDCcon_emissions_target_mtco2e = 
           case_when(
             iso3 == "IDN" & year == 2030 ~ NDC.con.idn,
             iso3 == "THA" & year == 2030 ~ NDC.con.tha,
             iso3 == "USA" & year == 2030 ~ NDC.upper.US,
             .default = NA)
        )

## ------------------ ##

## --- Approach 1 rebase --- ##

## rebase NDC to total emissions excl. LULUCF baseline

### IDN [31.89% - 43.20% below BAU]
NDC.rebase1.idn.uncon <- df.compare[df.compare$iso3 == "IDN" & df.compare$year == 2030, # rebase uncon NDC to CERc BAU in 2030
                                    "base_emissions_mtco2e"]*(1-.3189)
NDC.rebase1.idn.con <- df.compare[df.compare$iso3 == "IDN" & df.compare$year == 2030, # rebase con NDC to CERc BAU in 2030
                                    "base_emissions_mtco2e"]*(1-.4320)

### THA [30% - 40% below BAU]
NDC.rebase1.tha.uncon <- df.compare[df.compare$iso3 == "THA" & df.compare$year == 2030, # rebase uncon NDC to CERc BAU in 2030
                                    "base_emissions_mtco2e"]*(1-.30)
NDC.rebase1.tha.con <- df.compare[df.compare$iso3 == "THA" & df.compare$year == 2030, # rebase con NDC to CERc BAU in 2030
                                  "base_emissions_mtco2e"]*(1-.40)

### USA [30% - 40% below BAU]
NDC.rebase1.usa.lower <- df.compare[df.compare$iso3 == "USA" & df.compare$year == 2030, # rebase lower NDC to CERc BAU in 2030
                                    "base_emissions_mtco2e"]*(1-.50)
NDC.rebase1.usa.upper <- df.compare[df.compare$iso3 == "USA" & df.compare$year == 2030, # rebase upper NDC to CERc BAU in 2030
                                  "base_emissions_mtco2e"]*(1-.52)

### MYS [45% below 2005 EI]
#### rebase EI2005 to CERc emissions baseline
EI2005cerc <- df.compare[df.compare$iso3 == "MYS" & df.compare$year == 2005, "base_emissions_mtco2e"]/df.compare[df.compare$iso3 == "MYS" & df.compare$year == 2005, "gdp_blnUSDMER"]
EI2030cerc <- EI_Ef(EI2005cerc, -0.45) # find emissions intensity based on CERC 2005 figures

NDC.rebase1.mys <- EI2030cerc*df.compare[df.compare$iso3 == "MYS" & df.compare$year == 2030, "gdp_blnUSDMER"]

## add column with rebased NDC figures

df.compare <- df.compare %>% 
  mutate(rb1_NDCuncon_mtco2e = 
           case_when(
             iso3 == "IDN" & year == 2030 ~ NDC.rebase1.idn.uncon,
             iso3 == "THA" & year == 2030 ~ NDC.rebase1.tha.uncon,
             iso3 == "USA" & year == 2030 ~ NDC.rebase1.usa.lower,
             iso3 == "MYS" & year == 2030 ~ NDC.rebase1.mys,
             .default = NA)
  )
df.compare <- df.compare %>% 
  mutate(rb1_NDCcon_mtco2e = 
           case_when(
             iso3 == "IDN" & year == 2030 ~ NDC.rebase1.idn.con,
             iso3 == "THA" & year == 2030 ~ NDC.rebase1.tha.con,
             iso3 == "USA" & year == 2030 ~ NDC.rebase1.usa.upper,
             .default = NA)
  )

## plot preview

#fs.plot.nonlulucf    
ggplot(df.compare) +
  geom_ribbon(aes(x = year, ymin = allocation_MtCO2, ymax = base_emissions_mtco2e), 
              fill="#ff6718", alpha=0.4) +
  geom_path(aes(x = year, y = base_emissions_mtco2e),
            colour = "grey",
            size = 1.5) +
  geom_path(aes(x = year, y = allocation_MtCO2),
            colour = "#ff6718",
            size = 1.5) +
  geom_point(aes(x = year, y = NDCuncon_emissions_target_mtco2e),
             colour = "#004678",
             size = 2) +
  geom_point(aes(x = year, y = NDCcon_emissions_target_mtco2e),
             colour = "#AFDAFF",
             size = 2) +
  geom_point(aes(x = year, y = rb1_NDCuncon_mtco2e),
             colour = "#BF9000",
             size = 2) +
  geom_point(aes(x = year, y = rb1_NDCcon_mtco2e),
             colour = "#FFC000",
             size = 2) +
  scale_x_continuous(limit = c(1990,2030)) +
  facet_wrap(iso3 ~ ., scales="free") +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.text.x = element_text(vjust = -.75),
        axis.line.x = element_line(linewidth = .75),
        axis.ticks.x = element_line(linewidth = .75),
        axis.ticks.y = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold"),
        text = element_text(size = 16,
                            color = "black"))


## ------------------ ##

## --- Approach 2 --- ##

## national allocations are calculated as the difference between the national baseline 
## and national share of the global emissions reduction obligation
## RCI = national rdx/global rdx
## national rdx = RCI/global rdx
# global rdx = __

# use lulucf-based dataset
## create same comparison table

df.compare.lulucf <- df.lulucf.fs

## --- add NDC column for all countries --- ##
# create base and net-CO2 emissions line [fossil CO2 +- LULUCF removals]

df.compare.lulucf <- df.compare.lulucf %>% mutate(base_netemissions_mtco2e = fossil_CO2_MtCO2+NonCO2_MtCO2e+LULUCF_MtCO2)

## add NDC to new columns in lulucf compare dataframe 

df.compare.lulucf <- df.compare.lulucf %>% 
  mutate(NDCuncon_emissions_target_mtco2e = 
           case_when(
             iso3 == "IDN" & year == 2030 ~ NDC.uncon.idn,
             iso3 == "THA" & year == 2030 ~ NDC.uncon.tha,
             iso3 == "USA" & year == 2030 ~ NDC.lower.US,
             iso3 == "MYS" & year == 2030 ~ NDC.MYS.lulucf, # replace MYS NDC with net emissions adjusted figure
             .default = NA)
  )
df.compare.lulucf <- df.compare.lulucf %>% 
  mutate(NDCcon_emissions_target_mtco2e = 
           case_when(
             iso3 == "IDN" & year == 2030 ~ NDC.con.idn,
             iso3 == "THA" & year == 2030 ~ NDC.con.tha,
             iso3 == "USA" & year == 2030 ~ NDC.upper.US,
             iso3 == "MYS" & year == 2030 ~ NDC.MYS.lulucf.con,
             .default = NA)
  )

## rebase NDC to total emissions excl. LULUCF baseline

### IDN [31.89% - 43.20% below BAU]
NDC.rebase2.idn.uncon <- df.compare.lulucf[df.compare.lulucf$iso3 == "IDN" & df.compare.lulucf$year == 2030, # rebase uncon NDC to CERc BAU in 2030
                                    "base_netemissions_mtco2e"]*(1-.3189)
NDC.rebase2.idn.con <- df.compare.lulucf[df.compare.lulucf$iso3 == "IDN" & df.compare.lulucf$year == 2030, # rebase con NDC to CERc BAU in 2030
                                  "base_netemissions_mtco2e"]*(1-.4320)

### THA [30% - 40% below BAU]
NDC.rebase2.tha.uncon <- df.compare.lulucf[df.compare.lulucf$iso3 == "THA" & df.compare.lulucf$year == 2030, # rebase uncon NDC to CERc BAU in 2030
                                    "base_netemissions_mtco2e"]*(1-.30)
NDC.rebase2.tha.con <- df.compare.lulucf[df.compare.lulucf$iso3 == "THA" & df.compare.lulucf$year == 2030, # rebase con NDC to CERc BAU in 2030
                                  "base_netemissions_mtco2e"]*(1-.40)

### USA [30% - 40% below BAU]
NDC.rebase2.usa.lower <- df.compare.lulucf[df.compare.lulucf$iso3 == "USA" & df.compare.lulucf$year == 2030, # rebase lower NDC to CERc BAU in 2030
                                    "base_netemissions_mtco2e"]*(1-.50)
NDC.rebase2.usa.upper <- df.compare.lulucf[df.compare.lulucf$iso3 == "USA" & df.compare.lulucf$year == 2030, # rebase upper NDC to CERc BAU in 2030
                                    "base_netemissions_mtco2e"]*(1-.52)

### MYS [45% below 2005 EI]
#### rebase EI2005 to CERc emissions baseline
EI2005cerc.lulucf <- df.compare.lulucf[df.compare.lulucf$iso3 == "MYS" & df.compare.lulucf$year == 2005, "base_netemissions_mtco2e"]/df.compare.lulucf[df.compare.lulucf$iso3 == "MYS" & df.compare.lulucf$year == 2005, "gdp_blnUSDMER"]
EI2030cerc.lulucf <- EI_Ef(EI2005cerc.lulucf, -0.45) # find emissions intensity based on CERC 2005 figures

NDC.rebase2.mys <- EI2030cerc.lulucf*df.compare.lulucf[df.compare.lulucf$iso3 == "MYS" & df.compare.lulucf$year == 2030, "gdp_blnUSDMER"]

## add column with rebased NDC figures

df.compare.lulucf <- df.compare.lulucf %>% 
  mutate(rb2_NDCuncon_mtco2e = 
           case_when(
             iso3 == "IDN" & year == 2030 ~ NDC.rebase2.idn.uncon,
             iso3 == "THA" & year == 2030 ~ NDC.rebase2.tha.uncon,
             iso3 == "USA" & year == 2030 ~ NDC.rebase2.usa.lower,
             iso3 == "MYS" & year == 2030 ~ NDC.rebase2.mys,
             .default = NA)
  )
df.compare.lulucf <- df.compare.lulucf %>% 
  mutate(rb2_NDCcon_mtco2e = 
           case_when(
             iso3 == "IDN" & year == 2030 ~ NDC.rebase2.idn.con,
             iso3 == "THA" & year == 2030 ~ NDC.rebase2.tha.con,
             iso3 == "USA" & year == 2030 ~ NDC.rebase2.usa.upper,
             .default = NA)
  )

## --- plot preview --- ##

#fs.plot.wlulucf  
  ggplot(df.compare.lulucf) +
  geom_ribbon(aes(x = year, ymin = allocation_MtCO2, ymax = base_netemissions_mtco2e), 
              fill="#ff6718", alpha=0.4) +
  geom_path(aes(x = year, y = base_netemissions_mtco2e),
            colour = "grey",
            size = 1.5) +
  geom_path(aes(x = year, y = allocation_MtCO2),
            colour = "#ff6718",
            size = 1.5) +
  geom_point(aes(x = year, y = NDCuncon_emissions_target_mtco2e),
             colour = "#004678",
             size = 2) +
  geom_point(aes(x = year, y = NDCcon_emissions_target_mtco2e),
             colour = "#AFDAFF",
             size = 2) +
  geom_point(aes(x = year, y = rb2_NDCuncon_mtco2e),
             colour = "#BF9000",
             size = 2) +
  geom_point(aes(x = year, y = rb2_NDCcon_mtco2e),
             colour = "#FFC000",
             size = 2) +
  scale_x_continuous(limit = c(1990,2030)) +
  facet_wrap(iso3 ~ ., scales="free") +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.text.x = element_text(vjust = -.75),
        axis.line.x = element_line(linewidth = .75),
        axis.ticks.x = element_line(linewidth = .75),
        axis.ticks.y = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold"),
        text = element_text(size = 16,
                            color = "black"))

### Plot without rebase
## --- plot preview --- ##

ggplot(df.compare.lulucf) +
  geom_ribbon(aes(x = year, ymin = allocation_MtCO2, ymax = base_netemissions_mtco2e), 
              fill="#ff6718", alpha=0.4) +
  geom_path(aes(x = year, y = base_netemissions_mtco2e),
            colour = "grey",
            size = 1.2) +
  geom_path(aes(x = year, y = allocation_MtCO2),
            colour = "#ff6718",
            size = 1.2) +
  geom_point(aes(x = year, y = NDCuncon_emissions_target_mtco2e),
             colour = "#004678",
             size = 2) +
  geom_point(aes(x = year, y = NDCcon_emissions_target_mtco2e),
             colour = "#AFDAFF",
             size = 2) +
  scale_x_continuous(limit = c(1990,2030)) +
  facet_wrap(iso3 ~ ., scales="free") +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text.x = element_text(vjust = -.75, 
                                   color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.line.x = element_line(linewidth = .5),
        axis.ticks.x = element_line(linewidth = .5),
        axis.ticks.y = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold"),
        text = element_text(size = 10,
                            color = "black"))
ggsave("var_scale_fs_facetchart_wllcf.svg", device = "svg", width = 14.77, height = 10.14, unit = "cm")

ggplot(df.compare) +
  geom_ribbon(aes(x = year, ymin = allocation_MtCO2, ymax = base_emissions_mtco2e), 
              fill="#ff6718", alpha=0.4) +
  geom_path(aes(x = year, y = base_emissions_mtco2e),
            colour = "grey",
            size = 1.2) +
  geom_path(aes(x = year, y = allocation_MtCO2),
            colour = "#ff6718",
            size = 1.2) +
  geom_point(aes(x = year, y = NDCuncon_emissions_target_mtco2e),
             colour = "#004678",
             size = 2) +
  geom_point(aes(x = year, y = NDCcon_emissions_target_mtco2e),
             colour = "#AFDAFF",
             size = 2) +
  scale_x_continuous(limit = c(1990,2030)) +
  facet_wrap(iso3 ~ ., scales="free") +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text.x = element_text(vjust = -.75, 
                                   color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.line.x = element_line(linewidth = .5),
        axis.ticks.x = element_line(linewidth = .5),
        axis.ticks.y = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold"),
        text = element_text(size = 10,
                            color = "black"))
ggsave("var_scale_fs_facetchart_wllcf.svg", device = "svg", width = 14.77, height = 10.14, unit = "cm")
  
## --- pipeline to d3 plotting --- ##
## clean up df.compare to retain important

plot.columns <- c("iso3","year","base_emissions_mtco2e","allocation_MtCO2",
                  "NDCuncon_emissions_target_mtco2e","NDCcon_emissions_target_mtco2e",
                  "rb1_NDCuncon_mtco2e","rb1_NDCcon_mtco2e")

df.rb1.toplot <- df.compare[plot.columns]

# fix class of year var to date form
df.rb1.toplot[,"year"] <- ISOdate(df.rb1.toplot[,"year"], 1, 1)
df.rb1.toplot[,"year"] <- as.Date(df.rb1.toplot[,"year"]) 
str(df.rb1.toplot)

write.csv(df.rb1.toplot, "fairshare_df_toplot.csv",
          na = "",
          row.names = FALSE,
          col.names = FALSE,
          append = TRUE,
          sep = ",")

## ------------------ ##

## --- output reduction levels for fair share and NDCs --- ##

# total emissions excl. LULUCF baseline for 2030
## fair share reductions
fs.rdxn.idn <- df.compare[df.compare$iso3 == "IDN" & df.compare$year == "2030", "base_emissions_mtco2e"] - 
  df.compare[df.compare$iso3 == "IDN" & df.compare$year == "2030", "allocation_MtCO2"]
fs.rdxn.tha <- df.compare[df.compare$iso3 == "THA" & df.compare$year == "2030", "base_emissions_mtco2e"] - 
  df.compare[df.compare$iso3 == "THA" & df.compare$year == "2030", "allocation_MtCO2"]
fs.rdxn.usa <- df.compare[df.compare$iso3 == "USA" & df.compare$year == "2030", "base_emissions_mtco2e"] - 
  df.compare[df.compare$iso3 == "USA" & df.compare$year == "2030", "allocation_MtCO2"]
fs.rdxn.mys <- df.compare[df.compare$iso3 == "MYS" & df.compare$year == "2030", "base_emissions_mtco2e"] - 
  df.compare[df.compare$iso3 == "MYS" & df.compare$year == "2030", "allocation_MtCO2"]

##* recall NDC reductions calculations from above
##* use rebase 1

NDC.rdxn.uncon.rebase1.idn <- df.compare[df.compare$iso3 == "IDN" & df.compare$year == "2030", "base_emissions_mtco2e"] - NDC.rebase1.idn.uncon 
NDC.rdxn.con.rebase1.idn <- df.compare[df.compare$iso3 == "IDN" & df.compare$year == "2030", "base_emissions_mtco2e"] - NDC.rebase1.idn.con
NDC.rdxn.uncon.rebase1.tha <- df.compare[df.compare$iso3 == "THA" & df.compare$year == "2030", "base_emissions_mtco2e"] - NDC.rebase1.tha.uncon
NDC.rdxn.con.rebase1.tha <- df.compare[df.compare$iso3 == "THA" & df.compare$year == "2030", "base_emissions_mtco2e"] - NDC.rebase1.tha.con
NDC.rdxn.lower.rebase1.usa <- df.compare[df.compare$iso3 == "USA" & df.compare$year == "2030", "base_emissions_mtco2e"] - NDC.rebase1.usa.lower
NDC.rdxn.upper.rebase1.usa <- df.compare[df.compare$iso3 == "USA" & df.compare$year == "2030", "base_emissions_mtco2e"] - NDC.rebase1.usa.upper
NDC.rdxn.rebase1.mys <- df.compare[df.compare$iso3 == "MYS" & df.compare$year == "2030", "base_emissions_mtco2e"] - NDC.rebase1.mys

# output dataframe of NDC - rebase1 - fair share comparisons
fairshare.output <- data.frame(country  = c("IDN", "THA", "USA","MYS"),
                 NDC.emi.rdxn.uncon = c(NDC.rdxn.uncon.idn, NDC.rdxn.uncon.tha, NDC.rdxn.lower.US, NDC.rdxn.MYS),
                 NDC.emi.rdxn.con = c(NDC.rdxn.con.idn, NDC.rdxn.con.tha, NDC.rdxn.upper.US, NDC.rdxn.MYS),
                 NDC.rebase.uncon = c(NDC.rdxn.uncon.rebase1.idn,NDC.rdxn.uncon.rebase1.tha,NDC.rdxn.lower.rebase1.usa,NDC.rdxn.rebase1.mys),
                 NDC.rebase.con = c(NDC.rdxn.con.rebase1.idn,NDC.rdxn.con.rebase1.tha,NDC.rdxn.upper.rebase1.usa,NA),
                 fairshare.rdxn = c(fs.rdxn.idn, fs.rdxn.tha, fs.rdxn.usa, fs.rdxn.mys)
                 )

## total emissions incl. LULUCF baseline for 2030
fs.rdxn.idn.lulucf <- df.compare.lulucf[df.compare.lulucf$iso3 == "IDN" & df.compare.lulucf$year == "2030", "base_netemissions_mtco2e"] - 
  df.compare.lulucf[df.compare.lulucf$iso3 == "IDN" & df.compare.lulucf$year == "2030", "allocation_MtCO2"]
fs.rdxn.tha.lulucf <- df.compare.lulucf[df.compare.lulucf$iso3 == "THA" & df.compare.lulucf$year == "2030", "base_netemissions_mtco2e"] - 
  df.compare.lulucf[df.compare.lulucf$iso3 == "THA" & df.compare.lulucf$year == "2030", "allocation_MtCO2"]
fs.rdxn.usa.lulucf <- df.compare.lulucf[df.compare.lulucf$iso3 == "USA" & df.compare.lulucf$year == "2030", "base_netemissions_mtco2e"] - 
  df.compare.lulucf[df.compare.lulucf$iso3 == "USA" & df.compare.lulucf$year == "2030", "allocation_MtCO2"]
fs.rdxn.mys.lulucf <- df.compare.lulucf[df.compare.lulucf$iso3 == "MYS" & df.compare.lulucf$year == "2030", "base_netemissions_mtco2e"] - 
  df.compare.lulucf[df.compare.lulucf$iso3 == "MYS" & df.compare.lulucf$year == "2030", "allocation_MtCO2"]

##* recall NDC reductions /w netted emissions calculations from above
##* BAU for IDN, THA, USA should be the same
##* use rebase2

NDC.rdxn.uncon.rebase2.idn <- df.compare.lulucf[df.compare.lulucf$iso3 == "IDN" & df.compare.lulucf$year == "2030", "base_netemissions_mtco2e"] - NDC.rebase2.idn.uncon 
NDC.rdxn.con.rebase2.idn <- df.compare.lulucf[df.compare.lulucf$iso3 == "IDN" & df.compare.lulucf$year == "2030", "base_netemissions_mtco2e"] - NDC.rebase2.idn.con 
NDC.rdxn.uncon.rebase2.tha <- df.compare.lulucf[df.compare.lulucf$iso3 == "THA" & df.compare.lulucf$year == "2030", "base_netemissions_mtco2e"] - NDC.rebase2.tha.uncon 
NDC.rdxn.con.rebase2.tha <- df.compare.lulucf[df.compare.lulucf$iso3 == "THA" & df.compare.lulucf$year == "2030", "base_netemissions_mtco2e"] - NDC.rebase2.tha.con 
NDC.rdxn.lower.rebase2.usa <- df.compare.lulucf[df.compare.lulucf$iso3 == "USA" & df.compare.lulucf$year == "2030", "base_netemissions_mtco2e"] - NDC.rebase2.usa.lower
NDC.rdxn.upper.rebase2.usa <- df.compare.lulucf[df.compare.lulucf$iso3 == "USA" & df.compare.lulucf$year == "2030", "base_netemissions_mtco2e"] - NDC.rebase2.usa.upper
NDC.rdxn.rebase2.mys <- df.compare.lulucf[df.compare.lulucf$iso3 == "MYS" & df.compare.lulucf$year == "2030", "base_netemissions_mtco2e"] - NDC.rebase2.mys

# output dataframe of NDC - rebase2 - fair share comparisons
fairshare.output.lulucf <- data.frame(country  = c("IDN", "THA", "USA","MYS"),
                               NDC.emi.rdxn.uncon = c(NDC.rdxn.uncon.idn, NDC.rdxn.uncon.tha, NDC.rdxn.lower.US, NDC.rdxn.MYS.lulucf),
                               NDC.emi.rdxn.con = c(NDC.rdxn.con.idn, NDC.rdxn.con.tha, NDC.rdxn.upper.US, NDC.rdxn.MYS.lulucf),
                               NDC.rebase.uncon = c(NDC.rdxn.uncon.rebase2.idn,NDC.rdxn.uncon.rebase2.tha,NDC.rdxn.lower.rebase2.usa,NDC.rdxn.rebase2.mys),
                               NDC.rebase.con = c(NDC.rdxn.con.rebase2.idn,NDC.rdxn.con.rebase2.tha,NDC.rdxn.upper.rebase2.usa,NA),
                                fairshare.rdxn = c(fs.rdxn.idn.lulucf, fs.rdxn.tha.lulucf, fs.rdxn.usa.lulucf, fs.rdxn.mys.lulucf)
                               )
## --- plot preview --- ##

# without lulucf

ggplot(fairshare.output) +
  geom_point(aes(x = country, y = NDC.emi.rdxn.uncon),
              colour = "#004678",
              shape = 18,
              size = 2.5) +
  geom_point(aes(x = country, y = NDC.emi.rdxn.con),
             colour = "#AFDAFF",
             shape = 18,
             size = 2.5) +
  geom_point(aes(x = country, y = NDC.rebase.uncon),
             colour = "#BF9000",
             shape = 18,
             size = 2.5) +
  geom_point(aes(x = country, y = NDC.rebase.con),
             colour = "#FFC000",
             shape = 18,
             size = 2.5) +
  geom_point(aes(x = country, y = fairshare.rdxn),
             colour = "red",
             shape = 4,
             stroke = 1,
             size = 2.5) +
  annotate("point", x = .7, y = 8000, shape = 18, size = 2.5, colour = "#004678") +
  annotate("text", x = .8, y = 8000, label = "NDC unconditional", size = 2.5, hjust = 0) +
  annotate("point", x = .7, y = 7600, shape = 18, size = 2.5, colour = "#AFDAFF") +
  annotate("text", x = .8, y = 7600, label = "NDC conditional", size = 2.5, hjust = 0) +
  annotate("point", x = .7, y = 7200, shape = 18, size = 2.5, colour = "#BF9000") +
  annotate("text", x = .8, y = 7200, label = "rebased NDC uncon.", size = 2.5, hjust = 0) +
  annotate("point", x = .7, y = 6800, shape = 18, size = 2.5, colour = "#FFC000") +
  annotate("text", x = .8, y = 6800, label = "rebased NDC con.", size = 2.5, hjust = 0) +
  annotate("point", x = .7, y = 6400, shape = 4, size = 2.5, colour = "red") +
  annotate("text", x = .8, y = 6400, label = "fair share", size = 2.5, hjust = 0) +
  theme(panel.background = element_blank(),
        panel.grid.major.y = element_line(linewidth = 0.5,
                                          colour = "lightgrey"),
        panel.grid.major.x = element_blank(),
        axis.line.x = element_line(linewidth = .5),
        axis.ticks.x = element_line(linewidth = .5),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(vjust = -.2),
        axis.title = element_blank(),
        text = element_text(family = "Arial",
                            size = 10,
                            color = "black"))
ggsave("fairshare.output.svg", 
       width = 7.45, height = 6.69, units = "cm", device='svg')

# with lulucf

ggplot(fairshare.output.lulucf) +
  geom_point(aes(x = country, y = NDC.emi.rdxn.uncon),
             colour = "#004678",
             shape = 18,
             size = 2.5,) +
  geom_point(aes(x = country, y = NDC.emi.rdxn.con),
             colour = "#AFDAFF",
             shape = 18,
             size = 2.5) +
  geom_point(aes(x = country, y = NDC.rebase.uncon),
             colour = "#BF9000",
             shape = 18,
             size = 2.5) +
  geom_point(aes(x = country, y = NDC.rebase.con),
             colour = "#FFC000",
             shape = 18,
             size = 2.5) +
  geom_point(aes(x = country, y = fairshare.rdxn),
             colour = "red",
             shape = 4,
             stroke = 1,
             size = 2.5) +
  annotate("point", x = .7, y = 8000, shape = 18, size = 2.5, colour = "#004678") +
  annotate("text", x = .8, y = 8000, label = "NDC unconditional", size = 2.5, hjust = 0) +
  annotate("point", x = .7, y = 7600, shape = 18, size = 2.5, colour = "#AFDAFF") +
  annotate("text", x = .8, y = 7600, label = "NDC conditional", size = 2.5, hjust = 0) +
  annotate("point", x = .7, y = 7200, shape = 18, size = 2.5, colour = "#BF9000") +
  annotate("text", x = .8, y = 7200, label = "rebased NDC uncon.", size = 2.5, hjust = 0) +
  annotate("point", x = .7, y = 6800, shape = 18, size = 2.5, colour = "#FFC000") +
  annotate("text", x = .8, y = 6800, label = "rebased NDC con.", size = 2.5, hjust = 0) +
  annotate("point", x = .7, y = 6400, shape = 4, size = 2.5, colour = "red") +
  annotate("text", x = .8, y = 6400, label = "fair share", size = 2.5, hjust = 0) +
  scale_y_continuous(limit = c(0, 8000)) +
  theme(panel.background = element_blank(),
        panel.grid.major.y = element_line(linewidth = 0.5,
                                          colour = "lightgrey"),
        panel.grid.major.x = element_blank(),
        axis.line.x = element_line(linewidth = .5),
        axis.ticks.x = element_line(linewidth = .5),
        axis.ticks.y = element_blank(),
        axis.text.x = element_text(vjust = -.2),
        axis.title = element_blank(),
        text = element_text(family = "Arial",
                            size = 10,
                            color = "black"))
ggsave("fairshare.output.lulucf.svg", 
       width = 7.45, height = 6.69, units = "cm", device='svg')
  
## ------------------ ##
## --- fair share allocation comparison of the world --- ##
##* this analysis is performed to text the CERF model for grandfathering
##* we can compare this with per capita model *(see Fanning and Hickel, 2023)
##* we do not consider NDC of each country in this analysis

# fix the variable class for df.sliced

df.sliced[,c(3:21)] <- sapply(df.sliced[,c(3:21)], as.numeric)
str(df.sliced)

## create fair share reduction variable
##* fair share reduction = baseline emissions - emissions allocation 

df.sliced.wfs <- df.sliced %>%  #create baseline emissions
                mutate(base_emissions_mtco2e = .$fossil_CO2_MtCO2 + .$NonCO2_MtCO2e)
df.sliced.wfs <- df.sliced.wfs %>% # create fair share rdxn column
                mutate(fs_rdxn_mtco2e = .$base_emissions_mtco2e - .$allocation_MtCO2) 

## filter df.sliced to 2030 of all countries and all region

unique(df.sliced$iso3)
get.region <- 
  c("WORLD", "HIGH_INCOME", "UPPER_MID_INCOME", "LOWER_MID_INCOME", "LOW_INCOME", "ANNEX_1", "ANNEX_2", 
    "NON_ANNEX_1", "EIT", "LDC", "EU15", "EU13", "EU28", "EU27", "OECD_NA", "OECD", "OECD_EUROPE", "OECD_PACIFIC", 
    "EE_EURASIA", "ASIA", "AFRICA", "MIDDLE_EAST", "LATIN_AMERICA", "NON_OECD", "ASEAN", "CPA", "SSA",                     
    "NAMER", "WEU", "MAF", "PAS", "OECD90", "MEA", "JPAUNZ", "SAS", "OECD90EU", "") # get name of region to filter

df.countries <- df.sliced.wfs[df.sliced.wfs$year == "2030" & !(df.sliced.wfs$iso3 %in% get.region),]
unique(df.countries$country) # check if only countries remain in the dataframe

df.region <- df.sliced.wfs[df.sliced.wfs$year == "2030" & df.sliced.wfs$iso3 %in% get.region,]
unique(df.region$country) # check if only country groups remain in the dataframe

df.incomeclass <- df.sliced.wfs[df.sliced.wfs$year == "2030" & 
                                  df.sliced.wfs$iso3 %in% c("HIGH_INCOME", "UPPER_MID_INCOME", "LOWER_MID_INCOME", "LOW_INCOME"),]
  
df.annexes <- df.sliced.wfs[df.sliced.wfs$year == "2030" & 
                              df.sliced.wfs$iso3 %in% c("ANNEX_1", "ANNEX_2", "NON_ANNEX_1"),]

## --- plot preview --- ##

ggplot(df.countries) +
  geom_col(aes(x = forcats::fct_reorder(iso3, fs_rdxn_mtco2e), y = fs_rdxn_mtco2e),
           fill = "blue",
           color = "black") +
  geom_point(aes(x = forcats::fct_reorder(iso3, fs_rdxn_mtco2e), y = gdp_blnUSDPPP),
             color = "pink") +
  scale_y_continuous(expand = c(0, 0), sec.axis = sec_axis(trans = ~., name = "bln USD")) +
  theme(panel.background = element_blank(),
        axis.text.x = element_text(angle = 90))

#* Because of the high responsibility and capacity of a few countries, we assume the 
#* distribution of allocated burden/effort to be skewed. However, we check if the 
#* distribution is positively or negatively skewed. As higher share of burden should accrue
#* to the higher responsible (which are a few), we expect a fair burden sharing to have a
#* long right tail, with higher shares accruing to little. We also do a bivariate kernel density
#* plot to show the distribution between responsibility and their fair share parity. 

# plot distribution of fair share reduction
## import dependencies
library(ggdensity)
library(scales)  

options(scipen = 999)

ggplot(df.countries) +
  geom_density(aes(x = fs_rdxn_mtco2e)) +
  scale_x_continuous(expand = c(0, 0))
  
# plot bivariate distribution of fair shares and emissions

ggplot(df.countries, aes(x = fs_rdxn_mtco2e, y = base_emissions_mtco2e)) +
  geom_density2d() +
  geom_point(shape = 21) +
  theme(panel.background = element_blank())

ggplot(df.countries, aes(x = fs_rdxn_mtco2e, y = base_emissions_mtco2e)) + #log-transform to show details in 1 - 100 MtCO2e
  geom_density2d() +
  geom_point(shape = 21) +
  scale_x_continuous(trans = "log10") +
  scale_y_continuous(trans = "log10") +
  theme(panel.background = element_blank())

#* In a lot of ways, emissions curve behave like an income distribution, where a small percentage
#* emits (use carbon space) alot more than most. 
#* Following the lorenz method, we can imagine emissions as an income distribution, and fair 
#* share as an allocation of tax. A fair tax system would be progressive. We check this by finding
#* the gini coefficient. Where a more progressive allocation would be more concave than the 
#* normal lorenz curve. [Norregaard, 1990](https://www.oecd.org/tax/public-finance/35372059.pdf)

## --- lorenz method --- ##

# import dependencies
install.packages("ineq")
library(ineq)

# first we bin countries fair share allocations into deciles and plot
df.countries$fs_decile <- ntile(df.countries$fs_rdxn_mtco2e, 10) 

# plot preview

ggplot(df.countries) +
  geom_col(aes(x = fs_decile))

## ------------------ ##
## -- export to pptx device -- ##

pptx <- read_pptx()

pptx %>% 
  add_slide() %>% 
  # This first line puts it in as a static png image for comparison
  ph_with(fs.op.wlulucf, location = ph_location_type(type = "body")) %>% 
  add_slide() %>% 
  # This line puts in a shape object, which can be ungrouped and edited
  ph_with(rvg::dml(ggobj = fs.op.wlulucf),
          width = 4,
          height = 6, 
          location = ph_location_type(type = "body"))

print(pptx, "fs.op.wlulucf.pptx")

## --- for Azfar --- ##
  
  ggplot(df.compare[df.compare$iso3 == "MYS",]) +
  geom_path(aes(x = year, y = base_emissions_mtco2e),
            colour = "#ff6718",
            size = 1.5) +
  scale_x_continuous(limit = c(1990,2030)) +
  theme(panel.background = element_blank(),
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text.x = element_text(vjust = -.75),
        axis.line.x = element_line(linewidth = .75),
        axis.ticks.x = element_line(linewidth = .75, which = 'minor'),
        axis.ticks.y = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(face = "bold"),
        text = element_text(size = 16,
                            color = "black"))
ggsave("MYS_projected_emissions_for_azfar.svg", device = "svg", )

write.csv(df.compare.lulucf[df.compare.lulucf$iso3 == "MYS",], "fairshare_compare_df_lulucf.csv")
