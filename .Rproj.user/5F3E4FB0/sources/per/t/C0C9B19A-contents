# this code is migrated from earlier script fair share reanalysis code.R
# import dependencies

library(ggplot2)
library(magrittr)
library(dplyr)
library(forcats)
library(moments)
library(tidyr)

# data source should be in workspace and can be called from earlier script.

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

df.sliced.wfs <- slice(df.sliced.wfs, 1:(n() - 2)) # slice the bottom 2 rows 

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

df.big6 <- df.sliced.wfs[df.sliced.wfs$year == "2030" & 
                           df.sliced.wfs$iso3 %in% big6,]

# map on cumulative emissions
df.countries$cum_base_emissions_mtco2e <- 
  cum_base_emissions$cum_base_emissions_mtco2e[match(df.countries$iso3, cum_base_emissions$iso3)]

df.region$cum_base_emissions_mtco2e <- 
  cum_base_emissions$cum_base_emissions_mtco2e[match(df.region$iso3, cum_base_emissions$iso3)]

df.big6$cum_base_emissions_mtco2e <- 
  cum_base_emissions$cum_base_emissions_mtco2e[match(df.big6$iso3, cum_base_emissions$iso3)]

# as proxy of absolute responsibility, we look at cumulative emissions until 2030
#* relative responsibility = country cum emissions/sum of global cum. emissions
## create cumulative emissions variable

cum_base_emissions <- aggregate(df.sliced.wfs$base_emissions_mtco2e, 
                                by = list(df.sliced.wfs$iso3), FUN=sum)
colnames(cum_base_emissions)[1] <- "iso3"
colnames(cum_base_emissions)[2] <- "cum_base_emissions_mtco2e"

#* to understand current capacity compared against projected effort, we look at current rather than
#* projected economic size, for this we take 2019 USD PPP as a proxy.
## extract 2019 GDP into new df

cum_base_emissions$GDP_blnUSDPPP <- df.sliced.wfs[match(df.sliced.wfs$iso3, cum_base_emissions$iso3) & df.sliced.wfs$year == 2019, "gdp_blnUSDPPP"]

## -------------------- ##

## Step 1 ##
# Test grandfathering

## --- plot preview --- ##

ggplot(df.countries) +
  geom_col(aes(x = forcats::fct_reorder(iso3, cum_base_emissions_mtco2e), y = cum_base_emissions_mtco2e),
           fill = "#004876",
           color = "black") +
  geom_point(aes(x = forcats::fct_reorder(iso3, cum_base_emissions_mtco2e), y = gdp_blnUSDPPP),
             color = "#ff8674") +
  scale_y_continuous(trans = scales::modulus_trans(p= .5), 
                     expand = c(0, 0), 
                     sec.axis = sec_axis(trans = ~., name = "bln USD")) +
  scale_x_discrete(guide = guide_axis(check.overlap = F))+
  theme(panel.background = element_blank(),
        axis.text.x = element_text(angle = 90, 
                                   size = 5))


ggplot(df.countries, aes(x = gdp_blnUSDPPP, y = base_emissions_mtco2e)) +
  geom_density2d() +
  geom_point(shape = 21) +
  theme(panel.background = element_blank())

# plot bivariate point graph

ggplot(df.countries, aes(x = cum_base_emissions_mtco2e, y = gdp_blnUSDPPP, label = iso3)) +
  geom_point(shape = 21) +
  geom_smooth(method = "lm",
              linewidth = .6,
              color = "#ff8674") +
  geom_text(check_overlap = T, 
            size = 3,
            hjust = -.3) +
  theme(panel.background = element_blank(),
        )

# check bar plot of fair share redxn // gdp per cap

ggplot(df.countries) +
  geom_col(aes(x = forcats::fct_reorder(iso3, fs_rdxn_mtco2e), y = fs_rdxn_mtco2e),
           fill = "#004876",
           color = "black") +
  geom_point(aes(x = forcats::fct_reorder(iso3, fs_rdxn_mtco2e), y = gdp_blnUSDPPP),
             color = "#ff8674") +
  scale_y_continuous(trans = scales::modulus_trans(p= .5), 
                     expand = c(Inf, 0), 
                     limits = c(0, 35000),
                     sec.axis = sec_axis(trans = ~., name = "bln USD")) +
  theme(panel.background = element_blank(),
        axis.text.x = element_text(angle = 90))

# Show distribution curve of baseline emissions with distribution curve of allocation

ggplot(df.countries) +
  geom_density(aes(x = fs_rdxn_mtco2e),
               color = "#004876",
               linewidth = .8) +
  geom_density(aes(x = cum_base_emissions_mtco2e),
               color = "#ff8674",
               linewidth = .8) +
  scale_x_continuous(trans = scales::modulus_trans(p= .25), 
                     breaks = breaks_extended(10),
                     expand = c(Inf, 0)) +
  theme(panel.background = element_blank(),
        axis.line.x = element_line(),
        axis.text.x = element_text(angle = 90),
        axis.title = element_blank(),
        axis.ticks.y = element_blank())

#* Because of the high responsibility and capacity of a few countries, we assume the 
#* distribution of allocated burden/effort to be skewed. However, we check if the 
#* distribution is positively or negatively skewed. As higher share of burden should accrue
#* to the higher responsible (which are a few), we expect a fair burden sharing to have a
#* long right tail, with higher shares accruing to little. We also do a bivariate kernel density
#* plot to show the distribution between responsibility and their fair share parity. 

# plot distribution of fair share reduction
## import dependencies
library(ggdensity)
library(ggside)
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
  geom_xsidedensity() +
  geom_ysidedensity() +
  geom_point(shape = 21) +
  scale_x_continuous(trans = modulus_trans(.25)) +
  scale_y_continuous(trans = modulus_trans(.25)) +
  theme(panel.background = element_blank(),
        panel.border = element_rect(fill = NA))

## --- Redistributive power of fair share framework --- ##
#* In a lot of ways, country emissions distribution behave like an income distribution, where a small percentage
#* of individuals and countries emit (earn) a lot more than most. 
#* Following the lorenz method, we can imagine emissions as an income distribution, and fair 
#* share as a tax. A fair tax system would be progressive, and allocate more burden on those who have appropriated more carbon space.
#* We check this by finding the gini coefficient. Where a more progressive allocation would be more concave than the 
#* normal lorenz curve. [Norregaard, 1990](https://www.oecd.org/tax/public-finance/35372059.pdf). 

## --- lorenz method --- ##

# import dependencies
install.packages("ineq")
install.packages("gglorenz")
library(ineq)
library(gglorenz)

# we plot lorenz curve of the fair share allocations
plot(Lc(df.countries$base_emissions_mtco2e)) # pre-plot
plot(Lc(df.countries$fs_rdxn_mtco2e))
plot(Lc(df.countries$allocation_MtCO2))


## checking between emissions baseline (abs emission) and fair share allocation (tax)
ggplot(df.countries) +
  stat_lorenz(aes(x = fs_rdxn_mtco2e),
              color = "blue") +
  stat_lorenz(aes(x = base_emissions_mtco2e),
              color = "red") +
  annotate_ineq(df.countries$fs_rdxn_mtco2e) +
  annotate_ineq(df.countries$base_emissions_mtco2e,
                y = 0.9) +
  geom_abline() +
  theme(panel.background = element_blank())

## checking between pre-tax and post-tax emissions
ggplot(df.countries) +
  stat_lorenz_generalized(aes(x = allocation_MtCO2),
                          color = "darkgreen") +
  stat_lorenz(aes(x = base_emissions_mtco2e),
              color = "red") +
  annotate_ineq(df.countries$allocation_MtCO2) +
  annotate_ineq(df.countries$base_emissions_mtco2e,
                y = 0.9) +
  geom_abline() +
  theme(panel.background = element_blank())

#* However, unlike a tax system whose purpose is to redistribution and equalise, we might expect recompensation in 
#* an effort-sharing system, where emissions of some countries are allowed to grow. CERF based this on the capabilities of a
#* country, defined as the income share that is above a set development threshold. We use an annual income of 7500 USD PPP as
#* a static development threshold, we can calibrate the progressivity by staggeringly include income above this threshold to
#* contribute to mitigation but in this analysis we do not.

# plot allocation graph (post-allocation emissions allowance)

ggplot(df.countries) +
  geom_col(aes(x = forcats::fct_reorder(iso3, allocation_MtCO2), y = allocation_MtCO2),
           fill = "#004678",
           color = "black") +
  geom_point(aes(x = forcats::fct_reorder(iso3, allocation_MtCO2), y = pop_mln_above_dl),
             color = "limegreen") +
  scale_y_continuous(trans = scales::modulus_trans(p= .5), 
                     sec.axis = sec_axis(trans = ~., name = "mln ppl")) +
  scale_x_discrete(guide = guide_axis(check.overlap = F))+
  theme(panel.background = element_blank(),
        axis.text.x = element_text(angle = 90, size = 5))

# checking the proportion of population income share to see if China is unfairly exempted

ggplot(df.countries, aes(x = forcats::fct_reorder(iso3, 1-(pop_mln_above_dl/pop_mln)), 
                         y = 1-(pop_mln_above_dl/pop_mln))) +
  geom_point(color = "#004876") +
  geom_smooth() +
  scale_x_discrete(guide = guide_axis(check.overlap = F)) +
  theme(panel.background = element_blank(),
        axis.text.x = element_text(angle = 90, size = 5))

# plot density curve to see distribution of countries and their needs

ggplot(df.countries, aes(x = 1-(pop_mln_above_dl/pop_mln))) +
  geom_density(color = "#004876",
               fill = "#8cbede",
               linewidth = 0.8) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 1.5)) +
  theme(panel.background = element_blank(),
        axis.line.x = element_line())

# find allocation per capita


ggplot(df.countries) +
  geom_point(aes(x = forcats::fct_reorder(iso3, allocation_MtCO2/pop_mln), y = allocation_MtCO2/pop_mln),
             color = "#004876") +
  scale_x_discrete(guide = guide_axis(check.overlap = F))+
  theme(panel.background = element_blank(),
        axis.text.x = element_text(angle = 90, size = 5))

# plot bivariate density for allocation per capita

ggplot(df.countries, aes(x = 1-(pop_mln_above_dl/pop_mln), y = allocation_MtCO2/pop_mln, label = iso3)) +
  geom_density2d(color = "#ff8674") +
  geom_point(shape = 21) +
  scale_y_continuous(trans = scales::modulus_trans(0.25)) +
  scale_x_continuous(expand = c(0,0)) +
  geom_text(check_overlap = T, 
            size = 3,
            hjust = -.3) +
  scale_y_continuous(trans = scales::modulus_trans(.0025)) +
  scale_x_continuous(n.breaks = 5) +
  theme(panel.background = element_blank(),
        axis.line.x = element_line())

# plot bivariate density for reduction per capita

ggplot(df.countries, aes(x = 1-(pop_mln_above_dl/pop_mln), y = fs_rdxn_mtco2e/pop_mln, label = iso3)) +
  geom_smooth(method = "lm", color = "#004876") +
  geom_point(shape = 21) +
  scale_y_continuous(trans = scales::modulus_trans(0.25)) +
  scale_x_continuous(expand = c(0,0)) +
  geom_text(check_overlap = T, 
            size = 3,
            hjust = -.3) +
  scale_y_continuous(trans = scales::modulus_trans(.0025)) +
  scale_x_continuous(n.breaks = 5) +
  theme(panel.background = element_blank(),
        axis.line.x = element_line())

plot(df.big6$pop_mln)

world.carbon.space <- df.sliced.wfs[df.sliced.wfs$iso3 == "WORLD" & df.sliced.wfs$year == 2030, "allocation_MtCO2"]

# we compare between the big-6 emitters () fair share reduction and allocation, taking into account their relative capabilities

big6 <- c("USA","EU27", "GBR", "CHK", "IND", "RUS")

df.big6.tolong <- df.big6[c("iso3", "allocation_MtCO2", "fs_rdxn_mtco2e")] #extract variable to pivot
df.long.big6 <- df.big6.tolong %>% 
                  pivot_longer(!iso3,
                               names_to = "variable",
                               values_to = "value")

# plot bar plot to show allocation and reduction size among big 6

ggplot(df.long.big6) +
  geom_col(aes(x = iso3, y = value, fill = variable),
           position = "dodge",
           width = .5) +
  geom_point(data = df.big6, aes(x = iso3, y = gdp_blnUSDPPP),
             color = "maroon",
             size = 2) +
  scale_fill_manual(values = c("#004768", "#ff8674")) +
  scale_y_continuous(sec.axis = sec_axis(trans = ~., name = "bln USD")) +
  geom_hline(yintercept = 0)+
  theme(panel.background = element_blank(),
        axis.text.x = element_text(angle = 90),
        axis.ticks = element_blank(),
        panel.grid.major.y = element_line(color = "lightgrey"))
    
# we check the relationship between cumulative emissions responsibility and reduction obligation

ggplot(df.big6, aes(x = fs_rdxn_mtco2e, y = cum_base_emissions_mtco2e, label = iso3)) +
  geom_smooth(method = "lm",
              color = "#004768") + 
  geom_point(shape = 20,
             size = 3,
             color = "#ff8674") +
  geom_text(hjust = "inward") +
  scale_x_continuous(expand = c(0,0)) +
  scale_y_continuous(expand = c(0,0)) +
  theme(panel.background = element_blank(),
        axis.text.x = element_text(angle = 90))




















