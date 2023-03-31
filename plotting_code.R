# plotting code. apologies in advance - lots of chaos here

library(ggplot2)


# process single leaf data when you have just run the code and have results stored in variable called output

# transform bg emissions to format able to be joined with other leaf data
bg_emiss_out <- data.frame(t(output[["bg_emiss"]]))
colnames(bg_emiss_out) <- row.names(output[["bg_emiss"]])
bg_emiss_out$year <- seq(year0,last_year)
bg_emiss_out <- bg_emiss_out %>% tidyr::pivot_longer(cols=-c("year"),names_to = "name", values_to="bg_emiss")
#bg_emiss_out$year <- output[["leaf_data"]]$year


# transform ag_emiss
ag_emiss_full <- data.frame(t(output[["ag_emiss"]]))
colnames(ag_emiss_full) <- row.names(output[["ag_emiss"]])
ag_emiss_full$year <- seq(year0,last_year)
ag_emiss_full <- ag_emiss_full %>% tidyr::pivot_longer(cols=-c("year"),names_to = "name", values_to="ag_emiss")

plot_data <- dplyr::left_join(output[["leaf_data"]], bg_emiss_out, by=c("year","name"))


plot_data <- left_join(plot_data, ag_emiss_full, by=c("year","name"))
plot_data$tot_nbp <- plot_data$ag_emiss + plot_data$bg_emiss
plot_data$npp_rh <- plot_data$NPP/plot_data$Rh

plot_data_long <- plot_data %>%
  tidyr::pivot_longer(cols=c("land_alloc","agCDensity","bgCDensity","agCarbon",
                             "bgCarbon","NPP","Rh","litter","bg_emiss","ag_emiss","tot_nbp", "npp_rh"),
                      names_to="variable",
                      values_to="value")

ggplot(data=plot_data_long,aes(x=year,y=value))+
  geom_line()+
  facet_wrap(~variable,scales="free_y")+
  theme_classic()

ggsave(filename="sample_leaf_emissions.png",plot=fig,width=8,height=8)


# PROCESS single leaf, 2 scenario data

# process fully coupled
bg_emiss_out <- data.frame(t(output_full[["bg_emiss"]]))
colnames(bg_emiss_out) <- row.names(output_full[["bg_emiss"]])
bg_emiss_out$year <- seq(year0,last_year)
bg_emiss_out <- bg_emiss_out %>% tidyr::pivot_longer(cols=-c("year"),names_to = "name", values_to="bg_emiss")
#bg_emiss_out$year <- output_full[["leaf_data"]]$year


# transform ag_emiss
ag_emiss_full <- data.frame(t(output_full[["ag_emiss"]]))
colnames(ag_emiss_full) <- row.names(output_full[["ag_emiss"]])
ag_emiss_full$year <- seq(year0,last_year)
ag_emiss_full <- ag_emiss_full %>% tidyr::pivot_longer(cols=-c("year"),names_to = "name", values_to="ag_emiss")

plot_data <- dplyr::left_join(output_full[["leaf_data"]], bg_emiss_out, by=c("year","name"))


plot_data <- left_join(plot_data, ag_emiss_full, by=c("year","name"))
plot_data$tot_nbp <- plot_data$ag_emiss + plot_data$bg_emiss
plot_data$npp_rh <- plot_data$NPP/plot_data$Rh

plot_data_long <- plot_data %>%
  tidyr::pivot_longer(cols=c("land_alloc","agCDensity","bgCDensity","agCarbon",
                             "bgCarbon","NPP","Rh","litter","bg_emiss","ag_emiss","tot_nbp", "npp_rh"),
                      names_to="variable",
                      values_to="value")

plot_data_long$scenario <- "fully_coupled"

# process base scenario
bg_emiss_out <- data.frame(t(output[["bg_emiss"]]))
colnames(bg_emiss_out) <- row.names(output[["bg_emiss"]])
bg_emiss_out$year <- seq(year0,last_year)
bg_emiss_out <- bg_emiss_out %>% tidyr::pivot_longer(cols=-c("year"),names_to = "name", values_to="bg_emiss")
#bg_emiss_out$year <- output[["leaf_data"]]$year


# transform ag_emiss
ag_emiss_full <- data.frame(t(output[["ag_emiss"]]))
colnames(ag_emiss_full) <- row.names(output[["ag_emiss"]])
ag_emiss_full$year <- seq(year0,last_year)
ag_emiss_full <- ag_emiss_full %>% tidyr::pivot_longer(cols=-c("year"),names_to = "name", values_to="ag_emiss")

plot_data_base <- dplyr::left_join(output[["leaf_data"]], bg_emiss_out, by=c("year","name"))


plot_data_base <- left_join(plot_data_base, ag_emiss_full, by=c("year","name"))
plot_data_base$tot_nbp <- plot_data_base$ag_emiss + plot_data_base$bg_emiss
plot_data_base$npp_rh <- plot_data_base$NPP/plot_data_base$Rh

plot_data_long_base <- plot_data_base %>%
  tidyr::pivot_longer(cols=c("land_alloc","agCDensity","bgCDensity","agCarbon",
                             "bgCarbon","NPP","Rh","litter","bg_emiss","ag_emiss","tot_nbp", "npp_rh"),
                      names_to="variable",
                      values_to="value")

plot_data_long_base$scenario <- "baseline"


plot_data_all <- dplyr::bind_rows(plot_data_long,plot_data_long_base)


ggplot(data=filter(plot_data_all,variable %in% c("tot_nbp","agCDensity","bgCDensity")),
       aes(x=year,y=value,linetype=scenario))+
  geom_line()+
  facet_wrap(~variable,scales="free_y")+
  theme_classic()

ggsave(filename="sample_leaf_emissions.png",plot=fig,width=8,height=8)


# plot sample of leaves

test_leaves <- sample(selected,30)
#"tot_nbp","agCDensity",
ggplot(data=filter(plot_data_all,name %in% test_leaves,variable %in% c("bgCDensity")),
       aes(x=year,y=value,linetype=scenario))+
  geom_line()+
  #facet_grid(name~variable,scales="free_y")+
  facet_wrap(~name,scales="free_y")+
  theme_classic()

ggsave(filename="sample_leaf_emissions.png",plot=fig,width=8,height=8)



# test for unusual/edge case leaves

full_leaf_data <- output_full[["leaf_data"]] %>% mutate(scenario="fully-coupled")
base_leaf_data <- output[["leaf_data"]] %>% mutate(scenario="baseline")
leaf_data_all <- dplyr::bind_rows(base_leaf_data, full_leaf_data)

leaf_data_all %>% dplyr::filter(year==2009,scenario=="fully-coupled") -> test_world
leaf_data_all %>% dplyr::filter(year==1746,scenario=="fully-coupled") -> test_world_pre

test_world %>% mutate(agCDensity_diff=agCDensity-test_world_pre$agCDensity,
                      bgCDensity_diff=bgCDensity-test_world_pre$bgCDensity,
                      agCDensity_pct=100*(agCDensity-test_world_pre$agCDensity)/test_world_pre$agCDensity,
                      bgCDensity_pct=100*(bgCDensity-test_world_pre$bgCDensity)/test_world_pre$bgCDensity,
) ->
  test_world

filter(test_world,test_world$bgCDensity_diff >= 5)  # has 13
filter(test_world,test_world$bgCDensity_pct >= 15) # has 94
filter(test_world,test_world$Rh >=50)



# process results when they need to be read in

# process full world data

full_bg_emiss <- read.csv("data/bg_emiss_full_world_baseline_no-protected.csv")  # not actually baseline, despite the name
row.names(full_bg_emiss) <- full_bg_emiss$X
full_bg_emiss <- dplyr::select(full_bg_emiss,-c("X"))
full_ag_emiss <- read.csv("data/ag_emiss_full_world_baseline_no-protected.csv")
row.names(full_ag_emiss) <- full_ag_emiss$X
full_ag_emiss <- dplyr::select(full_ag_emiss,-c("X"))

full_bg_emiss <- output[["bg_emiss"]]
full_ag_emiss <- output[["ag_emiss"]]


base_bg_emiss <- read.csv("data/bg_emiss_full_world_real-baseline_no-protected_2100.csv")
row.names(base_bg_emiss) <- base_bg_emiss$X
base_bg_emiss <- dplyr::select(base_bg_emiss,-c("X"))
base_ag_emiss <- read.csv("data/ag_emiss_full_world_real-baseline_no-protected_2100.csv")
row.names(base_ag_emiss) <- base_ag_emiss$X
base_ag_emiss <- dplyr::select(base_ag_emiss,-c("X"))

#full_leaf_data <- read.csv("data/leaf_data_full_world_coupled_no-protected.csv")
#full_leaf_params <- read.csv("data/leaf_params_full_world_coupled_no-protected.csv")
#full_climate_data <- read.csv("data/climate_data_full_world_coupled_no-protected.csv")
full_leaf_data <- output[["leaf_data"]]
full_climate <- output[["climate"]]

base_leaf_data <- read.csv("data/leaf_data_full_world_real-baseline_no-protected_2100.csv")  
base_leaf_params <- read.csv("data/leaf_params_full_world_real-baseline_no-protected_2100.csv")
base_climate_data <- read.csv("data/climate_data_full_world_real-baseline_no-protected_2100.csv")
full_climate_data <- read.csv("data/climate_data_full_world_baseline_no-protected_2100.csv")

base_climate_data$scenario <- "baseline"
full_climate_data$scenario <- "fully-coupled"
all_climate <- dplyr::bind_rows(base_climate_data,full_climate_data)

library(dplyr)
ggplot(data=dplyr::filter(all_climate,year<=2050),aes(x=year,y=value,linetype=scenario))+
  geom_line()+
  facet_wrap(~variable,scales="free_y")+
  theme_classic() -> fig
ggsave(filename=paste0("climate_data.png"),plot=fig,width=10,height=6)



head(full_leaf_data)

#full_leaf_data <- full_leaf_data %>% select(-c("X")) %>% mutate(scenario="fully-coupled")
full_leaf_data <- full_leaf_data %>% mutate(scenario="fully-coupled")
base_leaf_data <- base_leaf_data %>% select(-c("X")) %>% mutate(scenario="baseline")
leaf_data_all <- dplyr::bind_rows(base_leaf_data, full_leaf_data)



# transform base ag_emiss
ag_emiss_base <- data.frame(t(base_ag_emiss))
colnames(ag_emiss_base) <- row.names(output[["ag_emiss"]])
ag_emiss_base$year <- seq(year0,last_year)

ag_emiss_base <- ag_emiss_base %>% tidyr::pivot_longer(cols=-c("year"),names_to = "name", values_to="ag_emiss")
ag_emiss_base$scenario <- "baseline"

# transform ag_emiss
ag_emiss_full <- data.frame(t(full_ag_emiss))
colnames(ag_emiss_full) <- row.names(output[["ag_emiss"]])
ag_emiss_full$year <- seq(year0,last_year)

ag_emiss_full <- ag_emiss_full %>% tidyr::pivot_longer(cols=-c("year"),names_to = "name", values_to="ag_emiss")
ag_emiss_full$scenario <- "fully-coupled"


ag_emiss_all <- dplyr::bind_rows(ag_emiss_base, ag_emiss_full)
ag_emiss_plt <- dplyr::filter(ag_emiss_all, year <= 2099)

# transform bg_emiss
bg_emiss_base <- data.frame(t(base_bg_emiss))
colnames(bg_emiss_base) <- row.names(output[["bg_emiss"]])
bg_emiss_base$year <- seq(year0,last_year)

bg_emiss_base <- bg_emiss_base %>% tidyr::pivot_longer(cols=-c("year"),names_to = "name", values_to="bg_emiss")
#bg_emiss_base$year <- output[["leaf_data"]]$year
bg_emiss_base$scenario <- "baseline"


# transform bg_emiss
bg_emiss_full <- data.frame(t(full_bg_emiss))
colnames(bg_emiss_full) <- row.names(output[["bg_emiss"]])
bg_emiss_full$year <- seq(year0,last_year)

bg_emiss_full <- bg_emiss_full %>% tidyr::pivot_longer(cols=-c("year"),names_to = "name", values_to="bg_emiss")
#bg_emiss_full$year <- output[["leaf_data"]]$year
bg_emiss_full$scenario <- "fully-coupled"


bg_emiss_all <- dplyr::bind_rows(bg_emiss_base, bg_emiss_full)

# trim off post-2010 years for the sake of plotting
bg_emiss_plt <- dplyr::filter(bg_emiss_all, year <= 2099)


plot_data <- dplyr::left_join(leaf_data_all, bg_emiss_plt, by=c("year","name","scenario"))


plot_data <- left_join(plot_data, ag_emiss_plt, by=c("year","name", "scenario"))
plot_data$tot_nbp <- plot_data$ag_emiss + plot_data$bg_emiss

plot_data_2050 <- plot_data %>% dplyr::filter(year<=2050)



plot_data_emiss <- select(plot_data_2050,c("year","name","scenario","region", "tot_nbp"))


plot_data_emiss %>% group_by(region,scenario, year) %>% summarise(nbp=sum(tot_nbp)) -> reg_totals

plot_data_emiss %>% group_by(scenario, year) %>% summarise(nbp=sum(tot_nbp)) -> world_totals


plot_data_land <- select(plot_data,c("year","name","scenario","region", "land_alloc"))
plot_data_land %>% group_by(region,scenario, year) %>% summarise(land_alloc=sum(land_alloc)) -> reg_land_totals


gcp_data <- read.csv("extdata/nbp_gcp.csv")

gcp_data$scenario <- "Global Carbon Project"
#library(zoo)
gcp_data$nbp <- rollmean(gcp_data$nbp*1000,k=10,fill=NA)
world_total_gcp <- dplyr::bind_rows(world_totals,gcp_data)

world_total_gcp$scenario <- factor(world_total_gcp$scenario,
                                   levels=c("Global Carbon Project","baseline","fully-coupled"))

#library(ggplot2)
#library(ggsci)
ggplot(data=dplyr::filter(world_total_gcp,year<=2015),aes(x=year,y=nbp,colour=scenario))+
  geom_line(size=1.5)+
  scale_color_uchicago()+
  ylab("Net Biome Production (Mt C/yr") +
  xlab("Year")+
  theme_classic() +
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size=14)) -> fig

ggsave(filename="world_2015.png",plot=fig,width=8,height=3.5)


# make raw figure

gcp_data_raw <- read.csv("extdata/nbp_gcp.csv")
gcp_data_raw$nbp <- gcp_data_raw$nbp*1000

gcp_data_raw$scenario <- "Global Carbon Project"
world_total_gcp_raw <- dplyr::bind_rows(world_totals,gcp_data_raw)

world_total_gcp_raw$scenario <- factor(world_total_gcp_raw$scenario,
                                       levels=c("Global Carbon Project","baseline","fully-coupled"))

#library(ggplot2)
#library(ggsci)
ggplot(data=dplyr::filter(world_total_gcp_raw,year<=2015),aes(x=year,y=nbp,colour=scenario))+
  geom_line(size=1.5)+
  scale_color_uchicago()+
  ylab("Net Biome Production (Mt C/yr") +
  xlab("Year")+
  theme_classic() +
  theme(axis.title = element_text(size=14),
        axis.text = element_text(size=14)) -> fig

ggsave(filename="world_2015_raw.png",plot=fig,width=8,height=3.5)




all_regs <- unique(reg_totals$region)

for (i in 1:4){
  first_idx <- i*9-8
  curr_regions <- all_regs[first_idx:i*9]
  ggplot(data=filter(reg_totals,region %in% curr_regions),aes(x=year,y=nbp,linetype=scenario))+
    geom_line()+
    facet_wrap(~region,scales="free_y")+
    theme_classic() -> fig
  ggsave(filename=paste0("regional_data_",i,".png"),plot=fig,width=10,height=6)
  
}

az_data <- filter(plot_data_land,region=="Australia_NZ")
az_leaves <- unique(az_data$name)
test_leaves <- sample(az_leaves,30)
test_leaves <- grep("OtherArable", az_leaves, value=TRUE)
length(test_leaves)
#"tot_nbp","agCDensity",
ggplot(data=filter(az_data,name %in% test_leaves),
       aes(x=year,y=land_alloc,linetype=scenario))+
  geom_line()+
  #facet_grid(name~variable,scales="free_y")+
  facet_wrap(~name,scales="free_y")+
  theme_classic()

ggsave(filename="sample_leaf_land.png",plot=fig,width=10,height=10)


i <- 1
first_idx <- i*9-8
curr_regions <- all_regs[first_idx:(i*9)]
#curr_regions <- all_regs[first_idx:length(all_regs)]
ggplot(data=filter(reg_land_totals,region %in% curr_regions),aes(x=year,y=land_alloc,linetype=scenario))+
  geom_line()+
  facet_wrap(~region,scales="free_y")+
  theme_classic() ->fig

ggsave(filename=paste0("regional_land_data_",i,".png"),plot=fig,width=10,height=6)



i <- 4
first_idx <- i*9-8
#curr_regions <- all_regs[first_idx:(i*9)]
curr_regions <- all_regs[first_idx:length(all_regs)]
ggplot(data=filter(reg_totals,region %in% curr_regions),aes(x=year,y=nbp,linetype=scenario))+
  geom_line()+
  facet_wrap(~region,scales="free_y")+
  theme_classic()

ggsave(filename=paste0("regional_data_",i,".png"),plot=fig,width=10,height=6)

i <- 4
first_idx <- i*9-8
#curr_regions <- all_regs[first_idx:(i*9)]
curr_regions <- all_regs[first_idx:length(all_regs)]
ggplot(data=filter(reg_land_totals,region %in% curr_regions),aes(x=year,y=land_alloc,linetype=scenario))+
  geom_line()+
  facet_wrap(~region,scales="free_y")+
  theme_classic()



test_leaves <- sample(row.names(output[["ag_emiss"]]),500)
grep("Forest",test_leaves,value=TRUE)

sample_leaves <- c("China_Grassland_IndusR", "India_Shrubland_BrahmaniR",
                   "USA_CornC4_GreatBasin_RFD_hi", "South America_Northern_Wheat_SAmerCstNE_RFD_lo",
                   "Russia_Tundra_BalticSea", "Africa_Northern_UnmanagedPasture_MeditS",
                   "Brazil_UnmanagedForest_RioLaPlata", "Russia_Forest_KaraSea")

sample_leaf_data <- dplyr::filter(plot_data_2050,name %in% sample_leaves)

plot_data_long <- sample_leaf_data %>%
  tidyr::pivot_longer(cols=c("land_alloc","agCDensity","bgCDensity","agCarbon",
                             "bgCarbon","NPP","Rh","litter","bg_emiss","ag_emiss","tot_nbp"),
                      names_to="variable",
                      values_to="value")

sample_emiss_data <- plot_data_long %>% dplyr::filter(variable=="tot_nbp") %>% select(-c("variable"))
sample_density <- plot_data_long %>% dplyr::filter(variable %in% c("agCDensity","bgCDensity"))

sample_bgDensity <- plot_data_long %>% dplyr::filter(variable=="bgCDensity") %>% select(-c("variable"))
sample_agDensity <- plot_data_long %>% dplyr::filter(variable=="agCDensity") %>% select(-c("variable"))


ggplot(data=sample_emiss_data,aes(x=year,y=value,linetype=scenario))+
  geom_line()+
  ylab("Land Carbon Flux (Mt C/yr)")+
  xlab("Year")+
  facet_wrap(~name,scales="free_y",nrow=4)+
  theme_classic()->fig
ggsave(filename="sample_leaf_emissions.png",plot=fig,width=10,height=8)


ggplot(data=sample_agDensity,aes(x=year,y=value,linetype=scenario))+
  geom_line()+
  ylab("Vegetation Carbon Density")+
  xlab("Year")+
  facet_wrap(~name,scales="free_y",nrow=4)+
  theme_classic()->fig
ggsave(filename="sample_leaf_agCDensities.png",plot=fig,width=10,height=8)

ggplot(data=sample_bgDensity,aes(x=year,y=value,linetype=scenario))+
  geom_line()+
  ylab("Soil Carbon Density")+
  xlab("Year")+
  facet_wrap(~name,scales="free_y",nrow=4)+
  theme_classic()->fig
ggsave(filename="sample_leaf_bgCDensities.png",plot=fig,width=10,height=8)



ggplot(data=filter(sample_density,name %in% c("China_Grassland_IndusR", "India_Shrubland_BrahmaniR",
                                              "USA_CornC4_GreatBasin_RFD_hi", "South America_Northern_Wheat_SAmerCstNE_RFD_lo",
                                              "Russia_Tundra_BalticSea")),
       aes(x=year,y=value,linetype=scenario))+
  geom_line(size=1.5)+
  ylab("Carbon Density")+
  xlab("Year")+
  facet_grid(name~variable,scales="free_y")+
  theme_classic()->fig
ggsave(filename="sample_leaf_emissions.png",plot=fig,width=10,height=8)



ggplot(data=dplyr::filter(plot_data_long,name==sample_leaves[[2]]),aes(x=year,y=value))+
  geom_line()+
  facet_wrap(~variable,scales="free_y")+
  theme_classic()

ggsave(filename="sample_leaf_emissions.png",plot=fig,width=8,height=8)


debug_leaf_long <- debug_leaf %>%
  tidyr::pivot_longer(cols=c("land_alloc","agCDensity","bgCDensity","agCarbon",
                             "bgCarbon","NPP","Rh","litter","bg_emiss","ag_emiss","tot_nbp"),
                      names_to="variable",
                      values_to="value")

ggplot(data=debug_leaf_long,aes(x=year,y=value,linetype=scenario))+
  geom_line()+
  facet_wrap(~variable,scales="free_y")+
  theme_classic()



for (leaf in sample_leaves){
  ggplot(data=dplyr::filter(plot_data_long,name==leaf),aes(x=year,y=value,linetype=scenario))+
    geom_line()+
    facet_wrap(~variable,scales="free_y")+
    theme_classic() -> fig
  ggsave(filename=paste0("leaf_plot_",leaf,".png"),plot=fig,width=10,height=6)
  
}


-> fig

ggsave(filename="single_leaf_Africa_Eastern_Soybean_RiftValley_RFD_lo_test.png",plot=fig,width=10,height=6)





################### current plots above this line ######################

#plot_data_long <- plot_data_first %>% tidyr::pivot_longer(cols=c("land_alloc","tot_nbp"),names_to="variable",
#                                                    values_to="value")



plot_data_long <- plot_data %>% filter(name %in% leaf_set) %>% tidyr::pivot_longer(cols=c("land_alloc","agCDensity","bgCDensity","agCarbon",
                                                                                          "bgCarbon","NPP","Rh","litter","bg_emiss","ag_emiss","tot_nbp"),names_to="variable",
                                                                                   values_to="value")


all_leaves <- unique(output[["leaf_data"]]$name)

#leaf_set <- sample(all_leaves,500)

ggplot(data=dplyr::filter(plot_data_long,name %in% leaves),aes(x=year,y=value,linetype=scenario))+
  geom_line()+
  facet_grid(variable~name,scales="free_y")+
  theme_classic()

# select 10 sample land leaves to plot all variables for in 10 separate plots
ggplot(data=dplyr::filter(plot_data_long,name==leaf_set[[375]],year<=1850),aes(x=year,y=value,linetype=scenario))+
  geom_line()+
  facet_wrap(~variable,scales="free_y")+
  theme_classic()

-> fig

ggsave(filename="single_leaf_Africa_Eastern_Soybean_RiftValley_RFD_lo_test.png",plot=fig,width=10,height=6)


# select 6 sample land leaves to plot key variables for: land alloc, agDensity, bgDensity, agEmiss, bgEmiss

ggplot(data=dplyr::filter(plot_data,variable %in% c("land_alloc","tot_nbp")),aes(x=year,y=value,linetype=scenario))+
  geom_line()+
  facet_grid(variable~landleaf,scales="free_y")+
  theme_classic()


# select 6 sample land leaves to plot key variables for: land alloc, agEmiss, bgEmiss, totalEmiss

ggplot(data=dplyr::filter(plot_data,variable %in% c("agCDensity","bgCDensity","land_alloc")),aes(x=year,y=value))+
  geom_line()+
  facet_grid(variable~landleaf,scales="free_y")+
  theme_classic()


# aggregate emissions and plot global

# aggregate emissions and plot regional (3 sets of 8?)


# plot climate


############################################# Calculate unmanaged emissions

plot_data_emiss <- select(plot_data,c("year","name","scenario","region", "tot_nbp"))


# get managed only

all_leaves <- unique(plot_data_emiss$name)
managed_leaves <- grep("Pasture", all_leaves, value=TRUE)
rest <- grep("Pasture", all_leaves, value=TRUE,invert=TRUE)
length(rest)
rest <- grep("Unmanaged | Tundra | RockIceDesert",rest,value=TRUE,invert=TRUE)  # remove all explicitly "unmanaged" leaves from 'rest'
rest <- grep("RockIceDesert",rest,value=TRUE,invert=TRUE)  # remove all explicitly "unmanaged" leaves from 'rest'



# remaining are all forest, crops, shrubland, grassland, & pasture


managed_leaves <- c(managed_leaves,rest) # add in all remaining leaves

managed_data <- dplyr::filter(plot_data_emiss,name %in% managed_leaves)
unmgd_data <- dplyr::filter(plot_data_emiss,!(name %in% managed_leaves))

managed_data %>% group_by(region,scenario, year) %>% summarise(nbp=sum(tot_nbp)) -> reg_totals_mgd

managed_data %>% group_by(scenario, year) %>% summarise(nbp=sum(tot_nbp)) -> world_totals_mgd

unmgd_data %>% group_by(region,scenario, year) %>% summarise(nbp=sum(tot_nbp)) -> reg_totals_unmgd

unmgd_data %>% group_by(scenario, year) %>% summarise(nbp=sum(tot_nbp)) -> world_totals_unmgd

world_totals_unmgd <- world_totals_unmgd %>% dplyr::filter(scenario=="fully-coupled") %>%
  mutate(scenario="Unmanaged")

world_totals_all <- world_totals %>% dplyr::filter(scenario=="fully-coupled") %>%
  mutate(scenario="Unmanaged+Managed")

world_totals_mgd <- world_totals_mgd %>% dplyr::filter(scenario=="fully-coupled") %>%
  mutate(scenario="Managed")

world_totals_base <- world_totals %>% dplyr::filter(scenario=="baseline") %>%
  mutate(scenario="Baseline")



world_totals_compare <- dplyr::bind_rows(world_totals_base,world_totals_mgd,world_totals_all,world_totals_unmgd)


ggplot(data=world_totals_compare,aes(x=year,y=nbp,linetype=scenario))+
  geom_line()+
  ylab("LUC Emissions (Mt C/yr") +
  theme_classic() -> fig

ggsave(filename="world_2010_mgd_comp.png",plot=fig,width=10,height=6)

reg_totals_unmgd <- reg_totals_unmgd %>% dplyr::filter(scenario=="fully-coupled") %>%
  mutate(scenario="Unmanaged")

reg_totals_all <- reg_totals %>% dplyr::filter(scenario=="fully-coupled") %>%
  mutate(scenario="Unmanaged+Managed")

reg_totals_mgd <- reg_totals_mgd %>% dplyr::filter(scenario=="fully-coupled") %>%
  mutate(scenario="Managed")

reg_totals_base <- reg_totals %>% dplyr::filter(scenario=="baseline") %>%
  mutate(scenario="Baseline")

reg_totals_compare <- dplyr::bind_rows(reg_totals_base,reg_totals_mgd,reg_totals_all,reg_totals_unmgd)


i <- 4
first_idx <- i*9-8
#curr_regions <- all_regs[first_idx:(i*9)]
curr_regions <- all_regs[first_idx:length(all_regs)]
ggplot(data=filter(reg_totals_compare,region %in% curr_regions),aes(x=year,y=nbp,linetype=scenario))+
  geom_line()+
  facet_wrap(~region,scales="free_y")+
  theme_classic()->fig

ggsave(filename=paste0("regional_data_mgd",i,".png"),plot=fig,width=10,height=6)



plot_data_emiss %>% group_by(region,scenario, year) %>% summarise(nbp=sum(tot_nbp)) -> reg_totals

plot_data_emiss %>% group_by(scenario, year) %>% summarise(nbp=sum(tot_nbp)) -> world_totals



############################################

# *************** TESTING US DATA

# GETTING DEBUG DATA FOR US LEAF
dplyr::filter(plot_data,name=="USA_OtherArableLand_UsaPacNW") -> debug_leaf

dplyr::filter(full_leaf_params,name=="USA_OtherArableLand_UsaPacNW") -> debug_full_leaf_params
dplyr::filter(base_leaf_params,name=="USA_OtherArableLand_UsaPacNW") -> debug_base_leaf_params
debug_base_leaf_params
debug_full_leaf_params

us_data <- dplyr::filter(plot_data_emiss,region=="USA")

us_data %>%
  tidyr::pivot_wider(id_cols=c("year","name",
                               "scenario", "region","tot_nbp"),
                     names_from=scenario,
                     values_from = tot_nbp) %>%
  mutate(diff=`fully-coupled`-baseline) %>%
  dplyr::filter(year==1975) -> us_test

dplyr::filter(us_test,`fully-coupled`>=10)

all_leaves <- unique(us_data$name)

i <- 1
first_idx <- i*12-11
curr_leaves <- all_leaves[first_idx:(i*12)]
#curr_leaves <- all_leaves[first_idx:length(all_leaves)]

curr_leaves <- sample(all_leaves,50)

sample(all_leaves,50)

curr_leaves <- grep("Vegetables", all_leaves,value=TRUE)
#curr_leaves <- grep("UnmanagedPasture", curr_leaves,value=TRUE,invert=TRUE)
length(curr_leaves)
curr_leaves <- curr_leaves[1:25]
ggplot(data=dplyr::filter(us_data,name %in% curr_leaves),aes(x=year,y=tot_nbp,linetype=scenario))+
  geom_line()+
  facet_wrap(~name,scales="free_y")+
  theme_classic()
