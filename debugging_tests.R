
source("gcam_utils.R")

# this is a somewhat chaotic set of code from trying to understand why land allocation was inconsistent. 
# Focused on debugging get_gcam_land_alloc and parse_xml_inputs


leaf_test <- xml2::xml_find_all(land_roots[[2]],"//UnmanagedLandLeaf[@name='UnmanagedPasture_AusCstS']")
leaf_node <- leaf_test

new_leaf_data <- process_leaf(leaf_test,gcam_land_alloc)


name <- xml2::xml_attr(leaf_node,"name")
region <- get_region(leaf_node)

land_alloc <- get_leaf_land_alloc(leaf_node, name, region, gcam_land_alloc)

leaf_data <- xml2::as_list(leaf_node)  # convert leaf data from xml into something parseable in R

land_alloc_df <- parse_land_alloc(leaf_data[[1]])  # get the historical land allocation data from the xmls

land_alloc_df

name <- xml2::xml_attr(leaf_node,"name")
region <- get_region(leaf_node)


gcam_leaf_land_alloc <- get_gcam_land_alloc_by_leaf(leaf_region=region, leaf_name=name, gcam_alloc=gcam_land_alloc)

# find all years of overlap between modeled and historical land alloc and remove from historical
first_model_year <- gcam_leaf_land_alloc$year[1]
idx <- match(first_model_year, land_alloc_df$year)
land_alloc_df <- land_alloc_df[1:idx-1,]  # remove any land allocation from the historical that's covered by gcam database output

land_alloc_all_years <- rbind(land_alloc_df,gcam_leaf_land_alloc)
rownames(land_alloc_all_years) <- 1:length(land_alloc_all_years$year)
return(land_alloc_all_years)
# interpolate land allocation data - currently linear. Spline was being weird.
all_years <- seq(min(land_alloc_all_years$year), max(land_alloc_all_years$year), 1)
land_alloc_interp <- approx(land_alloc_all_years$year,land_alloc_all_years$value, xout=all_years, ties="ordered")
land_alloc_interp_df <- as.data.frame(land_alloc_interp)  # approx returns named list with names x and y
colnames(land_alloc_interp_df) <- c("year", "value")





gcam_land_alloc <- get_gcam_land_alloc(db_name="database_basexdb",
                                       gcam_dir="~/Dropbox/Research/gcam_projects/task3.1a/gcam_output/",
                                       scenario="Reference", read_from_file=FALSE)  # scenario is doing nothing when read_from_file is TRUE

gcam_land_alloc %>% filter(region=="Australia_NZ") %>% group_by(year) %>% summarise(tot=sum(value)) -> test
tail(test,20)

land_roots <- read_land_inputs_xml2(folder="~/Dropbox/Github/gcam-core/input/gcamdata/xml")

gcam_land_alloc <- get_gcam_land_alloc(db_name="database_basexdb",
                                       gcam_dir="~/Dropbox/Research/gcam_projects/task3.1a/gcam_output/",
                                       scenario="Reference", read_from_file=FALSE)  # scenario is doing nothing when read_from_file is TRUE

leaf_data <- process_xml_inputs(land_roots, gcam_land_alloc)

leaf_data %>% filter(landleaf=="UnmanagedPasture_AusCstS") -> test
tail(test,20)

leaf_data %>% group_by(region, year) %>% summarise(tot=sum(land_alloc)) -> reg_totals
all_regs <- unique(reg_totals$region)

gcam_land_alloc %>% group_by(region, year) %>% summarise(tot=sum(value)) -> reg_totals_mdrn


library(ggplot2)
i <- 1
first_idx <- i*9-8
curr_regions <- all_regs[first_idx:(i*9)]
#curr_regions <- all_regs[first_idx:length(all_regs)]
ggplot(data=filter(reg_totals_mdrn,region %in% curr_regions),aes(x=year,y=tot))+
  geom_line()+
  facet_wrap(~region,scales="free_y")+
  theme_classic() 
#->fig

ggsave(filename=paste0("regional_land_data_",i,".png"),plot=fig,width=10,height=6)


plot_data_emiss %>% group_by(scenario, year) %>% summarise(nbp=sum(tot_nbp)) -> world_totals




saveRDS(leaf_data,file="data/leaf_data_test.RDS")  # store for future use


all_leaves <- xml2::xml_find_all(land_roots[[i]],"//LandLeaf")
unmanaged_leaves <- xml2::xml_find_all(land_roots[[i]], "//UnmanagedLandLeaf")
all_leaves <- c(all_leaves, unmanaged_leaves)
leaf_count <- 0



# test climate output


library(hector)
library(dplyr)

rcp <- "ssp245"
scenario_file <- paste0("input/hector_",rcp,".ini")
ini_file <- system.file(scenario_file, package="hector")

core <- hector::newcore(ini_file, suppresslogging = FALSE)
hector::run(core,runtodate = 2100)
out <- hector::fetchvars(core,dates=1746:2100,vars=c(hector::GLOBAL_TAS(),hector::CONCENTRATIONS_CO2(),hector::NBP()))
out$scenario <- "baseline"
out <- filter(out,year <= 2050)

hector_base_data <- filter(out,year<=2050) %>%
  select(-c("units","scenario")) %>%
  tidyr::pivot_wider(names_from="variable",values_from = "value")
head(hector_base_data)



full_climate_data <- read.csv("~/Dropbox/Research/gcam_projects/task3.1a/landtest/data/climate_data_full_world_baseline_no-protected_2100.csv")
head(full_climate_data)
clim_data <- filter(full_climate_data,year<=2050) %>%
  select(-c("units","scenario","X")) %>%
  tidyr::pivot_wider(names_from="variable",values_from = "value")
head(clim_data)


core <- hector::newcore(ini_file, suppresslogging = FALSE)
setvar(core,1745:2050,"NBP_constrain",-1*clim_data$NBP_constrain,"Pg C/yr")
hector::run(core,runtodate = 2050)
out_full <- hector::fetchvars(core,dates=1746:2050,vars=c(hector::GLOBAL_TAS(),hector::CONCENTRATIONS_CO2(),hector::NBP_CONSTRAIN()))
out_full$scenario <- "negative-sign"

core <- hector::newcore(ini_file, suppresslogging = FALSE)
setvar(core,1745:2050,"NBP_constrain",clim_data$NBP_constrain,"Pg C/yr")
hector::run(core,runtodate = 2050)
out_same <- hector::fetchvars(core,dates=1746:2050,vars=c(hector::GLOBAL_TAS(),hector::CONCENTRATIONS_CO2(),hector::NBP_CONSTRAIN()))
out_same$scenario <- "same-sign"


core <- hector::newcore(ini_file, suppresslogging = FALSE)
#setvar(core,1745:2050,"NBP_constrain",clim_data$NBP_constrain,"Pg C/yr")
setvar(core,1746:2050,"NBP_constrain",hector_base_data$NBP,"Pg C/yr")
hector::run(core,runtodate = 2050)
out_test <- hector::fetchvars(core,dates=1746:2050,vars=c(hector::GLOBAL_TAS(),hector::CONCENTRATIONS_CO2(),hector::NBP_CONSTRAIN()))
out_test$scenario <- "hector_base_rerun"

out_all <- dplyr::bind_rows(out,out_test,out_same,out_full)
#out_all <- dplyr::bind_rows(out_all,out_test)

#library(ggplot2)
ggplot(data=filter(out_all,year<=2050),aes(x=year,y=value,linetype=scenario))+
  geom_line()+
  facet_wrap(~variable,scales="free_y")+
  theme_classic() -> fig

ggsave(filename=paste0("climate_comp_all.png"),plot=fig,width=15,height=6)


library(ggplot2)
library(dplyr)
ggplot(data=filter(out,year<=2050,variable==NBP()),aes(x=year,y=value))+
  geom_line()+
  facet_wrap(~variable,scales="free_y")+
  theme_classic() -> fig

ggsave(filename=paste0("hector_data.png"),plot=fig,width=10,height=6)

fig

currTair <- dplyr::filter(out, variable == hector::GLOBAL_TAS())$value
currCO2 <- dplyr::filter(out, variable == hector::CONCENTRATIONS_CO2())$value
climate_data[1,(1:4) := list(year0,currTair,currCO2,currTair)]


setwd("~/Dropbox/Github/hector")


