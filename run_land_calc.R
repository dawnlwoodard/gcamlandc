# run world
setwd("/Users/wood663/Dropbox/Research/gcam_projects/task3.1a/landtest/")
source("gcam_utils.R")
source("land_utils.R")

# necessary inputs: 5 gcam land xmls + 2 protected lands, gcam database (for grabbing modern land allocation data)

# get input data - either GCAM or custom
land_roots <- read_land_inputs_xml2(folder="~/Dropbox/Github/gcam-core/input/gcamdata/xml")

gcam_land_alloc <- get_gcam_land_alloc(db_name="database_basexdb",
                                       gcam_dir="~/Dropbox/Research/gcam_projects/task3.1a/gcam_output/",
                                       scenario="Reference", read_from_file=FALSE)  # scenario is doing nothing when read_from_file is TRUE

leaf_data <- process_xml_inputs(land_roots, gcam_land_alloc)

# check outputs

# save without protected
#saveRDS(leaf_data,file="updated_leaf_data.RDS")

leaf_data <- readRDS(file="updated_leaf_data.RDS")
leaf_data$name <- paste0(leaf_data$region,"_",leaf_data$landleaf)

# add in protected lands
# may want to add the 250 strategy


input_file <- "~/Dropbox/Github/gcam-core/input/gcamdata/xml/protected_land_input_2.xml"
additional_file <- "~/Dropbox/Github/gcam-core/input/gcamdata/xml/protected_land_input_3.xml"
protected_data <- add_protected_leaves(leaf_data,input_file,additional_file,gcam_land_alloc)

# save with protected
#saveRDS(protected_data,file="updated_protected_leaf_data.RDS")
#protected_data <- readRDS(file="updated_protected_leaf_data.RDS")

# get soil timescale data
# TODO update to be compatible with eventual updates to make landleaf specific soil timescales
soil_timescales <- get_soilTS_byRegion(land_roots[[1]])
soil_timescales$soilTimeScale <- as.numeric(soil_timescales$soilTimeScale)


# read in parameters
#outer_params2 <- get_leaf_params(land_roots, soil_timescales, protected_data)

outer_params2 <- get_leaf_params(land_roots, soil_timescales, leaf_data)
#saveRDS(outer_params2,file="updated_param_data.RDS")





leaf_data <- readRDS(file="updated_leaf_data.RDS")
leaf_data$name <- paste0(leaf_data$region,"_",leaf_data$landleaf)

outer_params2 <- readRDS("updated_param_data.RDS")

outer_land_alloc2 <- leaf_data  # or protected_data
outer_land_alloc2$name <- paste(outer_land_alloc2$region, outer_land_alloc2$landleaf, sep="_")

#selected <- sample(outer_params2$name,500)
#selected <- c("USA_OtherArableLand_UsaPacNW")
#outer_params2 <- filter(outer_params2,name %in% selected)
#head(outer_params2)

year0 <- 1745
yearEnd <- 2100
years <- unique(outer_land_alloc2$year)
years <- years[years>=year0]
last_year <- 2100
stop_year <- 2099
run_years <- years[years<=stop_year]
outer_land_alloc2 <- filter(outer_land_alloc2,name %in% outer_params2$name, year %in% run_years) %>%
  mutate(value=land_alloc) %>% select(-c("land_alloc"))


# initialize Hector
rcp <- "ssp245"
scenario_file <- paste0("input/hector_",rcp,".ini")
ini_file <- system.file(scenario_file, package="hector")


ccycling=FALSE
rhEff=FALSE
betaEff=FALSE
coupled=FALSE

source("gcam_utils.R")
source("land_utils.R")

outer_land_alloc2 <- data.table::setDT(outer_land_alloc2)
outer_params2 <- data.table::setDT(outer_params2)

output <- run_all_years(outer_land_alloc2, outer_params2, ini_file, stop_year=stop_year, last_year=last_year, rhEff=rhEff, betaEff=betaEff, cCycling=ccycling, coupled=coupled)
#output[["params"]]
#output_base <- output

scenario_name <- "full_world_real-baseline_no-protected_2100"
write.csv(output[["leaf_data"]],file=paste0("data/leaf_data_",scenario_name,".csv"))
write.csv(output[["params"]],file=paste0("data/leaf_params_",scenario_name,".csv"))
write.csv(output[["climate"]],file=paste0("data/climate_data_",scenario_name,".csv"))
write.csv(output[["ag_emiss"]],file=paste0("data/ag_emiss_",scenario_name,".csv"))
write.csv(output[["bg_emiss"]],file=paste0("data/bg_emiss_",scenario_name,".csv"))

