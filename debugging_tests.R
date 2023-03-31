
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

