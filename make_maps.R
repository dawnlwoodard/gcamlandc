# make maps - this is old code so not neatly configured to work with current output. will need a bit of manipulation to get that working

# make basin sums and plot basin map

head(all_regions_wc_no_fixed)


# add basin column

all_regions_wc_no_fixed %>%
  mutate(tmp = landleaf) %>%
  tidyr::separate(tmp, into = c('other1', 'basin', 'other2'), sep = "_") %>%
  select(-other1, -other2) ->
  all_regions_wc_no_fixed_basins



all_basins <- unique(all_regions_wc_no_fixed_basins$basin)
world_luc_wc <- data.frame(year=integer(), basin=character(), variable=character(), value=double())

for (basin in all_basins){
  print(basin)
  single_reg_data <- all_regions_wc_no_fixed_basins[all_regions_wc_no_fixed_basins$basin=={{basin}},]
  
  reg_total_la <- dplyr::filter(gcam_land_alloc,basin=={{basin}},year==1975)
  reg_total_la <- sum(reg_total_la$value)
  
  #print(head(single_reg_data))
  #single_reg_data <- single_reg_data[grep("Tundra|UrbanLand|RockIceDesert",single_reg_data$landleaf),]
  #print(head(single_reg_data))
  single_reg_data %>% mutate(agCDensity=agCDensity*(land_alloc/reg_total_la), bgCDensity=bgCDensity*(land_alloc/reg_total_la),) %>%
    tidyr::pivot_longer(cols=c("land_alloc", "litter", "lucEmissions", "bgEmissions", "agEmissions", "agCDensity", "agCarbon", "bgCDensity", "bgCarbon", "NPP", "Rh"),
                        names_to = "variable", values_to="value") %>% group_by(variable,basin,year) %>% summarise(value=sum(value)) -> reg_totals
  
  reg_totals$scenario <- "static climate"
  
  world_luc_wc <- dplyr::bind_rows(world_luc_wc,reg_totals)
  
}

saveRDS(world_luc_wc,file="data/world_wc_data_by_basin.RDS")

world_luc_wc %>% group_by(variable,year) %>% summarise(value=sum(value)) -> world_totals



# SUM WORLD


all_regions_nc_no_fixed %>%
  mutate(tmp = landleaf) %>%
  tidyr::separate(tmp, into = c('other1', 'basin', 'other2'), sep = "_") %>%
  select(-other1, -other2) ->
  all_regions_nc_no_fixed_basins

world_luc_nc <- data.frame(year=integer(), region=character(), variable=character(), value=double())

for (basin in all_basins){
  print(basin)
  single_reg_data <- all_regions_nc_no_fixed_basins[all_regions_nc_no_fixed_basins$basin=={{basin}},]
  
  reg_total_la <- dplyr::filter(gcam_land_alloc,basin=={{basin}},year==1975)
  reg_total_la <- sum(reg_total_la$value)
  
  #print(head(single_reg_data))
  #single_reg_data <- single_reg_data[grep("Tundra|UrbanLand|RockIceDesert",single_reg_data$landleaf),]
  #print(head(single_reg_data))
  single_reg_data %>% mutate(agCDensity=agCDensity*(land_alloc/reg_total_la), bgCDensity=bgCDensity*(land_alloc/reg_total_la),) %>%
    tidyr::pivot_longer(cols=c("land_alloc", "litter", "lucEmissions", "bgEmissions", "agEmissions", "agCDensity", "agCarbon", "bgCDensity", "bgCarbon", "NPP", "Rh"),
                        names_to = "variable", values_to="value") %>% group_by(variable,basin,year) %>% summarise(value=sum(value)) -> reg_totals
  
  reg_totals$scenario <- "no climate"
  
  world_luc_nc <- dplyr::bind_rows(world_luc_nc,reg_totals)
  
}

saveRDS(world_luc_nc,file="data/world_nc_data_by_basin.RDS")

world_luc_nc %>% group_by(variable,year) %>% summarise(value=sum(value)) -> world_totals_nc



world_totals$diff <- world_totals$value - world_totals_nc$value

filter(world_totals,variable=="lucEmissions", year==2099)

basin_ids <- read.csv("extdata/gcam_basin_ids.csv")

basin_ids$GLU_code <- gsub("GLU","",basin_ids$GLU_code)
basin_ids$basin_code <- paste0(basin_ids$GLU_code,basin_ids$ISO_NUM)
tmp <- paste0(basin_ids$GLU_code,basin_ids$ISO_NUM)
head(tmp)

emission_diffs <- world_luc_wc %>% filter(variable=="lucEmissions")

emission_diffs$value = (emission_diffs$value - filter(world_luc_nc,variable=="lucEmissions")$value)*0.001

#emission_diffs %>% mutate(map_region=dplyr::recode(region,"EU-15"="EU_15", "EU-12"="EU_12")) %>%
#  dplyr::filter(region!="Taiwan") %>% filter(year==2099) -> map_emission_diffs

emission_diffs <- mutate(emission_diffs,GLU_name=basin) %>% select(-c("basin"))
emission_diffs_test <- left_join(emission_diffs,basin_ids,by="GLU_name")

emission_diffs_test$Basin_name <- gsub("_Basin","",emission_diffs_test$Basin_name)



# global regions
numeric2Cat_param <- list("lucEmissions")
numeric2Cat_breaks <- list(c(-Inf, -0.5, -0.25, -0.1, -0.05, Inf))
numeric2Cat_labels <- list(c("< -0.5 Pg C/yr",
                             "-0.5 to -0.25 Pg C/yr",
                             "-0.25 to -0.1 Pg C/yr",
                             "-0.1 to -0.05 Pg C/yr",
                             "-0.05 to 0 Pg C/yr"))
numeric2Cat_legendTextSize <- list(c(0.7))
numeric2Cat_palette <- list(c("< -0.5 Pg C/yr"="#163a30",
                              "-0.5 to -0.25 Pg C/yr"="#275f59",
                              "-0.25 to -0.1 Pg C/yr"="#488a84",
                              "-0.1 to -0.05 Pg C/yr"="#7dbab2",
                              "-0.05 to 0 Pg C/yr"="#b4dbd5"))


# basins

numeric2Cat_param <- list("lucEmissions")
numeric2Cat_breaks <- list(c(-Inf, -0.25, -0.1, -0.05, -0.01, Inf))
numeric2Cat_labels <- list(c("< -0.25 Pg C/yr",
                             "-0.25 to -0.1 Pg C/yr",
                             "-0.1 to -0.05 Pg C/yr",
                             "-0.05 to -0.01 Pg C/yr",
                             "-0.01 to 0 Pg C/yr"))
numeric2Cat_legendTextSize <- list(c(0.7))
numeric2Cat_palette <- list(c("< -0.25 Pg C/yr"="#275f59",
                              "-0.25 to -0.1 Pg C/yr"="#488a84",
                              "-0.1 to -0.05 Pg C/yr"="#7dbab2",
                              "-0.05 to -0.01 Pg C/yr"="#b4dbd5",
                              "-0.01 to 0 Pg C/yr"="#e8f7f1"))



numeric2Cat_list <-list(numeric2Cat_param = numeric2Cat_param,
                        numeric2Cat_breaks = numeric2Cat_breaks,
                        numeric2Cat_labels = numeric2Cat_labels,
                        numeric2Cat_palette = numeric2Cat_palette,
                        numeric2Cat_legendTextSize = numeric2Cat_legendTextSize); numeric2Cat_list





emission_diffs_test %>% mutate(map_region=dplyr::recode(Basin_name,"Hong_(Red_River)"="Hong_Red_River",
                                                        "Brahmani"="Brahamani","Mahanadi"="Mahandi",
                                                        "Yenisei"="Yenisey","Sittaung"="Sittang",
                                                        "HamuniMashkel"="Hamun_i_Mashkel")) %>%
  filter(year==2099) -> map_emission_diffs


#library(rmap)
#library(jgcricolors)
mapData <- data.frame(subRegion=map_emission_diffs$map_region,value=map_emission_diffs$value,param="lucEmissions")
rmap::map(mapData,
          folder = "figures",
          nameAppend ="_emissions_diffs_test",
          numeric2Cat_list = numeric2Cat_list
          #palette="pal_div_GnBr"
          #numeric2Cat_list = numeric2Cat_list
)


#emission_diffs <- world_luc_wc %>% filter(variable=="lucEmissions")

#emission_diffs$value = (emission_diffs$value - filter(world_luc_nc,variable=="lucEmissions")$value)*0.001

#emission_diffs %>% mutate(map_region=dplyr::recode(region,"EU-15"="EU_15", "EU-12"="EU_12")) %>%
#  dplyr::filter(region!="Taiwan") %>% filter(year==2000) -> map_emission_diffs_2000

emission_diffs_test %>% mutate(map_region=dplyr::recode(Basin_name,"Hong_(Red_River)"="Hong_Red_River",
                                                        "Brahmani"="Brahamani","Mahanadi"="Mahandi",
                                                        "Yenisei"="Yenisey","Sittaung"="Sittang",
                                                        "HamuniMashkel"="Hamun_i_Mashkel")) %>%
  filter(year==2050) -> map_emission_diffs_2000



#library(rmap)
#library(jgcricolors)
mapData <- data.frame(subRegion=map_emission_diffs_2000$map_region,value=map_emission_diffs_2000$value,param="lucEmissions")
rmap::map(mapData,
          folder = "figures",
          nameAppend ="_emissions_diffs2000",
          numeric2Cat_list = numeric2Cat_list
          #palette="pal_div_GnBr"
          #numeric2Cat_list = numeric2Cat_list
)


