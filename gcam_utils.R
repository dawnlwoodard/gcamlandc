library(dplyr)  # needed for pipelines

read_land_inputs_xml2 <- function(folder="inputs"){
  land1 <- xml2::read_xml(paste0(folder,"/land_input_1.xml"))
  land1_root <- xml2::xml_root(land1)

  land2 <- xml2::read_xml(paste0(folder,"/land_input_2.xml"))
  land2_root <- xml2::xml_root(land2)

  land3 <- xml2::read_xml(paste0(folder,"/land_input_3_IRR.xml"))
  land3_root <- xml2::xml_root(land3)

  land4 <- xml2::read_xml(paste0(folder,"/land_input_4_IRR_MGMT.xml"))
  land4_root <- xml2::xml_root(land4)

  land5 <- xml2::read_xml(paste0(folder,"/land_input_5_IRR_MGMT.xml"))
  land5_root <- xml2::xml_root(land5)

  land_roots <- list(land1_root, land2_root, land3_root, land4_root, land5_root)

  return(land_roots)
}


process_xml_inputs <- function(land_roots, gcam_land_alloc, nleaves=0, nrows=0){

  # make dataframe
  # Units | scenario | region | landleaf | year | value | variable

  table_cols <- c("region","landleaf","year","land_alloc")

  all_data <- data.frame(region=character(),
                         landleaf=character(),year=integer(),
                         land_alloc=double())

  colnames(all_data) <- table_cols


  # process leaves in each land root at a time
  count <- 0
  for (i in 1:length(land_roots)){
    data <- data.frame(region=character(),
                           landleaf=character(),year=integer(),
                           land_alloc=double())
    colnames(data) <- table_cols
    # find all leaves in a single land root
    # make sure this also gets unmanaged
    all_leaves <- xml2::xml_find_all(land_roots[[i]],"//LandLeaf")
    unmanaged_leaves <- xml2::xml_find_all(land_roots[[i]], "//UnmanagedLandLeaf")
    all_leaves <- c(all_leaves, unmanaged_leaves)
    leaf_count <- 0

    # TODO for efficiency: run leaves in batches of 250 at a time, then add 250 on to main database and redefine data
    for (leaf in all_leaves){
      new_leaf_data <- process_leaf(leaf,gcam_land_alloc)  # TODO filter this by leaf earlier so I don't pass whole thing
      count <- count+1
      idx <- count+nrows-1
      data <- dplyr::bind_rows(data,new_leaf_data)  # TODO update to rbindlist
      leaf_count <- leaf_count + 1

      if (leaf_count == 250){
        print(c(i, leaf_count, count))
        all_data <- dplyr::bind_rows(all_data,data)
        data <- data.frame(region=character(),
                           landleaf=character(),year=integer(),
                           land_alloc=double())
        colnames(data) <- table_cols
        leaf_count <- 0
      }

    }
    all_data <- dplyr::bind_rows(all_data,data)  # bind remaining leaves that have not been covered already
  }
  return(all_data)
}

process_leaf <- function(leaf_node, gcam_land_alloc){
  name <- xml2::xml_attr(leaf_node,"name")
  region <- get_region(leaf_node)

  land_alloc <- get_leaf_land_alloc(leaf_node, name, region, gcam_land_alloc)

  leaf_output <- data.frame(region={{region}}, landleaf={{name}}, year=land_alloc$year, land_alloc=land_alloc$value)

  return(leaf_output)
}

get_region <- function(node,la_str="/LandAllocatorRoot"){
  reg_path <- strsplit(xml2::xml_path(node),la_str)[[1]][1]
  reg_node <- xml2::xml_find_first(node,reg_path)
  reg_name <- xml2::xml_attr(reg_node,"name")
  return(reg_name)
}

#TODO handle leaves where historical is 0 but modern is not
parse_c_densities <- function(leaf_data, years){
  above_grnd <- as.numeric(leaf_data$`land-use-history`$`above-ground-carbon-density`)
  below_grnd <- as.numeric(leaf_data$`land-use-history`$`below-ground-carbon-density`)
  if (length(above_grnd)==0){
    return(NULL)
  } else {
    return(data.frame(year=years,above_ground=above_grnd, below_ground=below_grnd))
  }
}

get_leaf_land_alloc <- function(leaf_node, leaf_name, leaf_region, gcam_land_alloc){
  leaf_data <- xml2::as_list(leaf_node)  # convert leaf data from xml into something parseable in R
  
  land_alloc_df <- parse_land_alloc(leaf_data)  # get the historical land allocation data from the xmls
  
  gcam_leaf_land_alloc <- get_gcam_land_alloc_by_leaf(leaf_region=leaf_region, leaf_name=leaf_name, gcam_alloc=gcam_land_alloc)
  
  # find all years of overlap between modeled and historical land alloc and remove from historical
  first_model_year <- gcam_leaf_land_alloc$year[1]
  idx <- match(first_model_year, land_alloc_df$year)
  land_alloc_df <- land_alloc_df[1:idx-1,]  # remove any land allocation from the historical that's covered by gcam database output

  land_alloc_all_years <- rbind(land_alloc_df,gcam_leaf_land_alloc)
  rownames(land_alloc_all_years) <- 1:length(land_alloc_all_years$year)

  # interpolate land allocation data - currently linear. Spline was being weird.
  all_years <- seq(min(land_alloc_all_years$year), max(land_alloc_all_years$year), 1)
  land_alloc_interp <- approx(land_alloc_all_years$year,land_alloc_all_years$value, xout=all_years, ties="ordered")
  land_alloc_interp_df <- as.data.frame(land_alloc_interp)  # approx returns named list with names x and y
  colnames(land_alloc_interp_df) <- c("year", "value")
  return(land_alloc_interp_df)
}


parse_land_alloc <- function(leaf_data){
  # get historical land allocation
  hist_listed_alloc <- leaf_data$`land-use-history`[names(leaf_data$`land-use-history`)=='allocation']
  n_hist <- length(hist_listed_alloc)
  year_hist <- numeric(n_hist)
  value_hist <- numeric(n_hist)

  for (i in 1:n_hist){
    year_hist[i] <- as.numeric(attributes(hist_listed_alloc[[i]])$year) # this is specific to exact format of current land input xmls
    value_hist[i] <- as.numeric(hist_listed_alloc[[i]][[1]])
  }
  hist_df <- data.frame(year=year_hist, value=value_hist)
  hist_df <- hist_df[order(hist_df$year),]  # population[order(population$age),]
  # get modern land allocation if it exists
  listed_alloc <- leaf_data[names(leaf_data)=='landAllocation']
  n_mdrn <- length(listed_alloc)

  mdrn_years <- c(1975, 1990, 2005, 2010, 2015)

  mdrn_df <- data.frame(year=mdrn_years,value=NA)

  if (n_mdrn!=0){
    for (i in 1:n_mdrn){
      data_year <- as.numeric(attributes(listed_alloc[[i]])$year) # this is specific to exact format of current land input xmls
      data_value <- as.numeric(listed_alloc[[i]][[1]])
      if (data_year %in% mdrn_df$year){
        mdrn_df[mdrn_df$year==data_year,] <- c(data_year,data_value)
      }
    }
  }
  hist_df <- hist_df[1:(length(hist_df$year)-1),]  # remove 1975 overlap
  return(data.frame(year=c(hist_df$year,mdrn_df$year),value=c(hist_df$value,mdrn_df$value)))

}

add_protected_leaves <- function(leaf_data_out, first_xml, second_xml, gcam_land_alloc){

  first_set <- xml2::read_xml(first_xml)
  first_root <- xml2::xml_root(first_set)

  second_set <- xml2::read_xml(second_xml)
  second_root <- xml2::xml_root(second_set)

  print(paste0("LEAVES: ",length(unique(paste0(leaf_data_out$region,leaf_data_out$landleaf)))))

  table_cols <- c("region","landleaf","year","land_alloc")
  data <- data.frame(region=character(),
                     landleaf=character(),year=integer(),
                     land_alloc=double())
  colnames(data) <- table_cols

  # gather all managed and un-managed leaves from both xmls
  leaf_set <- c(xml2::xml_find_all(first_root,"//LandLeaf"),
                xml2::xml_find_all(first_root, "//UnmanagedLandLeaf"),
                xml2::xml_find_all(second_root,"//LandLeaf"),
                xml2::xml_find_all(second_root, "//UnmanagedLandLeaf"))

  print(length(leaf_set))
  count <- 0
  all_leaf_names <- NULL
  for (leaf in leaf_set){
    print(count)
    new_leaf_data <- process_leaf(leaf,gcam_land_alloc)
    leaf_reg <- new_leaf_data$region[[1]]
    leaf_name <- new_leaf_data$landleaf[[1]]
    all_leaf_names <- c(all_leaf_names,paste0(leaf_reg,"_",leaf_name))
    #leaf_idx <- which(leaf_data_out$region == leaf_reg & leaf_data_out$landleaf == leaf_name)
    #leaf_data_out <- leaf_data_out[-c(leaf_idx),]  # remove old leaf from original data
    #print(paste0("LEAVES: ",length(unique(paste0(leaf_data_out$region,leaf_data_out$landleaf)))))
    data <- dplyr::bind_rows(data,new_leaf_data)  # adding to smaller dataframe then will add to large one at end
    count <- count + 1
  }

  print(paste0("LEAVES BEFORE: ",length(unique(paste0(leaf_data_out$region,leaf_data_out$landleaf)))))
  print(length(all_leaf_names))
  leaf_data_out <- leaf_data_out[!(leaf_data_out$name %in% all_leaf_names),]
  print(paste0("LEAVES AFTER REMOVING: ",length(unique(paste0(leaf_data_out$region,leaf_data_out$landleaf)))))

  leaf_data_out <- dplyr::bind_rows(leaf_data_out,data)

  print(paste0("LEAVES AFTER ADDING: ",length(unique(paste0(leaf_data_out$region,leaf_data_out$landleaf)))))

  return(leaf_data_out)

}


get_leaf_params <- function(land_roots, soilTimeScales, land_alloc_data, data_names = c("above-ground-carbon-density","below-ground-carbon-density","mature-age")
){
  leaf_params <- data.frame(matrix(ncol=2+length(data_names),nrow=0))
  colnames(leaf_params) <- c("region","landleaf",data_names)
  count <- 0
  for (root in land_roots){
    count <- count + 1
    print("STARTING NEXT ROOT")
    print(count)
    tmp <- get_data_byLeaf2(root, data_names)
    leaf_params <- data.table::rbindlist(list(leaf_params,tmp))
  }
  head(leaf_params)

  leaf_params <- dplyr::mutate(leaf_params,agCarbon0=0.0,bgCarbon0=0.0,npp_factor=0.0,NPP0=0.0, co20=0.0)
  leaf_params <- dplyr::left_join(leaf_params,soilTimeScales,by="region")

  dplyr::filter(land_alloc_data,year==1700) %>% dplyr::mutate(land0=land_alloc) %>% dplyr::select(-c("year","land_alloc")) -> land0_df
  leaf_params <- dplyr::left_join(leaf_params,land0_df,by=c("region","landleaf"))

  for (col in colnames(leaf_params)[3:10]){
    leaf_params[[col]] <- as.numeric(leaf_params[[col]])
  }
  leaf_params$`above-ground-carbon-density` <- 0.2*leaf_params$`below-ground-carbon-density`
  leaf_params$name <- paste(leaf_params$region, leaf_params$landleaf, sep="_")

  # store leaf_data so it's easily accessible
  saveRDS(leaf_params,file="data/leaf_params.RDS")
  return(leaf_params)
}


get_data_byLeaf2 <- function(root_node, data_names){
  data <- data.frame(matrix(ncol=2+length(data_names),nrow=0))
  colnames(data) <- c("region","landleaf",data_names)

  all_regions <- xml2::xml_find_all(root_node,"//region")
  for (region in all_regions){
    reg_name <- xml2::xml_attr(region,"name")
    print(reg_name)
    all_leaves <- xml2::xml_find_all(region,paste0(xml2::xml_path(region),"//LandLeaf|",
                                                   xml2::xml_path(region),"//UnmanagedLandLeaf"))
    count <- 1
    tmp_data <- data.frame(matrix(ncol=2+length(data_names),nrow=length(all_leaves)))
    colnames(tmp_data) <- c("region","landleaf",data_names)
    #print(all_leaves)
    for (leaf in all_leaves){
      leaf_name <- xml2::xml_attr(leaf,"name")
      print(paste0(reg_name,", ",leaf_name))
      new_row <- c(reg_name,leaf_name)
      #row_names <- c("region","landleaf")
      for (data_name in data_names){
        sts_node <- xml2::xml_find_first(leaf,paste0(xml2::xml_path(leaf),"/land-carbon-densities/",
                                                     data_name,"|",xml2::xml_path(leaf),"/no-emiss-carbon-calc/",data_name))  # don't necessarily need the land-carbon-densities part.
        sts <- as.numeric(xml2::xml_text(sts_node))
        new_row <- c(new_row, sts)
        #row_names <- c(row_names,data_name)
      }
      #names(new_row) <- row_names
      #print(new_row)
      tmp_data[count,] <- new_row
      count <- count + 1
    }
    data <- data.table::rbindlist(list(data,tmp_data))
  }
  names(data) <- c("region","landleaf",data_names)
  return(data)
}

get_data_byLeaf <- function(root_node, data_names){
  data <- data.frame(matrix(ncol=2+length(data_names),nrow=0))
  colnames(data) <- c("region","landleaf",data_names)

  all_regions <- xml2::xml_find_all(root_node,"//region")
  for (region in all_regions){
    reg_name <- xml2::xml_attr(region,"name")
    print(reg_name)
    all_leaves <- xml2::xml_find_all(region,paste0(xml2::xml_path(region),"//LandLeaf|",xml2::xml_path(region),"//UnmanagedLandLeaf"))
    count <- 0
    tmp_data <- data.frame(matrix(ncol=2+length(data_names),nrow=0))
    colnames(tmp_data) <- c("region","landleaf",data_names)
    #print(all_leaves)
    for (leaf in all_leaves){
      leaf_name <- xml2::xml_attr(leaf,"name")
      #print(paste0(reg_name,", ",leaf_name))
      new_row <- c(reg_name,leaf_name)
      #row_names <- c("region","landleaf")
      for (data_name in data_names){
        sts_node <- xml2::xml_find_first(leaf,paste0(xml2::xml_path(leaf),"/land-carbon-densities/",data_name,"|",xml2::xml_path(leaf),"/no-emiss-carbon-calc/",data_name))  # don't necessarily need the land-carbon-densities part.
        sts <- as.numeric(xml2::xml_text(sts_node))
        new_row <- c(new_row, sts)
      }
      tmp_data <- data.table::rbindlist(list(tmp_data,new_row))
      count <- count + 1
      if (count == 100){
        data <- data.table::rbindlist(list(data,tmp_data))
        tmp_data <- data.frame(matrix(ncol=2+length(data_names),nrow=0))
        colnames(tmp_data) <- c("region","landleaf",data_names)
        count <- 0
      }

    }
  }
  names(data) <- c("region","landleaf",data_names)
  return(data)
}


get_soilTS_byRegion <- function(root_node){
  data <- data.frame(matrix(ncol=2,nrow=0))
  colnames(data) <- c("region","soilTimeScale")

  all_regions <- xml2::xml_find_all(root_node,"//region")
  for (region in all_regions){
    reg_name <- xml2::xml_attr(region,"name")
    sts_node <- xml2::xml_find_first(region,paste0(xml2::xml_path(region),"/LandAllocatorRoot/soilTimeScale"))
    sts <- as.numeric(xml2::xml_text(sts_node))
    new_row <- c("region"=reg_name, "soilTimeScale"=sts)
    data <- rbind(data,new_row)
  }
  colnames(data) <- c("region","soilTimeScale")
  return(data)
}

get_gcam_land_alloc <- function(db_name="database_basexdb", gcam_dir="~/Dropbox/Research/gcam_projects/gcamv5_4_release/output/", scenario="Reference", read_from_file=FALSE, filename="data/gcam_land_alloc.csv"){

  if (read_from_file) {
    gcam_land_alloc <- read.csv2(file=filename,header=TRUE)
  }
  else {
    base_conn <- rgcam::localDBConn(gcam_dir, db_name)

    land_alloc_query <- '<query title="detailed land allocation">
    <axis1 name="LandLeaf">LandLeaf[@name]</axis1>
    <axis2 name="Year">land-allocation[@year]</axis2>
    <xPath group="false" sumAll="false" buildList="true" dataName="LandLeaf">/LandNode[@name=\'root\' or @type=\'LandNode\' (:collapse:)]//land-allocation/text()</xPath>
    <comments/>
</query>'

    new.proj <- rgcam::addSingleQuery(base_conn, "new.proj", "Land Allocation", land_alloc_query, c(scenario), clobber=TRUE)

    gcam_land_alloc <- rgcam::getQuery(new.proj, "Land Allocation")
    write.csv2(gcam_land_alloc, file=filename, row.names = FALSE)
  }

  return(gcam_land_alloc)
}

get_gcam_land_alloc_by_leaf <- function(leaf_region, leaf_name, gcam_alloc){
  leaf_land_alloc <- gcam_land_alloc[gcam_alloc$region==leaf_region & gcam_alloc$landleaf==leaf_name,]
  leaf_land_alloc <- leaf_land_alloc[,c("year", "value")]  # match columns from historical xml data
  return(leaf_land_alloc)
}

get_gcam_emissions <- function(db_name="database_basexdb", gcam_dir="~/Dropbox/Research/gcam_projects/gcamv5_4_release/output/"){
  base_conn <- localDBConn(gcam_dir, db_name)
  luc_query <- '<query title="Land Use Change Emission">
         <axis1 name="land-use-change-emission">LandLeaf</axis1>
         <axis2 name="Year">land-use-change-emission[@year]</axis2>
         <xPath buildList="true" dataName="land-use-change-emission" group="false" sumAll="true">/LandNode[@name=\'root\' or @type=\'LandNode\' (: collapse :)]//land-use-change-emission/text()</xPath>
         <comments/>
      </query>'

  new.proj <- addSingleQuery(base_conn, "new.proj", "Land Use Change Emissions", luc_query, c("Reference"), clobber=TRUE)

  gcam_luc <- getQuery(new.proj, "Land Use Change Emissions")
  return(gcam_luc)
}


# TODO figure out better place to put this
read_luc_data <- function() {
  gcp_hist <- read.csv("~/Dropbox/Research/gcam_projects/LUC/data/gcp_historical.csv", header=TRUE, skip=14)
  gcp_mdrn <- read.csv("~/Dropbox/Research/gcam_projects/LUC/data/gcp_modern.csv",header=TRUE, skip=27)
  gcp_hist <- gcp_hist[101:nrow(gcp_hist),1:7]
  gcp_hist <- as_tibble(gcp_hist)
  gcp_mdrn <- as_tibble(gcp_mdrn)
  names(gcp_hist) <- c("year", "fossil", "luc", "atm", "ocean", "land", "imbalance")

  houghton_regions <- read.csv("~/Dropbox/Research/gcam_projects/LUC/data/houghton_regions.csv")

  nc <- nc_open("~/Dropbox/Research/gcam_projects/LUC/data/Gasser_et_al_2020_best_guess.nc")
  v4 <- nc$var[[4]]
  gasser_yr <- v4$dim[[5]]$vals
  gasser <- ncvar_get(nc,v4)

  varsize <- v4$varsize
  ndims   <- v4$ndims
  nt      <- varsize[ndims]  # Remember timelike dim is always the LAST dimension!

  gasser_keys <- c("Unknown"=0, "Sub-Saharan Africa"=1, "Latin America"=2,  "South and Southeast Asia"=3,
                   "North America"=4, "Europe"=5, "Former USSR"=6, "China"=7,
                   "North Africa and the Middle East"=8, "East Asia"=9,  "Oceania"=10)

  gcam_gasser <- list("Sub-Saharan Africa"=c("South Africa", "Africa_Eastern", "Africa_Southern", "Africa_Western"),
                      "Latin America"=c("Argentina", "Brazil", "Columbia", "Mexico", "South America_Northern", "South America_Southern", "Central America and the Caribbean"),
                      "South and Southeast Asia" = c("Pakistan", "India", "Indonesia", "South Asia", "Southeast Asia"),
                      "North America" = c("Canada", "USA"),
                      "Europe"= c("EU-12", "EU-15", "Europe Free Trade Association", "Europe Non-EU"),
                      "Former USSR" = c("Russia", "Europe Eastern", "Central Asia"),
                      "China" = c("China"),
                      "North Africa and the Middle East" = c("Middle East", "Africa_Northern"),
                      "East Asia" = c("Japan", "South Korea"),
                      "Oceania" = c("Australia_NZ"))

  gasser_luc <- rep(0,nt)
  for (i in 1:nt){
    gasser_luc[i] <- sum(gasser[,,,,i])
  }

  gasser_luc_reg <- array(0L,c(11,nt))

  for (i in 1:10){
    for (j in 1:nt){
      gasser_luc_reg[i,j] <- sum(gasser[,,,i,j])
    }
  }

  gasser_df <- tibble(luc = gasser_luc, year=gasser_yr)

  return(list("gasser"=gasser_df, "houghton"=houghton_regions, "gcp_hist"=gcp_hist, "gcp_mdrn"=gcp_mdrn, "gasser_reg"=gasser_luc_reg))

}
