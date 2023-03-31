# land utils
library(data.table)
library(hector)

getLitter <- function(prevC, currLand, land0){
  f_det <- 0.035
  #f_det <- 0.1  # this is for if we isolate NPP to above ground
  if (land0 == 0.0){
    litter <- 0.0
  } else litter <- f_det*prevC*(currLand/land0)

  return(litter)
}

getNPP <- function(idx, NPP0, startYear, currCO2, co20, currLand, Land0, betaEff){

  if (betaEff){
    beta <- 0.15  # TODO make this an input
  } else {
    beta <- 0  # turns off CO2 fertilization effect
  }

  if (Land0 == 0) NPP <- 0  # no NPP if we don't have any land allocation. This also guarantees we don't divide by zero in the next line
  else {
    NPP <- NPP0*(1 + beta*log(currCO2/co20))*(currLand/Land0)
  }

  return(NPP)
}

getTairMean <- function(lag, year, climate_data){

  Tair_mean <- 0.0
  if (year <= lag) {
    years <- seq(0,year)+climate_data$year[1] # 1700
  } else years <- seq(year-lag,year)+climate_data$year[1]

  Tair_mean <- sum(climate_data[climate_data$year %in% years,]$tas)
  Tair_mean <- Tair_mean/length(years)

  return(Tair_mean)
}

# currently passing in belowC0 to test
getRh <- function(idx, prevBelowC, climate_data, rhEff, currLand, land0){

  # TODO make these parameters inputs
  f_rhs <- 0.02
  q10 <- 2.0
  if (rhEff){
    T_rm <- climate_data[idx,]$tairMean
  } else {
    T_rm <- 0
  }
  if (land0 == 0){
    currRh <- 0  # no Rh if we don't have any land allocation. This also guarantees we don't divide by zero in the next line
  } else currRh <- f_rhs*prevBelowC*q10^(T_rm/10.0)*(currLand/land0)

  return(currRh)
}

# should we do this as values or set property of landleaf for NPP, cDensity
getCDensityAbove <- function(idx, NPP, litter, prevDensityAbove, landArea, prevCAbove,aboveC0, lucAG){
  if (is_equal(landArea,0.0)) { return(prevDensityAbove) }
  f_agnpp <- 0.35
  #f_agnpp <- 1.0
  currDensityAbove <- prevDensityAbove + (f_agnpp*NPP - litter)/landArea

  return(max(currDensityAbove,0))
}

getCDensityBelow <- function(idx, NPP, Rh, litter, prevDensityBelow, landArea, prevAboveC, aboveC0, lucBG){

  if (is_equal(landArea,0.0)) { return(prevDensityBelow) }

  f_bgnpp <- 0.65
  #f_bgnpp <- 0.0
  #print(c(f_bgnpp*NPP,litter,Rh,(f_bgnpp*NPP + litter - Rh)/landArea))
  currDensityBelow <- prevDensityBelow + (f_bgnpp*NPP + litter - Rh)/landArea

  return(max(currDensityBelow,0))
}

get_npp_factor <- function(belowGroundCDensity){
  npp_factor <- 0.02*belowGroundCDensity
}

calc_above_ground_luc_emissions <- function(prevC, prevLand, currLand, prevCDensity, currCDensity, year, startYear, endYear, matureAge, aboveGroundEmissions){

  idx <- year - startYear

  cDiff <- prevCDensity*prevLand-currCDensity*currLand  # this is an update from previous GCAM. Was density*(prevLand-currLand)

  if (is_equal(cDiff,0.0)){
  } else if (cDiff < 0 && matureAge > 1 ) {
    aboveGroundEmissions <- calc_sigmoid_curve(cDiff, startYear, year, endYear, matureAge, aboveGroundEmissions)
  } else if ( is_equal(prevLand,0.0) ){
    aboveGroundEmissions[idx] <- aboveGroundEmissions[idx] - currLand*prevCDensity
  } else {

    aboveGroundEmissions[idx] <- aboveGroundEmissions[idx] + (prevC/prevLand)*(prevLand-currLand)

    if (matureAge > 1) {
      cFutureAdjust <- (prevLand - currLand) * (prevCDensity - (prevC/prevLand))

      aboveGroundEmissions <- calc_sigmoid_curve(cFutureAdjust, startYear, year, endYear, matureAge, aboveGroundEmissions)
    }
  }
  return(aboveGroundEmissions)
}

calc_below_ground_luc_emissions <- function(cDiff, year, startYear, endYear, soilTimeScale, belowGroundEmissions){

  idx <- year-startYear

  if (is_equal(cDiff,0.0)){
    return(belowGroundEmissions)
  }

  halfLife <- soilTimeScale/10.0
  log2 <- log(2.0)
  lmda <- log2/halfLife
  yearCounter<-0
  cumStockDiff_t1 <- 0.0
  for (i in seq(year,endYear)){
    yearCounter <- yearCounter + 1
    cumStockDiff_t2 <- cDiff*(1.0 - exp(-1*lmda*yearCounter))
    belowGroundEmissions[i-startYear] <- belowGroundEmissions[i-startYear] + (cumStockDiff_t2-cumStockDiff_t1)
    cumStockDiff_t1 <- cumStockDiff_t2
  }

  return(belowGroundEmissions)
}

calc_annual_leaf_luc <- function(climate_data, agEmissions, bgEmissions, prev_data, currLand, currYear, startYear, lastYear, yr_idx, params, rhEff=TRUE, betaEff=TRUE, ccycling=TRUE){

  lucAG_sum <- sum(agEmissions[1:yr_idx])
  lucBG_sum <- sum(bgEmissions[1:yr_idx])

  currCO2 <- climate_data$co2[yr_idx]

  if (currLand == 0){
    currNPP <- 0
    currRh <- 0
    currLitter <- 0
  } else if (prev_data$land_alloc == 0 & currLand != 0 ) { # previously this was prev_data$NPP == 0
    currNPP <- params$npp_factor*currLand  # no need to call getNPP since we know exactly what this will be
    params$NPP0 <- params$npp_factor*currLand
    params$agCarbon0 <- prev_data$agCDensity*currLand
    params$bgCarbon0 <- prev_data$bgCDensity*currLand
    params$land0 <- currLand
    params$co20 <- currCO2 # update co20 value
    currLitter <- getLitter(params$agCarbon0, currLand, params$land0) # don't need to use values in params since we just updated
    currRh <- currNPP # these should start in equilibrium

  } else {
    currNPP <- getNPP(currYear, params$NPP0, startYear, currCO2, params$co20, currLand, params$land0, betaEff)
    currLitter <- getLitter(params$agCarbon0, currLand, params$land0)
    currRh <- getRh(yr_idx, params$bgCarbon0, climate_data, rhEff, currLand, params$land0)

  }

  if (ccycling){
    currAGDensity <- getCDensityAbove(yr_idx, currNPP, currLitter, prev_data$agCDensity, prev_data$land_alloc, prev_data$agCarbon, params$agCarbon0,lucAG_sum)
    currBGDensity <- getCDensityBelow(yr_idx, currNPP, currRh, currLitter, prev_data$bgCDensity, prev_data$land_alloc, prev_data$agCarbon, params$agCarbon0,lucBG_sum)

  } else {
    currAGDensity <- prev_data$agCDensity
    currBGDensity <- prev_data$bgCDensity
  }

  landDiff <- prev_data$land_alloc - currLand
  aboveCDiff <- prev_data$land_alloc*prev_data$agCDensity - currLand*currAGDensity
  belowCDiff <- prev_data$land_alloc*prev_data$bgCDensity - currLand*currBGDensity

  agEmissions <- calc_above_ground_luc_emissions(prevC=prev_data$agCarbon,
                                                 prevLand=prev_data$land_alloc,
                                                 currLand=currLand,
                                                 prevCDensity=prev_data$agCDensity,
                                                 currCDensity=currAGDensity,
                                                 year=currYear, startYear=startYear, endYear=lastYear,
                                                 matureAge=params$mature_age,
                                                 aboveGroundEmissions=agEmissions)

  bgEmissions <- calc_below_ground_luc_emissions(belowCDiff,
                                                 currYear, startYear, lastYear, params$soilTimeScale,
                                                 bgEmissions)

  currAGCarbon <- prev_data$agCarbon-agEmissions[yr_idx]
  currBGCarbon <- prev_data$bgCarbon-bgEmissions[yr_idx]

  return(list("land"=currLand,"agDensity"=currAGDensity,"bgDensity"=currBGDensity,
              "agCarbon"=currAGCarbon,
              "bgCarbon"=currBGCarbon, "bgEmissions"=bgEmissions, "agEmissions"=agEmissions,
              "NPP"=currNPP, "Rh"=currRh, "litter"=currLitter,"NPP0"=params$NPP0,"co20"=params$co20,
              "agCarbon0"=params$agCarbon0,"bgCarbon0"=params$bgCarbon0, "land0"=params$land0))

}

# assumes params is wide format
initialize_data <- function(land_alloc, params, year0, leaf_data, climate_data, rhEff,write=TRUE){
  regions <- unique(land_alloc$region)

  data_idx <- 1
  leaf_idx <- 1
  leaf_count <- 0
  leaf_names <- NULL

  leaf_data0 <- leaf_data[year=={{year0}},]
  land_alloc0 <- land_alloc[year=={{year0}},]

  for (leaf_region in regions){
    print(leaf_region)
    reg_land_alloc <- land_alloc0[region=={{leaf_region}},]
    reg_params <- params[region=={{leaf_region}},]
    leaves <- unique(reg_land_alloc$landleaf)
    leaf_count <- leaf_count + length(leaves)
    for (i in 1:length(leaves)){
      leaf <- leaves[[i]]
      leaf_names <- c(leaf_names,paste0(leaf_region,"_",leaf))
      leaf_land_alloc <- reg_land_alloc[landleaf=={{leaf}},]$value
      leaf_params <- reg_params[landleaf=={{leaf}},]

      leaf_agCarbon <- leaf_params$`above-ground-carbon-density`*leaf_land_alloc
      leaf_bgCarbon <- leaf_params$`below-ground-carbon-density`*leaf_land_alloc

      npp_factor <- get_npp_factor(leaf_params$`below-ground-carbon-density`)

      leaf_idx <- which(params$region==leaf_region & params$landleaf==leaf)
      params[leaf_idx,"npp_factor"] <- npp_factor
      params[leaf_idx,"agCarbon0"] <- leaf_agCarbon
      params[leaf_idx,"bgCarbon0"] <- leaf_bgCarbon
      params[leaf_idx,"NPP0"] <- npp_factor*leaf_land_alloc
      params[leaf_idx,"land0"] <- leaf_land_alloc

      leaf_npp <- npp_factor*leaf_land_alloc
      leaf_rh <- getRh(1, leaf_bgCarbon, climate_data,
                       rhEff, leaf_land_alloc, leaf_land_alloc)

      leaf_data[data_idx, 1:12 := list(year0,leaf_region,leaf,paste0(leaf_region,"_",leaf), leaf_land_alloc,leaf_params$`above-ground-carbon-density`,
                                       leaf_params$`below-ground-carbon-density`,leaf_agCarbon,leaf_bgCarbon,leaf_npp,
                                       leaf_rh, getLitter(leaf_agCarbon, leaf_land_alloc, leaf_land_alloc))]

      data_idx <- data_idx + 1
      leaf_idx <- leaf_idx + 1
    }
  }

  params$co20 <- climate_data$co2[1]  # all leaves start out the same. This changes depending on non zero land allocation start year

  params <- mutate(params,mature_age=`mature-age`,agCDensity=`above-ground-carbon-density`,
                   bgCDensity=`below-ground-carbon-density`) %>% select(-c("mature-age","above-ground-carbon-density","below-ground-carbon-density"))
  outdata <- list("params"=params,"leaf_data"=leaf_data, "leaf_names"=leaf_names, "leaf_count"=leaf_count, "data_idx"=data_idx)
  if (write) saveRDS(outdata,file=paste0("data/initialized_data.RDS"))
  return(outdata)
}


# TODO sub-divide into smaller functions
run_all_years <- function(land_alloc, params, ini_file, last_year=2100, stop_year=last_year, rhEff=FALSE, betaEff=FALSE, cCycling=FALSE, coupled=FALSE){

  years <- unique(land_alloc$year)
  year0 <- years[1]
  yearEnd <- last_year
  regions <- unique(land_alloc$region)

  reg_leaves <- unique(paste0(land_alloc$region,'_',land_alloc$landleaf))

  print(length(reg_leaves))
  len <- (stop_year-year0+1)*length(reg_leaves)
  print(len)

  leaf_data <- data.table::data.table(year=integer(len),region=character(len),
                                      landleaf=character(len),name=character(len),land_alloc=double(len),
                                      agCDensity=double(len), bgCDensity=double(len),
                                      agCarbon=double(len), bgCarbon=double(len),
                                      NPP=double(len), Rh=double(len), litter=double(len))

  outer_yr_idx <- 1

  # set up climate output
  climate_data <- data.table::data.table(matrix(ncol=4,nrow=stop_year-year0+1))  # only need to store climate data through run years
  colnames(climate_data) <- c("year","tas","co2","tairMean")
  climate_data[,(1:4) := lapply(.SD, as.numeric), .SDcols = c("year","tas","co2","tairMean")]

  # initialize Hector model and set initial climate data
  core <- hector::newcore(ini_file, suppresslogging = FALSE)
  hector::run(core,runtodate = year0)
  out <- hector::fetchvars(core,dates=year0:year0,vars=c(hector::GLOBAL_TAS(),hector::CONCENTRATIONS_CO2()))
  currTair <- dplyr::filter(out, variable == hector::GLOBAL_TAS())$value
  currCO2 <- dplyr::filter(out, variable == hector::CONCENTRATIONS_CO2())$value
  climate_data[1,(1:4) := list(year0,currTair,currCO2,currTair)]

  # probably don't need anymore. was part of an effort to do pre-determined indices
  data.table::setindex(land_alloc, NULL)
  data.table::setindex(leaf_data, NULL)
  data.table::setindex(climate_data, NULL)
  data.table::setindex(params, NULL)

  init_output <- initialize_data(land_alloc, params, year0, leaf_data, climate_data, rhEff=rhEff,write=TRUE)

  params <- init_output[["params"]]
  leaf_data <- init_output[["leaf_data"]]
  leaf_count <- init_output[["leaf_count"]]
  leaf_names <- init_output[["leaf_names"]]
  leaf_names1 <- paste0(params$region,"_",params$landleaf)
  data_idx <- init_output[["data_idx"]]

  # Emissions data needs to be the length of the run all the way through the final year of interest
  ag_emiss_data <- matrix(0.0,nrow=leaf_count,ncol=yearEnd-year0+1)
  row.names(ag_emiss_data) <- leaf_names
  bg_emiss_data <- matrix(0.0,nrow=leaf_count,ncol=yearEnd-year0+1)
  row.names(bg_emiss_data) <- leaf_names

  for (yr in years[2:length(years)]){
    print(yr)

    prev_yr_data <- leaf_data[leaf_data$year=={{yr}}-1,]
    curr_land_all_leaves <- land_alloc[land_alloc$year=={{yr}},]

    leaf_idx <- 1
    for (leaf_region in regions){
      print(leaf_region)
      reg_land_alloc <- curr_land_all_leaves[region=={{leaf_region}},]
      leaves <- unique(reg_land_alloc$landleaf)

      reg_leaf_data <- prev_yr_data[region=={{leaf_region}},]

      reg_params <- params[region=={{leaf_region}},]

      reg_leaf_idx <- 1

      for (leaf in leaves){

        reg_leaf_data_idx <- which(reg_leaf_data$landleaf=={{leaf}})
        prev_data <- reg_leaf_data[reg_leaf_data_idx,]

        reg_land_idx <- which(reg_land_alloc$landleaf=={{leaf}})
        curr_land <- reg_land_alloc[reg_land_idx,]$value

        leaf_params <- reg_params[which(reg_params$landleaf=={{leaf}}),]

        # get emissions vectors
        agEmissions <- ag_emiss_data[paste0(leaf_region,"_",leaf),]
        bgEmissions <- bg_emiss_data[paste0(leaf_region,"_",leaf),]

        curr_data <- calc_annual_leaf_luc(climate_data,agEmissions, bgEmissions, prev_data, curr_land, yr, year0, yearEnd, outer_yr_idx,leaf_params, rhEff, betaEff, cCycling)

        leaf_idx <- which(params$region=={{leaf_region}} & params$landleaf=={{leaf}})
        params[leaf_idx,c("agCarbon0","bgCarbon0",
                          "npp_factor", "NPP0", "co20", "land0") := list(curr_data$agCarbon0,
                                                                         curr_data$bgCarbon0,leaf_params$npp_factor,
                                                                         curr_data$NPP0,
                                                                         curr_data$co20,
                                                                         curr_data$land0)]


        leaf_data[data_idx,1:12 := list(yr,leaf_region,leaf, paste0(leaf_region,"_",leaf),
                                        curr_land, curr_data$agDensity, curr_data$bgDensity,
                                        curr_data$agCarbon, curr_data$bgCarbon,
                                        curr_data$NPP, curr_data$Rh, curr_data$litter)]

        ag_emiss_data[paste0(leaf_region,"_",leaf),] <- curr_data$agEmissions
        bg_emiss_data[paste0(leaf_region,"_",leaf),] <- curr_data$bgEmissions

        data_idx <- data_idx + 1
        leaf_idx <- leaf_idx+1
        reg_leaf_idx <- reg_leaf_idx+1
      }
    }

    luc_tot <- sum(ag_emiss_data[,outer_yr_idx]) + sum(bg_emiss_data[,outer_yr_idx])

    # send to Hector
    if (coupled) setvar(core,yr,"NBP_constrain",-luc_tot*0.001,"Pg C/yr")
    # run Hector
    hector::run(core,runtodate = yr)
    out<-fetchvars(core,dates=yr:yr,vars=c(GLOBAL_TAS(),CONCENTRATIONS_CO2()))

    currTair <- dplyr::filter(out,variable==GLOBAL_TAS())$value
    currCO2 <- dplyr::filter(out,variable==CONCENTRATIONS_CO2())$value

    annualTairMean <- getTairMean(200, outer_yr_idx, climate_data)

    outer_yr_idx <- outer_yr_idx+1
    climate_data[outer_yr_idx,(1:4) := list(yr,currTair,currCO2,annualTairMean)]

  }
  climate_output <-fetchvars(core,dates=years[1:length(years)-1],vars=c(GLOBAL_TAS(),CONCENTRATIONS_CO2(),
                                                                        NBP_CONSTRAIN(),OCEAN_UPTAKE()))
  return(list("climate"=climate_output,"leaf_data"=leaf_data, "ag_emiss"=ag_emiss_data, "bg_emiss"=bg_emiss_data, "params"=params))

}

is_equal <- function(value, compare_value){
  if (abs(value-compare_value)<1e-15){
    return(TRUE)
  } else { return(FALSE) }
}

calc_sigmoid_curve <- function(carbon_diff, init_year, start_year, end_year, mature_age, emissions_vector){
  prev_sigmoid <- 0
  for (year in seq(start_year, end_year, 1)){
    idx <- year-init_year  # this doesn't seem right...
    curr_sigmoid <- calc_sigmoid_diff(year-start_year, mature_age)
    emissions_vector[idx] <- emissions_vector[idx] + (curr_sigmoid-prev_sigmoid) * carbon_diff
    prev_sigmoid <- curr_sigmoid
  }
  return(emissions_vector)
}


calc_sigmoid_diff <- function(year_diff, mature_age){
  sigmoid_val <- (1 - exp( ( -3.0 * (year_diff + 1) ) / mature_age ) )^2.0
}
