### table of pre-model numbers for 2-year semifixed cohorts, 1st and 2nd year separate, needed for all-age & all-years smoothing


#library(fst)
library(data.table)
library(ggplot2)
library(kza)
#library(cowplot)
#library(lubridate)

## Impact of newD and ZU on derived U numbers
## Goal: table with 
## region imd sex agegroup year  D, icd, newD, U_min, U_max, U_min0, U_max0
## where U_tp0 is mortality-only deviation of U
## With 2^3 variations of smoothing (rates, numbers, final mU&U)
## Input: input_data 5-yr tables with numbers S, D, newD, icd, delta_Z0, delta_Z0S, delta_Z0D, delta_ZU, delta_ZD (created in run_test_mmorb_intersections.R)

## Steps:
## (by disease, type tp (av,min,max) & coefficient for mortality assumptions if using)
## I. by region, sex, imd, agegroup:
##  1. init2 <- initial_factors_mult_filename()
##  2. dt_data_semifix1 & 2 either 
##            a. from pre-computed file if those exist 
##            b. by numbers_from_files_fromraw() (from input_data 5-yr tables)
##  3. mortality assumption (Stroke, CHD, other diseases) & changing coefficients
##  4. mort1 <- mort_choice_full1()
##  5. smoothing: 
##            1. state_smoothing_kz_choice1() of initial numbers and/or rates
##            2. state_smoothing_kz_choice2() of mU and U 
##  6. output: dt_yr1 & 2 (into a file if tofile != 0)
##  7. depending on output_type, inserting into the cumulative table average result (U_tp or icd) between yr1 & yr2
## II. inserting 0 instead of NA into the cumulative table and saving the tables into "./data/model_output/Semifixed/full_dataset/nogaps/secondary_deaths/RatesVariationsCCG/" 
## repeat for all possible smoothing combinations

## additional parameters: 
## 1. `output_type`: "prop" = proportions, "num" = numbers, "icd" = number of icd codes
## 2. `tp`: type of estimations, "_av" = average, "_min", "_max" 
## 3. `cft`: coefficient to multiply the historic boundary (mD_chd_81 :=  cft*0.1446751)
## more can be added for other diseases and stages, see input_parameters_model.R



############################################
######### 2-year cohorts leading to lU
### for diseases *without groups by complications*

source("./auxil/input_parameters_model.R")


#source("./codes/codes_functions/initial_factors_mult2.R")
source("./auxil/initial_factors_mult_filename.R")

# source("./codes/codes_functions/initial_cens.R")
# source("./codes/codes_functions/initial_cens_sec_deaths.R")

## 2-year semifixed leading to rates
#source("./codes/codes_functions/initial_cens_2ys.R")

#source("./codes/codes_functions/initial_cens_sec_2ys.R")

#source("./codes/codes_functions/initial_cens_3gcond_2ys.R")
#source("./codes/codes_functions/initial_state_cens_3gcond_2ys_AE.R")

source("./auxil/state_cens.R")
#source("./codes/codes_functions/state_cens_3gcond.R")

source("./auxil/numbers_from_files_fromraw.R")

## mU and U
# #source("./codes/codes_functions/mort_choice.R")
# #source("./codes/codes_functions/mort_choice_full.R")
# source("./codes/codes_functions/mort_choice_full1.R")
# source("./codes/codes_functions/mort_choice_full_test.R")
# #source("./codes/codes_functions/mort_choice_full_grps.R")
# 
# # #local
# # source(data_dir_z("codes/codes_functions/mort_choice_full1.R"))
# # source(data_dir_z("codes/codes_functions/mort_choice_full_test.R"))
# 
# source("./codes/codes_functions/state_smoothing_choice.R") # both parts of smoothing
# source("./codes/codes_functions/state_smoothing_choice2_test.R") # 2nd part only, for testing newD impact
# source("./codes/codes_functions/state_smoothing_choice_kmD.R") # 1st part only, coef of mD and groups/stages

# #local
# source(data_dir_z("codes/codes_functions/state_smoothing_choice.R"))
# source(data_dir_z("codes/codes_functions/state_smoothing_choice2_test.R"))
# source(data_dir_z("codes/codes_functions/state_smoothing_choice_kmD.R"))

# source("./codes/codes_functions/visual_2gr.R")
# source("./codes/codes_functions/visual_mort.R")
# source("./codes/codes_functions/visual_2gr_smoothing.R")


##smoothing a variable by year and agegroup; 3 smoothing parameters
kz_smooth <- function(dt_fix,vrbl, m1,m2,k1){
  dt_fixm <- dcast(dt_fix, year ~ agegroup, value.var = paste0(vrbl,"_data"))
  dt_fixm <- as.matrix(dt_fixm, rownames = "year")
  
  dt_kz <- as.data.table(kza::kz(dt_fixm, m=c(m1,m2),k=k1))
  #dt_kz <- as.data.table(kza::kz(dt_fixm, m=c(2,1),k=2))
  dt_kz[, year := 2008:2019]
  setnames(dt_kz, c("V1", "V2", "V3", "V4","V5"), c("30_49", "50_59", "60_69", "70_79", "80_99"))
  dt_kz1 <- melt(dt_kz, id.vars = c("year"),
                 measure.vars = c("30_49", "50_59", "60_69", "70_79", "80_99"),
                 variable.name = "agegroup", value.name = paste0(vrbl,"_",m1,"_",m2,"_",k1))
  # dt_kz1 <- melt(dt_kz, id.vars = c("year"),
  #                measure.vars = c("30_49", "50_59", "60_69", "70_79", "80_99"),
  #                variable.name = "agegroup", value.name = paste0(vrbl,"_2_1_2"))
  dt_kz1[,":="(Region = region, imd5 = imd_5, gender = sex)]
  return(dt_kz1)
}


#d <- diseases_un[[3]]

#approx 40 min per disease; 20 min on server
system.time({
  for(d in c(diseases_un[c(F,F,F,F,F,F,F,F,F,F,F,T,F)])){

  #for(tp in tp_vect){
  
  # cft_vect <- c(1,1.25,1.5,1.75,2,2.25,2.5,2.75,3) # coefficients to multiply historic mortality boundary (CHD)
  # for(cft in cft_vect){
   
 
  ages_list1 <- list(c(30,49), c(50,59), c(60,69), c(70,79), c(80,99)) # just to create the table
  
  
  # a table to fill 
  #dt_temp <- data.table(Region = regions_new_all)
  dt_temp <- data.table(Region = regions_aa)
  #dt_temp <- data.table(Region = "All")
  dt_temp1 <- data.table(imd5 = seq(1,5))
  dt_temp <- setkey(dt_temp[,c(k=1,.SD)], k)[dt_temp1[, c(k=1,.SD)], allow.cartesian = TRUE][, ":="(k= NULL)]
  
  dt_temp1 <- data.table(gender = as.character(genders))
  dt_temp <- setkey(dt_temp[,c(k=1,.SD)], k)[dt_temp1[, c(k=1,.SD)], allow.cartesian = TRUE][, ":="(k= NULL)]
  
  dt_temp1 <- data.table(agegroup = lapply(lapply(ages_list1,"[",c(1,2)), paste0, collapse = "_"))
  dt_temp1[, agegroup := as.character(agegroup)]
  dt_temp <- setkey(dt_temp[,c(k=1,.SD)], k)[dt_temp1[, c(k=1,.SD)], allow.cartesian = TRUE][, ":="(k= NULL)]
  
  
  dt_temp1<- data.table(year = seq(2008,2019), S_data = 0, D_data = 0, icd_data = 0, newD_data = 0, 
                        delta_Z0S_data = 0, delta_Z0D_data = 0, delta_ZU_data = 0, delta_ZD_data = 0)
  dt_temp <- setkey(dt_temp[,c(k=1,.SD)], k)[dt_temp1[, c(k=1,.SD)], allow.cartesian = TRUE][, ":="(k= NULL)]
  
  rm(dt_temp1, ages_list1)
  
  dt_temp_pr <- copy(dt_temp) # proportional numbers
  dt_temp2 <- copy(dt_temp) # 2nd year
  dt_temp_pr2 <- copy(dt_temp)

  # only kz estimates
  dt_tf <- data.table(year=0, agegroup=0, "Region"=0, "imd5"=0, "gender"=0, 
                      "S_2_1_3" =0, "S_2_2_2" =0, "S_3_2_3"=0, 
                      "D_2_1_3"=0, "D_2_2_2"=0, "D_3_2_3" =0, 
                      "icd_2_1_3"=0, "icd_2_2_2"=0, "icd_3_2_3"=0, 
                      "newD_2_1_3"=0, "newD_2_2_2"=0, "newD_3_2_3"=0, 
                      "delta_Z0S_2_1_3"=0, "delta_Z0S_2_2_2"=0, "delta_Z0S_3_2_3"=0, 
                      "delta_Z0D_2_1_3"=0, "delta_Z0D_2_2_2"=0, "delta_Z0D_3_2_3" =0, 
                      "delta_ZU_2_1_3"=0, "delta_ZU_2_2_2"=0, "delta_ZU_3_2_3" =0, 
                      "delta_ZD_2_1_3" =0, "delta_ZD_2_2_2"=0, "delta_ZD_3_2_3"=0) 
  dt_tf2 <- copy(dt_tf)
  dt_tf_pr <- copy(dt_tf)
  dt_tf_pr2 <- copy(dt_tf)
    
#for(region in regions_new_all){
for(region in regions_aa){
# region <- "All"  
   for(imd in imds){
     imd_5 <- as.integer(imd[[2]])/2
      for(sex in genders){

        # ## ys-size age groups
        # for(i in seq(30,90,ys)){
        #   min_age <- i
        #   max_age <- i+ys

        ## larger age groups
        ages_list <- list(c(30,50), c(50,60), c(60,70), c(70,80), c(80,100))
        ages_list1 <- list(c(30,49), c(50,59), c(60,69), c(70,79), c(80,99))#for the table
        
        dt_t <- data.table(agegroup = lapply(lapply(ages_list1,"[",c(1,2)), paste0, collapse = "_"))
        dt_t[, agegroup := as.character(agegroup)]
        #dt_t <- setkey(dt_t[,c(k=1,.SD)], k)[dt_temp1[, c(k=1,.SD)], allow.cartesian = TRUE][, ":="(k= NULL)]
        
        dt_t1<- data.table(year = seq(2008,2019))
        dt_t <- setkey(dt_t[,c(k=1,.SD)], k)[dt_t1[, c(k=1,.SD)], allow.cartesian = TRUE][, ":="(k= NULL)]
        dt_t[,":="(Region = region, imd5 = imd_5, gender = sex)]
        rm(dt_t1)
        
        dt_t_pr <- copy(dt_t) # proportional numbers
        dt_t2 <- copy(dt_t) # 2nd year
        dt_t_pr2 <- copy(dt_t)
        
        
        #ages_list <- list(c(30,50), c(50,60), c(80,100))
        for(i in seq(1,length(ages_list))){
          min_age <- ages_list[[i]][1]
          max_age <- ages_list[[i]][2]

          agerange <- c(min_age,max_age)

# ####### testing individual group
#        region <- regions[[3]]
#        sex <- genders[[2]]
#        imd <- imds[[3]]
#        imd_5 <- as.integer(imd[[2]])/2
# 
#         # i <- 60
#         # min_age <- i
#         # max_age <- i+ys
#        ages_list <- list(c(30,50), c(50,60), c(60,70), c(70,80), c(80,100))
#         i <- 3
#         min_age <- ages_list[[i]][1]
#         max_age <- ages_list[[i]][2]
#         agerange <- c(min_age,max_age)
# #######
        
#for(region in regions){
      print(d)
      print(region)
      print(agerange)
      print(imd)
      print(sex)
      
      #labels only, no table
      init2 <- initial_factors_mult_filename(region, sex, imd, ethn, agerange[[1]], agerange[[2]], "Semifixed/full_dataset/nogaps/secondary_deaths")
      
      output_dir <- file.path("./data/model_output", paste0((init2[[3]]),"/tables"))
      
      for(yr_nu in 1:2){
        #yr_nu <- 1 
        
        # TODO: uncomment after all files are pre-created
        if(file.exists(paste0((output_dir),"/dt_result_cens_",d, paste0("_sec_2ys_",init2[[2]], "_yr", yr_nu),".csv"))){
          assign(paste0("dt_data_semifix",yr_nu), fread(paste0((output_dir),"/dt_result_cens_",d, paste0("_sec_2ys_",init2[[2]], "_yr", yr_nu),".csv")))
        }else{ # no pre-saved tables

          if(region %in% regions){
            assign(paste0("dt_data_semifix",yr_nu), numbers_from_files_fromraw(d, region, sex, imd, ethn, min_age, max_age, year_nu = yr_nu, two_year_data = TRUE, cond = FALSE))
            
          }else{
            if(region == "North East and Yorkshire"){
              region_orig <- list("Yorkshire And The Humber", "North East")
            }else if(region == "South East"){
              region_orig <- list("South East Coast", "South Central")
            }else if(region == "Midlands"){
              region_orig <- list("East Midlands", "West Midlands")
            }else if(region == "All"){
              region_orig <- as.list(regions)
            }
            assign(paste0("dt_data_semifix",yr_nu), numbers_from_files_fromraw(d, region_orig, sex, imd, ethn, min_age, max_age, year_nu = yr_nu, two_year_data = TRUE, cond = FALSE))
          }
          if (!dir.exists(output_dir)){ dir.create(output_dir, recursive = TRUE)}
          fwrite(get(paste0("dt_data_semifix",yr_nu)), paste0((output_dir),"/dt_result_cens_",d, paste0("_sec_2ys_",init2[[2]], "_yr", yr_nu),".csv"))
          
        } #pre-saved tables
      } #yr_nu   
      rm(output_dir)

      ## newD, icd & D are in dt_data_semifix1&2  
      for(yar in (2008:2019)){
        
        dt_temp[Region == region & imd5 == imd_5 & gender == sex & agegroup == paste0(agerange[1],"_", agerange[2]-1) & year == yar, 
                ":=" (S_data = dt_data_semifix1[year == yar, S],
                      D_data = dt_data_semifix1[year == yar, D], 
                      newD_data = dt_data_semifix1[year == yar, newD],
                      icd_data = dt_data_semifix1[year == yar, icd],
                      delta_Z0S_data = dt_data_semifix1[year == yar, delta_Z0S],
                      delta_Z0D_data = dt_data_semifix1[year == yar, delta_Z0D],
                      delta_ZU_data = dt_data_semifix1[year == yar, delta_ZU],
                      delta_ZD_data = dt_data_semifix1[year == yar, delta_ZD])]
        
        dt_temp2[Region == region & imd5 == imd_5 & gender == sex & agegroup == paste0(agerange[1],"_", agerange[2]-1) & year == yar, 
                ":=" (S_data = dt_data_semifix2[year == yar, S],
                      D_data = dt_data_semifix2[year == yar, D], 
                      newD_data = dt_data_semifix2[year == yar, newD],
                      icd_data = dt_data_semifix2[year == yar, icd],
                      delta_Z0S_data = dt_data_semifix2[year == yar, delta_Z0S],
                      delta_Z0D_data = dt_data_semifix2[year == yar, delta_Z0D],
                      delta_ZU_data = dt_data_semifix2[year == yar, delta_ZU],
                      delta_ZD_data = dt_data_semifix2[year == yar, delta_ZD])]
        
        dt_temp_pr[Region == region & imd5 == imd_5 & gender == sex & agegroup == paste0(agerange[1],"_", agerange[2]-1) & year == yar, 
                ":=" (S_data = dt_data_semifix1[year == yar, S_pr],
                      D_data = dt_data_semifix1[year == yar, D_pr], 
                      newD_data = dt_data_semifix1[year == yar, newD/(S+D)],
                      icd_data = dt_data_semifix1[year == yar, icd],
                      delta_Z0S_data = dt_data_semifix1[year == yar, delta_Z0S/(S+D)],
                      delta_Z0D_data = dt_data_semifix1[year == yar, delta_Z0D/(S+D)],
                      delta_ZU_data = dt_data_semifix1[year == yar, delta_ZU/(S+D)],
                      delta_ZD_data = dt_data_semifix1[year == yar, delta_ZD/(S+D)])]
        
        dt_temp_pr2[Region == region & imd5 == imd_5 & gender == sex & agegroup == paste0(agerange[1],"_", agerange[2]-1) & year == yar, 
                   ":=" (S_data = dt_data_semifix2[year == yar, S_pr],
                         D_data = dt_data_semifix2[year == yar, D_pr], 
                         newD_data = dt_data_semifix2[year == yar, newD/(S+D)],
                         icd_data = dt_data_semifix2[year == yar, icd],
                         delta_Z0S_data = dt_data_semifix2[year == yar, delta_Z0S/(S+D)],
                         delta_Z0D_data = dt_data_semifix2[year == yar, delta_Z0D/(S+D)],
                         delta_ZU_data = dt_data_semifix2[year == yar, delta_ZU/(S+D)],
                         delta_ZD_data = dt_data_semifix2[year == yar, delta_ZD/(S+D)])]
      }
      
      rm(dt_data_semifix1, dt_data_semifix2)
      
      
        }#age
        
        rm(i)
        
        # Region <- "London"
        # imd_5 <- 4
        # sex <- "F"
        
        
        
        
        ## adding smoothened variables (3 types of smoothing), by regions
        dt_fix1 <- dt_temp[Region == region & imd5 == imd_5 & gender == sex]
        dt_fix2 <- dt_temp2[Region == region & imd5 == imd_5 & gender == sex]
        dt_fix3 <- dt_temp_pr[Region == region & imd5 == imd_5 & gender == sex]
        dt_fix4 <- dt_temp_pr2[Region == region & imd5 == imd_5 & gender == sex]
        for(vrble in c("S", "D", "icd", "newD", "delta_Z0S", "delta_Z0D", 
                       "delta_ZU", "delta_ZD")){
          dt_sm <- kz_smooth(dt_fix1,vrble,2,1,3)
          dt_t <- merge(dt_t, dt_sm, by = c("year","agegroup", "Region", "imd5", "gender"), all = TRUE) 
          dt_sm <- kz_smooth(dt_fix1,vrble,2,2,2)
          dt_t <- merge(dt_t, dt_sm, by = c("year","agegroup", "Region", "imd5", "gender"), all = TRUE)
          dt_sm <- kz_smooth(dt_fix1,vrble,3,2,3)
          dt_t <- merge(dt_t, dt_sm, by = c("year","agegroup", "Region", "imd5", "gender"), all = TRUE)
          rm(dt_sm)
          
          dt_sm <- kz_smooth(dt_fix2,vrble,2,1,3)
          dt_t2 <- merge(dt_t2, dt_sm, by = c("year","agegroup", "Region", "imd5", "gender"), all = TRUE) 
          dt_sm <- kz_smooth(dt_fix2,vrble,2,2,2)
          dt_t2 <- merge(dt_t2, dt_sm, by = c("year","agegroup", "Region", "imd5", "gender"), all = TRUE)
          dt_sm <- kz_smooth(dt_fix2,vrble,3,2,3)
          dt_t2 <- merge(dt_t2, dt_sm, by = c("year","agegroup", "Region", "imd5", "gender"), all = TRUE)
          rm(dt_sm)
          
          dt_sm <- kz_smooth(dt_fix3,vrble,2,1,3)
          dt_t_pr <- merge(dt_t_pr, dt_sm, by = c("year","agegroup", "Region", "imd5", "gender"), all = TRUE) 
          dt_sm <- kz_smooth(dt_fix3,vrble,2,2,2)
          dt_t_pr <- merge(dt_t_pr, dt_sm, by = c("year","agegroup", "Region", "imd5", "gender"), all = TRUE)
          dt_sm <- kz_smooth(dt_fix3,vrble,3,2,3)
          dt_t_pr <- merge(dt_t_pr, dt_sm, by = c("year","agegroup", "Region", "imd5", "gender"), all = TRUE)
          rm(dt_sm)
          
          dt_sm <- kz_smooth(dt_fix4,vrble,2,1,3)
          dt_t_pr2 <- merge(dt_t_pr2, dt_sm, by = c("year","agegroup", "Region", "imd5", "gender"), all = TRUE) 
          dt_sm <- kz_smooth(dt_fix4,vrble,2,2,2)
          dt_t_pr2 <- merge(dt_t_pr2, dt_sm, by = c("year","agegroup", "Region", "imd5", "gender"), all = TRUE)
          dt_sm <- kz_smooth(dt_fix4,vrble,3,2,3)
          dt_t_pr2 <- merge(dt_t_pr2, dt_sm, by = c("year","agegroup", "Region", "imd5", "gender"), all = TRUE)
          rm(dt_sm)
        }
        rm(dt_fix1, dt_fix2, dt_fix3, dt_fix4)
        #vrble <- "delta_ZU"
        dt_tf <- rbind(dt_tf,dt_t)
        dt_tf2 <- rbind(dt_tf2,dt_t2)
        dt_tf_pr <- rbind(dt_tf_pr,dt_t_pr)
        dt_tf_pr2 <- rbind(dt_tf_pr2,dt_t_pr2)
        rm(dt_t, dt_t2, dt_t_pr, dt_t_pr2)
      }#sex
     rm(sex)
   }#imd
  rm(imd, imd_5)
}#region
  rm(region)
  
  dt_temp <- merge(dt_temp, dt_tf, by = c("year", "agegroup", "Region", "imd5", "gender"))
  dt_temp2 <- merge(dt_temp2, dt_tf2, by = c("year", "agegroup", "Region", "imd5", "gender"))
  dt_temp_pr <- merge(dt_temp_pr, dt_tf_pr, by = c("year", "agegroup", "Region", "imd5", "gender"))
  dt_temp_pr2 <- merge(dt_temp_pr2, dt_tf_pr2, by = c("year", "agegroup", "Region", "imd5", "gender"))
  
  rm(dt_tf,dt_tf2,dt_tf_pr,dt_tf_pr2)
  
  output_dir1 <- file.path(paste0("./data/model_input/input_numbers/Semifixed/skz")) 
  if (!dir.exists(output_dir1)){ dir.create(output_dir1, recursive = TRUE)}
  fwrite(dt_temp, paste0(output_dir1,"/semifixed_",d, nuyrs,"_yr1.csv"))
  fwrite(dt_temp2, paste0(output_dir1,"/semifixed_",d, nuyrs,"_yr2.csv"))
  fwrite(dt_temp_pr, paste0(output_dir1,"/semifixed_pr_",d, nuyrs,"_yr1.csv"))
  fwrite(dt_temp_pr2, paste0(output_dir1,"/semifixed_pr_",d, nuyrs,"_yr2.csv"))
  rm(output_dir1)
  
  
  

}#disease      
})


# ########## stats
# output_dir1 <- file.path(paste0("./data/model_input/input_numbers/Semifixed/skz")) 
# dt_sem1 <- fread(paste0(output_dir1,"/semifixed_",d, nuyrs,"_yr1.csv"))
# rm(output_dir1)
# 
# colnames(dt_sem1)
# dt_sem1[, k_0 := 2*D_data*delta_ZU_data/(S_data*delta_ZD_data)]
# dt_sem1[, k_est1 := 2*D_2_1_3*delta_ZU_2_1_3/(S_2_1_3*delta_ZD_2_1_3)]
# dt_sem1[, k_est2 := 2*D_2_2_2*delta_ZU_2_2_2/(S_2_2_2*delta_ZD_2_2_2)]
# dt_sem1[, k_est3 := 2*D_3_2_3*delta_ZU_3_2_3/(S_3_2_3*delta_ZD_3_2_3)]
# 
# dt_sem1[, k_est1_data := 2*D_data*delta_ZU_2_1_3/(S_data*delta_ZD_2_1_3)]
# dt_sem1[, k_est2_data := 2*D_data*delta_ZU_2_2_2/(S_data*delta_ZD_2_2_2)]
# dt_sem1[, k_est3_data := 2*D_data*delta_ZU_3_2_3/(S_data*delta_ZD_3_2_3)]
# dt_sem1[,.SD, .SDcols = c(1:5,35:length(dt_sem1))]
# summary(dt_sem1)
# dt_sem1[k_0 == Inf,.N] #802
# dt_sem1[is.na(k_0),.N] #2107
# 
# dt_sem1[k_est3 == Inf,.N] #8
# dt_sem1[is.na(k_est3),.N] #4
# 
# dt_sem1[k_est3_data == Inf,.N] #6
# dt_sem1[is.na(k_est3_data),.N] #6
# View(dt_sem1[is.na(k_est3) | k_est3 == Inf])
# ## EM 2008, 2009, 2010, 2011 F imd3,imd5 30-49 (*8)
# ## EM 2012 F imd5 30_49
# ## EE 2008 F imd3 30_49
# ## EE 2013,2014 imd5 30_49 (*2)
# 
# dt_sem1[!(is.na(k_est3) | k_est3 == Inf), summary(k_est3)] # max = 5
# View(dt_sem1[!(is.na(k_est3) | k_est3 == Inf) & k_est3 > 1]) # 13 require k >1?
# dt_sem1[!(is.na(k_est3) | k_est3 == Inf) & k_est3 > 1, .N, by = agegroup]
# # agegroup N
# # 1:    30_49 9
# # 2:    80_99 4
# dt_sem1[!(is.na(k_est3) | k_est3 == Inf) & k_est3 > 1 & agegroup == "80_99"]
# #    year agegroup        Region imd5 gender 
# # 1: 2008    80_99    North East    1      F  1.599760
# # 2: 2009    80_99    North East    1      F  1.103456
# # 3: 2011    80_99 East Midlands    5      M  1.075804
# # 4: 2012    80_99 East Midlands    5      M  1.112452
# ## different from glm
