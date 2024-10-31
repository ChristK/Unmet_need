## reading files with pre-computed numbers *from ./data/model_input/input_numbers* 
## regions from list
## for each disease
## pre-computed files: 5-year age groups, imd decile pairs (aka c("1","2")), gender F & M, region, ethnicity = All
## (location: \data\model_output\Semifixed\full_dataset\nogaps\secondary_deaths\"region"\GenderImd\tables or
##  \data\model_output\app\full_dataset\nogaps\secondary_deaths\region\GenderImd\tables)

## Input: region_list, gender (F/M/All), list of imds (consecutive, in decile pairs), ethnicity (set to "All" in pre-created tables), 
## min age, max age (both divisible by 5),  
## start_year of the panel data (2008), nt = number of years to follow, 
## year_nu = year number for 2-yr data, should be 1 or 2, 
## two_year_data = logical, TRUE if loading 2-yr data

## Output: data.table with summative numbers  "S", "delta_S, "newD", "D", 
## "delta_D", "delta_Z0", "delta_Z0S", "delta_Z0D", "delta_ZU", "delta_ZD" 
## and  corresponding rates "m_0", "m_0S", "m_0D", "m_D", "S_pr", "D_pr" 


# ####### testing individual group
#         ethn <- ethnicities[[1]]
#         #start_yr <- 2008
#         #nt <- 11
# 
#         #two_year_data <- TRUE
#         #year_nu <- 1
# 
#         d <- diseases_list1[[4]]
#         region <- regions[[2]]
# 
#         sex_sum <- genders[[2]]
#         #imd_sum <- c("1","2", "3","4","5","6","7","8","9","10")
#         imd_sum <- c("1","2")
# 
# 
#        i <- 80
#        ys <- 20
#        min_age <- i
#        max_age <- i+ys
#
#
#          reg_list <- list(regions[[2]],regions[[10]])
# ######
# 
#          disease <- d 
#dt_test <- numbers_from_files_fromraw(d, reg_list, sex_sum, imd_sum, ethn, min_age, max_age, lagadd = "_lag2ys", year_nu = 1, two_year_data = TRUE, cond = TRUE)
#dt_test <- numbers_from_files_fromraw(d, region, sex, imd, ethn, min_age, max_age, year_nu = 1, two_year_data = TRUE, cond = FALSE)
        
numbers_from_files_fromraw <- function(disease, regions_list, sex_sum, imd_sum, ethn, min_age, max_age, lagadd = NULL, year_nu = 1, two_year_data = TRUE, cond = FALSE, start_yr = 2008, nt = 11){        
  source("./codes/codes_functions/initial_factors_mult_filename.R")
  
  
  print(paste(disease, regions_list, "sex=", sex_sum, "imd = ", paste0(imd_sum, collapse = "&"), ethn, 
              "ages =", min_age,"-", max_age, "two years=", two_year_data, "year_nu=", year_nu))
  
  imds_tmp <- list(c("All"),
               c("1","2"),
               c("3","4"),
               c("5","6"),
               c("7","8"),
               c("9","10")
  )
  
  
  
  if(cond == FALSE){
    dt_sum <- suppressWarnings(data.table(times = seq(0,nt), year = seq(start_yr,start_yr+nt), S = integer(), 
                                                          D = integer(), newD = integer(), icd = integer(), delta_Z0 = integer(), 
                                                          delta_Z0S = integer(), delta_Z0D = integer(), delta_ZU = integer(), delta_ZD = integer()))
    c_names <- c("times", "year", "S", "D", "newD", "icd", "delta_Z0", "delta_Z0S", "delta_Z0D", "delta_ZU", "delta_ZD")
  }else{ # cond = T
    dt_sum <- suppressWarnings(data.table(times = seq(0,nt), year = seq(start_yr,start_yr+nt), S = integer(), 
                                          D = integer(), newD = integer(), icd = integer(), preD2 = integer(), preD3 = integer(), delta_Z0 = integer(), 
                                          delta_Z0S = integer(), delta_Z0D = integer(), delta_ZU = integer(), delta_ZD = integer()))
    c_names <- c("times", "year", "S", "D", "newD", "icd", "preD2", "preD3", "delta_Z0", "delta_Z0S", "delta_Z0D", "delta_ZU", "delta_ZD")
  }
  
  
  if("All" %in% imd_sum){imd_sum <- c("1","2", "3","4","5","6","7","8","9","10")}
  
  if(sex_sum == "All"){
    gender_tmp = list("F", "M")
  }else{
    gender_tmp = list(sex_sum)
  }
  
  for (region in regions_list){
  for(sex in gender_tmp){
    
    imd_first <- (min(as.integer(imd_sum)) +1)/2 +1
    k1 <- (max(as.integer(imd_sum)) - min(as.integer(imd_sum)) - 1)/2
    
    for(k in c(0:k1)){
      imd <- imds_tmp[[imd_first + k]]
      
      
      #if(two_year_data == TRUE){ # yr1, yr2
        agegrps <- ((max_age - 5) - min_age)/5
        
        for(i1 in c(0:agegrps)){ # adding tables into one
          a1 <- min_age + 5*i1
          a2 <- a1+5 
          print(paste(a1, a2, sex,paste0(imd, collapse = "&")))
          
          # loading the table from file
          init1 <- initial_factors_mult_filename(region, sex, imd, ethn, a1, a2, "Semifixed/full_dataset/nogaps/secondary_deaths")
          output_dir_tmp <- file.path("./data/model_input/input_numbers", paste0((init1[[3]]),"/tables",lagadd))
          dt_load <- fread(paste0((output_dir_tmp),"/dt_init_cens_",disease, paste0("_sec_2ys_",init1[[2]], "_yr", year_nu),".csv"))
          rm(output_dir_tmp)
          
          # adding the numbers from the new table
          dt_sum <- rbind(dt_sum, dt_load[, .SD, .SDcols = c_names])
          if(cond== TRUE){ # with preD2 and preD3 groups of pre-existing conditions
            dt_sum[, ":=" (S = sum(S, na.rm = TRUE), D = sum(D, na.rm = TRUE), newD = sum(newD, na.rm = TRUE),
                           icd = sum(icd, na.rm = TRUE),
                           preD2 = sum(preD2, na.rm = TRUE), preD3 = sum(preD3, na.rm = TRUE),
                           delta_Z0 = sum(delta_Z0, na.rm = TRUE),
                           delta_Z0S = sum(delta_Z0S, na.rm = TRUE), 
                           delta_Z0D = sum(delta_Z0D, na.rm = TRUE),
                           delta_ZU = sum(delta_ZU, na.rm = TRUE), 
                           delta_ZD = sum(delta_ZD, na.rm = TRUE)), by = c("times", "year")]
          }else{ # no preDi
            dt_sum[, ":=" (S = sum(S, na.rm = TRUE), D = sum(D, na.rm = TRUE), newD = sum(newD, na.rm = TRUE),
                           icd = sum(icd, na.rm = TRUE),
                           delta_Z0 = sum(delta_Z0, na.rm = TRUE),
                           delta_Z0S = sum(delta_Z0S, na.rm = TRUE), 
                           delta_Z0D = sum(delta_Z0D, na.rm = TRUE),
                           delta_ZU = sum(delta_ZU, na.rm = TRUE), 
                           delta_ZD = sum(delta_ZD, na.rm = TRUE)), by = c("times", "year")]
            
          }
          dt_sum <- dt_sum[c(1:(nrow(dt_sum)/2))]
          rm(dt_load)
          }
        rm(i1,a1,a2, init1)
      # }else{ # 1-year tables, without preDi
      #   agegrps <- ((max_age - 5) - min_age)/5
      #   
      #   for(i1 in c(0:agegrps)){
      #     a1 <- min_age + 5*i1
      #     a2 <- a1+5 
      #     print(paste(a1,a2, sex,paste0(imd, collapse = "&")))
      #     
      #     init1 <- initial_factors_mult_filename(region, sex, imd, ethn, a1, a2, "app/full_dataset/nogaps/secondary_deaths")
      #     output_dir_tmp <- file.path("./data/model_output", paste0((init1[[3]]),"/tables",lagadd))
      #     dt_load <- fread(paste0((output_dir_tmp),"/dt_result_cens_",disease, paste0("_sec_",init1[[2]]),".csv"))
      #     rm(output_dir_tmp)
      #     
      #     dt_sum <- rbind(dt_sum, dt_load[, .SD, .SDcols = c_names])
      #     dt_sum[, ":=" (S = sum(S, na.rm = TRUE), D = sum(D, na.rm = TRUE), newD = sum(newD, na.rm = TRUE),
      #                    delta_Z0 = sum(delta_Z0, na.rm = TRUE),
      #                    delta_Z0S = sum(delta_Z0S, na.rm = TRUE), 
      #                    delta_Z0D = sum(delta_Z0D, na.rm = TRUE),
      #                    delta_ZU = sum(delta_ZU, na.rm = TRUE), 
      #                    delta_ZD = sum(delta_ZD, na.rm = TRUE)), by = c("times", "year")]
      #     dt_sum <- dt_sum[c(1:(nrow(dt_sum)/2))]
      #     rm(dt_load)
      #     
      #   }
      #   rm(i1,a1,a2, init1)
      # }
      rm(agegrps, imd)
     
      
    }
    rm(k,imd_first, k1)
  }
  }
  rm(sex,region)
  
  #state_cens part:
  dt_sum[, delta_S := -newD - delta_Z0S - delta_ZU][, delta_D := newD - delta_Z0D - delta_ZD]
  dt_sum[, ":=" (S_pr = S/(S+D), D_pr = D/(S+D)) ]
  
  dt_sum[, m_0 := delta_Z0/(S+D)][D != 0, m_D := delta_ZD/D][, m_0S := delta_Z0S/S][D != 0, m_0D := delta_Z0D/D]
  
  if(cond == FALSE){
    setcolorder(dt_sum,c("times","year","S", "delta_S", "newD", "icd", "D", "delta_D", 
                         "delta_Z0", "delta_Z0S", "delta_Z0D", "delta_ZU", "delta_ZD",
                         "m_0", "m_0S", "m_0D", "m_D",
                         "S_pr","D_pr"))
  }else{ # cond = T
    setcolorder(dt_sum,c("times","year","S", "delta_S", "newD", "icd", "preD2", "preD3", "D", "delta_D", 
                         "delta_Z0", "delta_Z0S", "delta_Z0D", "delta_ZU", "delta_ZD",
                         "m_0", "m_0S", "m_0D", "m_D",
                         "S_pr","D_pr"))
  }
    

  rm(gender_tmp, imds_tmp)
  return(dt_sum)
}

