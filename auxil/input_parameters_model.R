## Input with the new panel

library(fst)
library(data.table)
#library(bit64)
#library(ggplot2)
#library(cowplot)
#library(lubridate)

#oldset <- FALSE

# files from AA
data_dir_aa <-
  function(x = character(0)){
    paste0("./UNMET_002_SAE_CCG/UNMET_002_SAE_CCG/", x)
  }


# files from KD
data_dir_kd <-
  function(x = character(0)){
    paste0("../../../UNCCG/", x)
  }

# files on z
data_dir_z <-
  function(x = character(0)){
    paste0("C:/Users/oanosova/ODA/UnmetNeed/U6Model/", x)
  }

### regions:
regions <- sort(c(#"All",
                  "East Midlands",
                  "East of England",
                  "London",
                  "North East",
                  "North West",
                  "South Central",
                  "South East Coast",
                  "South West",
                  "West Midlands",
                  "Yorkshire And The Humber"
))

## redefined regions for maps - get rid?
regions_new_all <- c(#"All",
                     "East of England",
                     "London",
                     "Midlands",
                     "North East and Yorkshire",
                     "North West",
                     "South East",
                     "South West"
)

#for AA maps
regions_aa <- sort(c(#"All",
  "East Midlands",
  "East of England",
  "London",
  "North East",
  "North West",
  #"South Central",
  #"South East Coast",
  "South East",
  "South West",
  "West Midlands",
  "Yorkshire And The Humber"
))

### newly redefined larger regions for maps - get rid?
#regions_new <- c("Midlands", "North East and Yorkshire", "South East")


diseases_un <- c("Anxiety_Depression", 
                 "Asthma", 
                 "CHD", #3
                 "COPD", #4
                 "Dementia", #5
                 "Hypertension", #6
                 "Primary Malignancy_Breast", 
                 "Primary Malignancy_Colorectal", 
                 "Primary Malignancy_Lung", #9 
                 "Primary Malignancy_Melanoma", 
                 "Primary Malignancy_Prostate", #11 
                 "Stroke", #12
                 "Type 2 Diabetes Mellitus") 
# if(oldset == FALSE){
#   diseases_un <- gsub(" ", "_", diseases_un) # getting rid of spaces # DON'T or causes of death will not work
# }

# ## all conditions in the panel
# diseasenames <- c("Anxiety_Depression", 
#                   "Asthma", 
#                   "CHD", 
#                   "COPD", 
#                   "Dementia", 
#                   "Heart failure", 
#                   "Hypertension",
#                   "Primary Malignancy_Breast",
#                   "Primary Malignancy_Colorectal",
#                   "Primary Malignancy_Lung",
#                   "Primary Malignancy_Melanoma",
#                   "Primary Malignancy_Prostate",
#                   "Stroke",
#                   "Type 2 Diabetes Mellitus",
#                   "Intentional self_harm",
#                   "Hypertension_camb",
#                   "Psychosis_camb",
#                   "Asthma_spell",
#                   "Anxiety_Depression_spell")
#   


imds <- list(#c("All"),
  c("1","2"),
  c("3","4"),
  c("5","6"),
  c("7","8"),
  c("9","10")
)

genders <- list(#"All", 
  "F", "M")

ethnicities <- list("All")
# ethnicities <- list("All", 
#                     "white", 
#                     "indian", 
#                     "bl_carib", 
#                     "chinese", 
#                     "other", 
#                     "bl_afric", 
#                     "bangladeshi", 
#                     "pakistani", 
#                     "oth_asian")


# #### Datasets - not needed if model from pre-computed numbers
# 
# if(oldset == FALSE){
#   dt_panel <- read_fst("./AH/data_ah_tmp/panel.fst", as.data.table = TRUE)
#   dt_panel[, e_patid := as.integer64(e_patid)]
#   #colnames(dt_panel) <- gsub(" ", "_", colnames(dt_panel)) # getting rid of spaces
# }else{ #old panel
#   ## dataset from input_data_prep-sept22.R
#   #dt_full_nogaps_cond <- read_fst("./data/model_input/NewJuly22/dt_full_nogaps_cond1.fst", as.data.table = TRUE)
#   #dt_full_nogaps_cond_t2dm <- read_fst("./data/model_input/NewJuly22/dt_full_nogaps_cond_t2dm.fst", as.data.table = TRUE)
#   
#   dt_full_nogaps_cond <- read_fst("./data/model_input/NewJuly22/dt_full_nogaps_cond_grps.fst", as.data.table = TRUE)
#   
#   dt_full_nogaps_cond[, linked_id := e_patid] # to run older codes
#   
#   ## ATTENTION: deaths causes
#   ## `death_cause` = 
#   ## `dis_death` = 
#   ## `dis_diagn` = 
#   
#   #dt_full_nogaps_cond[, Disease := death_cause]
#   dt_full_nogaps_cond[, Disease := dis_death]
#   
# }


#### input parameters

## ethnicity
ethn <- ethnicities[[1]]

# ## mD coefficients 
k1 <- 0.05 #from 19/07
k2 <- 1
# k1 <- 0.1
# k2 <- 0.5


##disease-specific coefficients
disease_specific <- FALSE
#disease_specific <- TRUE

koeff <- function(d1, disease_spec, prim){
  if(disease_spec == TRUE){
    if(d1 == diseases_un[1]){#AD
      if(prim == FALSE){
        koef1 <- 0.8 
      }else{
        koef1 <- 0.9
      }
      koef2 <- 5
    }else if(d1 == diseases_un[3]){#CHD
      if(prim == FALSE){
        koef1 <- 0.4 
      }else{
        koef1 <- 0.5
      }
      koef2 <- 2.4
    }else if(d1 == diseases_un[4]){#COPD
      if(prim == FALSE){
        koef1 <- 0.16 
      }else{
        koef1 <- 0.14
      }
      koef2 <- 0.82
    }else if(d1 == diseases_un[5]){#Dementia
      if(prim == FALSE){
        koef1 <- 0.18 
      }else{
        koef1 <- 0.14
      }
      koef2 <- 0.5
    }else if(d1 == diseases_un[6]){#Hyper
      if(prim == FALSE){
        koef1 <- 0.2 
      }else{
        koef1 <- 0.15
      }
      koef2 <- 1.3
    }else if(d1 == diseases_un[7]){#Breast
      if(prim == FALSE){
        koef1 <- 0.25 
      }else{
        koef1 <- 0.23
      }
      koef2 <- 1.1
    }else if(d1 == diseases_un[8]){#Colorectal
      koef1 <- 0.5 
      koef2 <- 2.1
    }else if(d1 == diseases_un[9]){#Lung
      koef1 <- 0.7 
      koef2 <- 1.6
    }else if(d1 == diseases_un[10]){#Melanoma
      koef1 <- 0.2 
      koef2 <- 1
    }else if(d1 == diseases_un[11]){#Prostate
      koef1 <- 0.16 
      koef2 <- 0.7
    }else if(d1 == diseases_un[12]){#Stroke
      if(prim == FALSE){
        koef1 <- 1.27 
      }else{
        koef1 <- 1.6
      }
      koef2 <- 6.6
    }else if(d1 == diseases_un[13]){#T2DM
      koef1 <- 0.04 
      koef2 <- 0.25
    }
    return(c(koef1,koef2))
  }
}


## year step
ys <- 5

### diseases

## single
#d <- diseases_list1[[1]]
#d <- diseases_un[[9]]

## multiple
#d_mult <- c("CHD","Stroke")
#d_mult <- c(diseases_un[[2]], diseases_un[[3]], diseases_un[[4]], disease_un[[5]])
#d_mult <- c(diseases_un[c(F,F,T,T,T,T,F,F,F,F,F,T,T)])


# sex <- genders[[2]] 
# imd <- imds[[3]]

## single region 
#region <- regions[[1]]

## multiple regions
#reg_list <- list(regions[[2]],regions[[10]])


######## for the groups of complications

### with/without GROUPS  
groups <- FALSE # with/without groups
#groups <- TRUE # with/without groups

## number of lag years (for file names)
if(groups == FALSE){
  nuyrs <- NULL
}else{#with groups of complications
  #grp2_lag <- 2
  #grp3_lag <- 3
  #nuyrs <- paste0("_", max(lag_gr2,lag_gr3, na.rm = TRUE))  
  #nuyrs <- "_2yrs"
  #nuyrs <- "_5yrs"
}

## for the file names ## redundant??
lagnuyrs <- NULL
#lagnuyrs <- "_AE" # AE codes, unfinished
#lagnuyrs <- "_lag2ys"
#lagnuyrs <- "_lag5ys"

# ## for calculations of complications
# ## 2 group types for T2DM; only grp2 for Hypertension
# grp2_lag <- 1
# grp3_lag <- 5

## min or average of the estimates
tp_vect <- list(av = "_av", min = "_min", max = "_max", ae = "_ae")
#tp_vect <- c("_av", "_min", "_max")
#tp <- "_av"
#tp <- "_min"
tp <- tp_vect[2]


## proportions vs raw numbers vs icd
output_type <- "prop"
#output_type <- "num"
#output_type <- "icd"
#output_type_vect <- c("prop", "num", "icd")


# # coefficients to multiply historic mortality boundary (CHD)
# cft_vect <- c(1,1.25,1.5,1.75,2,2.25,2.5,2.75,3) 
# cft <- cft_vect[1]
cft <- 1

glm_est <- TRUE # glm vs kz estimates of mD_all and ZU numbers
pop_estim <- "midyr" # "midyr" = mid-year estimates for 1981 rate; alternative: "census"

primary <- FALSE
#primary <- TRUE
if(primary == TRUE){
  insrt <- "_prim"
  insrt1 <- "prim"
}else{
  insrt <- NULL 
  insrt1 <- "sec"
}

newDind <- FALSE
#newDind <- TRUE
if(newDind == TRUE){
  newDlbl <- "_newD" # separate ZnewD state
}else{
  newDlbl <- NULL # if old method, no separate ZnewD state
} 
