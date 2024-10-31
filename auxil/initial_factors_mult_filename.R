###################### FOR THE NAMES OF FILES OF PRE-CALC TABLES ONLY, filtering by multiple factors - not age, full cohort

# gender,imd,eth,reg can be "All" or from a list; 2^4=16 variants;
# gender & region are chosen from mutually exclusive cases; imd & ethn can include several from the list
# additional_dirlab = additional label for the top directory, 0 for none
# output: table to be fed into initial_cens (which can then be altered), with dt, ageg, label

######### groups of multiple ethnicity and imd10; ethn and imd as vectors; not filter by age, label only

initial_factors_mult_filename <- function(reg, gndr, imd, ethn, min_age, max_age, additional_dirlab){
  
  dt <- data.table(region = integer(),
                   gender = integer(), 
                   imd10 = integer(), 
                   ethnicity = integer(),
                   age = integer())
  ## indicator column for imd from the vector
  for(imdval in imd){
    dt[imd10 == imdval, imd_ind:= 1]
  }
  
  ## indicator column for ethnicity from the vector
  for(ethnval in ethn){
    dt[ethnicity == ethnval, ethn_ind:= 1]
  }
  
  
  if(additional_dirlab == 0){
    if(reg == "All"){
      if(gndr == "All" & ("All" %in% imd) & ("All" %in% ethn)){ # could use `all(imd == "All")`
        dirlab <- paste0("All")
        ageg <- paste0(min_age,"_",max_age)
        dt1 <- dt
        #dt1 <- dt[yob>2008-max_age & yob<=2008-min_age]
      }else if(gndr != "All" & ("All" %in% imd) & ("All" %in% ethn)){
        dirlab <- paste0("All/Gender")
        ageg <- paste0(gndr,"_",min_age,"_",max_age)
        dt1 <- dt[gender == gndr] 
        #dt1 <- dt[yob>2008-max_age & yob<=2008-min_age & gender == gndr] 
      }else if(gndr == "All" & !("All" %in% imd) & ("All" %in% ethn)){
        dirlab <- paste0("All/Imd")
        ageg <- paste0(paste0("imd",imd, collapse = "_"), "_", min_age,"_",max_age)
        #ageg <- paste0("imd",imd,"_",min_age,"_",max_age)
        dt1 <- dt[imd_ind == 1] 
        #dt1 <- dt[yob>2008-max_age & yob<=2008-min_age & imd_ind == 1] 
      }else if(gndr == "All" & ("All" %in% imd) & !("All" %in% ethn)){
        dirlab <- paste0("All/Ethnicity")
        ageg <- paste0(paste0("ethn",ethn, collapse = "_"), "_", min_age,"_",max_age)
        #ageg <- paste0(ethn,"_",min_age,"_",max_age)
        dt1 <- dt[ethn_ind == 1]  
        #dt1 <- dt[yob>2008-max_age & yob<=2008-min_age & ethn_ind == 1] 
      }else if(gndr == "All" & !("All" %in% imd) & !("All" %in% ethn)){
        dirlab <- paste0("All/ImdEthnicity")
        ageg <- paste0(paste0("imd",imd, collapse = "_"),"_",paste0("ethn",ethn, collapse = "_"),"_",min_age,"_",max_age)
        #ageg <- paste0(paste0("imd",imd, collapse = "_"),"_",ethn,"_",min_age,"_",max_age)
        #ageg <- paste0("imd",imd,"_",ethn,"_",min_age,"_",max_age)
        dt1 <- dt[imd_ind == 1 & ethn_ind == 1] 
        #dt1 <- dt[yob>2008-max_age & yob<=2008-min_age & imd_ind == 1 & ethn_ind == 1]
      }else if(gndr != "All" & ("All" %in% imd) & !("All" %in% ethn)){
        dirlab <- paste0("All/GenderEthnicity")
        ageg <- paste0(gndr, "_",paste0("ethn",ethn, collapse = "_"),"_",min_age,"_",max_age)
        #ageg <- paste0(gndr, "_",ethn,"_",min_age,"_",max_age)
        dt1 <- dt[gender == gndr & ethn_ind == 1]  
        #dt1 <- dt[yob>2008-max_age & yob<=2008-min_age & gender == gndr & ethn_ind == 1]
      }else if(gndr != "All" & !("All" %in% imd) & ("All" %in% ethn)){
        dirlab <- paste0("All/GenderImd")
        ageg <- paste0(gndr,"_",paste0("imd",imd, collapse = "_"),"_",min_age,"_",max_age)
        dt1 <- dt[gender == gndr & imd_ind == 1] 
        #dt1 <- dt[yob>2008-max_age & yob<=2008-min_age & gender == gndr & imd_ind == 1]
      }else{
        dirlab <- paste0("All/GenderEthnicityImd")
        ageg <- paste0(gndr,"_",paste0("ethn",ethn, collapse = "_"),"_",paste0("imd",imd, collapse = "_"),"_",min_age,"_",max_age)
        dt1 <- dt[gender == gndr & ethn_ind == 1 & imd_ind == 1]  
        #dt1 <- dt[yob>2008-max_age & yob<=2008-min_age & gender == gndr & ethn_ind == 1 & imd_ind == 1]
      }
    }else{ # reg!="All"
      if(gndr == "All" & ("All" %in% imd) & ("All" %in% ethn)){
        dirlab <- paste0(reg)
        ageg <- paste0(min_age,"_",max_age)
        dt1 <- dt[region == reg]
        #dt1 <- dt[yob>2008-max_age & yob<=2008-min_age & region == reg]
      }else if(gndr != "All" & ("All" %in% imd) &("All" %in% ethn)){
        dirlab <- paste0(reg,"/Gender")
        ageg <- paste0(gndr,"_",min_age,"_",max_age)
        dt1 <- dt[gender == gndr & region == reg] 
        #dt1 <- dt[yob>2008-max_age & yob<=2008-min_age & gender == gndr & region == reg]
      }else if(gndr == "All" & !("All" %in% imd) & ("All" %in% ethn)){
        dirlab <- paste0(reg,"/Imd")
        ageg <- paste0(paste0("imd",imd, collapse = "_"),"_",min_age,"_",max_age)
        dt1 <- dt[imd_ind == 1 & region == reg]  
        #dt1 <- dt[yob>2008-max_.age & yob<=2008-min_age & imd_ind == 1 & region == reg]
      }else if(gndr == "All" & ("All" %in% imd) & !("All" %in% ethn)){
        dirlab <- paste0(reg,"/Ethnicity")
        ageg <- paste0(paste0("ethn",ethn, collapse = "_"),"_",min_age,"_",max_age)
        dt1 <- dt[ethn_ind == 1 & region == reg]    
        #dt1 <- dt[yob>2008-max_age & yob<=2008-min_age & ethn_ind == 1 & region == reg]
      }else if(gndr == "All" & !("All" %in% imd) & !("All" %in% ethn)){
        dirlab <- paste0(reg,"/ImdEthnicity")
        ageg <- paste0(paste0("imd",imd, collapse = "_"),"_",paste0("ethn",ethn, collapse = "_"),"_",min_age,"_",max_age)
        dt1 <- dt[imd_ind == 1 & ethn_ind == 1 & region == reg]  
        #dt1 <- dt[yob>2008-max_age & yob<=2008-min_age & imd_ind == 1 & ethn_ind == 1 & region == reg]
      }else if(gndr != "All" & ("All" %in% imd) & !("All" %in% ethn)){
        dirlab <- paste0(reg,"/GenderEthnicity")
        ageg <- paste0(gndr, "_",paste0("ethn",ethn, collapse = "_"),"_",min_age,"_",max_age)
        dt1 <- dt[gender == gndr & ethn_ind == 1 & region == reg] 
        #dt1 <- dt[yob>2008-max_age & yob<=2008-min_age & gender == gndr & ethn_ind == 1 & region == reg]
      }else if(gndr != "All" & !("All" %in% imd) & ("All" %in% ethn)){
        dirlab <- paste0(reg,"/GenderImd")
        ageg <- paste0(gndr,"_",paste0("imd",imd, collapse = "_"),"_",min_age,"_",max_age)
        dt1 <- dt[gender == gndr & imd_ind == 1 & region == reg]  
        #dt1 <- dt[yob>2008-max_age & yob<=2008-min_age & gender == gndr & imd_ind == 1 & region == reg]
      }else{
        dirlab <- paste0(reg,"/GenderEthnicityImd")
        ageg <- paste0(gndr,"_",paste0("ethn",ethn, collapse = "_"),"_",paste0("imd",imd, collapse = "_"),"_",min_age,"_",max_age)
        dt1 <- dt[gender == gndr & ethn_ind == 1 & imd_ind == 1 &region == reg] 
        #dt1 <- dt[yob>2008-max_age & yob<=2008-min_age & gender == gndr & ethn_ind == 1 & imd_ind == 1 &region == reg]
      }
      
    }}else{ # additional_dirlab != 0
      if(reg == "All"){
        if(gndr == "All" & ("All" %in% imd) & ("All" %in% ethn)){
          dirlab <- paste0(additional_dirlab,"/All")
          ageg <- paste0(min_age,"_",max_age)
          dt1 <- dt
          #dt1 <- dt[yob>2008-max_age & yob<=2008-min_age]
        }else if(gndr != "All" & ("All" %in% imd) &("All" %in% ethn)){
          dirlab <- paste0(additional_dirlab,"/All/Gender")
          ageg <- paste0(gndr,"_",min_age,"_",max_age)
          dt1 <- dt[gender == gndr] 
          #dt1 <- dt[yob>2008-max_age & yob<=2008-min_age & gender == gndr]
        }else if(gndr == "All" & !("All" %in% imd) & ("All" %in% ethn)){
          dirlab <- paste0(additional_dirlab,"/All/Imd")
          ageg <- paste0(paste0("imd",imd, collapse = "_"),"_",min_age,"_",max_age)
          dt1 <- dt[imd_ind == 1] 
          #dt1 <- dt[yob>2008-max_age & yob<=2008-min_age & imd_ind == 1]
        }else if(gndr == "All" & ("All" %in% imd) & !("All" %in% ethn)){
          dirlab <- paste0(additional_dirlab,"/All/Ethnicity")
          ageg <- paste0(paste0("ethn",ethn, collapse = "_"),"_",min_age,"_",max_age)
          dt1 <- dt[ethn_ind == 1] 
          #dt1 <- dt[yob>2008-max_age & yob<=2008-min_age & ethn_ind == 1]
        }else if(gndr == "All" & !("All" %in% imd) & !("All" %in% ethn)){
          dirlab <- paste0(additional_dirlab,"/All/ImdEthnicity")
          ageg <- paste0(paste0("imd",imd, collapse = "_"),"_",paste0("ethn",ethn, collapse = "_"),"_",min_age,"_",max_age)
          dt1 <- dt[imd_ind == 1 & ethn_ind == 1]  
          #dt1 <- dt[yob>2008-max_age & yob<=2008-min_age & imd_ind == 1 & ethn_ind == 1]
        }else if(gndr != "All" & ("All" %in% imd) & !("All" %in% ethn)){
          dirlab <- paste0(additional_dirlab,"/All/GenderEthnicity")
          ageg <- paste0(gndr, "_",paste0("ethn",ethn, collapse = "_"),"_",min_age,"_",max_age)
          dt1 <- dt[gender == gndr & ethn_ind == 1] 
          #dt1 <- dt[yob>2008-max_age & yob<=2008-min_age & gender == gndr & ethn_ind == 1]
        }else if(gndr != "All" & !("All" %in% imd) & ("All" %in% ethn)){
          dirlab <- paste0(additional_dirlab,"/All/GenderImd")
          ageg <- paste0(gndr,"_",paste0("imd",imd, collapse = "_"),"_",min_age,"_",max_age)
          dt1 <- dt[gender == gndr & imd_ind == 1] 
          #dt1 <- dt[yob>2008-max_age & yob<=2008-min_age & gender == gndr & imd_ind == 1]
        }else{
          dirlab <- paste0(additional_dirlab,"/All/GenderEthnicityImd")
          ageg <- paste0(gndr,"_",paste0("ethn",ethn, collapse = "_"),"_",paste0("imd",imd, collapse = "_"),"_",min_age,"_",max_age)
          dt1 <- dt[gender == gndr & ethn_ind == 1 & imd_ind == 1]  
          #dt1 <- dt[yob>2008-max_age & yob<=2008-min_age & gender == gndr & ethn_ind == 1 & imd_ind == 1]
        }
      }else{
        if(gndr == "All" & ("All" %in% imd) & ("All" %in% ethn)){
          dirlab <- paste0(additional_dirlab,"/",reg)
          ageg <- paste0(min_age,"_",max_age)
          dt1 <- dt[region == reg]
          #dt1 <- dt[yob>2008-max_age & yob<=2008-min_age & region == reg]
        }else if(gndr != "All" & ("All" %in% imd) &("All" %in% ethn)){
          dirlab <- paste0(additional_dirlab,"/",reg,"/Gender")
          ageg <- paste0(gndr,"_",min_age,"_",max_age)
          dt1 <- dt[gender == gndr & region == reg] 
          #dt1 <- dt[yob>2008-max_age & yob<=2008-min_age & gender == gndr & region == reg]
        }else if(gndr == "All" & !("All" %in% imd) & ("All" %in% ethn)){
          dirlab <- paste0(additional_dirlab,"/",reg,"/Imd")
          ageg <- paste0(paste0("imd",imd, collapse = "_"),"_",min_age,"_",max_age)
          dt1 <- dt[imd_ind == 1 & region == reg]  
          #dt1 <- dt[yob>2008-max_age & yob<=2008-min_age & imd_ind == 1 & region == reg]
        }else if(gndr == "All" & ("All" %in% imd) & !("All" %in% ethn)){
          dirlab <- paste0(additional_dirlab,"/",reg,"/Ethnicity")
          ageg <- paste0(paste0("ethn",ethn, collapse = "_"),"_",min_age,"_",max_age)
          dt1 <- dt[ethn_ind == 1 & region == reg] 
          #dt1 <- dt[yob>2008-max_age & yob<=2008-min_age & ethn_ind == 1 & region == reg]
        }else if(gndr == "All" & !("All" %in% imd) & !("All" %in% ethn)){
          dirlab <- paste0(additional_dirlab,"/",reg,"/ImdEthnicity")
          ageg <- paste0(paste0("imd",imd, collapse = "_"),"_",paste0("ethn",ethn, collapse = "_"),"_",min_age,"_",max_age)
          dt1 <- dt[imd_ind == 1 & ethn_ind == 1 & region == reg] 
          #dt1 <- dt[yob>2008-max_age & yob<=2008-min_age & imd_ind == 1 & ethn_ind == 1 & region == reg]
        }else if(gndr != "All" & ("All" %in% imd) & !("All" %in% ethn)){
          dirlab <- paste0(additional_dirlab,"/",reg,"/GenderEthnicity")
          ageg <- paste0(gndr, "_",paste0("ethn",ethn, collapse = "_"),"_",min_age,"_",max_age)
          dt1 <- dt[gender == gndr & ethn_ind == 1 & region == reg]  
          #dt1 <- dt[yob>2008-max_age & yob<=2008-min_age & gender == gndr & ethn_ind == 1 & region == reg]
        }else if(gndr != "All" & !("All" %in% imd) & ("All" %in% ethn)){
          dirlab <- paste0(additional_dirlab,"/",reg,"/GenderImd")
          ageg <- paste0(gndr,"_",paste0("imd",imd, collapse = "_"),"_",min_age,"_",max_age)
          dt1 <- dt[gender == gndr & imd_ind == 1 & region == reg] 
          #dt1 <- dt[yob>2008-max_age & yob<=2008-min_age & gender == gndr & imd_ind == 1 & region == reg]
        }else{
          dirlab <- paste0(additional_dirlab,"/",reg,"/GenderEthnicityImd")
          ageg <- paste0(gndr,"_",paste0("ethn",ethn, collapse = "_"),"_",paste0("imd",imd, collapse = "_"),"_",min_age,"_",max_age)
          dt1 <- dt[gender == gndr & ethn_ind == 1 & imd_ind == 1 & region == reg]  
          #dt1 <- dt[yob>2008-max_age & yob<=2008-min_age & gender == gndr & ethn_ind == 1 & imd_ind == 1 & region == reg]
        }
        
      }
      
    }
  dt1[,imd_ind := NULL][, ethn_ind := NULL]
  dt[,imd_ind := NULL][, ethn_ind := NULL]
  return(list(dt1,ageg, dirlab))
}

