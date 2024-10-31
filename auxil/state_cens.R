##########
### boundaries for the number of undiagnosed people U via 6-state model from numbers
##########


#input: data table with S,D, delta_Z0, delta_ZD, delta_ZU (from initial_noncens function), `disease_name`, agegroup `ageg`, `label` to write into dir
#output: data table with added columns m_0, m_D, mU_min, mU_max, U_min, U_max

## tofile = do not print to file if 0



state_cens <- function(dt_initial, disease_name, ageg, label, tofile){
  
  dt_model0 <- dt_initial
  
  ## inserting 1 instead of 0 in deltas, delete if not appropriate and replace by commented line below
  #dt_model0[delta_Z0 == 0, delta_Z0 := 1][delta_ZD == 0, delta_ZD := 1 ][delta_ZU == 0, delta_ZU := 1]

  
  dt_model0[, delta_S := -newD - delta_Z0S - delta_ZU][, delta_D := newD - delta_Z0D - delta_ZD]
  dt_model0[, ":=" (S_pr = S/(S+D), D_pr = D/(S+D)) ]
  
  dt_model0[, m_0 := delta_Z0/(S+D)][D != 0, m_D := delta_ZD/D][, m_0S := delta_Z0S/S][D != 0, m_0D := delta_Z0D/D]
  
  # ## replace if not inserting 1 instead of 0, to avoid division by 0; works ok with smoothing
  # #dt_model0[m_0 != 0 & m_D != 0, mU_min := pmin(m_0, m_D)][, mU_max := pmax(m_0,m_D)]
  # dt_model0[, mU_min := pmin(m_0, m_D)][, mU_max := pmax(m_0,m_D)]
  # 
  # dt_model0[, U_min := pmax(pmin(S,delta_ZU/mU_max), newD)/(S+D)][, U_max := pmin(S,delta_ZU/mU_min)/(S+D)]
  # 
  # # dt_model0[m_D != 0 & m_0D != 0, mU_minD := pmin(m_D, m_0D)][, mU_maxD := pmax(m_D,m_0D)]#[m_D == 0 | m_0D == 0,mU_minD := m_0]
  # # 
  # # dt_model0[, U_minD := pmax(pmin(S,delta_ZU/mU_maxD), newD)/(S+D)][, U_maxD := pmin(S,delta_ZU/mU_minD)/(S+D)]
  # # 
  # #dt_model0 <- dt_model0[1:.N-1,] - not needed if 2020 is removed? check
   
  setcolorder(dt_model0,c("times","year","S", "delta_S", "newD", "D", "delta_D", 
                          "delta_Z0", "delta_Z0S", "delta_Z0D", "delta_ZU", "delta_ZD",
                          "m_0", "m_0S", "m_0D", "m_D",
                          "S_pr","D_pr"))
  if(tofile != 0){
    output_dir <- file.path("./data/model_output", paste0((label),"/tables"))
    if (!dir.exists(output_dir)){ dir.create(output_dir, recursive = TRUE) }
    
    #write_fst(dt_model0, paste0((output_dir),"/dt_result_noncens_",(disease_name),"_",(ageg),".fst"))
    fwrite(dt_model0, paste0((output_dir),"/dt_result_cens_",(disease_name),"_",(ageg),".csv"))
    rm(output_dir)
  }
  
  return(dt_model0)
}