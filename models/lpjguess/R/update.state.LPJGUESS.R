#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

##' Adjust LPJ-GUESS state
##'
##' @title updateState.LPJGUESS
##'
##' @description 
##'
##'
##' @param model.state A large multiply-nested list containing the entire LPJ-GUESS state as read by 
##' function \code{readStateBinary.LPJGUESS} 
##' @param pft.params A data.frame containing the parameters for each PFT.  Each row represents one PFT (ordering must be consistent with the vectors below. 
##' The names of the columns describe the per-PFT parameter and must include: 
##' wooddens, crownarea_max, lifeform (1 = tree, 2 = grass), k_latosa, k_rp, k_allom1,  k_allom2, k_allom3, crownarea_max and sla. 
##' wooddens, crownarea_max, lifeform (1 = tree, 2 = grass), k_latosa, k_rp, k_allom1,  k_allom2, k_allom3, crownarea_max and sla. 
##' @param dens.initial A numeric vector of the initial stand-level stem densities (indiv/m^2) as named numeric vector 
##' with one entry per PFT/species, with the names being the PFT/species codes.  These values should be produced
##' using state data assimilation from function XXXXXX.  
##' @param dens.target A numeric vector of the target stand-level stem densities (indiv/m^2) as named numeric vector 
##' with one entry per PFT/species, with the names being the PFT/species codes.  These values should be produced
##' using state data assimilation from function XXXXXX 
##' @param cmass.initial A numeric vector of the target stand-level biomasses (kgC/m^2) as named numeric vector 
##' with one entry per PFT/species, with the names being the PFT/species codes.  These values should be produced
##' using state data assimilation from function XXXXXX 
##' @param cmass.target A numeric vector of the target stand-level biomasses (kgC/m^2) as named numeric vector 
##' with one entry per PFT/species, with the names being the PFT/species codes.  These values should be produced
##' using state data assimilation from function XXXXXX 
##' @param AGSB.epsilon A single numeric specifying how close the final above ground stem biomass needs to be to the target
##' above ground stem biomass for each individual.  eg. 0.05 requires that the final above ground stem biomass is within 5%
##' of the target above ground stem biomass 
##' @param HEIGHT_MAX Maximum allowed height of an individual.  This is the maximum height that a tree
##' can have.  This is hard-coded in LPJ-GUESS to 150 m, but for SDA that might be unrealistically big, 
##' so this argument allows adjustment. 
##' @return  And updated model state (as a big old list o' lists)
##' @export update.state.LPJGUESS 
##' @author Matthew Forrest
update.state.LPJGUESS <- function(model.state, pft.params, dens.initial, dens.target, cmass.initial, cmass.target, AGSB.epsilon, trace, HEIGHT_MAX = 150) {
  
  
  # calculate relative increases to be applied later on (per PFT)
  dens.rel.change <- dens.target/dens.initial
  biomass.rel.change <- cmass.target/cmass.initial
  #print(dens.rel.change)
  #print(biomass.rel.change)
  
  print(pft.params)
 
  
  # nstands - should always be 1 but lets make sure
  nstands <- unlist(model.state$nstands)
  if(nstands != 1) warning("More than one Stand found in LPJ-GUESS state.  This possibly implies that land use has been enabled
                           which the PEcAn code might not be robust against.")
  
  #
  for(stand.counter in 1:nstands) {
    
    # get the number of patches
    npatches <- model.state$Stand[[stand.counter]]$npatches
    # MF hack for now
    #npatches <- 1
    
    # get list of all the PFTs included in this stand
    active.PFTs <- c()
    for(stand.pft.id in 1:length(model.state$Stand[[stand.counter]]$Standpft$active)) {
      if(model.state$Stand[[stand.counter]]$Standpft$active[[stand.pft.id]]) active.PFTs <- append(active.PFTs, stand.pft.id -1)
    }
    
    
    # loop through each patch
    for(patch.counter in 1:npatches) {
      
      this.patch <- model.state$Stand[[stand.counter]]$Patch[[patch.counter]]
      
      
      # for each individual
      for(individual.counter in 1:length(this.patch$Vegetation)) {
        
        
        # IMPORTANT: note that this is for convenience to *read* variables from the original individual 
        # but it should not be written to.  Instead the 'updated.individual' (defined in the loop below)
        # should be updated and then used to update the main state (model.state)
        original.individual <- this.patch$Vegetation$Individuals[[individual.counter]]
        
        # don't adjust non-alive individuals as they will soon be removed
        if(original.individual$alive) {
          
          # get the PFT id and check that it is active
          this.pft.id <- original.individual$indiv.pft.id
          pft.index <- this.pft.id + 1
          if(!this.pft.id %in% active.PFTs) stop(paste0("Found individual of PFT id = ",this.pft.id, 
                                                        " but this doesn't seem to be active in the LPJ-GUESS run"))
          
          
          # initialise the result code to "FIRST" for the first iteration
          result.code <- "FIRST"
          
          # get the target changes in densindiv and cmass
          target.biomass.rel.change <- biomass.rel.change[pft.index]
          target.densindiv.rel.change <- dens.rel.change[pft.index]
          
          # while loop, break out when code is "OK"
          counter <- 0
          
          while(result.code != "OK") {
            print(result.code)
            
            # 'adjust the adjustment'
            
            # "FIRST" - the first iteration
            if(result.code == "FIRST") {
              
              # if the biomass nudge is less that 0.75 the allocation will probably fail so increase the biomass
              # to 0.75 and increase the stem density accordingly
              if(target.biomass.rel.change < 0.75) {
                target.overall.rel.change <- target.biomass.rel.change * target.densindiv.rel.change
                print(target.overall.rel.change)
                current.target.biomass.rel.change <- 0.75
                current.target.densindiv.rel.change <- target.overall.rel.change / current.target.biomass.rel.change 
                derived.overall.rel.change <- current.target.biomass.rel.change * current.target.densindiv.rel.change
                print(derived.overall.rel.change)
                
              } 
              else {
                current.target.biomass.rel.change <- target.biomass.rel.change
                current.target.densindiv.rel.change <- target.densindiv.rel.change
              }
              
            }
            # this error normally arises because of a too large negative biomass increment, do here set a softer
            # biomass nudge
            else if(result.code == "LowWoodDensity"){
              target.overall.rel.change <- target.biomass.rel.change * target.densindiv.rel.change
              print(target.overall.rel.change)
              current.target.biomass.rel.change <- 1.1 * current.target.biomass.rel.change
              current.target.densindiv.rel.change <- target.overall.rel.change / current.target.biomass.rel.change 
              derived.overall.rel.change <- current.target.biomass.rel.change * current.target.densindiv.rel.change
              print(derived.overall.rel.change)
            }
            # in the case individuals (as in each actual tree) get too big, so increase the individual density, 
            # but this needs to be balanced by the a smaller biomass nudge (do 10% increments)
            else if(result.code == "MaxHeightExceeded"){
              
              current.target.densindiv.rel.change <-  current.target.densindiv.rel.change * 1.1
              current.target.biomass.rel.change <- current.target.biomass.rel.change / 1.1
              print(paste(counter, result.code))
              
            }
            else if(result.code == "NegligibleLeafMass"){
              target.overall.rel.change <- target.biomass.rel.change * target.densindiv.rel.change
              print(target.overall.rel.change)
              current.target.biomass.rel.change <- 0.75
              current.target.densindiv.rel.change <- target.overall.rel.change / current.target.biomass.rel.change 
              derived.overall.rel.change <- current.target.biomass.rel.change * current.target.densindiv.rel.change
              print(derived.overall.rel.change)
            }
            
            # calculate the target absolute above ground stem biomass for this iteration
            target.AGSB <- biomass.rel.change[pft.index] * (original.individual$cmass_sap + original.individual$cmass_heart) * (1 -0.23)
            
            # STEP 1 - nudge density of stems by adjusting the "densindiv" and also scaling the biomass pools appropriately
            updated.individual <- adjust.density.LPJGUESS(original.individual, current.target.densindiv.rel.change)
            
            
            # STEP 2 - nudge biomass by performing the LPJ-GUESS allocation routine
            
            # this function call runs the LPJ-GUESS allocation routine and adjusts the pools vegetation pools accordingly
            # however, it doesn't adjust the litter pools or do anything with 'exceeds_cmass', these are returned
            # as elements of the list, because they should only be applied to the state *if* this was a valid allocation
            updated.list <- adjust.biomass.LPJGUESS(individual = updated.individual, 
                                                    rel.change = current.target.biomass.rel.change,  
                                                    sla = pft.params[pft.index, "sla"], 
                                                    wooddens = pft.params[pft.index, "wooddens"], 
                                                    lifeform = pft.params[pft.index, "lifeform"], 
                                                    k_latosa = pft.params[pft.index, "k_latosa"], 
                                                    k_allom2 = pft.params[pft.index, "k_allom2"], 
                                                    k_allom3 = pft.params[pft.index, "k_allom3"])
            # extract the elements from the return list
            updated.individual <- updated.list[["individual"]]
            litter_root_inc <- updated.list[["litter_root_inc"]]
            litter_leaf_inc <- updated.list[["litter_leaf_inc"]]
            exceeds_cmass <- updated.list[["exceeds_cmass"]]
            rm(updated.list)
            
            
            # STEP 3 - calculate the new allometry of the individual based on the updated pools
            
            allometry.results <- allometry(
              # initial allometry/pools
              cmass_leaf = updated.individual$cmass_leaf, 
              cmass_sap = updated.individual$cmass_sap, 
              cmass_heart = updated.individual$cmass_heart, 
              densindiv = updated.individual$densindiv, 
              age = updated.individual$age, 
              fpc = updated.individual$fpc,
              deltafpc = updated.individual$deltafpc,
              # parameter values
              lifeform = pft.params[pft.index, "lifeform"], 
              sla = pft.params[pft.index, "sla"], 
              k_latosa = pft.params[pft.index, "k_latosa"], 
              k_rp = pft.params[pft.index, "k_rp"],
              k_allom1 = pft.params[pft.index, "k_allom1"],
              k_allom2 = pft.params[pft.index, "k_allom2"], 
              k_allom3 = pft.params[pft.index, "k_allom3"], 
              wooddens = pft.params[pft.index, "wooddens"],
              crownarea_max = pft.params[pft.index, "crownarea_max"], 
              HEIGHT_MAX = HEIGHT_MAX) 
            
            
            # STEP 4 - check if new allometry is valid. If yes, update state and move on,
            # if not adjust the nudging and start again
            result.code <- allometry.results$error.string
            
            # right now just accept the neglibable leaf mass error
            if(result.code == "NegligibleLeafMass"){
              result.code <- "OK"
            }
            
            # if "OK"
            if(result.code == "OK") {
              
              
              # check if the change in the wood compartment is close to the nudge
              total.wood.before <- original.individual$cmass_sap + original.individual$cmass_heart
              total.wood.after <- updated.individual$cmass_sap + updated.individual$cmass_heart
              # TODO here apply a better allometric relationship to go from total woody biomass (resulting from the allocation 
              # with this iteration's biomass increment) to above ground woody biomass.  
              # This number (below ground biomass / total biomass = 0.23) is an approximation from the allometric relationship 
              # in Table 6 of Chojnacky et al. 2014 Forestry 
              ag.wood.before <- total.wood.before * (1-0.23)
              ag.wood.after <- total.wood.after * (1-0.23)
              
              print("--------------------------------------------")
              print(paste("nudge = ", current.target.biomass.rel.change ))
              print(ag.wood.after)
              print(target.AGSB)
              
              
              
              # check if the above ground stem biomass is acceptably close
              # if((ag.wood.after - target.AGSB)/ target.AGSB > AGSB.epsilon) {
              #   #result.code <- "AGSBTooBig"
              #   result.code <- "OK"
              # }
              # else if((ag.wood.after - target.AGSB)/ target.AGSB < -AGSB.epsilon) {
              #   #result.code <- "AGSBTooSmall"
              #   result.code <- "OK"
              # }
              
              # okay, allocation produces AGSB acceptables close to the target so update the allometry, 
              # save the individual back to the state, update the litter pools, 
              # deal with exceeds_cmass, and the code will break out of the while loop
              # if not, there will be a new iteration of allocation with new multipliers
              #else {
                # first update the allometry
                updated.individual$height <- allometry.results$height
                updated.individual$crownarea <- allometry.results$crownarea
                updated.individual$lai_indiv <- allometry.results$lai_indiv
                updated.individual$lai <- allometry.results$lai
                updated.individual$deltafpc <- allometry.results$deltafpc
                updated.individual$fpc <- allometry.results$fpc
                updated.individual$boleht <- allometry.results$boleht
                
                # save the individual back to the state
                model.state$Stand[[stand.counter]]$Patch[[patch.counter]]$Vegetation$Individuals[[individual.counter]] <- updated.individual
                
                # now the litter pools (determine N based on intial C:N ratio)
                # C:N ratios
                leaf_litter_cton <- model.state$Stand[[stand.counter]]$Patch[[patch.counter]]$Patchpft$litter_leaf[[pft.index]] / model.state$Stand[[stand.counter]]$Patch[[patch.counter]]$Patchpft$nmass_litter_leaf[[pft.index]]
                root_litter_cton <- model.state$Stand[[stand.counter]]$Patch[[patch.counter]]$Patchpft$litter_root[[pft.index]] / model.state$Stand[[stand.counter]]$Patch[[patch.counter]]$Patchpft$nmass_litter_root[[pft.index]]
                # update the C pools based on the calculated increments from the allocation call (these will only be non-zero in 'abnormal cases)
                model.state$Stand[[stand.counter]]$Patch[[patch.counter]]$Patchpft$litter_leaf[[pft.index]]  <- model.state$Stand[[stand.counter]]$Patch[[patch.counter]]$Patchpft$litter_leaf[[pft.index]] + (litter_leaf_inc * updated.individual$densindiv) 
                model.state$Stand[[stand.counter]]$Patch[[patch.counter]]$Patchpft$litter_root[[pft.index]]  <- model.state$Stand[[stand.counter]]$Patch[[patch.counter]]$Patchpft$litter_root[[pft.index]] + (litter_root_inc * updated.individual$densindiv) 
                # update the N pools simple by dividing the new C pool by the C:N ratio
                model.state$Stand[[stand.counter]]$Patch[[patch.counter]]$Patchpft$nmass_litter_leaf[[pft.index]]  <- model.state$Stand[[stand.counter]]$Patch[[patch.counter]]$Patchpft$litter_leaf[[pft.index]] / leaf_litter_cton
                model.state$Stand[[stand.counter]]$Patch[[patch.counter]]$Patchpft$nmass_litter_root[[pft.index]]  <- model.state$Stand[[stand.counter]]$Patch[[patch.counter]]$Patchpft$litter_root[[pft.index]] / root_litter_cton
                
                # and finally exceeds_cmass - not currently dealing with this because it is only used to maintin mass balance which
                # we *probably* don't need to do here, but print a warning if it is non-zero
                if(!exceeds_cmass == 0) warning(paste("Non-zero exceeds_cmass following allocation, exceeds_cmass =", exceeds_cmass))
                
              # } # if AGSB nudge okay
              
            } # if allometry valid
            
            counter <- counter + 1
            
          } # while code is not "OK"
          
        } # if individual is alive
        
      } # for each individual
      
    } # for each patch
    
  } # for each stand
  
  
  
  # TODO MISSING - introduce new individuals to represent PFTs present in data but not in the model output
  
  return(model.state)
  
}

