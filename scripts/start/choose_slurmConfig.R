# |  (C) 2006-2020 Potsdam Institute for Climate Impact Research (PIK)
# |  authors, and contributors see CITATION.cff file. This file is part
# |  of REMIND and licensed under AGPL-3.0-or-later. Under Section 7 of
# |  AGPL-3.0, you are granted additional permissions described in the
# |  REMIND License Exception, version 1.0 (see LICENSE file).
# |  Contact: remind@pik-potsdam.de
#######################################################################
############### Select slurm partitiion ###############################
#######################################################################

get_line <- function(){
  # gets characters (line) from the terminal or from a connection
  # and returns it
  if(interactive()){
    s <- readline()
  } else {
    con <- file("stdin")
    s <- readLines(con, 1, warn=FALSE)
    on.exit(close(con))
  }
  return(s);
}

choose_slurmConfig <- function() {
  require(crayon, quietly = TRUE, warn.conflicts = FALSE)

  slurm <- suppressWarnings(ifelse(system2("srun",stdout=FALSE,stderr=FALSE) != 127, TRUE, FALSE))
  if (slurm) { 
    cat("\nCurrent cluster utilization:\n")
    system("sclass")
    cat("\n\n")

    cat(paste0(                  "Please choose the SLURM configuration for your submission:\n",
               crayon::blue     ("    QOS             tasks per node   suitable for\n"),
               crayon::blue     ("=======================================================================\n"),
               crayon::green(" 1:")," SLURM standby               12   nash H12             [recommended]\n",
               crayon::green(" 2:")," SLURM standby               13   nash H12 coupled\n",
               crayon::green(" 3:")," SLURM standby               16   nash H12+\n",
               crayon::green(" 4:")," SLURM standby                1   nash debug, testOneRegi, reporting\n",
               crayon::blue     ("--------------------------------------------------------------\n"),
               crayon::green(" 5:")," SLURM priority              12   nash H12             [recommended]\n",
               crayon::green(" 6:")," SLURM priority              13   nash H12 coupled\n",
               crayon::green(" 7:")," SLURM priority              16   nash H12+\n",
               crayon::green(" 8:")," SLURM priority               1   nash debug, testOneRegi, reporting\n",
               crayon::blue     ("--------------------------------------------------------------\n"),
               crayon::green(" 9:")," SLURM short                 12   nash H12\n",
               crayon::green("10:")," SLURM short                 16   nash H12+\n",
               crayon::green("11:")," SLURM short                  1   nash debug, testOneRegi, reporting\n",
               crayon::green("12:")," SLURM medium                 1   negishi\n",
               crayon::green("13:")," SLURM long                   1   negishi\n",
               crayon::blue     ("=======================================================================\n"),
               crayon::green    ("Number: ")))

    identifier <- get_line()
    identifier <- as.numeric(strsplit(identifier,",")[[1]])
    comp <- switch(identifier,
                   "1" = "--qos=standby --nodes=1 --tasks-per-node=12"  , # SLURM standby  - task per node: 12 (nash H12) [recommended]
                   "2" = "--qos=standby --nodes=1 --tasks-per-node=13"  , # SLURM standby  - task per node: 13 (nash H12 coupled)
                   "3" = "--qos=standby --nodes=1 --tasks-per-node=16"  , # SLURM standby  - task per node: 16 (nash H12+)
                   "4" = "--qos=standby --nodes=1 --tasks-per-node=1"   , # SLURM standby  - task per node:  1 (nash debug, test one regi)
                   "5" = "--qos=priority --nodes=1 --tasks-per-node=12" , # SLURM priority - task per node: 12 (nash H12) [recommended]
                   "6" = "--qos=priority --nodes=1 --tasks-per-node=13" , # SLURM priority - task per node: 13 (nash H12 coupled)
                   "7" = "--qos=priority --nodes=1 --tasks-per-node=16" , # SLURM priority - task per node: 16 (nash H12+)
                   "8" = "--qos=priority --nodes=1 --tasks-per-node=1"  , # SLURM priority - task per node:  1 (nash debug, test one regi)
                   "9" = "--qos=short --nodes=1 --tasks-per-node=12"    , # SLURM short    - task per node: 12 (nash H12)
                  "10" = "--qos=short --nodes=1 --tasks-per-node=16"    , # SLURM short    - task per node: 16 (nash H12+)
                  "11" = "--qos=short --nodes=1 --tasks-per-node=1"     , # SLURM short    - task per node:  1 (nash debug, test one regi)
                  "12" = "--qos=medium --nodes=1 --tasks-per-node=1"    , # SLURM medium   - task per node:  1 (negishi)
                  "13" = "--qos=long --nodes=1 --tasks-per-node=1"      , # SLURM long     - task per node:  1 (negishi)
                  '14' = '--qos=medium --nodes=1 --tasks-per-node=12'   , # SLURM medium   - task per node:  1 (nash H12)
                  NULL)
                  
    if(is.null(comp)) stop("This type is invalid. Please choose a valid type")
  } else {
    comp <- "direct"
  }

  return(comp)
}

# combine_slurmconfig takes two strings with SLURM parameters (e.g. "--qos=priority --time=03:30:00") 
# and combines them into one sting of SLURM parameters overwriting the parameters in "original" 
# if they also exist in "update_with".
 
combine_slurmConfig <- function (original, update_with) {
  
  # trim whitespaces
  original <- trimws(original)
  update_with <- trimws(update_with)
  
  # remove double whitespaces
  original <- gsub("\\s+"," ",original)
  update_with <- gsub("\\s+"," ",update_with)
  
  # if user chose "direct" dont update any slurm commands
  if(update_with == "direct") return(update_with)

  # ignore original if it is "direct"
  if (original == "direct") original <- ""
  
  # put RHS strings into vector
  v_update_with <- gsub("--.*=(.*)","\\1",unlist(strsplit(update_with,split=" ")))
  # name the vector using LHS strings
  names(v_update_with) <- gsub("--(.*)=.*","\\1",unlist(strsplit(update_with,split=" ")))
  
  # put RHS strings into vector
  v_original <- gsub("--.*=(.*)","\\1",unlist(strsplit(original,split=" ")))
  # name the vector using LHS strings
  names(v_original) <- gsub("--(.*)=.*","\\1",unlist(strsplit(original,split=" ")))
  
  # remove elements from "original" that are existing in "update_with"
  v_original <- v_original[!names(v_original) %in% "qos"]
  
  combined <- c(v_update_with,v_original)
  
  # concatenate SLURM command (insert "--" and "=")
  res <- paste(paste0("--",names(combined),"=",combined),collapse = " ")
  
  return(res)
}
