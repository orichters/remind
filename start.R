# |  (C) 2006-2020 Potsdam Institute for Climate Impact Research (PIK)
# |  authors, and contributors see CITATION.cff file. This file is part
# |  of REMIND and licensed under AGPL-3.0-or-later. Under Section 7 of
# |  AGPL-3.0, you are granted additional permissions described in the
# |  REMIND License Exception, version 1.0 (see LICENSE file).
# |  Contact: remind@pik-potsdam.de
#!/usr/bin/env Rscript
#'
#' Usage:
#' Rscript start.R [options] [file]
#'
#' Without additional arguments this script prepares and runs a single REMIND run using the settings
#' from `config/default.cfg`.
#'
#' Control the script's behavior by providing additional arguments:
#'
#' --testOneRegi: Starting a single REMIND run in OneRegi mode using the
#'   settings from `config/default.cfg`
#'
#' --restart: Restart runs, with interactive choice
#'
#' Start a bundle of REMIND runs by using the settings from a scenario_config_XYZ.csv:
#'
#'   Rscript start.R config/scenario_config_XYZ.csv
#'

source("scripts/start/start_functions.R")
source("scripts/start/choose_slurmConfig.R")


############## Check command-line arguments ###########################
# There are 4 possible arguments:
#    1. A scenario config file
#    2. The flag: "--testOneRegi"
#    4. The flag: "--restart"
argv <- commandArgs(trailingOnly = TRUE)

# Check for a scenario config file (argument not starting with '--')
if (!identical(grep("^--", argv, invert = TRUE),integer(0))) {
  config.file <- grep("^--", argv, value = TRUE, invert = TRUE)
  if (!file.exists(config.file)) stop("User error: config file doesn't exist.")
  scenConfigFileExists <- TRUE
} else {
  scenConfigFileExists <- FALSE
}

# Check that only accepted flags were given as arguments
if(any(! grep("^--", argv, value=TRUE) %in% c("--restart","--testOneRegi"))){
  stop("User error: unkown command line argument. Possible arguments are '--restart','--testOneRegi'")
}

# Create booleans for each of the possible flags
testOneRegi <- "--testOneRegi" %in% argv
restart <- "--restart" %in% argv

# Check for incompatibilities
if (testOneRegi && scenConfigFileExists) stop("User error: command line arguments are incompatible.")
if (restart && scenConfigFileExists) stop("User error: command line arguments are incompatible.")
if (restart && testOneRegi) stop("User error: command line arguments are incompatible.")
#######################################################################



############## Choose submission type #################################
slurmConfig <- choose_slurmConfig()
#######################################################################


# Get scenarios
# If restarting runs, then choose the folders and submit_remind_run
############## If restart==TRUE, restart runs and STOP ################
if (restart) {
  # Choose results folder from list
  outputdirs <- choose_folder("./output","Please choose the runs to be restarted")
  for (outputdir in outputdirs) {
    cat("\nRestarting",outputdir,"\n")
    # Read config.Rdata from results folder
    load(paste0("output/",outputdir,"/config.Rdata")) 
    # Update the slurmConfig setting to what the user just chose (it was being ignored before)
    cfg$slurmConfig <- combine_slurmConfig(cfg$slurmConfig,slurmConfig) 
    # overwrite results_folder in cfg with name of the folder the user wants to restart, 
    # because user might have renamed the folder before restarting
    cfg$results_folder <- paste0("output/",outputdir) 
    job_id <- submit_remind_run(cfg)
  }

  # Little function to stop without printing an error
  stop_quietly <- function() {
    opt <- options(show.error.messages = FALSE)
    on.exit(options(opt))
    stop()
  }
  stop_quietly()
}
#######################################################################



# If not restarting runs, get the scenarios to be run.
###################### Scenario settings ##############################
if (scenConfigFileExists) {
  cat("\nStarting REMIND runs configured with:",config.file,"\n")

  # Read-in the switches table, use first column as row names
  settings <- read.csv2(config.file, stringsAsFactors = FALSE, row.names = 1, comment.char = "#", na.strings = "")

  # Select scenarios that are flagged to start
  scenarios  <- settings[settings$start==1,]

  # Make sure scenario names don't includes a "." and have less than 75 characters
  if (any(grepl("\\.",rownames(scenarios))) || any(nchar(rownames(scenarios))>75)) {
    stop("One or more titles contain dots - GAMS would not tolerate this, and quit working at a point where you least expect it. Stopping now. ")
  }

} else {
  # If no csv was provided create dummy list with 'default' as the only scenario
  cat("\nStarting REMIND run configured with: scripts/default.cfg\n")
  scenarios <- data.frame("default" = "default",row.names = "default")
}
#######################################################################


###### Saving Git information to avoid having to copy .git folder  ####
cat("\nSaving Git information.\n")
git_info <- list()
git_info$info_str <- paste0("\n===== git info =====\nLatest commit: ",
                            try(system("git show -s --format='%h %ci %cn'", intern=TRUE), silent=TRUE),
                            "\nChanges since then: ",
                            paste(try(system("git status -uno", intern=TRUE), silent=TRUE),collapse="\n"),
                            "\n====================\n")
git_info$commit <- try(system("git rev-parse --short HEAD", intern=TRUE), silent=TRUE)
git_info$status <- try(system("git status -uno", intern=TRUE), silent=TRUE)
#######################################################################



# Create a temporary copy of remind, from which to prepare the runs
############## Create tmp copy of remind folder  ######################
# Copy the remind folder using the rsync command, excluding certain directories
# (-a -> copy eveything. -W and --inplace -> do it fast because we're copying locally)
cat("\nCopying remind into temporary folder and starting runs from there (~ 10-30 sec) ... ")
base_copy <- paste0("../tmp_remind_base", format(Sys.time(), "_%Y-%m-%d_%H.%M.%S"),"/")
rsync_cmd <- paste0("rsync -a -W --inplace ../remind/ ", base_copy,
                    " --include scripts/output/ --exclude output/ --exclude tutorials/ --exclude .git/ --exclude doc/ ")
system(rsync_cmd)
# If start gdxs are given, then make sure they're copied as well
if (scenConfigFileExists) { 
  start_gdxs <- unique(grep("\\.gdx",c(scenarios[,"path_gdx"],scenarios[,"path_gdx_ref"],scenarios[,"path_gdx_bau"]), value = T))
  if (length(start_gdxs) != 0) q <- lapply(start_gdxs, function(x) {system(paste0("rsync -a -W --inplace -R ",x," ",base_copy))})
}
cat( "done.\n")
# Switch working directory
setwd(base_copy)
#######################################################################



# For every scenario, a cfg is defined, a prepare_job is submitted and a run_job is submitted.
# A "run" job is submitted with a dependency on the prepare job.
# If a scenario depends on another, its prepare job is submitted with a dependency on the base's run_job.
# Post-processing jobs are submitted with a dependency the run job.
#######################################################################
######### Loop over scenarios, configure, and submit jobs #############
#######################################################################
job_ids <- character(0)
job_ids_prepare <- character(0)
job_resultFolders <- character(0)

for (scen in rownames(scenarios)) {

  # ---------- Configure ----------------------------------------------
  source("config/default.cfg")
  cfg$slurmConfig <- slurmConfig
  cfg$remind_folder <- getwd()
  cfg$git_info <- git_info
  cfg$logoption   <- 2  # log output written to file (not to screen)

  if (testOneRegi) {
    cfg$title            <- 'testOneRegi'
    cfg$gms$optimization <- 'testOneRegi'
    cfg$output           <- NA
    cfg$results_folder   <- 'output/testOneRegi'
    cfg$force_replace    <- TRUE # delete existing Results directory
  }

  if (scenConfigFileExists) cat("\n",scen,"\n") else cat("\n",cfg$title,"\n") 

  # The result folder is still placed in the original remind/ directory.
  # To that end, the cfg$results_folder template is modified:
  cfg$results_folder <- paste0("../remind/", cfg$results_folder)

  # Configure cfg based on settings from csv if provided
  if (scenConfigFileExists) {
    # Check if this scen is dependent on other jobs finishing before it can start. If so, then
    # the ids, and the pathways to the folders, of the jobs they depend on have to be determined.
    if (is.na(scenarios[scen,"path_gdx_ref"])) {
        wait_for_ids <- NULL
        ref_bau_pathways <- list()

    } else if (grepl("\\.gdx$", scenarios[scen,"path_gdx_ref"])) {
        wait_for_ids <- NULL
        ref_bau_pathways <-list(ref = scenarios[scen,"path_gdx_ref"],
                                bau = scenarios[scen,"path_gdx_bau"])

    } else {
        wait_for_ids <- job_ids[scenarios[scen,'path_gdx_ref']]
        ref_bau_pathways <- list(ref = job_resultFolders[scenarios[scen,"path_gdx_ref"]],
                                 bau = job_resultFolders[scenarios[scen,"path_gdx_bau"]])
    }

    cfg <- configure_cfg(cfg, scen, scenarios, settings, ref_bau_pathways)
  } else {
    wait_for_ids <- NULL
  }
  # --------------------------------------------------------------------


  # Create results folder
  cfg <- create_results_folder(cfg)


  # ---------- Submit jobs ----------------------------------------------
  # Submit remind prepare job 
  job_id_prepare <- submit_remind_prepare(cfg, wait_for = wait_for_ids)
  wait_for_ids <- c(wait_for_ids, job_id_prepare)

  # Submit remind run job
  job_id <- submit_remind_run(cfg, wait_for = wait_for_ids)

  # Submit remind post-processing job
  job_id_postProc <- submit_remind_postProc(cfg, wait_for = job_id)
  # --------------------------------------------------------------------


  # Save job_ids and results folders
  job_ids <- c(job_ids, job_id)
  job_ids_prepare <- c(job_ids_prepare, job_id_prepare)

  job_resultFolder <- cfg$results_folder
  names(job_resultFolder) <- cfg$title
  job_resultFolders <- c(job_resultFolders, job_resultFolder)
}
#######################################################################


# Delete the tmp copy of remind, once all prepare jobs are done
####################### Clean up after prepare jobs ###################
submit_clean_up(base_copy, job_ids_prepare)
#######################################################################
