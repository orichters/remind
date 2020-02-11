############## Define functions ###########################################

configure_cfg <- function(icfg, iscen, iscenarios, isettings, iref_bau_pathways = list()) {

    .setgdxcopy <- function(needle, stack, new) {
      # delete entries in stack that contain needle and append new
      out <- c(stack[-grep(needle, stack)], new)
      return(out)
    }

    # Edit run title
    icfg$title <- iscen
    cat("   Configuring cfg for", iscen,"\n")

    # Edit main file of model
    if( "model" %in% names(iscenarios)){
      icfg$model <- iscenarios[iscen,"model"]
    }

    # Edit regional aggregation
    if( "regionmapping" %in% names(iscenarios)){
      icfg$regionmapping <- iscenarios[iscen,"regionmapping"]
    }

    # Edit input data revision
    if( "revision" %in% names(iscenarios)){
      icfg$revision <- iscenarios[iscen,"revision"]
    }

    # Edit switches in default.cfg according to the values given in the scenarios table
    for (switchname in intersect(names(icfg$gms), names(iscenarios))) {
      icfg$gms[[switchname]] <- iscenarios[iscen,switchname]
    }

    # Set reporting script
    if( "output" %in% names(iscenarios)){
      icfg$output <- paste0("c(\"",gsub(",","\",\"",gsub(", ",",",iscenarios[iscen,"output"])),"\")")
    }

    # check if full input.gdx path is provided and, if not, search for correct path
    if (!substr(isettings[iscen,"path_gdx"], nchar(isettings[iscen,"path_gdx"])-3, nchar(isettings[iscen,"path_gdx"])) == ".gdx"){
      #if there is no correct scenario folder within the output folder path provided, take the config/input.gdx
      if(length(grep(iscen,list.files(path=isettings[iscen,"path_gdx"]),value=T))==0){
        isettings[iscen,"path_gdx"] <- "config/input.gdx"
      #if there is only one instance of an output folder with that name, take the fulldata.gdx from this
      } else if (length(grep(iscen,list.files(path=isettings[iscen,"path_gdx"]),value=T))==1){
        isettings[iscen,"path_gdx"] <- paste0(isettings[iscen,"path_gdx"],"/",
                                            grep(iscen,list.files(path=isettings[iscen,"path_gdx"]),value=T),"/fulldata.gdx")
      } else {
        #if there are multiple instances, take the newest one
        isettings[iscen,"path_gdx"] <- paste0(isettings[iscen,"path_gdx"],"/",
                                            substr(grep(iscen,list.files(path=isettings[iscen,"path_gdx"]),value=T),1,
                                                   nchar(grep(iscen,list.files(path=isettings[iscen,"path_gdx"]),value=T))-19)[1],
        max(substr(grep(iscen,list.files(path=isettings[iscen,"path_gdx"]),value=T),
                                 nchar(grep(iscen,list.files(path=isettings[iscen,"path_gdx"]),value=T))-18,
                                 nchar(grep(iscen,list.files(path=isettings[iscen,"path_gdx"]),value=T)))),"/fulldata.gdx")
      }
    }

    # if the above has not created a path to a valid gdx, take config/input.gdx
    if (!file.exists(isettings[iscen,"path_gdx"])){
      isettings[iscen,"path_gdx"] <- "config/input.gdx"
      #if even this is not existent, stop
      if (!file.exists(isettings[iscen,"path_gdx"])){
      stop("Cant find a gdx under path_gdx, please specify full path to gdx or else location of output folder that contains previous run")
      }
    }

    # Define path where the GDXs will be taken from
    gdxlist <- c(input.gdx     = isettings[iscen, "path_gdx"],
                 input_ref.gdx = isettings[iscen, "path_gdx_ref"],
                 input_bau.gdx = isettings[iscen, "path_gdx_bau"])

    # Remove potential elements that contain ".gdx" and append gdxlist
    icfg$files2export$start <- .setgdxcopy("\\.gdx", icfg$files2export$start, gdxlist)

    if (!is.null(iref_bau_pathways$ref)) {
      if (grepl("\\.gdx$",iref_bau_pathways$ref)){
        icfg$files2export$start['input_ref.gdx'] <- paste0(icfg$remind_folder,"/",iref_bau_pathways$ref)
      } else {
        icfg$files2export$start['input_ref.gdx'] <- paste0(icfg$remind_folder,"/",iref_bau_pathways$ref,"/fulldata.gdx")
      }
    }
    if (!is.null(iref_bau_pathways$bau)) {
      if (grepl("\\.gdx$",iref_bau_pathways$ref)){
        icfg$files2export$start['input_bau.gdx'] <- paste0(icfg$remind_folder,"/",iref_bau_pathways$bau)
      } else {
        icfg$files2export$start['input_bau.gdx'] <- paste0(icfg$remind_folder,"/",iref_bau_pathways$bau,"/fulldata.gdx")
      }
    }

    # add gdx information for subsequent runs
    icfg$subsequentruns        <- rownames(isettings[isettings$path_gdx_ref == iscen & !is.na(isettings$path_gdx_ref) & isettings$start == 1,])
    icfg$RunsUsingTHISgdxAsBAU <- rownames(isettings[isettings$path_gdx_bau == iscen & !is.na(isettings$path_gdx_bau) & isettings$start == 1,])

    return(icfg)
}


get_line <- function(){
	# gets characters (line) from the terminal of from a connection
	# and stores it in the return object
	if(interactive()){
		s <- readline()
	} else {
		con <- file("stdin")
		s <- readLines(con, 1, warn=FALSE)
		on.exit(close(con))
	}
	return(s);
}

choose_folder <- function(folder,title="Please choose a folder") {
  dirs <- NULL
  
  # Detect all output folders containing fulldata.gdx or non_optimal.gdx
  # For coupled runs please use the outcommented text block below

  dirs <- sub("/full.gms","",sub("./output/","",Sys.glob(file.path(folder,"*","full.gms"))))

  # DK: The following outcommented lines are specially made for listing results of coupled runs
  #runs <- findCoupledruns(folder)
  #dirs <- findIterations(runs,modelpath=folder,latest=TRUE)
  #dirs <- sub("./output/","",dirs)
  
  dirs <- c("all",dirs)
  cat("\n\n",title,":\n\n")
  cat(paste(1:length(dirs), dirs, sep=": " ),sep="\n")
	cat(paste(length(dirs)+1, "Search by the pattern.\n", sep=": "))
  cat("\nNumber: ")
	identifier <- get_line()
  identifier <- strsplit(identifier,",")[[1]]
  tmp <- NULL
  for (i in 1:length(identifier)) {
    if (length(strsplit(identifier,":")[[i]]) > 1) tmp <- c(tmp,as.numeric(strsplit(identifier,":")[[i]])[1]:as.numeric(strsplit(identifier,":")[[i]])[2])
    else tmp <- c(tmp,as.numeric(identifier[i]))
  }
  identifier <- tmp
  # PATTERN
	if(length(identifier==1) && identifier==(length(dirs)+1)){
		cat("\nInsert the search pattern or the regular expression: ")
		pattern <- get_line()
		id <- grep(pattern=pattern, dirs[-1])
		# lists all chosen directories and ask for the confirmation of the made choice
		cat("\n\nYou have chosen the following directories:\n")
		cat(paste(1:length(id), dirs[id+1], sep=": "), sep="\n")
		cat("\nAre you sure these are the right directories?(y/n): ")
		answer <- get_line()
		if(answer=="y"){
			return(dirs[id+1])
		} else choose_folder(folder,title)
	# 
	} else if(any(dirs[identifier] == "all")){
		identifier <- 2:length(dirs)
		return(dirs[identifier])
	} else return(dirs[identifier])
}


create_results_folder <- function(cfg){

  # Function definition
  .copy.fromlist <- function(filelist,destfolder) {
    if(is.null(names(filelist))) names(filelist) <- rep("",length(filelist))
    for(i in 1:length(filelist)) {
      if(!is.na(filelist[i])) {
        to <- paste0(destfolder,"/",names(filelist)[i])
        if(!file.copy(filelist[i],to=to,recursive=dir.exists(to),overwrite=T)) cat(paste0("Could not copy ",filelist[i]," to ",to,"\n"))
      }
    }
  }
  
  # Generate name of results folder and create the folder
  date <- format(Sys.time(), "_%Y-%m-%d_%H.%M.%S")
  cfg$results_folder <- gsub(":date:", date, cfg$results_folder, fixed = TRUE)
  cfg$results_folder <- gsub(":title:", cfg$title, cfg$results_folder, fixed = TRUE)

  # Create results folder
  if (!file.exists(cfg$results_folder)) {
    dir.create(cfg$results_folder, recursive = TRUE, showWarnings = FALSE)
    cat("   Creating results folder",cfg$results_folder,"\n")
  } else if (!cfg$force_replace) {
    stop(paste0("Results folder ",cfg$results_folder," could not be created because it already exists."))
  } else {
    cat("   Overwriting existing results folder:",cfg$results_folder,"\n")
    unlink(cfg$results_folder, recursive = TRUE)
    dir.create(cfg$results_folder, recursive = TRUE, showWarnings = FALSE)
  }

  # Save the cfg (with the updated name of the result folder) into the results folder. 
  # Do not save the new name of the results folder to the .RData file in REMINDs main folder, because it 
  # might be needed to restart subsequent runs manually and should not contain the time stamp in this case.
  filename <- paste0(cfg$results_folder,"/config.Rdata")
  #cat("   Writing cfg to file",filename,"\n")
  save(cfg, file=filename)
  
  # Copy files required to confiugre and start a run
  filelist <- c("prepare.R" = "scripts/start/prepare.R",
                "run.R" = "scripts/start/run.R",
                "postProc.R" = "scripts/start/postProc.R")
  .copy.fromlist(filelist, cfg$results_folder)

  return(cfg)
}


create_dependency_string <- function(wait_for, type="afterok") {
  out <- paste0(" --dependency=",type)
  for (id in unname(wait_for)){
    out <- paste0(out,":",id)
  }
  return(out)
}


submit_remind_run <- function(cfg, wait_for = NULL) {
  
  # Change to run folder while in function (the job has to be submitted from there)
  old_wd <- setwd(cfg$results_folder)
  on.exit(setwd(old_wd))
  
  if(cfg$slurmConfig=="direct") {
    cat("   Executing run.R locally \n")
    source("run.R")
    job_id <- NULL

  } else {

    if (!is.null(wait_for)) dependency <- create_dependency_string(wait_for) else dependency <- ""

    job_id <- system(paste0("sbatch --job-name=run_",cfg$title, dependency," --kill-on-invalid-dep=yes --output=log.txt --mail-type=END --parsable --comment=REMIND --wrap=\"Rscript --no-site-file run.R \" ",cfg$slurmConfig), intern = TRUE)
    
    names(job_id) <- cfg$title

    cat("   Submited run.R script as batch job",crayon::green(names(job_id)),"with id:",job_id,"\n")
    if (!is.null(wait_for)){ 
      cat("     Will wait for",crayon::yellow(paste(names(wait_for), collapse = ", ")),"to finish before starting\n")
    }

  }
  return(job_id)
}



submit_remind_prepare <- function(cfg, wait_for = NULL) {

  # Change to run folder while in function (the job has to be submitted from there)
  old_wd <- setwd(cfg$results_folder)
  on.exit(setwd(old_wd))

  if(cfg$slurmConfig=="direct") {
    cat("   Executing prepare.R locally \n")
    source("prepare.R")
    job_id <- NULL

  } else {

    if (!is.null(wait_for)) dependency <- create_dependency_string(wait_for) else dependency <- ""
    qos <- stringr::str_match(cfg$slurmConfig,"--qos=(.*?) ")[2]

    job_id <- system(paste0("sbatch --job-name=prepare_",cfg$title, dependency," --kill-on-invalid-dep=yes --output=prepare_log.txt --mail-type=FAIL --time=10 --parsable --comment=REMIND --wrap=\"Rscript prepare.R \"  --qos=", qos), intern = TRUE)

    names(job_id) <- paste0("prepare_",cfg$title)

    cat("   Submited prepare.R script as batch job",crayon::green(names(job_id)),"with id:",job_id,"\n")
    if (!is.null(wait_for)){ 
      cat("     Will wait for",crayon::yellow(paste(names(wait_for), collapse = ", ")),"to finish before starting\n")
    }
  }
  return(job_id)
}



submit_remind_postProc <- function(cfg, wait_for = NULL){
  
  # Change to run folder while in function (the job has to be submitted from there)
  old_wd <- setwd(cfg$results_folder)
  on.exit(setwd(old_wd))

  if(cfg$slurmConfig=="direct") {
    cat("   Executing postProc.R locally \n")
    source("postProc.R")
    job_id <- NULL

  } else {

    if (!is.null(wait_for)) dependency <- create_dependency_string(wait_for) else dependency <- ""
    qos <- stringr::str_match(cfg$slurmConfig,"--qos=(.*?) ")[2]

    job_id <- system(paste0("sbatch --job-name=postProc_",cfg$title, dependency," --kill-on-invalid-dep=yes --output=postProc_log.txt --mail-type=FAIL --time=20 --parsable --comment=REMIND --wrap=\"Rscript postProc.R \"  --qos=",qos," --ntasks=1 --cpus-per-task=8"), intern = TRUE)

    names(job_id) <- paste0("postProc_",cfg$title)

    cat("   Submited postProc.R script as batch job",crayon::green(names(job_id)),"with id:",job_id,"\n")
    if (!is.null(wait_for)){ 
      cat("     Will wait for",crayon::yellow(paste(names(wait_for), collapse = ", ")),"to finish before starting\n")
    }
  }
  return(job_id)
} 



submit_clean_up <- function(copy_dir, wait_for){
  cat("\n Clean up\n")
  dependency <- create_dependency_string(wait_for, type="afterany")
  cmd <- paste0("sbatch --job-name=clean_up",dependency," --mail-type=FAIL --time=5 --parsable --comment=REMIND --wrap=\"rm -rf ",
                copy_dir,"\" --qos=standby")
  job_id <- system(cmd, intern = TRUE)
  cat("   Submited batch job",crayon::green("clean_up"),"with id:",job_id,"\n")
  cat("     Will wait for",crayon::yellow(paste(names(wait_for), collapse = ", ")),"to finish before starting\n")
} 