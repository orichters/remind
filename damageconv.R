suppressMessages(library(tidyverse))
suppressMessages(library(gdx))
suppressMessages(library(modelstats))
vars <- c("p15_gmt_conv", "p51_sccConvergenceMaxDeviation")
argv <- commandArgs(trailingOnly = TRUE)
if (length(argv) > 0 && ! isTRUE(argv == "")) vars <- strsplit(argv, ",")[[1]]
folder <- if (sum(file.exists(c("output", "output.R", "start.R", "main.gms"))) == 4) "output" else "."
dirs <- grep("^C_.*-rem-[0-9]+$", dir(folder), value = TRUE)
maxrem <- max(as.numeric(gsub("^C_.*-rem-", "", dirs)))
runs <- unique(gsub("-rem-[0-9]+$", "", dirs))
message("\nNumbers in parentheses indicate runs currently in slurm.")
for (v in vars) {
  message("\n### ", v)
  results <- matrix(nrow = length(runs), ncol = maxrem + 1)
  colnames(results) <- c("run", paste0("rem", seq(maxrem)))
  results <- as_tibble(results)
  for (r in seq_along(runs)) {
    results[[r, "run"]] <- runs[[r]]
    for (m in seq(maxrem)) {
      rfolder <- file.path(folder, paste0(runs[r], "-rem-", m))
      gdx <- file.path(rfolder, "fulldata.gdx")
      if (file.exists(gdx)) {
        data <- NULL
        data <- try(gdx::readGDX(gdx, v, react = "silent"), silent = TRUE)
        if (inherits(data, "try-error")) data <- NA
        if (is.null(data)) data <- "null"
        if (is.numeric(data)) data <- if (data < 10) signif(data, 2) else round(data, 2)
        data <- paste(data, collapse = ",")
        if (! modelstats::foundInSlurm(rfolder) == "no" && data != "null") {
          data <- paste0("(", data, ")")
        }
        results[[r, paste0("rem", m)]] <- data
      }
    }
  }
  print(results)
}
