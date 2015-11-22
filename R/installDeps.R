## Install DCI dependencies

installDeps <- function() {
  # Install dependencies
  .pkgs = c("gdata", "reshape2", "ggplot2", "ggthemes", "stringr")
  # Install required packages from CRAN (if not)
  .inst <- .pkgs %in% installed.packages()
  if(length(.pkgs[!.inst]) > 0) install.packages(.pkgs[!.inst])
  
}

