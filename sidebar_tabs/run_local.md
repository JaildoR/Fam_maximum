    # Packages to be loaded vector
    pack.toload <- c("shiny", "shinydashboard", "shinythemes",
                     "dplyr", "classInt", "DT",
                     "lmom" , "moments", "FAdist",
                     "trend", "randtests","fitdistrplus",
                     "ADGofTest", "highcharter")

    # Compare packages required with installed packages
    # and create a vector of packages to be installed
    pack.toinstall <- pack.toload[!(pack.toload %in% 
                                      installed.packages()[,"Package"])]

    # install non previously installed
    if(length(pack.toinstall)) {
      install.packages(pack.toinstall)
    }

    # load all
    lapply(pack.toload, require, character.only = TRUE)

    # https://stackoverflow.com/questions/4090169/
    # elegant-way-to-check-for-missing-packages-and-install-them
