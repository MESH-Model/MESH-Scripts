#!/bin/bash

# sudo apt-get update
#
# sudo apt-get install r-base
# sudo apt-get install r-base-dev
# sudo apt install build-essential
# sudo apt install libcurl4-openssl-dev libssl-dev libxml2-dev
# sudo apt-get install pandoc

Rscript InstallPackages.R

# For installing R on Graham
    # After loading the R module (and any dependencies):
      # If you see the "Setting LC_* failed, using "C"" errors when opening R, then
      # run the following on the command line:
        # export LC_ALL="en_US.UTF-8"
    # You may need to install the packages individually via R on Graham
