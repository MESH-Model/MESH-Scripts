# MESH Installation Scripts

## Author
Scripts originally created by Summaworkflow_public with modifications by MESH vector-based workflow 

## Description
These scripts are provided to facilitate the retreival and compilation of the sandalone MESH application. Retrieval and compilation are separated into two scripts to provide modularity such that the user may modify each script to customize the installation.

By default, the script '1a_clone_MESH' will download a zipped folder containing MESH version r1813 into '/root_path/installs'. Currently, it may be impractical to use this script to download a different version of MESH. For MESH versions other than r1813, users can visit the User Page on the Standalone MESH Wiki.

The script '1b_compile_MESH' unzips the downloaded file and compiles it with the specified compiler. As compiling can be a machine-specific process, there is a significant chance that this script will not work properly. It has been tested on Graham and Ubuntu, however there are several depandancies.

For more information on compiling MESH, please visit the Getting Started page on the Standalone MESH Wiki.
https://wiki.usask.ca/display/MESH/