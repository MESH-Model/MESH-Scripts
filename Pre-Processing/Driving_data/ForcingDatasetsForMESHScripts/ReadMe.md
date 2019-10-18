# Forcing Datasets for MESH
The scripts for use with each dataset are stored in their respectively-named folders, along with a ReadMe file with instructions for their use. Refer to the MESH Wiki page [Forcing Datasets for MESH](https://wiki.usask.ca/display/MESH/Forcing+Datasets+for+MESH) for more details.

During the course of the [CCRN Project](http://www.ccrnetwork.ca/), the CCRN project team downloaded, processed and created several datasets for running [MESH](https://wiki.usask.ca/pages/viewpage.action?pageId=220332269). The [Forcing Datasets for MESH](https://wiki.usask.ca/display/MESH/Forcing+Datasets+for+MESH) page on the MESH Wiki documents the locally available datasets and scripts developed to extract the data for a sub-basin (provided it is within the domain of the dataset).

This is a top-level document; each dataset is further described in README files and papers in their storage locations mentioned on the Wiki page. **Disclaimer: It is the responsibility of the user to make sure that the data processing is correct and that the scripts work correctly for the basin provided.**

- To run MESH, 7 variables are needed at sub-daily time step (see the [Meteorological Input page](https://wiki.usask.ca/display/MESH/Meteorological+Input)).  
- Any bugs or questions can be directed to Mohamed Elshamy @mee067.
- The scripts provided process one variable at a time.
  - One can easily parallelize by invoking several instances of MATLAB and changing the variable being processed.
  - Another way would be to build a loop into those scripts to process all variables and use the MATLAB parallel pool.
- The scripts print the input and output file names and the time stamps ... please take care that the time progresses in the manner expected, depending on the time step of the dataset (hourly, 3hourly, etc.) and time span.

[This presentation](https://wiki.usask.ca/display/MESH/Forcing+Datasets+for+MESH?preview=/1433436170/1441497245/Bias%20Corrected%20Climate%20Forcing%20Datasets%20for%20Land%20Surface%20Modeling.pdf) sheds some light on the datasets and bias correction

The datasets can be accessed by one of the following methods:

- If you have access to Graham, then you can download the data to use the MATLAB scripts (they were written and tested on Windows machine) or if you have a MATLAB license on Graham, then you can transfer the scripts there and adjust them (normally all what's needed is switch the path slashes from '\' to '/') but it is your responsibility to test them.
- If you are at GIWS and have access to GLOBALWATER share on datastore, you can access the files there.
- If you do not have access to either location, you need to download the data from their original sources if they are published already. Otherwise, contact Mohamed Elshamy @mee067 or the data custodian to see if you can get access.
