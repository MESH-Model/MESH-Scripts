#!/bin/bash
#SBATCH --account=rpp-kshook 
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=16
#SBATCH --mem=30G
#SBATCH --time=02:00:00
#SBATCH --job-name=BowAtBanff
#SBATCH --mail-user=cooper.albano@usask.ca
#SBATCH --mail-type=BEGIN
#SBATCH --mail-type=END

# module loads
module load openmpi/4.0.3
module load netcdf-fortran/4.5.2

# foring, config and run dirs
#dir_forcing=../../4_climate_forcing/Output71
#config=../../workflow_data/domain_PFAF71
#dir_mesh=/project/6008034/baha2501/MESH_code/r1813
dir_mesh=/project/6008034/calbano/MESH_code/r1773

# link forcing 
#ln -sf  $dir_forcing/PFAF71_MESH_RDRS_71_remapped_1980-0112-13-00-00.nc     PFAF71_MESH_RDRS_71.nc

# config
# ln -sf $config/MESH_input_streamflow.txt 			MESH_input_streamflow.txt
#ln -sf $config/drainagedatabase/PFAF71_MESH_drainage_database.nc 	MESH_drainage_database.nc
# ln -sf $config/MESH_input_reservoir.txt				MESH_input_reservoir.txt
# ln -sf $config/MESH_input_soil_levels.txt			MESH_input_soil_levels.txt
# ln -sf $config/MESH_input_run_options.ini		        				MESH_input_run_options.ini
# ln -sf $config/distributed_param/MESH_parameters.r2c		    		MESH_parameters.r2c
# ln -sf $config/class_hydrology/MESH_parameters_CLASS.ini				MESH_parameters_CLASS.ini
# ln -sf $config/class_hydrology/MESH_parameters_hydrology.ini			MESH_parameters_hydrology.ini

mpirun $dir_mesh/mpi_sa_mesh_1773        
