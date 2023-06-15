#!/bin/bash
#SBATCH --account=rpp-kshook 
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=16
#SBATCH --mem=30G
#SBATCH --time=02:00:00
#SBATCH --job-name=BowAtBanff
#SBATCH --mail-user=example.email@usask.ca
#SBATCH --mail-type=BEGIN
#SBATCH --mail-type=END

# module loads
#module load openmpi/4.0.3
module load netcdf-fortran/4.5.2

# foring, config and run dirs
dir_mesh=../../installs/MESH/r1813

ln -sf ../workflow_data/domain_BowAtBanff/drainagedatabase/BowAtBanff_MESH_drainage_database.nc ./MESH_drainage_database.nc

$dir_mesh/sa_mesh     
