#!/bin/bash
#SBATCH --account=rpp-kshook
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --mem=20G
#SBATCH --array=1986-2018
#SBATCH --time=04:00:00
#SBATCH --job-name=reord78
#SBATCH --error=errors
#SBATCH --mail-user=cooper.albano@usask.ca
#SBATCH --mail-type=BEGIN
#SBATCH --mail-type=END

# load modules
module load StdEnv/2020 gcc/9.3.0 openmpi/4.0.3
module load gdal/3.5.1 libspatialindex/1.8.5
module load python/3.8.10 scipy-stack/2022a mpi4py/3.0.3

# # create and activate virtual environment
#rm -rf $HOME/MESH-env
#virtualenv --no-download $HOME/MESH-env 
source $HOME/MESH-env/bin/activate 

#pip install --no-index --upgrade pip
#pip install --no-index netcdf4
#pip install --no-index h5netcdf
#pip install --no-index xarray
#pip install --no-index geopandas
#pip install --no-index Rtree
#pip install --no-index easymore

#suffix='-01-13-00-00.nc'
#for i in 02 03 04 05 06 07 08 09 10 11 12; do

#var1="$i"
#var2=$suffix
year=$SLURM_ARRAY_TASK_ID
PFAF=78
#python MESH_vectorbased_forcing.py "$var1" "$var2"
python MESH_vectorbased_forcing_array.py "$year" "$PFAF"

#done

echo  finished
