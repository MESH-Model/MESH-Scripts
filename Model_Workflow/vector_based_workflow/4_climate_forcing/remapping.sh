#!/bin/bash
#SBATCH --account=rpp-kshook
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --mem=20G
#SBATCH --array=1980-2018
#SBATCH --time=04:00:00
#SBATCH --job-name=remap_78_8198
#SBATCH --error=errors
#SBATCH --mail-user=cooper.albano@usask.ca
#SBATCH --mail-type=BEGIN
#SBATCH --mail-type=END


# load modules
module load StdEnv/2020 gcc/9.3.0 openmpi/4.0.3
module load gdal/3.5.1 libspatialindex/1.8.5
module load python/3.8.10 scipy-stack/2022a mpi4py/3.0.3

# # create and activate virtual environment

virtualenv --no-download $HOME/MESH-env$SLURM_ARRAY_TASK_ID 
source $HOME/MESH-env/bin/activate 

pip install --no-index --upgrade pip
pip install --no-index netcdf4
pip install --no-index h5netcdf
pip install --no-index xarray
pip install --no-index geopandas
pip install --no-index Rtree
pip install --no-index easymore

#year=$SLURM_ARRAY_TASK_ID
year=1980
python easymore_remapping.py "$year"
rm -rf $HOME/MESH-env$SLURM_ARRAY_TASK_ID
echo  finished