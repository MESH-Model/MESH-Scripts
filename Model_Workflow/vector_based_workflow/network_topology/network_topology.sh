#!/bin/bash
#SBATCH --account=<def-someuser>
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --mem=20G
#SBATCH --time=00:10:00
#SBATCH --job-name=<job_name>
#SBATCH --error=errors
#SBATCH --mail-user=you@some.email.address
#SBATCH --mail-type=BEGIN
#SBATCH --mail-type=END

# load module 
module load python/3.8

# virtual environemnt 
virtualenv --no-download $SLURM_TMPDIR/env
source $SLURM_TMPDIR/env/bin/activate

pip install --no-index --upgrade pip
module load gdal/3.0.4 python/3.8 gcc/9.3.0 qgis/3.10.6 proj/7.0.1 geos/3.8.1 libspatialindex/1.8.5 mpi4py 

pip install --no-index Rtree
pip install --no-index easymore

python create_network_topology_file.py 

echo "finished"
