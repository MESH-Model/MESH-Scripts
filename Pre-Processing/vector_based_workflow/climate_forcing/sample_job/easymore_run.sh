#!/bin/bash
#SBATCH --account=def-someuser
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --mem=1G
#SBATCH --time=00:30:00
#SBATCH --job-name=easymore_extract
#SBATCH --error=errors
#SBATCH --mail-user=your.email@example.com
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

# calling one sample of easymore program 
python call_easymore.py 
# or calling it directly ./call_easymore.py. 

echo "finished"