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

# load modules for easymore dependencies
module load StdEnv/2020 gcc/9.3.0 openmpi/4.0.3
module load gdal/3.5.1 libspatialindex/1.8.5
module load python/3.8.10 scipy-stack/2022a mpi4py/3.0.3

# virtual environemnt 
# remove the virtual environment
rm -rf $HOME/easymore-env
# create the virtual env for the job
virtualenv --no-download $HOME/easymore-env 
# activate the virtual env
source $HOME/easymore-env/bin/activate 

# upgrade pip, install easymore and another dependency
pip install --no-index --upgrade pip
pip install --no-index easymore[complete]

# calling one sample of easymore program 
python call_easymore.py 
# or calling it directly ./call_easymore.py. 

echo "finished"
