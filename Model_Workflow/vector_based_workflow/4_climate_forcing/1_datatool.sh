#!/bin/bash
#SBATCH --account=rpp-kshook
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --mem=256M
#SBATCH --time=00:10:00
#SBATCH --job-name=fextract1980
#SBATCH --error=errors80
#SBATCH --mail-user=cooper.albano@usask.ca
#SBATCH --mail-type=BEGIN
#SBATCH --mail-type=END

cd 
#rm -rf $HOME/scratch/rdrs_output/
rm -rf datatool
git clone https://github.com/kasra-keshavarz/datatool # clone the repository
cd datatool

./extract-dataset.sh  --dataset=RDRS \
  --dataset-dir="/project/rpp-kshook/Model_Output/RDRSv2.1" \
  --output-dir="$HOME/scratch/rdrs_output/" \
  --start-date="1980-01-01 00:00:00" \
  --end-date="1980-12-31 00:00:00" \
  --lat-lims=0,89  \
  --lon-lims=-182,0 \
  --variable="RDRS_v2.1_P_P0_SFC,RDRS_v2.1_P_HU_09944,RDRS_v2.1_P_TT_09944,RDRS_v2.1_P_UVC_09944,RDRS_v2.1_A_PR0_SFC,RDRS_v2.1_P_FB_SFC,RDRS_v2.1_P_FI_SFC" \
  --prefix="rdrsv2.1_" \
  --email="example.email@usask.ca" \
  -j;
  
  cd ../scratch

