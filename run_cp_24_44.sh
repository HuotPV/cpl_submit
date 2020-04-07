#!/bin/bash

##################################################################
#           Script to run the coupled model NEMO MAR             #
##################################################################

#--------------------#
#  Slurm options     #
#--------------------#

#SBATCH --job-name=CPL-ref00
#SBATCH --time=00:40:00
#SBATCH --mail-type=ALL
#SBATCH --open-mode=append
#SBATCH --switches=1@47:50:00
#SBATCH --ntasks=44
#SBATCH --mem-per-cpu=3072
#SBATCH --output=JAslurm-%j.out

set -ueo pipefail

#--------------------#
# Experiment options #
#--------------------#

exp_name=CPL-ref00
run_start_date="2011-05-01"
run_duration="10 day"
info_file="nemo.info.$exp_name"

leg_length="1 day"  # divide run_duration in sub jobs of $leg_length
rst_freq=${leg_length}

homedir=$(pwd)
scratchd=/scratch/ucl/elic/${USER}/  # EVERYTHING has to be in this scratch (forcing, inputs files, codes ...)
archive_dir=${scratchd}nemo/archive/${exp_name}

nem_exe_file=nemo_oa3_fixrad.exe
mar_exe_file=MAR_q22.exe 
xio_exe_file=xios_oa3.exe

echo ${homedir}
cd ${homedir}
#------------------#
# NEMO params      #
#------------------#

nem_time_step_sec=150
lim_time_step_sec=450
nem_restart_offset=0

oasis_dir=${scratchd}oasis24
code_dir=${scratchd}codes
ini_data_dir=${scratchd}data24

#-----------------#
#    MAR parm     #
#-----------------#

dt=90                              #MAR time step
DIR="/scratch/ucl/elic/phuot/CK/"  #MAR code and inputs

#------------------#
# Coupling options #
#------------------#

o2afreq=450                           # Frequency of ocean to atm exchange 
a2ofreq=450                           # Frequency of atm to ocean exchange
cploutopt=EXPORTED                  
cpl_oce_rst=start_ocean_cpl_24.nc     # Initial restart for exchanged ocean variable
cpl_atm_rst=start_atmos_cpl_new.nc    # Initial restart for exchanged atmos variable
ndx=532                               # nx nemo grid
ndy=522                               # ny  ' ' ' ' 
mdx=150                               # nx  mar grid
mdy=140                               # ny  '  '  '

ntranst=2                             # Number of transformation ocean to atm
ntransd=2                             #  '     '       '       '  atm to ocean

transt1='LOCTRANS SCRIPR'             # Name of transformations ocean 2 atm
transt2='AVERAGE'                     # Option for trans #1
transt3='BILINEAR LR SCALAR LATLON 1' # Option for trans #2
transd1='LOCTRANS SCRIPR'
transd2='AVERAGE'
transd3='BILINEAR LR SCALAR LATLON 1'

#------------------#
# Job options      #
#------------------#

# general opts

module_list="2016a netCDF-Fortran/4.4.4-intel-2016a"   # Needed modules ...
extralibs_list=""

#sbatch opts

nem_numproc=37  # Number of procs for NEMO
xio_numproc=6   # Number of procs for xios2
mar_numproc=1   # Number of procs for MAR


################################################################################
#                !!! END OF NORMAL USER MODIFICATION !!!                       #
################################################################################


#------------------#
# Modules          #
#------------------#

module load ${module_list:?}
export LD_LIBRARY_PATH=${LD_LIBRARY_PATH:+${LD_LIBRARY_PATH}:}"${extralibs_list}"

#--------------------------------------------#
#    Prepare the RunDIr                      #
#--------------------------------------------#

function leap_days()
{
    local ld=0
    local frstYYYY=$(date -ud "$1" +%Y)
    local lastYYYY=$(date -ud "$2" +%Y)

    set +e

    $(date -ud "${frstYYYY}-02-29" > /dev/null 2>&1) \
    && (( $(date -ud "$1" +%s) < $(date -ud "${frstYYYY}-03-01" +%s) )) \
    && (( $(date -ud "$2" +%s) > $(date -ud "${lastYYYY}-02-28" +%s) )) \
    && (( ld++ ))

    for (( y=(( ${frstYYYY}+1 )); y<=(( ${lastYYYY}-1 )); y++ ))
    do
        $(date -ud "$y-02-29" > /dev/null 2>&1) && (( ld++ ))
    done

    (( $lastYYYY > $frstYYYY )) \
    && $(date -ud "${lastYYYY}-02-29" > /dev/null 2>&1) \
    && (( $(date -ud "$1" +%s) < $(date -ud "${frstYYYY}-03-01" +%s) )) \
    && (( $(date -ud "$2" +%s) > $(date -ud "${lastYYYY}-02-28" +%s) )) \
    && (( ld++ ))

    set -e

    echo "$ld"
}

#---------------------------------------#
#    Actual execution of the thing      #
#---------------------------------------#

cd ${homedir}

<<<<<<< HEAD
# find run start and end, and leg duration !
run_start_date=$(date -uR -d "${run_start_date}")
run_end_date="${run_start_date} + ${run_duration:?}"
run_end_date=$(date -uR -d "${run_end_date}")
run_start_epoch=$(date -u -d"${run_start_date}" +%s)
run_end_epoch=$(date -u -d"${run_end_date}" +%s)

# Maybe we need to find a way to bypass this if info_file exists but we don't want to use it ?
[[ -r "${ini_data_dir}/${info_file:?}" ]] && source "${ini_data_dir}/${info_file:?}"  # READ info file if it exist ?

leg_start_date=${leg_end_date:-$run_start_date}
leg_number=$((${leg_number:=0}+1))
leg_start_epoch=$(date -u -d "${leg_start_date}" +%s)
leg_end_epoch=$(date -u -d "${leg_start_date:?} + ${leg_length}" +%s)
leg_end_date=$(date -uR -d@"${leg_end_epoch}")
leg_length_sec=$(( leg_end_epoch - leg_start_epoch ))
leg_start_sec=$(( leg_start_epoch - run_start_epoch ))
leg_length_sec=$(( leg_length_sec  ))   # I've removed the leap day manager because he couldn't handle daily restarts ....
leg_start_sec=$(( leg_start_sec  ))
leg_end_sec=$(( leg_end_epoch - run_start_epoch ))
leg_end_sec=$(( leg_end_sec ))

leg_length_sec=$(( leg_length_sec - $(leap_days "${leg_start_date}" "${leg_end_date}")*24*3600 ))
leg_start_sec=$(( leg_start_sec - $(leap_days "${run_start_date}" "${leg_start_date}")*24*3600 ))
leg_end_sec=$(( leg_end_sec - $(leap_days "${run_start_date}" "${leg_end_date}")*24*3600 ))

if [ "$leg_length" = "1 day" ]; then
	mm=$(date -d "${leg_end_date}" +%m)
        dd=$(date -d "${leg_end_date}" +%d)
        if (( mm==02 & dd=="29")); then
        	echo "Next day should be 29th of feb, but we go to 1st of march directly."
                leg_end_ep=$(date -u -d "${leg_start_date:?} + ${leg_length} + ${leg_length}" +%s)
                leg_end_epoch=$(date -u -d "${leg_start_date:?} + ${leg_length}" +%s)
                leg_end_date=$(date -uR -d@"${leg_end_ep}")
                leg_length_sec=$(( leg_end_epoch - leg_start_epoch ))
                leg_start_sec=$(( leg_start_epoch - run_start_epoch ))
                leg_end_sec=$(( leg_end_epoch - run_start_epoch ))
        fi
fi

leg_start_date_yyyymmdd=$(date -u -d "${leg_start_date}" +%Y%m%d)

YYYY=$(date -d "${leg_start_date}" +%Y)
MM=$(date -d "${leg_start_date}" +%m)
DDs=$(date -d "${leg_start_date}" +%d)
#HH=$(date -d "${leg_start_date}" +%H)

YYYYb=$(date -d "${leg_start_date} - ${leg_length}" +%Y)
MMb=$(date -d "${leg_start_date} - ${leg_length}" +%m)
DDb=$(date -d "${leg_start_date} - ${leg_length}" +%d)

if [ "$leg_length" = "1 day" ]; then
	if (( MMb==02 & DDb=="29")); then
		DDb=$(date -d "${leg_start_date} - ${leg_length} - ${leg_length}" +%d)
		# Previous day is 28th feb since we do not use leap years
	fi
fi


#HHb=$(date -d "${leg_start_date} - ${leg_length}" +%H)
#----------------------------------------#
# Create rundir and link / gather files  #
#----------------------------------------#




run_dir=${scratchd}/${exp_name}-${YYYY}-${MM}-${DDs}

if [ ! -d ${run_dir:?} ]
then
     mkdir -p ${run_dir}
fi

source prep_nemo_24.sh
cd $homedir
source prep_mar.sh
 [ $? -eq 4 ] && exit #error in prep_mar!

#---------------------------------------#
#    Actual execution of the thing      #
#---------------------------------------#

ns=$(printf %08d $(( leg_start_sec / nem_time_step_sec - nem_restart_offset )))

if (( leg_number > 1 ))
then
   cp ${scratchd}/${exp_name}-${YYYYb}-${MMb}-${DDb}/${exp_name}_${ns}_restart_?ce* ${run_dir}
#   cp ${scratchd}/${exp_name}-${YYYYb}-${MMb}-${DDb}/${cpl_oce_rst} ${run_dir}
#   cp ${scratchd}/${exp_name}-${YYYYb}-${MMb}-${DDb}/${cpl_atm_rst} ${run_dir}
fi

(( leg_number > 1 )) && leg_is_restart=true || leg_is_restart=false

(( leg_end_epoch > run_end_epoch )) && leg_end_date=${run_end_epoch}

if (( leg_start_epoch >= run_end_epoch ))
  then
     echo "Leg start date equal to or after end of simulation."
     echo "Nothing left to do. Cleaning and exiting."
     for (( n=0 ; n<nem_numproc ; n++ ))
	do
            np=$(printf %04d ${n})
	    rm -f "restart_oce_${np}.nc"
	    rm -f "restart_ice_${np}.nc"
	done
        exit 0
fi

cd $homedir

# Build the namelist / namcouple

source build_namelist_cfg_24.sh > namelist_cfg
source build_namcouple.sh > namcouple
mv namelist_cfg ${run_dir}
mv namcouple ${run_dir}

cd ${run_dir}

ns=$(printf %08d $(( leg_start_sec / nem_time_step_sec - nem_restart_offset )))
if ((leg_start_sec > 0 )); then
for (( n=0 ; n<nem_numproc ; n++ ))
  do
    np=$(printf %04d ${n})
    [[ -f "${exp_name:?}_${ns}_restart_oce_${np}.nc" ]] || { echo "Error: restart file not found." ; exit 2 ; }
    ln -fs "${exp_name:?}_${ns}_restart_oce_${np}.nc" "restart_oce_${np}.nc"
    [[ -f "${exp_name:?}_${ns}_restart_ice_${np}.nc" ]] || { echo "Error: restart file not found." ; exit 2 ; }
    ln -fs "${exp_name:?}_${ns}_restart_ice_${np}.nc" "restart_ice_${np}.nc"
  done
fi

#---------------------------------------#
#             Actual run                #
#---------------------------------------#
module load ${module_list:?}

#while [ ! -f MAR.OK ] ; do
pwd
cd ${run_dir}

[[ $@ == *verbose* ]] && set -x


ulimit -s unlimited
rm -f MAR.log MARphy.out &> /dev/null
export OMP_NUM_THREADS=${mar_numproc}

time_begin=$(date +%s)
mpirun -np "${nem_numproc:?}" ./"${nem_exe_file:?}" : -np "${xio_numproc:?}" ./"${xio_exe_file:?}" :  -np "${mar_numproc}" ./"${mar_exe_file:?}" > log_cpl
time_end=$(date +%s)


 if [ ! -f MAR.OK ] ; then
  echo "MAR crash at dt=$dt"
  echo "Crash MAR" && exit 5
#  tar xzf $DIR/MARsim/MARsim_${YYYY}${MM}.tgz
 fi

#done

#
# Move outputs and stuff out of rundir 
#


formatted_leg_number=$(printf %03d $((leg_number)))
outdir="${archive_dir:?}/output/${formatted_leg_number}"
mkdir -p "${outdir}"

shopt -s nullglob

outdir="$archive_dir/restart/${formatted_leg_number}"
mkdir -p "${outdir}"

#-----------------#
# MAR outputs 
#-----------------#

date_next=${leg_end_date}

YYYYn=$(date -d "${date_next}" +%Y)
MMn=$(date -d "${date_next}" +%m)
DDn=$(date -d "${date_next}" +%d)

gzip ICE*.nc
tar czf MARsim_${exp_name}_${YYYYn}${MMn}${DDn}.tgz MARdom.dat MARcld.DAT MARcva.DAT MARdyn.DAT MARsol.DAT MARsvt.DAT MARtur.DAT

mv      MARsim_${exp_name}_${YYYYn}${MMn}${DDn}.tgz $DIR/MARsim/
[ ! -f $DIR/MARsim/MARsim_${exp_name}_${YYYYn}${MMn}${DDn}.tgz ] && echo "ERROR MARsim_${YYYYn}${MMn}${DDn}${HHn}.tgz" && exit 8


outdir="$archive_dir/log/${formatted_leg_number}"
mkdir -p "${outdir}"
for f in ocean.output time.step solver.stat ; do mv "${f}" "${outdir}"; done
shopt -u nullglob

#
# Write nemo.info
#
   
tr=$(date -d "0 -$time_begin sec + $time_end sec" +%T) 
current_date=$(date +'%F %T')
{
  echo "#"
  echo "# Finished leg at ${current_date} after ${tr} (hh:mm:ss)" 
  echo "leg_number=${leg_number}"
  echo "leg_start_date=\"${leg_start_date}\""
  echo "leg_end_date=\"${leg_end_date}\""
} | tee -a "${info_file}"

cp ${info_file} ${ini_data_dir}/

#
# End of leg: resubmit or not ?
#

cd - >/dev/null
[[ $@ == *noresubmit* ]] && exit 0


if (( leg_end_epoch < run_end_epoch )) ; then
    cd ${homedir}
    echo "Leg end earlier than end of simulation."
    echo "Submitting another job."
    sleep 20
    "qsub" -v PBS_OPTIONS="$@" "$0" | tee -a coral_jobs
    sleep 2
    jobid=`cat coral_jobs`
    rm -f coral_jobs
    jobid=${jobid%.*}
    echo "${jobid}" >> "${run_dir}"/.coral_jobs
else
    echo "Nothing left to do. Cleaning and exiting." # FIXME Factorize this (we have two exit points)
    for (( n=0 ; n<nem_numproc ; n++ ))
    do
        np=$(printf %04d ${n})
        rm -f "restart_oce_${np}.nc"
        rm -f "restart_ice_${np}.nc"
    done
fi

exit 0

