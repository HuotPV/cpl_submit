#!/bin/bash

##################################################################
#           Script to run the coupled model NEMO MAR             #
##################################################################

#--------------------#
#  Slurm options     #
#--------------------#

#SBATCH --job-name=CPL-tstst
#SBATCH --time=00:40:00
#SBATCH --mail-type=ALL
#SBATCH --open-mode=append
#SBATCH --switches=1@47:50:00
#SBATCH --ntasks=46
#SBATCH --mem-per-cpu=3072
#SBATCH --output=JAslurm-%j.out
#SBATCH --partition=batch

set -ueo pipefail

#--------------------#
# Experiment options #
#--------------------#

exp_name=CPL-tstst
run_start_date="2011-06-01"
run_duration="10 day"
info_file="nemo.info.$exp_name"
from_rest=1 # Option to start a new run using restart from CPL-april-rfifz0 -> Only possible if run_start E [2011-05-02 -- 2013-04-30]
            # And if there is no $info_file
	    # 0 by default

leg_length="1 day"  # divide run_duration in sub jobs of $leg_length
rst_freq=${leg_length}

homedir=$(pwd)

if [ $USER == ckittel ] ; then  
scratchd=/scratch/ulg/topoclim/$USER/nemo-mar_coupling/
archive_dir=${scratchd}/out/${exp_name}/nemo-archive/
archive_dir_mar=${scratchd}/out/${exp_name}/MAR-ICE/
fi
if [ $USER == phuot ] ; then
scratchd=/scratch/ucl/elic/${USER}/  # EVERYTHING has to be in this scratch (forcing, inputs files, codes ...)
archive_dir=${scratchd}nemo/archive/${exp_name}
fi

#-----------------#
#    Programs     #
#-----------------#

nem_exe_file=nemo_oa3_fixrad.exe
mar_exe_file=MAR_q22_mai_ref.exe 
xio_exe_file=xios_oa3.exe

#------------------#
# NEMO params      #
#------------------#

nem_time_step_sec=150
lim_time_step_sec=600
nem_restart_offset=0

oasis_dir=${scratchd}oasis24
code_dir=${scratchd}codes
ini_data_dir=${scratchd}data24

#-----------------#
#    MAR parm     #
#-----------------#

dt=60                              #MAR time step
mar_forcing="ERAI"                 #ERA-Int or ERA5 (or Mertz)
DIR="/scratch/ucl/elic/phuot/CK/"  #MAR code and inputs 

#------------------#
# Coupling options #
#------------------#

o2afreq=600                           # Frequency of ocean to atm exchange 
a2ofreq=600                           # Frequency of atm to ocean exchange
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
xio_numproc=8   # Number of procs for xios2
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
#    Function to count leap days             #
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

#---------------------------------------------------------#
#   Retrive start and time and associated time steps      #
#---------------------------------------------------------#

cd ${homedir}

# find run start and end, and leg duration !
if (( from_rest == 1 ))
then
before_std="${run_start_date} - ${leg_length:?}"
run_end_date="${run_start_date} + ${run_duration:?}"
run_start_date=$(date -uR -d "${before_std}")
run_end_date=$(date -uR -d "${run_end_date}")
run_start_epoch=$(date -u -d"${run_start_date}" +%s)
run_end_epoch=$(date -u -d"${run_end_date}" +%s)
else
run_start_date=$(date -uR -d "${run_start_date}")
run_end_date="${run_start_date} + ${run_duration:?}"
run_end_date=$(date -uR -d "${run_end_date}")
run_start_epoch=$(date -u -d"${run_start_date}" +%s)
run_end_epoch=$(date -u -d"${run_end_date}" +%s)
fi

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
leg_start_date_yyyymmdd=$(date -u -d "${leg_start_date}" +%Y%m%d) # FIXME appears unused

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


YYYY=$(date -d "${leg_start_date}" +%Y)
MM=$(date -d "${leg_start_date}" +%m)
DDs=$(date -d "${leg_start_date}" +%d)

YYYYb=$(date -d "${leg_start_date} - ${leg_length}" +%Y)
MMb=$(date -d "${leg_start_date} - ${leg_length}" +%m)
DDb=$(date -d "${leg_start_date} - ${leg_length}" +%d)

YYYYa=$(date -d "${leg_start_date} + ${leg_length}" +%Y)
MMa=$(date -d "${leg_start_date} + ${leg_length}" +%m)

if [ "$leg_length" = "1 day" ]; then
	if (( MMb==02 & DDb=="29")); then
		DDb=$(date -d "${leg_start_date} - ${leg_length} - ${leg_length}" +%d)
		# Previous day is 28th feb since we do not use leap years
	fi
fi

#----------------------------------------#
# Create rundir and link / gather files  #
#----------------------------------------#

# Restart management if special restart
if [ -f ${ini_data_dir}/${info_file} ]; then
    echo "${info_file} exist, normal restart"
else
    if (( from_rest == 0 ))
    then
        echo "${info_file} does not exist, normal start"
    else
        echo "${info_file} does not exist, force restart"
        source make_init.sh > debug_init
        echo "First leg, duplicate reference MARsim"

        [[ -r "${ini_data_dir}/${info_file:?}" ]] && source "${ini_data_dir}/${info_file:?}"  # READ info file if it exist ?

        echo "leg_end_date" ${leg_end_date}

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
        leg_start_date_yyyymmdd=$(date -u -d "${leg_start_date}" +%Y%m%d) # FIXME appears unused

        YYYY=$(date -d "${leg_start_date}" +%Y)
        MM=$(date -d "${leg_start_date}" +%m)
        DDs=$(date -d "${leg_start_date}" +%d)

        YYYYb=$(date -d "${leg_start_date} - ${leg_length}" +%Y)
        MMb=$(date -d "${leg_start_date} - ${leg_length}" +%m)
        DDb=$(date -d "${leg_start_date} - ${leg_length}" +%d)

        cp $DIR/MARsim/MARsim_${YYYY}${MM}${DDs}.tgz $DIR/MARsim/MARsim_${exp_name}_${YYYY}${MM}${DDs}.tgz

    fi
fi


run_dir=${scratchd}/${exp_name}-${YYYY}-${MM}-${DDs}

if [ ! -d ${run_dir:?} ]
then
     mkdir -p ${run_dir}
fi


cd ${homedir}
source prep_nemo_24.sh
cd ${homedir}
source prep_mar.sh
 [ $? -eq 4 ] && exit #error in prep_mar!

#---------------------------------------#
#    Actual execution of the thing      #
#---------------------------------------#

ns=$(printf %08d $(( leg_start_sec / nem_time_step_sec - nem_restart_offset )))

#new restarts 
if (( leg_number == 1 )) ; then
echo "WARNING New RESTARTS CK"
cp /scratch/ulg/topoclim/ckittel/nemo-mar_coupling/${cpl_oce_rst} ${run_dir}
cp /scratch/ulg/topoclim/ckittel/nemo-mar_coupling/${cpl_atm_rst} ${run_dir}
fi

if (( leg_number > 1 ))
then
   cp ${scratchd}/${exp_name}-${YYYYb}-${MMb}-${DDb}/${exp_name}_${ns}_restart_?ce* ${run_dir}
   cp ${scratchd}/${exp_name}-${YYYYb}-${MMb}-${DDb}/${cpl_oce_rst} ${run_dir}
   cp ${scratchd}/${exp_name}-${YYYYb}-${MMb}-${DDb}/${cpl_atm_rst} ${run_dir}
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
if ((leg_number > 1 )); then
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
 fi

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

outdirICE=$outdir
if [ $USER == ckittel ] ; then
outdirICE="${archive_dir_mar}/${YYYY}"
mkdir -p $outdirICE
fi

for ICEf in ICE*.nc ; do
 mv ${ICEf} $outdirICE
 [ ! -f $outdirICE/${ICEf} ] && echo "ERROR ${ICEf}.gz" && exit 8
done


if [ $USER == ckittel ] ; then
MARsim_r=$scratchd/input_MARsim/${exp_name}/${YYYYn}/
mkdir -p $MARsim_r
MARsim=MARsim_${YYYYn}${MMn}${DDn}.tgz
fi


if [ $USER == phuot ] ; then
MARsim_r=$DIR/MARsim/  #CK: il est deja cree quelque part chez toi?
MARsim=MARsim_${exp_name}_${YYYYn}${MMn}${DDn}.tgz
fi


tar czf MARsim_${exp_name}_${YYYYn}${MMn}${DDn}.tgz MARdom.dat MARcld.DAT MARcva.DAT MARdyn.DAT MARsol.DAT MARsvt.DAT MARtur.DAT

mv      $MARsim $MARsim_r
[ ! -f $MARsim_r/$MARsim ] && echo "ERROR MARsim_${YYYYn}${MMn}${DDn}${HHn}.tgz" && exit 9

#-----------------#
# Write nemo.info #
#-----------------#
   
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

