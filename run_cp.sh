#!/bin/bash
#SBATCH --job-name=ADEL24_cpl
#SBATCH --time=0:30:00
#SBATCH --mail-type=ALL
#SBATCH --open-mode=append
#SBATCH --switches=1@47:50:00
#SBATCH --ntasks=7
#SBATCH --partition=debug
#SBATCH --mem-per-cpu=3072
#SBATCH --output=JAslurm-%j.out
set -ueo pipefail

##################################################################
#   Script to run the coupled model NEMO MAR                     #
#   bla bla and stuff                                            #
##################################################################

#--------------------#
# Experiment options #
#--------------------#

exp_name=CPL-6h
run_start_date="2011-01-01"
run_duration="2 day"
info_file="nemo.info.$exp_name"

leg_length="6 hour"  # Parceque dans MAR c'est plus ou moins : en dur / fixe ?
rst_freq=${leg_length}

homedir=/home/ucl/elic/phuot/script_cpl_sub2/cpl_submit/
scratchd=/scratch/ucl/elic/${USER}/
archive_dir=${scratchd}nemo/archive/${exp_name}

nem_exe_file=nemo.exe
mar_exe_file=MAR_sivelo2.exe
xio_exe_file=xios_server.exe

echo ${homedir}
cd ${homedir}
#------------------#
# NEMO params      #
#------------------#

nem_time_step_sec=900
lim_time_step_sec=900
nem_restart_offset=0

oasis_dir=${scratchd}oasis
code_dir=${scratchd}codes
ini_data_dir=${scratchd}data

#-----------------#
#    MAR parm     #
#-----------------#

dt=90
DIR="/scratch/ucl/elic/phuot/CK/"

#------------------#
# Coupling options #
#------------------#

o2afreq=900
a2ofreq=900
cploutopt=EXPORTED
cpl_oce_rst=start_ocean_cpl_025.nc
cpl_atm_rst=start_atmos_cpl_new.nc
ndx=124
ndy=144
mdx=150
mdy=140

#------------------#
# Job options      #
#------------------#

# general opts

module_list="2016a netCDF-Fortran/4.4.4-intel-2016a"
extralibs_list=""

#sbatch opts

nem_numproc=4
xio_numproc=2
mar_numproc=1

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
leg_length_sec=$(( leg_length_sec - $(leap_days "${leg_start_date}" "${leg_end_date}")*24*3600 ))
leg_start_sec=$(( leg_start_sec - $(leap_days "${run_start_date}" "${leg_start_date}")*24*3600 ))
leg_end_sec=$(( leg_end_epoch - run_start_epoch ))
leg_end_sec=$(( leg_end_sec - $(leap_days "${run_start_date}" "${leg_end_date}")*24*3600 ))
leg_start_date_yyyymmdd=$(date -u -d "${leg_start_date}" +%Y%m%d) # FIXME appears unused

YYYY=$(date -d "${leg_start_date}" +%Y)
MM=$(date -d "${leg_start_date}" +%m)
DDs=$(date -d "${leg_start_date}" +%d)
HH=$(date -d "${leg_start_date}" +%H)

YYYYb=$(date -d "${leg_start_date} - ${leg_length}" +%Y)
MMb=$(date -d "${leg_start_date} - ${leg_length}" +%m)
DDb=$(date -d "${leg_start_date} - ${leg_length}" +%d)
HHb=$(date -d "${leg_start_date} - ${leg_length}" +%H)
#----------------------------------------#
# Create rundir and link / gather files  #
#----------------------------------------#

run_dir=${scratchd}/${exp_name}-${YYYY}-${MM}-${DDs}-${HH}

if [ ! -d ${run_dir:?} ]
then
     mkdir -p ${run_dir}
fi

source prep_nemo.sh
cd $homedir
source prep_mar.sh
 [ $? -eq 4 ] && exit #error in prep_mar!

#---------------------------------------#
#    Actual execution of the thing      #
#---------------------------------------#

if (( leg_number > 1 ))
then
   cp ${scratchd}/${exp_name}-${YYYYb}-${MMb}-${DDb}-${HHb}/${exp_name}*_restart_?ce* ${run_dir}
#  cp ${scratchd}/${exp_name}-${YYYYb}-${MMb}-${DDb}-${HHb}/nemo.info ${run_dir}
   cp ${scratchd}/${exp_name}-${YYYYb}-${MMb}-${DDb}-${HHb}/${cpl_oce_rst} ${run_dir}
   cp ${scratchd}/${exp_name}-${YYYYb}-${MMb}-${DDb}-${HHb}/${cpl_atm_rst} ${run_dir}
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

source build_namelist_cfg.sh > namelist_cfg
source build_namcouple.sh > namcouple
mv namelist_cfg ${run_dir}
mv namcouple ${run_dir}
# done in prep_mar.sh for MAR

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

#while [ ! -f MAR.OK ] ; do
pwd
cd ${run_dir}

[[ $@ == *verbose* ]] && set -x


 ulimit -s unlimited
 rm -f MAR.log MARphy.out &> /dev/null

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
for v in grid_U grid_V grid_W grid_T icemod icemoa SBC SBC_scalar scalar
do
    for f in ${exp_name}_??_????????_????????_${v}_????.nc
    do
         mv "$f" "$outdir/"
    done
    for f in ${exp_name}_??_????????_????????_${v}.nc
    do
         mv "$f" "$outdir/"
    done
    for f in ${exp_name}_??_${v}.nc
    do
        mv "$f" "$outdir/"
    done
done

outdir="$archive_dir/restart/${formatted_leg_number}"
mkdir -p "${outdir}"

#-----------------#
# MAR outputs 
#-----------------#

# Another way to do it ....
date_next=${leg_end_date}

YYYYn=$(date -d "${date_next}" +%Y)
MMn=$(date -d "${date_next}" +%m)
DDn=$(date -d "${date_next}" +%d)
HHn=$(date -d "${date_next}" +%H)

gzip ICE*.nc
tar czf MARsim_${YYYYn}${MMn}${DDn}${HHn}.tgz MARdom.dat MARcld.DAT MARcva.DAT MARdyn.DAT MARsol.DAT MARsvt.DAT MARtur.DAT

mv      MARsim_${YYYYn}${MMn}${DDn}${HHn}.tgz $DIR/MARsim/
[ ! -f $DIR/MARsim/MARsim_${YYYYn}${MMn}${DDn}${HHn}.tgz ] && echo "ERROR MARsim_${YYYYn}${MMn}${DDn}${HHn}.tgz" && exit 8



#for f in ${exp_name}_${ns}_restart_???_????.nc
#do
#    [ -f "$f" ] && mv "$f" "${outdir}"
#done

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

