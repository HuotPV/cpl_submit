#!/bin/bash
# We make nemo believe he already did one leg before
# Script to initialize the first leg if start from restart

# Create rundir

cp -R ${scratchd}CPL-april-rfifz0-${YYYY}-${MM}-${DDs} ${scratchd}${exp_name}-${YYYY}-${MM}-${DDs}

cd ${scratchd}${exp_name}-${YYYY}-${MM}-${DDs}

for proc in $(seq 0 $nem_numproc)
do
np=$(printf %04d ${proc})
fil1=restart_ice_${np}.nc
fil2=restart_oce_${np}.nc
echo ${fil1}
rm -f "$(readlink ${fil1})"
rm -f "$(readlink ${fil2})"
done


# Create nemo.info

date_before1=$(date -uR -d "${leg_start_date}")
date_after=$(date -uR -d "${leg_start_date} + ${leg_length}")
current_date=$(date +'%F %T')

{
  echo "#"
  echo "# Finished leg at ${current_date} after  (hh:mm:ss)" 
  echo "leg_number=2"
  echo "leg_start_date=\"${date_before1}\""
  echo "leg_end_date=\"${date_after}\""
} | tee -a "${info_file}"

cp ${info_file} ${ini_data_dir}/

tsss=$(( leg_length_sec / nem_time_step_sec ))

# Rename ocean restart
dirtmp=${scratchd}${exp_name}-${YYYY}-${MM}-${DDs}
npc=$(( nem_numproc - 1 ))
tss=$(printf %08d ${tsss})

for proc in $(seq 0 $npc)
do
np=$(printf %04d ${proc})
mv ${dirtmp}/*_*_restart_oce_${np}.nc ${dirtmp}/${exp_name}_${tss}_restart_oce_${np}.nc
mv ${dirtmp}/*_*_restart_ice_${np}.nc ${dirtmp}/${exp_name}_${tss}_restart_ice_${np}.nc
done

