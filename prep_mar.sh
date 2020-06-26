
[ ${MM} -eq  1 ] && DD=31
[ ${MM} -eq  2 ] && DD=28
[ ${MM} -eq  3 ] && DD=31
[ ${MM} -eq  4 ] && DD=30
[ ${MM} -eq  5 ] && DD=31
[ ${MM} -eq  6 ] && DD=30
[ ${MM} -eq  7 ] && DD=31
[ ${MM} -eq  8 ] && DD=31
[ ${MM} -eq  9 ] && DD=30
[ ${MM} -eq 10 ] && DD=31
[ ${MM} -eq 11 ] && DD=30
[ ${MM} -eq 12 ] && DD=31
echo $MM $DDs



# need to use leg_length here ....

DDD=$(( ${leg_length_sec} / 86400 ))
echo Number of days in prep mar
echo ${leg_length_sec}
echo $DDD
nbo=$((21600 / $dt))
np=$(( $DDD * 4 ))

source build_marctr.sh > MARctr.dat #TO DO chemin vers build car a refaire si necessaire dans le run?

cp MARctr.dat ${run_dir}

#General input files for MAR

if [ $USER == ckittel ] ; then
echo "cp -r ${DIR}/MAR-src-CK/* ${run_dir}"
cp -r ${DIR}/MAR-src-CK/*.DAT ${run_dir}/ 
cp -r ${DIR}/MAR-src-CK/*.dat ${run_dir}/ 
cp -r ${DIR}/MAR-src-CK/*.ctr ${run_dir}/ 
cp -r ${DIR}/MAR-src-CK/${mar_exe_file} ${run_dir}/ 
cp -r ${DIR}/MAR-src-CK/TROUPLE-150x140.cdf ${run_dir}/ 
cp -r ${DIR}/MAR-src-CK/TROUPLE-AN10km-150x140.cdf ${run_dir}/
fi

if [ $USER == phuot ] ; then
echo "On va dans MAR-src-PV pour le moment ... /!/ "
cp -r ${DIR}/MAR-src-PV/*.DAT ${run_dir}/ 
cp -r ${DIR}/MAR-src-PV/*.dat ${run_dir}/ 
cp -r ${DIR}/MAR-src-PV/*.ctr ${run_dir}/ 
cp -r ${DIR}/MAR-src-PV/${mar_exe_file} ${run_dir}/ 
cp -r ${DIR}/MAR-src-PV/TROUPLE-150x140.cdf ${run_dir}/ 
cp -r ${DIR}/MAR-src-PV/TROUPLE-AN10km-150x140.cdf ${run_dir}/ 
fi


#Input files from NESTOR for MAR
cp  ${DIR}/NESTOR-${mar_forcing}/${YYYY}/${MM}/*.DAT ${run_dir}/
cp  ${DIR}/NESTOR-${mar_forcing}/${YYYYa}/${MMa}/*.DAT ${run_dir}/

cd ${run_dir}
if [ -f $mar_exe_file ] ; then
    echo "MAR.exe found"
else
    echo "MAR ERROR NO MARexe <<<" && exit 4
fi

#MARsim files from previous MAR timestep
if (( leg_number == 1 )) ; then #First leg
 echo "First leg, duplicate reference MARsim"

 MARsim="$DIR/MARsim/MARsim_${YYYY}${MM}${DDs}.tgz"

 if [ -f $MARsim ] ; then
  echo "MARsim first time step: $MARsim" 
  tar xzf $MARsim
 else
  echo "MAR ERROR NO MARsim FOR FIRST LEG <<<" && exit 4
 fi

else #next step

#def of the files

  MARsim="$DIR/MARsim/MARsim_${exp_name}_${YYYY}${MM}${DDs}.tgz"


 if [ -f $MARsim ] ; then
  echo "MARsim: $MARsim" 
  tar xzf $MARsim
  tar xzf $MARsim
 else
  echo "MAR ERROR NO MARsim <<<" && exit 4
 fi
fi

