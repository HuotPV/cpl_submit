
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
echo "On va dans MAR-src-PV pour le moment ... /!/ "
cp -r ${DIR}/MAR-src-PV/* ${run_dir}/ #cp MAR.exe, MARdom; ICEvou.dat, RCPsc; MARcst + TROUPLE  #WARNING: MARdom to start from the run

#Input files from NESTOR for MAR
cp  ${DIR}/NESTOR/${YYYY}/${MM}/*.DAT ${run_dir}/

cd ${run_dir}
if [ -f $mar_exe_file ] ; then
    echo "MAR.exe found"
else
    echo "MAR ERROR NO MARexe <<<" && exit 4
fi

#MARsim files from previous MAR timestep

if [ -f $DIR/MARsim/MARsim_${YYYY}${MM}.tgz ] ; then
 echo "MARsim: $DIR/MARsim/MARsim_${YYYY}${MM}.tgz" 
 tar xzf $DIR/MARsim/MARsim_${YYYY}${MM}.tgz
 else
 echo "MAR ERROR NO MARsim <<<" && exit 4
fi

