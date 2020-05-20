# cpl_submit

Submission script for coupled NEMO/MAR jobs on lemaitre3

# Content:

run_cp_24_44.sh (or run_cp.sh): main script, containing user options

prep_nemo_24.sh: 'manage' input files for NEMO

prep_mar.sh: manage input files for MAR

build_namcouple.sh: build namcouple using specifications of run_cp_\*.sh

build_namelist_cfg_24.sh: build namelist for nemo (ocean component). Mostly hard coded (ice shelf cavities and stuff ...)

build_marctr.sh: build MARctr (set MAR time step and ?)

ForXIOS2/file_def*: files specifying NEMO and LIM I/O (fields, frequency ...)

clean_log.sh: clean slurm logs

make_init.sh: used for restarting nemo from another run (ongoing work)


# To do:
- clean script
- add namelist entries for mixed coupled/forced nemo

# Done:
- make MARsim files unique for each simulation (done if USER=/=Ckittel)
- fix issue with leap days (removed leap days for now)

