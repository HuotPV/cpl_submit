# namelist.nemo-ORCA1L75.cfg.sh writes the NEMO namelist for ORCA1L75 in
# This namelist will overwrite the reference namelist (namelist.nemo.ref.sh). 
#
if $leg_is_restart
then
    nemo_restart=".TRUE."
else
    nemo_restart=".FALSE." # ATTENTION FALSE ICI si pas de restart manuel
fi

cat << EOF
! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
! NEMO/OPA  Configuration namelist : used to overwrite defaults values defined in SHARED/namelist_ref
! >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
!
!-----------------------------------------------------------------------
&namrun        !   parameters of the run
!-----------------------------------------------------------------------
   cn_exp        =  "${exp_name}" !  experience name
   nn_it000      =  $(( leg_start_sec / nem_time_step_sec + 1 )) !  first time step
   nn_itend      =  $(( leg_end_sec / nem_time_step_sec ))       !  last  time step (std 5475)
   nn_date0      =  ${leg_start_date_yyyymmdd} !  date at nit_0000 (format yyyymmdd)

                                  !    used if ln_rstart=F or
                                  !    (ln_rstart=T and nn_rstctl=0 or 1)
   ln_rstart     =  ${nemo_restart}	!  start from rest (F) or from a restart file (T)
   nn_rstctl     =  2             !  restart control ==> activated only if ln_rstart=T
                                  !  = 0 nn_date0 read in namelist ; nn_it000 : read in namelist
                                  !  = 1 nn_date0 read in namelist ; nn_it000 : check consistancy between namelist and restart
                                  !  = 2 nn_date0 read in restart  ; nn_it000 : check consistancy between namelist and restart
   cn_ocerst_in  = "restart_oce"  !  suffix of ocean restart name (input)
   cn_ocerst_out = "restart_oce"  !  suffix of ocean restart name (output)
   nn_stock      =  0            !  frequency of creation of a restart file (modulo referenced to 1)
   nn_write      = -1             !  frequency of write in the output file   (modulo referenced to nn_it000)
/
!-----------------------------------------------------------------------
&namcfg        !   parameters of the configuration
!-----------------------------------------------------------------------
   cp_cfg      =  "ORCA1"               !  name of the configuration
   jp_cfg      =    1               !  resolution of the configuration
   jpidta      =     124               !  1st lateral dimension ( >= jpi )
   jpjdta      =     144               !  2nd    "         "    ( >= jpj )
   jpkdta      =      75               !  number of levels      ( >= jpk )
   jpiglo      =     124               !  1st dimension of global domain --> i =jpidta
   jpjglo      =     144               !  2nd    -                  -    --> j  =jpjdta
   jperio      =       0               !  6 cyclic East-West AND North fold F-point pivot
   ln_use_jattr = .true.               !  use (T) the file attribute: open_ocean_jstart if present
/
!-----------------------------------------------------------------------
&namzgr        !   vertical coordinate
!-----------------------------------------------------------------------
/
!-----------------------------------------------------------------------
&namzgr_sco    !   s-coordinate or hybrid z-s-coordinate
!-----------------------------------------------------------------------
/
!-----------------------------------------------------------------------
&namdom        !   space and time domain (bathymetry, mesh, timestep)
!-----------------------------------------------------------------------
   nn_msh      =    0                  !  create (=1) a mesh file or not (=0)
   rn_hmin     =    6.                !  min depth of the ocean (>0) or min number of ocean level (<0) COM 20 ici normalement
   rn_rdt      = ${nem_time_step_sec} !  time step for the dynamics (and tracer if nn_acc=0)
   ppglam0     =  999999.0             !  longitude of first raw and column T-point (jphgr_msh = 1)
   ppgphi0     =  999999.0             !  latitude  of first raw and column T-point (jphgr_msh = 1)
   ppe1_deg    =  999999.0             !  zonal      grid-spacing (degrees)
   ppe2_deg    =  999999.0             !  meridional grid-spacing (degrees)
   ppe1_m      =  999999.0             !  zonal      grid-spacing (degrees)
   ppe2_m      =  999999.0             !  meridional grid-spacing (degrees)
   ppsur       =  -3958.951371276829   !  ORCA r4, r2 and r05 coefficients
   ppa0        =    103.95300960000000 ! (default coefficients)
   ppa1        =      2.41595126900000 !
   ppkth       =     15.35101370000000 !
   ppacr       =      7.0              !
   ppdzmin     =  999999.              !  Minimum vertical spacing
   pphmax      =  999999.              !  Maximum depth
   ppa2        =  100.7609285000000    !  Double tanh function parameters
   ppkth2      =  48.02989372000000    !
   ppacr2      =  13.00000000000       !
/
!-----------------------------------------------------------------------
&namsplit      !   time splitting parameters  ("key_dynspg_ts")
!-----------------------------------------------------------------------
/
!-----------------------------------------------------------------------
&namcrs        !   Grid coarsening for dynamics output and/or
               !   passive tracer coarsened online simulations
!-----------------------------------------------------------------------
/
!-----------------------------------------------------------------------
&namc1d        !   1D configuration options                             ("key_c1d")
!-----------------------------------------------------------------------
/
!-----------------------------------------------------------------------
&namtsd    !   data : Temperature  & Salinity
!-----------------------------------------------------------------------
!          !  file name                                            ! frequency (hours) ! variable  ! time interp. !  clim  ! 'yearly'/ ! weights                                         ! rotation ! land/sea mask !
!          !                                                       !  (if <0  months)  !   name    !   (logical)  !  (T/F) ! 'monthly' ! filename                                        ! pairing  ! filename      !
!   sn_tem  = '1_EXP_REF_2010_gridT.nc',         -1        ,'thetao' ,    .true.    , .true. , 'yearly'  ,'',   ''     ,    ''
   sn_tem  = 'startT.nc',         -1        ,'votemper'        ,    .true.    , .true. , 'yearly'  ,'',   ''     ,    ''
   sn_sal  = 'startS.nc'       ,         -1        ,'vosaline' ,    .true.    , .true. , 'yearly'  ,'',   ''     ,    ''

!   sn_sal  = '1_EXP_REF_2010_gridT.nc'       ,         -1        ,'so' ,    .true.    , .true. , 'yearly'  ,'',   ''     ,    ''
   !
   ln_tsd_init   = .true.   !  Initialisation of ocean T & S with T &S input data (T) or not (F) !MODIF

   ln_tsd_tradmp = .false.  !  damping of ocean T & S toward T &S input data (T) or not (F)
/
!-----------------------------------------------------------------------
&namsbc        !   Surface Boundary Condition (surface module)
!-----------------------------------------------------------------------
   nn_fsbc     = $(( lim_time_step_sec / nem_time_step_sec )) !  frequency of surface boundary condition computation
                           !     (also = the frequency of sea-ice model call)
   nn_isf      = 0         !  ice shelf melting/freezing                (/=0 => fill namsbc_isf)
                           !  0 =no isf                  1 = presence of ISF
                           !  2 = bg03 parametrisation   3 = rnf file for isf
                           !  4 = ISF fwf specified
                           !  option 1 and 4 need ln_isfcav = .true. (domzgr)
   nn_fwb      = 0         !  FreshWater Budget: =0 unchecked
                           !     =1 global mean of e-p-r set to zero at each time step
                           !     =2 annual global mean of e-p-r set to zero
   ln_ssr      = .false.    !  Sea Surface Restoring on T and/or S       (T => fill namsbc_ssr)
   ln_rnf      = .true.    !  runoffs                                   (T   => fill namsbc_rnf) COM test

   nn_lsm=0

   ln_cpl      = .true.   !  atmosphere coupled   formulation          ( requires key_oasis3 )
/
!-----------------------------------------------------------------------
&namsbc_ana    !   analytical surface boundary condition
!-----------------------------------------------------------------------
/
!-----------------------------------------------------------------------
&namsbc_flx    !   surface boundary condition : flux formulation
!-----------------------------------------------------------------------
/
!-----------------------------------------------------------------------
&namsbc_clio   !   namsbc_clio  CLIO bulk formulae
!-----------------------------------------------------------------------
!              !  file name  ! frequency (hours) ! variable  ! time interp. !  clim  ! 'yearly'/ ! weights  ! rotation ! land/sea mask !
!              !             !  (if <0  months)  !   name    !   (logical)  !  (T/F) ! 'monthly' ! filename ! pairing  ! filename      !
!   sn_utau     = 'uwnd_10m'  ,       24          , 'uwnd'    ,   .true.     , .false., 'yearly'  , ''       , ''
!   sn_vtau     = 'vwnd_10m'  ,       24          , 'vwnd'    ,   .true.     , .false., 'yearly'  , ''       , ''
!   sn_wndm     = 'ncep_bulk' ,       24          , 'wspd'    ,   .true.     , .false., 'yearly'  , ''       , ''
!   sn_tair     = 'ncep_bulk' ,       24          , 'air'     ,   .true.     , .false., 'yearly'  , ''       , ''
!   sn_humi     = 'flx_correc',       -1          , 'socliohu',   .true.     , .true. , 'yearly'  , ''       , ''
!   sn_ccov     = 'flx_correc',       -1          , 'socliocl',   .true.     , .true. , 'yearly'  , ''       , ''
!   sn_prec     = 'flx_correc',       -1          , 'socliopl',   .true.     , .true. , 'yearly'  , ''       , ''
/
!-----------------------------------------------------------------------
&namsbc_core   !   namsbc_core  CORE bulk formulae
!-----------------------------------------------------------------------
!              !  file name              ! frequency (hours) ! variable ! time interp. !  clim   ! 'yearly'/ ! weights                                     ! rotation ! land/sea mask !
!              !                         !  (if <0  months)  !   name   !   (logical)  !  (T/F)  ! 'monthly' ! filename                                    ! pairing  ! filename      !
   ln_blk_core = .false. ! CORE bulk formulation (T => fill namsbc_core)
   sn_wndi     = 'drowned_u10_DFS5.2'    ,        3         , 'u10'    ,   .true.     , .false. , 'yearly'  , 'weights_bicub_DFStoBETA025.nc' , 'U1'     , ''
   sn_wndj     = 'drowned_v10_DFS5.2'    ,        3         , 'v10'    ,   .true.     , .false. , 'yearly'  , 'weights_bicub_DFStoBETA025.nc' , 'V1'     , ''
   sn_qsr      = 'drowned_radsw_DFS5.2'  ,        24         , 'radsw'  ,   .true.     , .false. , 'yearly'  , 'weights_bilin_DFStoBETA025.nc' , ''       , ''
   sn_qlw      = 'drowned_radlw_DFS5.2'  ,        24         , 'radlw'  ,   .true.     , .false. , 'yearly'  , 'weights_bilin_DFStoBETA025.nc' , ''       , ''
   sn_tair     = 'drowned_t2_DFS5.2'     ,        3         , 't2'     ,   .true.     , .false. , 'yearly'  , 'weights_bilin_DFStoBETA025.nc' , ''       , ''
   sn_humi     = 'drowned_q2_DFS5.2'     ,        3         , 'q2'     ,   .true.     , .false. , 'yearly'  , 'weights_bilin_DFStoBETA025.nc' , ''       , ''
   sn_prec     = 'drowned_precip_DFS5.2' ,        24         , 'precip' ,   .true.     , .false. , 'yearly'  , 'weights_bilin_DFStoBETA025.nc' , ''       , ''
   sn_snow     = 'drowned_snow_DFS5.2'   ,        24         , 'snow'   ,   .true.     , .false. , 'yearly'  , 'weights_bilin_DFStoBETA025.nc' , ''       , ''
!   nn_limflx = 0          !  LIM3 Multi-category heat flux formulation (use -1 if LIM3 is not used) (namsbc aussi)
   rn_zqt      = 2.        !  Air temperature and humidity reference height (m)
/
!-----------------------------------------------------------------------
&namsbc_mfs   !   namsbc_mfs  MFS bulk formulae
!-----------------------------------------------------------------------
/
!-----------------------------------------------------------------------
&namsbc_cpl    !   coupled ocean/atmosphere model                       ("key_coupled")
!-----------------------------------------------------------------------
!                    !     description       !  multiple  !    vector   !      vector          ! vector !
!                    !                       ! categories !  reference  !    orientation       ! grids  !
! send
   sn_snd_temp   =       'weighted oce and ice' ,    'no'    ,     ''      ,         ''           ,   ''
!   sn_snd_temp   =       'none' ,    'no'    ,     ''      ,         ''           ,   ''
   sn_snd_alb    =       'none'         ,    'no'    ,     ''      ,         ''           ,   ''
   sn_snd_thick  =       'none'                 ,    'no'   ,     ''      ,         ''           ,   ''
   sn_snd_crt    =       'none'                 ,    'no'    , 'spherical' , 'eastward-northward' ,  'T'
   sn_snd_co2    =       'none'              ,    'no'    ,     ''      ,         ''           ,   ''
! receive
   sn_rcv_w10m   =       'none'                 ,    'no'    ,     ''      ,         ''          ,   ''
   sn_rcv_taumod =       'none'              ,    'no'    ,     ''      ,         ''          ,   ''
   sn_rcv_tau    =       'none'             ,    'no'    , 'cartesian' , 'eastward-northward',  'U,V'
   sn_rcv_dqnsdt =       'coupled'              ,    'no'    ,     ''      ,         ''          ,   ''
   sn_rcv_qsr    =       'none'          ,    'no'    ,     ''      ,         ''          ,   ''
   sn_rcv_qns    =       'none'          ,    'no'    ,     ''      ,         ''          ,   ''
   sn_rcv_emp    =       'none'         ,    'no'    ,     ''      ,         ''          ,   ''
   sn_rcv_rnf    =       'none'              ,    'no'    ,     ''      ,         ''          ,   ''
   sn_rcv_cal    =       'none'              ,    'no'    ,     ''      ,         ''          ,   ''
   sn_rcv_co2    =       'none'              ,    'no'    ,     ''      ,         ''          ,   ''
!
   nn_cplmodel   =     1     !  Maximum number of models to/from which NEMO is potentialy sending/receiving data
   ln_usecplmask = .false.   !  use a coupling mask file to merge data received from several models
                             !   -> file cplmask.nc with the float variable called cplmask (jpi,jpj,nn_cplmodel)
/
!-----------------------------------------------------------------------
&namsbc_sas    !   analytical surface boundary condition
!-----------------------------------------------------------------------
/
!-----------------------------------------------------------------------
&namtra_qsr    !   penetrative solar radiation
!-----------------------------------------------------------------------
   nn_chldta   =      0    !  RGB : Chl data (=1) or cst value (=0)
/
!-----------------------------------------------------------------------
&namsbc_rnf    !   runoffs namelist surface boundary condition
!-----------------------------------------------------------------------
!              !  file name                                              ! frequency (hours) ! variable   ! time interp. !  clim  ! 'yearly'/ ! weights  ! rotation ! land/sea mask !
!              !                                                         !  (if <0  months)  !   name     !   (logical)  !  (T/F) ! 'monthly' ! filename ! pairing  ! filename      !
   sn_rnf      = 'clipped_runoff-icb_DaiTrenberth_Depoorter_eORCA1_JD.nc',        -1         , 'sorunoff' , .true.       , .true. , 'yearly'  , 'weights_bilin_ORCA1-2DtoBETA025.nc'       ,''       , ''
   sn_cnf      = 'clipped_runoff-icb_DaiTrenberth_Depoorter_eORCA1_JD.nc',        -1         , 'socoefr'  , .false.      , .true. , 'yearly'  , 'weights_bilin_ORCA1-2DtoBETA025.nc'       ,''       , ''

   ln_rnf_mouth = .false.   !  specific treatment at rivers mouths
   ln_rnf_depth_ini = .true.   ! compute depth at initialisation from runoff file !COM là c'était true
   rn_rnf_max   = 0.05      !  max value of the runoff climatologie over global domain ( ln_rnf_depth_ini = .true )
/
!-----------------------------------------------------------------------
&namsbc_isf    !  Top boundary layer (ISF)
!-----------------------------------------------------------------------
!                ! file name                                                ! frequency (hours) ! variable     ! time interpol. !  clim   ! 'yearly'/ ! weights  ! rotation !
!                !                                                          !  (if <0  months)  !   name       !    (logical)   !  (T/F)  ! 'monthly' ! filename ! pairing  !
   sn_rnfisf     = 'clipped_runoff-icb_DaiTrenberth_Depoorter_eORCA1_JD.nc' ,       -12         ,'sornfisf'    ,    .false.     , .true.  , 'yearly'  ,  'weights_bilin_ORCA1-2DtoBETA025.nc'      ,   ''  , ''
   sn_depmax_isf = 'clipped_runoff-icb_DaiTrenberth_Depoorter_eORCA1_JD.nc' ,       -12         ,'sodepmax_isf',    .false.     , .true.  , 'yearly'  ,  'weights_bilin_ORCA1-2DtoBETA025.nc'      ,   ''  , ''
   sn_depmin_isf = 'clipped_runoff-icb_DaiTrenberth_Depoorter_eORCA1_JD.nc' ,       -12         ,'sodepmin_isf',    .false.     , .true.  , 'yearly'  ,  'weights_bilin_ORCA1-2DtoBETA025.nc'      ,   ''  , ''
/
!-----------------------------------------------------------------------
&namsbc_apr    !   Atmospheric pressure used as ocean forcing or in bulk
!-----------------------------------------------------------------------
/
!-----------------------------------------------------------------------
&namsbc_ssr    !   surface boundary condition : sea surface restoring
!-----------------------------------------------------------------------
!              !  file name                                          ! frequency (hours) ! variable  ! time interp. !  clim  ! 'yearly'/ ! weights                                         ! rotation ! land/sea mask !
!              !                                                     !  (if <0  months)  !   name    !   (logical)  !  (T/F) ! 'monthly' ! filename                                        ! pairing  ! filename      !
   sn_sss      = 'sss_absolute_salinity_WOA13_decav_Reg1L75_clim.nc' ,        -1         , 'sosaline',    .true.    , .true. , 'yearly'  ,'weights_bilin_WOA13toBETA025.nc', ''       , ''
/
!-----------------------------------------------------------------------
&namsbc_alb    !   albedo parameters
!-----------------------------------------------------------------------
/
!-----------------------------------------------------------------------
&namberg       !   iceberg parameters
!-----------------------------------------------------------------------
/
!-----------------------------------------------------------------------
&namlbc        !   lateral momentum boundary condition
!-----------------------------------------------------------------------
   rn_shlat    =    0.     !  shlat = 0  !  0 < shlat < 2  !  shlat = 2  !  2 < shlat
                           !  free slip  !   partial slip  !   no slip   ! strong slip
/
!-----------------------------------------------------------------------
&namcla        !   cross land advection
!-----------------------------------------------------------------------
/
!-----------------------------------------------------------------------
&namagrif      !  AGRIF zoom                                            ("key_agrif")
!-----------------------------------------------------------------------
/
!-----------------------------------------------------------------------
&nam_tide      !   tide parameters (#ifdef key_tide)
!-----------------------------------------------------------------------
   ln_tide_pot = .false.
   rdttideramp   =    0.    !
   clname(1)     = ''  !  name of constituent - all tidal components must be set in namelist_cfg !COM ici il y avait K1
/
!-----------------------------------------------------------------------
&nambdy        !  unstructured open boundaries                          ("key_bdy")
!-----------------------------------------------------------------------
    nb_bdy         = 1                    !  number of open boundary sets
    ln_coords_file = .true.               !  =T : read bdy coordinates from file
    cn_coords_file = 'My_bdy_coordinates.nc' !  bdy coordinates files
    ln_mask_file   = .false.              !  =T : read mask from file
    cn_mask_file   = 'bdy_msk.nc'                   !  name of mask file (if ln_mask_file=.TRUE.)
    cn_dyn2d       = 'flather'               !
    nn_dyn2d_dta   =  1                   !  = 0, bdy data are equal to the initial state
                                          !  = 1, bdy data are read in 'bdydata   .nc' files
                                          !  = 2, use tidal harmonic forcing data from files
                                          !  = 3, use external data AND tidal harmonic forcing
    cn_dyn3d      =  'frs'               !
    nn_dyn3d_dta  =  1                    !  = 0, bdy data are equal to the initial state
                                          !  = 1, bdy data are read in 'bdydata   .nc' files
    cn_tra        =  'frs'               !
    nn_tra_dta    =  1                    !  = 0, bdy data are equal to the initial state
                                          !  = 1, bdy data are read in 'bdydata   .nc' files
    cn_ice_lim      =  'frs'             ! 'frs' or 'flather'
    nn_ice_lim_dta  =  1                  !  = 0, bdy data are equal to the initial state
                                          !  = 1, bdy data are read in 'bdydata   .nc' files
    rn_ice_tem      = 270.                !  lim3 only: arbitrary temperature of incoming sea ice
    rn_ice_sal      = 10.                 !  lim3 only:      --   salinity           --
    rn_ice_age      = 30.                 !  lim3 only:      --   age                --

    ln_tra_dmp    =.false.                !  open boudaries conditions for tracers
    ln_dyn3d_dmp  =.false.                !  open boundary condition for baroclinic velocities
    rn_time_dmp   =  1.                   ! Damping time scale in days
    rn_time_dmp_out =  1.                 ! Outflow damping time scale
    nn_rimwidth   = 1                    !  width of the relaxation zone
    ln_vol        = .true.               !  total volume correction (see nn_volctl parameter)
    nn_volctl     = 0                     !  = 0, the total water flux across open boundaries is zero
/

!-----------------------------------------------------------------------
&nambdy_index    !  open boundaries - definition           ("key_bdy")
!-----------------------------------------------------------------------
/

!-----------------------------------------------------------------------
&nambdy_dta      !  open boundaries - external data           ("key_bdy")
!-----------------------------------------------------------------------
!              !  file name     				 ! frequency (hours) ! variable   ! time interp.   !  clim   ! 'yearly'/ ! weights  ! rotation ! land/sea mask !
!              !              				         !  (if <0  months)  !   name     !   (logical)    !  (T/F ) ! 'monthly' ! filename ! pairing  ! filename      !
   bn_ssh =     'My_bdy_zos' ,         24        , 'sossheig' ,     .true.     , .false. ,  'yearly'  ,    ''    ,   ''     , ''
   bn_u2d =     'My_bdy_u2' ,         24        , 'ubaro' ,     .true.     , .false. ,  'yearly'  ,    ''    ,   ''     , ''
   bn_v2d =     'My_bdy_v2' ,         24        , 'vbaro' ,     .true.     , .false. ,  'yearly'  ,    ''    ,   ''     , ''
   bn_u3d  =    'My_bdy_uo' ,         24        , 'uo' ,     .true.     , .false. ,  'yearly'  ,    ''    ,   ''     , ''
   bn_v3d  =    'My_bdy_vo' ,         24        , 'vo' ,     .true.     , .false. ,  'yearly'  ,    ''    ,   ''     , ''
   bn_tem  =    'My_bdy_thetao' ,         24        , 'votemper' ,     .true.     , .false. ,  'yearly'  ,    ''    ,   ''     , ''
   bn_sal  =    'My_bdy_so' ,         24        , 'vosaline' ,     .true.     , .false. ,  'yearly'  ,    ''    ,   ''     , ''
! for lim3
   bn_a_i  =    'My_bdy_siconc' ,         24        , 'ileadfra' ,     .true.     , .false. ,  'yearly'  ,    ''    ,   ''     , ''
   bn_ht_i =    'My_bdy_sithic' ,         24        , 'iicethic' ,     .true.     , .false. ,  'yearly'  ,    ''    ,   ''     , ''
   bn_ht_s =    'My_bdy_snthic' ,         24        , 'isnowthi' ,     .true.     , .false. ,  'yearly'  ,    ''    ,   ''     , ''
   cn_dir  =    '/scratch/ucl/elic/phuot/nemo/run/${exp_name}/'
   ln_full_vel = .false.
/
!-----------------------------------------------------------------------
&nambdy_tide     ! tidal forcing at open boundaries
!-----------------------------------------------------------------------
/
!-----------------------------------------------------------------------
&nambfr        !   bottom friction
!-----------------------------------------------------------------------
   nn_bfr      =    2      !  type of bottom friction :   = 0 : free slip,  = 1 : linear friction
                           !                              = 2 : nonlinear friction
   rn_bfeb2    =    0 !  bottom turbulent kinetic energy background  (m2/s2)
/
!-----------------------------------------------------------------------
&nambbc        !   bottom temperature boundary condition
!-----------------------------------------------------------------------
   nn_geoflx   =    1      !  geothermal heat flux: = 0 no flux
                           !     = 1 constant flux
                           !     = 2 variable flux (read in geothermal_heating.nc in mW/m2)
/
!-----------------------------------------------------------------------
&nambbl        !   bottom boundary layer scheme
!-----------------------------------------------------------------------
/
!-----------------------------------------------------------------------
&nameos        !   ocean physical parameters
!-----------------------------------------------------------------------
/
!-----------------------------------------------------------------------
&namtra_adv    !   advection scheme for tracer
!-----------------------------------------------------------------------
   ln_traadv_tvd    =  .false.    !  TVD scheme
   ln_traadv_ubs    =  .true.   !  UBS scheme
/
!-----------------------------------------------------------------------
&namtra_adv_mle !  mixed layer eddy parametrisation (Fox-Kemper param)
!-----------------------------------------------------------------------
/
!----------------------------------------------------------------------------------
&namtra_ldf    !   lateral diffusion scheme for tracers
!----------------------------------------------------------------------------------
   rn_aht_0         =  1000.     !  horizontal eddy diffusivity for tracers [m2/s]
   rn_aeiv_0        =  1000.     !  eddy induced velocity coefficient [m2/s]    (require "key_traldf_eiv")
    ln_traldf_iso    =  .true.   !  iso-neutral                 (needs "key_ldfslp")
    ln_traldf_bilap  =  .false.  !  bilaplacian operator
    ln_traldf_lap    =  .true.
!    rn_aht_0         =  -1    !  horizontal eddy diffusivity for tracers [m2/s]
/
!-----------------------------------------------------------------------
&namtra_dmp    !   tracer: T & S newtonian damping
!-----------------------------------------------------------------------
   ln_tradmp   =  .false.  !  add a damping termn (T) or not (F)
/
!-----------------------------------------------------------------------
&namdyn_adv    !   formulation of the momentum advection
!-----------------------------------------------------------------------
   ln_dynadv_vec = .true.  !  vector form (T) or flux form (F)

/
!-----------------------------------------------------------------------
&nam_vvl    !   vertical coordinate options
!-----------------------------------------------------------------------
/
!-----------------------------------------------------------------------
&namdyn_vor    !   option of physics/algorithm (not control by CPP keys)
!-----------------------------------------------------------------------
/
!-----------------------------------------------------------------------
&namdyn_hpg    !   Hydrostatic pressure gradient option
!-----------------------------------------------------------------------
   ln_hpg_zps  = .false.   !  z-coordinate - partial steps (interpolation)
   ln_hpg_sco  = .true.    !  s-coordinate (standard jacobian formulation) 
/
!-----------------------------------------------------------------------
&namdyn_ldf    !   lateral diffusion on momentum
!-----------------------------------------------------------------------
        rn_ahm_0_lap     = 5000.    !  horizontal laplacian eddy viscosity   [m2/s]
	ln_dynldf_bilap=.true.
	ln_dynldf_hor=.true.
	ln_dynldf_iso=.false.
	ln_dynldf_lap=.false.
	ln_dynldf_level=.false.
	rn_ahm_0_blp=-1.25e10
/
!-----------------------------------------------------------------------
&namzdf        !   vertical physics
!-----------------------------------------------------------------------
/
!-----------------------------------------------------------------------
&namzdf_ric    !   richardson number dependent vertical diffusion       ("key_zdfric" )
!-----------------------------------------------------------------------
/
!-----------------------------------------------------------------------
&namzdf_tke    !   turbulent eddy kinetic dependent vertical diffusion  ("key_zdftke")
!-----------------------------------------------------------------------
/
!------------------------------------------------------------------------
&namzdf_kpp    !   K-Profile Parameterization dependent vertical mixing  ("key_zdfkpp", and optionally:
!------------------------------------------------------------------------ "key_kppcustom" or "key_kpplktb")
/
!-----------------------------------------------------------------------
&namzdf_gls                !   GLS vertical diffusion                   ("key_zdfgls")
!-----------------------------------------------------------------------
/
!-----------------------------------------------------------------------
&namzdf_ddm    !   double diffusive mixing parameterization             ("key_zdfddm")
!-----------------------------------------------------------------------
/
!-----------------------------------------------------------------------
&namzdf_tmx    !   tidal mixing parameterization                        ("key_zdftmx")
!-----------------------------------------------------------------------
/
!-----------------------------------------------------------------------
&namzdf_tmx_new    !   new tidal mixing parameterization                ("key_zdftmx_new")
!-----------------------------------------------------------------------
/
!-----------------------------------------------------------------------
&namsol        !   elliptic solver / island / free surface
!-----------------------------------------------------------------------
/
!-----------------------------------------------------------------------
&nammpp        !   Massively Parallel Processing                        ("key_mpp_mpi")
!-----------------------------------------------------------------------
   cn_mpi_send =  'I'      !  mpi send/recieve type   ='S', 'B', or 'I' for standard send,
                           !  buffer blocking send or immediate non-blocking sends, resp.
   nn_buffer   =   0       !  size in bytes of exported buffer ('B' case), 0 no exportation
   ln_nnogather=  .false.  !  activate code to avoid mpi_allgather use at the northfold
   jpni        =   0       !  jpni   number of processors following i (set automatically if < 1)
   jpnj        =   0       !  jpnj   number of processors following j (set automatically if < 1)
   jpnij       =   0       !  jpnij  number of local domains (set automatically if < 1)
/
!-----------------------------------------------------------------------
&namctl        !   Control prints & Benchmark
!-----------------------------------------------------------------------
/
!-----------------------------------------------------------------------
&namc1d_uvd    !   data: U & V currents                                 ("key_c1d")
!-----------------------------------------------------------------------
/
!-----------------------------------------------------------------------
&namc1d_dyndmp !   U & V newtonian damping                              ("key_c1d")
!-----------------------------------------------------------------------
/
!-----------------------------------------------------------------------
&namsto       ! Stochastic parametrization of EOS
!-----------------------------------------------------------------------
/
!-----------------------------------------------------------------------
&namnc4        !   netcdf4 chunking and compression settings            ("key_netcdf4")
!-----------------------------------------------------------------------
/
!-----------------------------------------------------------------------
&namtrd        !   diagnostics on dynamics and/or tracer trends         ("key_trddyn" and/or "key_trdtra")
!-----------------------------------------------------------------------
/
!-----------------------------------------------------------------------
&namflo       !   float parameters                                      ("key_float")
!-----------------------------------------------------------------------
/
!-----------------------------------------------------------------------
&namptr       !   Poleward Transport Diagnostic
!-----------------------------------------------------------------------
/
!-----------------------------------------------------------------------
&namhsb       !  Heat and salt budgets
!-----------------------------------------------------------------------
/
!-----------------------------------------------------------------------
&nam_diaharm   !   Harmonic analysis of tidal constituents ('key_diaharm')
!-----------------------------------------------------------------------
/
!-----------------------------------------------------------------------
&namdct        ! transports through sections
!-----------------------------------------------------------------------
/
!-----------------------------------------------------------------------
&namobs       !  observation usage switch                               ('key_diaobs')
!-----------------------------------------------------------------------
/
!-----------------------------------------------------------------------
&nam_asminc   !   assimilation increments                               ('key_asminc')
!-----------------------------------------------------------------------
/
!-----------------------------------------------------------------------
&namsbc_wave   ! External fields from wave model
!-----------------------------------------------------------------------
/
!-----------------------------------------------------------------------
&namdyn_nept  !   Neptune effect (simplified: lateral and vertical diffusions removed)
!-----------------------------------------------------------------------
/
EOF
