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

!!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
!! NEMO/OPA  :  1 - run manager      (namrun, namcfg)
!! namelists    2 - Domain           (namzgr, namzgr_sco, namdom, namtsd)
!!              3 - Surface boundary (namsbc, namsbc_ana, namsbc_flx, namsbc_clio, namsbc_core, namsbc_sas
!!                                    namsbc_cpl, namtra_qsr, namsbc_rnf,
!!                                    namsbc_apr, namsbc_ssr, namsbc_alb)
!!              4 - lateral boundary (namlbc, namcla, namobc, namagrif, nambdy, nambdy_tide)
!!              5 - bottom  boundary (nambfr, nambbc, nambbl)
!!              6 - Tracer           (nameos, namtra_adv, namtra_ldf, namtra_dmp)
!!              7 - dynamics         (namdyn_adv, namdyn_vor, namdyn_hpg, namdyn_spg, namdyn_ldf)
!!              8 - Verical physics  (namzdf, namzdf_ric, namzdf_tke, namzdf_kpp, namzdf_ddm, namzdf_tmx)
!!              9 - diagnostics      (namnc4, namtrd, namspr, namflo, namptr, namhsb)
!!             10 - miscellaneous    (namsol, nammpp, namctl)
!!             11 - Obs & Assim      (namobs, nam_asminc)
!!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

!!======================================================================
!!                   ***  Run management namelists  ***
!!======================================================================
!!   namrun        parameters of the run
!!======================================================================
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
   ln_rstart     =  ${nemo_restart}     !  start from rest (F) or from a restart file (T)
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
&namcfg     !   default parameters of the configuration      
!-----------------------------------------------------------------------
   cp_cfg      =  "ISF_scratcg"            !  name of the configuration
   cp_cfz      =         ''            !  name of the zoom of configuration
   jp_cfg      =     03               !  resolution of the configuration
   jpidta      =     532               !  1st lateral dimension ( >= jpi )
   jpjdta      =     522               !  2nd    "         "    ( >= jpj )
   jpkdta      =      75               !  number of levels      ( >= jpk )
   jpiglo      =      532               !  1st dimension of global domain --> i =jpidta
   jpjglo      =     522               !  2nd    -                  -    --> j  =jpjdta
   jpizoom     =       1               !  left bottom (i,j) indices of the zoom
   jpjzoom     =       1               !  in data domain indices
   jperio      =       0               !  lateral cond. type (between 0 and 6)
                                       !  = 0 closed                 ;   = 1 cyclic East-West
                                       !  = 2 equatorial symmetric   ;   = 3 North fold T-point pivot 
                                       !  = 4 cyclic East-West AND North fold T-point pivot
                                       !  = 5 North fold F-point pivot
                                       !  = 6 cyclic East-West AND North fold F-point pivot
/
!!======================================================================
!!                      ***  Domain namelists  ***
!!======================================================================
!!   namzgr       vertical coordinate
!!   namzgr_sco   s-coordinate or hybrid z-s-coordinate
!!   namdom       space and time domain (bathymetry, mesh, timestep)
!!   namtsd       data: temperature & salinity
!!======================================================================
!
!-----------------------------------------------------------------------
&namzgr        !   vertical coordinate
!-----------------------------------------------------------------------
   ln_zco      = .false.   !  z-coordinate - full    steps   (T/F)      ("key_zco" may also be defined)
   ln_zps      = .true.    !  z-coordinate - partial steps   (T/F)
   ln_sco      = .false.   !  s- or hybrid z-s-coordinate    (T/F)
   ln_isfcav   = .true.    !  ice shelf cavity
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
&namsplit      !   time splitting parameters                            ("key_dynspg_ts")
!-----------------------------------------------------------------------

rn_bt_cmax = 0.1 !COM ?

/
!-----------------------------------------------------------------------
&namcrs        !   Grid coarsening for dynamics output and/or
               !   passive tracer coarsened online simulations
!-----------------------------------------------------------------------
/
!-----------------------------------------------------------------------
&namtsd    !   data : Temperature  & Salinity
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!          !  file name                            ! frequency (hours) ! variable  ! time interp. !  clim  ! 'yearly'/ ! weights  ! rotation ! land/sea mask !
!          !                                       !  (if <0  months)  !   name    !   (logical)  !  (T/F) ! 'monthly' ! filename ! pairing  ! filename      !
    sn_tem  = 'starttemp.nc',         -12        ,'thetao' ,    .true.    , .true. , 'yearly'  ,'',   ''     ,    ''
    sn_sal  = 'startsalt.nc'       ,         -12        ,'so' ,    .true.    , .true. , 'yearly'  ,'',   ''     ,    ''
!
   cn_dir        = './'     !  root directory for the location of the runoff files
   ln_tsd_init   = .true.   !  Initialisation of ocean T & S with T &S input data (T) or not (F)
   ln_tsd_tradmp = .false.   !  damping of ocean T & S toward T &S input data (T) or not (F)
/
!!======================================================================
!!            ***  Surface Boundary Condition namelists  ***
!!======================================================================
!!   namsbc          surface boundary condition
!!   namsbc_ana      analytical         formulation
!!   namsbc_flx      flux               formulation
!!   namsbc_clio     CLIO bulk formulae formulation
!!   namsbc_core     CORE bulk formulae formulation
!!   namsbc_mfs      MFS  bulk formulae formulation
!!   namsbc_cpl      CouPLed            formulation                     ("key_coupled")
!!   namsbc_sas      StAndalone Surface module
!!   namtra_qsr      penetrative solar radiation
!!   namsbc_rnf      river runoffs
!!   namsbc_isf      ice shelf melting/freezing
!!   namsbc_apr      Atmospheric Pressure
!!   namsbc_ssr      sea surface restoring term (for T and/or S)
!!   namsbc_alb      albedo parameters
!!======================================================================
!
!-----------------------------------------------------------------------
&namsbc        !   Surface Boundary Condition (surface module)
!-----------------------------------------------------------------------
   nn_fsbc     = 1         !  frequency of surface boundary condition computation
                           !     (also = the frequency of sea-ice model call)
   ln_ana      = .false.    !  analytical formulation                    (T => fill namsbc_ana )
   ln_blk_core = .false.   !  CORE bulk formulation                     (T => fill namsbc_core)
   nn_ice      = 2         !  =0 no ice boundary condition   ,
                           !  =1 use observed ice-cover      , 
                           !  =2 ice-model used                
   nn_ice_embd = 1         !  =0 levitating ice (no mass exchange, concentration/dilution effect)
                           !  =1 levitating ice with mass and salt exchange but no presure effect
                           !  =2 embedded sea-ice (full salt and mass exchanges and pressure)
   ln_rnf      = .false.   !  runoffs                                   (T => fill namsbc_rnf)
   nn_limflx = 0          !  LIM3 Multi-category heat flux formulation (use -1 if LIM3 is not used)
   nn_isf      = 1         !  ice shelf melting/freezing                (/=0 => fill namsbc_isf)
                           !  0 = no isf               / 1 = presence of ISF 
                           !  2 = bg03 parametrisation / 3 = rnf file for isf
                           !  4 = ISF are prescribed
                           !  options 1 and 4 need ln_isfcav = .true. (domzgr)
   ln_ssr      = .false.   !  Sea Surface Restoring on T and/or S       (T => fill namsbc_ssr)
   nn_fwb      = 0         !  FreshWater Budget: =0 unchecked
                           !     =1 global mean of e-p-r set to zero at each time step
                           !     =2 annual global mean of e-p-r set to zero
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
/
!-----------------------------------------------------------------------
&namsbc_core   !   namsbc_core  CORE bulk formulae
!-----------------------------------------------------------------------

   sn_wndi     = 'drowned_u10_DFS5.2'    ,        3         , 'u10'    ,   .true.     , .false. , 'yearly'  , 'weights_bicub_DFStoADE20.nc' , 'U1'     , ''
   sn_wndj     = 'drowned_v10_DFS5.2'    ,        3         , 'v10'    ,   .true.     , .false. , 'yearly'  , 'weights_bicub_DFStoADE20.nc' , 'V1'     , ''
   sn_qsr      = 'drowned_radsw_DFS5.2'  ,        24         , 'radsw'  ,   .true.     , .false. , 'yearly'  , 'weights_bilin_DFStoADE20.nc' , ''       , ''
   sn_qlw      = 'drowned_radlw_DFS5.2'  ,        24         , 'radlw'  ,   .true.     , .false. , 'yearly'  , 'weights_bilin_DFStoADE20.nc' , ''       , ''
   sn_tair     = 'drowned_t2_DFS5.2'     ,        3         , 't2'     ,   .true.     , .false. , 'yearly'  , 'weights_bilin_DFStoADE20.nc' , ''       , ''
   sn_humi     = 'drowned_q2_DFS5.2'     ,        3         , 'q2'     ,   .true.     , .false. , 'yearly'  , 'weights_bilin_DFStoADE20.nc' , ''       , ''
   sn_prec     = 'drowned_precip_DFS5.2' ,        24         , 'precip' ,   .true.     , .false. , 'yearly'  , 'weights_bilin_DFStoADE20.nc' , ''       , ''
   sn_snow     = 'drowned_snow_DFS5.2'   ,        24         , 'snow'   ,   .true.     , .false. , 'yearly'  , 'weights_bilin_DFStoADE20.nc' , ''       , ''

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
   sn_snd_temp   =   'weighted oce and ice'  ,    'no'    ,     ''      ,         ''           ,   ''
   sn_snd_alb    =   'weighted ice'          ,    'no'    ,     ''      ,         ''           ,   ''
   sn_snd_thick  =   'ice and snow' ,    'no'    ,     ''      ,         ''           ,   ''
   sn_snd_crt    =   'weighted oce and ice'         ,    'no'    , 'spherical' , 'eastward-northward' ,  'U,V'
   sn_snd_co2    =   'none'         ,    'no'    ,     ''      ,         ''           ,   ''
! receive
   sn_rcv_w10m   =   'none'         ,    'no'    ,     ''      ,         ''           ,   ''
   sn_rcv_taumod =   'none'         ,    'no'    ,     ''      ,         ''           ,   ''
   sn_rcv_tau    =   'oce and ice'  ,    'no'    , 'spherical' , '' ,   'U,V'
   sn_rcv_dqnsdt =   'coupled'      ,    'no'    ,     ''      ,         ''           ,   ''
   sn_rcv_qsr    =   'oce and ice' ,    'no'    ,     ''      ,         ''           ,   ''
   sn_rcv_qns    =   'oce and ice' ,    'no'    ,     ''      ,         ''           ,   ''
   sn_rcv_emp    =   'conservative' ,    'no'    ,     ''      ,         ''           ,   ''
   sn_rcv_rnf    =   'none'      ,    'no'    ,     ''      ,         ''           ,   ''
   sn_rcv_cal    =   'none'      ,    'no'    ,     ''      ,         ''           ,   ''
   sn_rcv_iceflx =   'none'      ,    'no'    ,     ''      ,         ''           ,   ''
   sn_rcv_co2    =   'none'         ,    'no'    ,     ''      ,         ''           ,   ''
!
   nn_cplmodel   =     1.     !  Maximum number of models to/from which NEMO is potentialy sending/receiving data
   ln_usecplmask = .false.   !  use a coupling mask file to merge data received from several models
/
!-----------------------------------------------------------------------
&namsbc_sas    !   analytical surface boundary condition
!-----------------------------------------------------------------------
/
!-----------------------------------------------------------------------
&namtra_qsr    !   penetrative solar radiation
!-----------------------------------------------------------------------
   ln_traqsr   = .false.    !  Light penetration (T) or not (F)
/
!-----------------------------------------------------------------------
&namsbc_rnf    !   runoffs namelist surface boundary condition
!-----------------------------------------------------------------------
!              !  file name                                              ! frequency (hours) ! variable   ! time interp. !  clim  ! 'yearly'/ ! weights  ! rotation ! land/sea mask !
!              !                                                         !  (if <0  months)  !   name     !   (logical)  !  (T/F) ! 'monthly' ! filename ! pairing  ! filename      !
   sn_rnf       = 'icb_flx.nc',        -1         , 'icb_flx' , .true.       , .true. , 'yearly'  , ''       ,''       , ''
!   sn_t_cnf      = 'icb_flx.nc',        -1         , 'temperature'  , .true.      , .true. , 'yearly'  , ''       ,''       , ''

   ln_rnf_mouth = .false.   !  specific treatment at rivers mouths
   ln_rnf_depth_ini = .false.   ! compute depth at initialisation from runoff file !COM là c'était true
   rn_rnf_max   = 0.05      !  max value of the runoff climatologie over global domain ( ln_rnf_depth_ini = .true )
!   ln_rnf_tem = .true.


/
!-----------------------------------------------------------------------
&namsbc_isf    !  Top boundary layer (ISF) 
!-----------------------------------------------------------------------
!              ! file name ! frequency (hours) ! variable ! time interpol. !  clim   ! 'yearly'/ ! weights  ! rotation !
!              !           !  (if <0  months)  !   name   !    (logical)   !  (T/F)  ! 'monthly' ! filename ! pairing  !
! nn_isf == 4
   sn_qisf      = 'rnfisf' ,         -12      ,'sohflisf',    .false.      , .true.  , 'yearly'  ,  ''      ,   ''
   sn_fwfisf    = 'rnfisf' ,         -12      ,'sowflisf',    .false.      , .true.  , 'yearly'  ,  ''      ,   ''
! nn_isf == 3
   sn_rnfisf    = 'runoffs' ,         -12      ,'sofwfisf',    .false.      , .true.  , 'yearly'  ,  ''      ,   ''
! nn_isf == 2 and 3
   sn_depmax_isf = 'runoffs' ,       -12        ,'sozisfmax' ,   .false.  , .true.  , 'yearly'  ,  ''      ,   ''
   sn_depmin_isf = 'runoffs' ,       -12        ,'sozisfmin' ,   .false.  , .true.  , 'yearly'  ,  ''      ,   ''
! nn_isf == 2
   sn_Leff_isf = 'rnfisf' ,       0          ,'Leff'         ,   .false.  , .true.  , 'yearly'  ,  ''      ,   ''
! for all case
   ln_divisf   = .true.  ! apply isf melting as a mass flux or in the salinity trend. (maybe I should remove this option as for runoff?)
! only for nn_isf = 1 or 2
   rn_gammat0  = 2.21e-2   !1e-4WORKING !5: 0.45e-2 !4: 1.51e-2 !3: 2.21e-2   ! gammat coefficient used in blk formula
   rn_gammas0  = 6.4e-4   !2.8e-6WORKING !5: 0.129e-4 !4: 4.31e-4 !3: 6.19e-4   ! gammas coefficient used in blk formula
! only for nn_isf = 1
   nn_isfblk   =  2       ! 1 ISOMIP ; 2 conservative (3 equation formulation, Jenkins et al. 1991 ??)
   rn_hisf_tbl =  0.      ! thickness of the top boundary layer           (Losh et al. 2008)
                          ! 0 => thickness of the tbl = thickness of the first wet cell
   ln_conserve = .true.   ! conservative case (take into account meltwater advection)
   nn_gammablk = 1        ! 0 = cst Gammat (= gammat/s)
                          ! 1 = velocity dependend Gamma (u* * gammat/s)  (Jenkins et al. 2010)
                          !     if you want to keep the cd as in global config, adjust rn_gammat0 to compensate
                          ! 2 = velocity and stability dependent Gamma    Holland et al. 1999
/
!-----------------------------------------------------------------------
&namsbc_apr    !   Atmospheric pressure used as ocean forcing or in bulk
!-----------------------------------------------------------------------
/
!-----------------------------------------------------------------------
&namsbc_ssr    !   surface boundary condition : sea surface restoring
!-----------------------------------------------------------------------
/
!-----------------------------------------------------------------------
&namsbc_alb    !   albedo parameters
!-----------------------------------------------------------------------
/
!-----------------------------------------------------------------------
&namberg       !   iceberg parameters
!-----------------------------------------------------------------------
/

!!======================================================================
!!               ***  Lateral boundary condition  ***
!!======================================================================
!!   namlbc        lateral momentum boundary condition
!!   namcla        cross land advection
!!   namobc        open boundaries parameters                           ("key_obc")
!!   namagrif      agrif nested grid ( read by child model only )       ("key_agrif")
!!   nambdy        Unstructured open boundaries                         ("key_bdy")
!!   namtide       Tidal forcing at open boundaries                     ("key_bdy_tides")
!!======================================================================
!
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
&namobc        !   open boundaries parameters                           ("key_obc")
!-----------------------------------------------------------------------
/
!-----------------------------------------------------------------------
&namagrif      !  AGRIF zoom                                            ("key_agrif")
!-----------------------------------------------------------------------
/
!-----------------------------------------------------------------------
&nam_tide      !   tide parameters (#ifdef key_tide)
!-----------------------------------------------------------------------
   ln_tide_pot   = .false.   !  use tidal potential forcing
   clname(1)     = 'K1'  !  name of constituent - all tidal components must be set in namelist_cfg
   clname(2)     = 'K2'  !  name of constituent - all tidal components must be set in namelist_cfg
   clname(3)     = 'M2'  !  name of constituent - all tidal components must be set in namelist_cfg
   clname(4)     = 'P1'  !  name of constituent - all tidal components must be set in namelist_cfg
   clname(5)     = 'O1'  !  name of constituent - all tidal components must be set in namelist_cfg
   clname(6)     = 'S2'  !  name of constituent - all tidal components must be set in namelist_cfg
/
!-----------------------------------------------------------------------
&nambdy        !  unstructured open boundaries                          ("key_bdy")
!-----------------------------------------------------------------------
    nb_bdy         = 1                   !  number of open boundary sets
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
    ln_vol        = .false.               !  total volume correction (see nn_volctl parameter)
    nn_volctl     = 0                     !  = 0, the total water flux across open boundaries is zero
/
!-----------------------------------------------------------------------
&nambdy_dta      !  open boundaries - external data           ("key_bdy")
!-----------------------------------------------------------------------
   bn_ssh =     'My_bdy_zos' ,         24        , 'sossheig' ,     .true.     , .false. ,  'yearly'  ,    ''    ,   ''     , ''
   bn_u2d =     'My_bdy_u2' ,         24        , 'ubaro' ,     .true.     , .false. ,  'yearly'  ,    ''    ,   ''     , ''
   bn_v2d =     'My_bdy_v2' ,         24        , 'vbaro' ,     .true.     , .false. ,  'yearly'  ,    ''    ,   ''     , ''
   bn_u3d  =    'My_bdy_uo' ,         24        , 'vozocrtx' ,     .true.     , .false. ,  'yearly'  ,    ''    ,   ''     , ''
   bn_v3d  =    'My_bdy_vo' ,         24        , 'vomecrty' ,     .true.     , .false. ,  'yearly'  ,    ''    ,   ''     , ''
   bn_tem  =    'My_bdy_thetao' ,         24        , 'votemper' ,     .true.     , .false. ,  'yearly'  ,    ''    ,   ''     , ''
   bn_sal  =    'My_bdy_so' ,         24        , 'vosaline' ,     .true.     , .false. ,  'yearly'  ,    ''    ,   ''     , ''
! for lim3
   bn_a_i  =    'My_bdy_siconc' ,         24        , 'ileadfra' ,     .true.     , .false. ,  'yearly'  ,    ''    ,   ''     , ''
   bn_ht_i =    'My_bdy_sithic' ,         24        , 'iicethic' ,     .true.     , .false. ,  'yearly'  ,    ''    ,   ''     , ''
   bn_ht_s =    'My_bdy_snthic' ,         24        , 'isnowthi' ,     .true.     , .false. ,  'yearly'  ,    ''    ,   ''     , ''
   cn_dir  =   '${run_dir}/' 
   ln_full_vel = .false.
/
!-----------------------------------------------------------------------
&nambdy_tide     ! tidal forcing at open boundaries
!-----------------------------------------------------------------------
   filtide      = '/SCRATCH/acad/limhr/phuot/CONF_files/ADEL20/ORCA20_bdytide_'         !  file name root of tidal forcing files

/
!!======================================================================
!!                 ***  Bottom boundary condition  ***
!!======================================================================
!!   nambfr        bottom friction
!!   nambbc        bottom temperature boundary condition
!!   nambbl        bottom boundary layer scheme                         ("key_trabbl")
!!======================================================================
!
!-----------------------------------------------------------------------
&nambfr        !   bottom/top friction
!-----------------------------------------------------------------------
   nn_bfr      =    2      !  type of bottom friction :   = 0 : free slip,  = 1 : linear friction
                           !                              = 2 : nonlinear friction
   rn_bfri1    =    4.e-4  !  bottom drag coefficient (linear case)
   rn_bfri2    =    1.e-3  !  bottom drag coefficient (non linear case). Minimum coeft if ln_loglayer=T
   rn_bfri2_max =   1.e-1  !  max. bottom drag coefficient (non linear case and ln_loglayer=T)
   rn_bfeb2    =    2.5e-3 !  bottom turbulent kinetic energy background  (m2/s2)
   rn_bfrz0    =    3.e-3  !  bottom roughness [m] if ln_loglayer=T 
   ln_bfr2d    = .false.   !  horizontal variation of the bottom friction coef (read a 2D mask file )
   rn_bfrien   =    50.    !  local multiplying factor of bfr (ln_bfr2d=T)
   rn_tfri1    =    4.e-4  !  top drag coefficient (linear case)
!   rn_tfri2    =    2.5e-3 !  top drag coefficient (non linear case). Minimum coeft if ln_loglayer=T
   rn_tfri2 = 1e-3
   rn_tfri2_max =   1.e-1  !  max. top drag coefficient (non linear case and ln_loglayer=T)
   rn_tfeb2    =    0.0    !  top turbulent kinetic energy background  (m2/s2)
   rn_tfrz0    =    3.e-3  !  top roughness [m] if ln_loglayer=T
   ln_tfr2d    = .false.   !  horizontal variation of the top friction coef (read a 2D mask file )
   rn_tfrien   =    50.    !  local multiplying factor of tfr (ln_tfr2d=T)

   ln_bfrimp   = .true.    !  implicit bottom friction (requires ln_zdfexp = .false. if true)
   ln_loglayer = .false.   !  logarithmic formulation (non linear case)
/
!-----------------------------------------------------------------------
&nambbc        !   bottom temperature boundary condition
!-----------------------------------------------------------------------
   ln_trabbc   = .false.    !  Apply a geothermal heating at the ocean bottom
/
!-----------------------------------------------------------------------
&nambbl        !   bottom boundary layer scheme
!-----------------------------------------------------------------------
   nn_bbl_ldf  =  0      !  diffusive bbl (=1)   or not (=0)
   nn_bbl_adv  =  0      !  advective bbl (=1/2) or not (=0)
/

!!======================================================================
!!                        Tracer (T & S ) namelists
!!======================================================================
!!   nameos        equation of state
!!   namtra_adv    advection scheme
!!   namtra_ldf    lateral diffusion scheme
!!   namtra_dmp    T & S newtonian damping
!!======================================================================
!
!-----------------------------------------------------------------------
&nameos        !   ocean physical parameters
!-----------------------------------------------------------------------
/
!-----------------------------------------------------------------------
&namtra_adv    !   advection scheme for tracer
!-----------------------------------------------------------------------
/
!-----------------------------------------------------------------------
&namtra_adv_mle !   mixed layer eddy parametrisation (Fox-Kemper param)
!-----------------------------------------------------------------------
   ln_mle    = .false.      ! (T) use the Mixed Layer Eddy (MLE) parameterisation
/
!----------------------------------------------------------------------------------
&namtra_ldf    !   lateral diffusion scheme for tracers
!----------------------------------------------------------------------------------
   rn_aht_0         =  30.     !  horizontal eddy diffusivity for tracers [m2/s]
   rn_aeiv_0        =  1000.     !  eddy induced velocity coefficient [m2/s]    (require "key_traldf_eiv")
    ln_traldf_iso    =  .true.   !  iso-neutral                 (needs "key_ldfslp")
    ln_traldf_bilap  =  .false.  !  bilaplacian operator
    ln_traldf_lap    =  .true.
/
!-----------------------------------------------------------------------
&namtra_dmp    !   tracer: T & S newtonian damping
!-----------------------------------------------------------------------
   ln_tradmp   =  .false.   !  add a damping termn (T) or not (F)
/

!!======================================================================
!!                      ***  Dynamics namelists  ***
!!======================================================================
!!   namdyn_adv    formulation of the momentum advection
!!   namdyn_vor    advection scheme
!!   namdyn_hpg    hydrostatic pressure gradient
!!   namdyn_spg    surface pressure gradient                            (CPP key only)
!!   namdyn_ldf    lateral diffusion scheme
!!======================================================================
!
!-----------------------------------------------------------------------
&namdyn_adv    !   formulation of the momentum advection
!-----------------------------------------------------------------------
/
!-----------------------------------------------------------------------
&nam_vvl    !   vertical coordinate options
!-----------------------------------------------------------------------
   ln_vvl_zstar  = .true.           !  zstar vertical coordinate                   
/
!-----------------------------------------------------------------------
&namdyn_vor    !   option of physics/algorithm (not control by CPP keys)
!-----------------------------------------------------------------------
/
!-----------------------------------------------------------------------
&namdyn_hpg    !   Hydrostatic pressure gradient option
!-----------------------------------------------------------------------
   ln_hpg_zps  = .false.   !  z-coordinate - partial steps (interpolation)
   ln_hpg_sco  = .false.   !  z-coordinate - partial steps (interpolation)
   ln_hpg_isf  = .true.    !  s-coordinate adapted for isf (standard jacobian formulation)
   ln_dynhpg_imp = .false. !  time stepping: semi-implicit time scheme  (T)
                                 !           centered      time scheme  (F)
/
!-----------------------------------------------------------------------
!namdyn_spg    !   surface pressure gradient   (CPP key only)
!-----------------------------------------------------------------------
!                          !  explicit free surface                     ("key_dynspg_exp")
!                          !  filtered free surface                     ("key_dynspg_flt")
!                          !  split-explicit free surface               ("key_dynspg_ts")

!-----------------------------------------------------------------------
&namdyn_ldf    !   lateral diffusion on momentum
!-----------------------------------------------------------------------
   !                       !  Type of the operator :
   ln_dynldf_lap    =  .false.   !  laplacian operator
   ln_dynldf_bilap  =  .true.  !  bilaplacian operator
   !                       !  Direction of action  :
   ln_dynldf_level  =  .false.  !  iso-level
   ln_dynldf_hor    =  .true.   !  horizontal (geopotential)            (require "key_ldfslp" in s-coord.)
   ln_dynldf_iso    =  .false.  !  iso-neutral                          (require "key_ldfslp")
   !                       !  Coefficient
   rn_ahm_0_lap     = 5000.    !  horizontal laplacian eddy viscosity   [m2/s]
   rn_ahmb_0        =     0.    !  background eddy viscosity for ldf_iso [m2/s]
   rn_ahm_0_blp     =   -2.3e8    !  horizontal bilaplacian eddy viscosity [m4/s] -1.25e10
   rn_cmsmag_1      =     3.    !  constant in laplacian Smagorinsky viscosity
   rn_cmsmag_2      =     3.     !  constant in bilaplacian Smagorinsky viscosity
   rn_cmsh          =     1.    !  1 or 0 , if 0 -use only shear for Smagorinsky viscosity
   rn_ahm_m_blp     =   -2.3e8 !  upper limit for bilap  abs(ahm) < min( dx^4/128rdt, rn_ahm_m_blp)
   rn_ahm_m_lap     = 5000.    !  upper limit for lap  ahm < min(dx^2/16rdt, rn_ahm_m_lap)
/

!!======================================================================
!!             Tracers & Dynamics vertical physics namelists
!!======================================================================
!!    namzdf        vertical physics
!!    namzdf_ric    richardson number dependent vertical mixing         ("key_zdfric")
!!    namzdf_tke    TKE dependent vertical mixing                       ("key_zdftke")
!!    namzdf_kpp    KPP dependent vertical mixing                       ("key_zdfkpp")
!!    namzdf_ddm    double diffusive mixing parameterization            ("key_zdfddm")
!!    namzdf_tmx    tidal mixing parameterization                       ("key_zdftmx")
!!======================================================================
!
!-----------------------------------------------------------------------
&namzdf        !   vertical physics
!-----------------------------------------------------------------------
   rn_avm0     =   1.0e-3  !  vertical eddy viscosity   [m2/s]          (background Kz if not "key_zdfcst")
   rn_avt0     =   5.0e-5  !  vertical eddy diffusivity [m2/s]          (background Kz if not "key_zdfcst")
   nn_avb      =    0      !  profile for background avt & avm (=1) or not (=0)
   nn_havtb    =    0      !  horizontal shape for avtb (=1) or not (=0)
   ln_zdfevd   = .true.    !  enhanced vertical diffusion (evd) (T) or not (F)
   nn_evdm     =    1      !  evd apply on tracer (=0) or on tracer and momentum (=1)
   rn_avevd    =   0.1     !  evd mixing coefficient [m2/s]
   ln_zdfnpc   = .false.   !  Non-Penetrative Convective algorithm (T) or not (F)
   nn_npc      =    1            !  frequency of application of npc
   nn_npcp     =  365            !  npc control print frequency
   ln_zdfexp   = .false.   !  time-stepping: split-explicit (T) or implicit (F) time stepping
   nn_zdfexp   =    3            !  number of sub-timestep for ln_zdfexp=T
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

!!======================================================================
!!                  ***  Miscellaneous namelists  ***
!!======================================================================
!!   nammpp            Massively Parallel Processing                    ("key_mpp_mpi)
!!   namctl            Control prints & Benchmark
!!   namsol            elliptic solver / island / free surface
!!======================================================================
!
!-----------------------------------------------------------------------
&namsol        !   elliptic solver / island / free surface
!-----------------------------------------------------------------------
/
!-----------------------------------------------------------------------
&nammpp        !   Massively Parallel Processing                        ("key_mpp_mpi)
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
   ln_ctl      = .false.   !  trends control print (expensive!)
   nn_print    =    0      !  level of print (0 no extra print)
   nn_ictls    =    0      !  start i indice of control sum (use to compare mono versus
   nn_ictle    =    0      !  end   i indice of control sum        multi processor runs
   nn_jctls    =    0      !  start j indice of control               over a subdomain)
   nn_jctle    =    0      !  end   j indice of control
   nn_isplt    =    1      !  number of processors in i-direction
   nn_jsplt    =    1      !  number of processors in j-direction
   nn_bench    =    0      !  Bench mode (1/0): CAUTION use zero except for bench
                           !     (no physical validity of the results)
   nn_timing   =    0      !  timing by routine activated (=1) creates timing.output file, or not (=0)
/
!-----------------------------------------------------------------------
&namc1d        !   1D configuration options                             ("key_c1d")
!-----------------------------------------------------------------------
/
!-----------------------------------------------------------------------
&namc1d_uvd    !   data: U & V currents                                 ("key_c1d")
!-----------------------------------------------------------------------
/
!-----------------------------------------------------------------------
&namc1d_dyndmp !   U & V newtonian damping                              ("key_c1d")
!-----------------------------------------------------------------------
   ln_dyndmp   =  .false.  !  add a damping term (T) or not (F)
/
!!======================================================================
!!                  ***  Diagnostics namelists  ***
!!======================================================================
!!   namnc4       netcdf4 chunking and compression settings             ("key_netcdf4")
!!   namtrd       dynamics and/or tracer trends                         ("key_trddyn","key_trdtra","key_trdmld")
!!   namflo       float parameters                                      ("key_float")
!!   namptr       Poleward Transport Diagnostics
!!   namhsb       Heat and salt budgets
!!======================================================================
!
!-----------------------------------------------------------------------
&namnc4        !   netcdf4 chunking and compression settings            ("key_netcdf4")
!-----------------------------------------------------------------------
   nn_nchunks_i=   4       !  number of chunks in i-dimension
   nn_nchunks_j=   4       !  number of chunks in j-dimension
   nn_nchunks_k=   31      !  number of chunks in k-dimension
                           !  setting nn_nchunks_k = jpk will give a chunk size of 1 in the vertical which
                           !  is optimal for postprocessing which works exclusively with horizontal slabs
   ln_nc4zip   = .true.    !  (T) use netcdf4 chunking and compression
                           !  (F) ignore chunking information and produce netcdf3-compatible files
/
!-----------------------------------------------------------------------
&namtrd        !   diagnostics on dynamics and/or tracer trends         ("key_trddyn" and/or "key_trdtra")
!              !       or mixed-layer trends or barotropic vorticity    ("key_trdmld" or     "key_trdvor")
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
   ln_diahsb  = .false.    !  check the heat and salt budgets (T) or not (F)
/ 
!-----------------------------------------------------------------------
&nam_diaharm   !   Harmonic analysis of tidal constituents ('key_diaharm')
!-----------------------------------------------------------------------
/
!-----------------------------------------------------------------------
&namdct        ! transports through sections
!-----------------------------------------------------------------------
/
!!======================================================================
!!            ***  Observation & Assimilation namelists ***
!!======================================================================
!!   namobs       observation and model comparison                      ('key_diaobs')
!!   nam_asminc   assimilation increments                               ('key_asminc')
!!======================================================================
!
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
