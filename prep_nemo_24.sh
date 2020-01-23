#!/bin/bash

#
# Inputs files
#

oasis_dir=/scratch/ucl/elic/phuot/oasis24
code_dir=/scratch/ucl/elic/phuot/codes
ini_data_dir=/scratch/ucl/elic/phuot/data24
ic_files=(    
    "1_ext-PSY4V3R1_mesh_hgr.nc => coordinates.nc" 

    "start24_cpl.nc => start_oce_adel24.nc"

    "ADEL24_bathynisf_all3.nc => isf_draft_meter.nc"
    "ADEL24_bathynisf_all3.nc => bathy_meter.nc"

    "ADEL24_fwf_icb.nc => icb_flx.nc"

    "start24_2011.nc => starttemp.nc"
    "start24_2011.nc => startsalt.nc"

    "bdyT_2D_ORCA20_y2007.nc => My_bdy_zos_y2006.nc"
    "bdyT_2D_ORCA20_y2007.nc => My_bdy_zos_y2007.nc"
    "bdyT_2D_ORCA20_y2008.nc => My_bdy_zos_y2008.nc"
    "bdyT_2D_ORCA20_y2009.nc => My_bdy_zos_y2009.nc"
    "bdyT_2D_ORCA20_y2010.nc => My_bdy_zos_y2010.nc"
    "bdyT_2D_ORCA20_y2011.nc => My_bdy_zos_y2011.nc"
    "bdyT_2D_ORCA20_y2012.nc => My_bdy_zos_y2012.nc"
    "bdyT_2D_ORCA20_y2013.nc => My_bdy_zos_y2013.nc"
    "bdyT_2D_ORCA20_y2013.nc => My_bdy_zos_y2014.nc"

    "bdyT_ice_ORCA20_y2007.nc => My_bdy_siconc_y2006.nc"
    "bdyT_ice_ORCA20_y2007.nc => My_bdy_siconc_y2007.nc"
    "bdyT_ice_ORCA20_y2008.nc => My_bdy_siconc_y2008.nc"
    "bdyT_ice_ORCA20_y2009.nc => My_bdy_siconc_y2009.nc"
    "bdyT_ice_ORCA20_y2010.nc => My_bdy_siconc_y2010.nc"
    "bdyT_ice_ORCA20_y2011.nc => My_bdy_siconc_y2011.nc"
    "bdyT_ice_ORCA20_y2012.nc => My_bdy_siconc_y2012.nc"
    "bdyT_ice_ORCA20_y2013.nc => My_bdy_siconc_y2013.nc"
    "bdyT_ice_ORCA20_y2013.nc => My_bdy_siconc_y2014.nc"

    "bdyT_ice_ORCA20_y2007.nc => My_bdy_sithic_y2006.nc"
    "bdyT_ice_ORCA20_y2007.nc => My_bdy_sithic_y2007.nc"
    "bdyT_ice_ORCA20_y2008.nc => My_bdy_sithic_y2008.nc"
    "bdyT_ice_ORCA20_y2009.nc => My_bdy_sithic_y2009.nc"
    "bdyT_ice_ORCA20_y2010.nc => My_bdy_sithic_y2010.nc"
    "bdyT_ice_ORCA20_y2011.nc => My_bdy_sithic_y2011.nc"
    "bdyT_ice_ORCA20_y2012.nc => My_bdy_sithic_y2012.nc"
    "bdyT_ice_ORCA20_y2013.nc => My_bdy_sithic_y2013.nc"
    "bdyT_ice_ORCA20_y2013.nc => My_bdy_sithic_y2014.nc"

    "bdyT_ice_ORCA20_y2007.nc => My_bdy_snthic_y2006.nc"
    "bdyT_ice_ORCA20_y2007.nc => My_bdy_snthic_y2007.nc"
    "bdyT_ice_ORCA20_y2008.nc => My_bdy_snthic_y2008.nc"
    "bdyT_ice_ORCA20_y2009.nc => My_bdy_snthic_y2009.nc"
    "bdyT_ice_ORCA20_y2010.nc => My_bdy_snthic_y2010.nc"
    "bdyT_ice_ORCA20_y2011.nc => My_bdy_snthic_y2011.nc"
    "bdyT_ice_ORCA20_y2012.nc => My_bdy_snthic_y2012.nc"
    "bdyT_ice_ORCA20_y2013.nc => My_bdy_snthic_y2013.nc"
    "bdyT_ice_ORCA20_y2013.nc => My_bdy_snthic_y2014.nc"

    "bdyT_tem_ORCA20_eos10_y2007_L75.nc => My_bdy_thetao_y2006.nc"
    "bdyT_tem_ORCA20_eos10_y2007_L75.nc => My_bdy_thetao_y2007.nc"
    "bdyT_tem_ORCA20_eos10_y2008_L75.nc => My_bdy_thetao_y2008.nc"
    "bdyT_tem_ORCA20_eos10_y2009_L75.nc => My_bdy_thetao_y2009.nc"
    "bdyT_tem_ORCA20_eos10_y2010_L75.nc => My_bdy_thetao_y2010.nc"
    "bdyT_tem_ORCA20_eos10_y2011_L75.nc => My_bdy_thetao_y2011.nc"
    "bdyT_tem_ORCA20_eos10_y2012_L75.nc => My_bdy_thetao_y2012.nc"
    "bdyT_tem_ORCA20_eos10_y2013_L75.nc => My_bdy_thetao_y2013.nc"
    "bdyT_tem_ORCA20_eos10_y2013_L75.nc => My_bdy_thetao_y2014.nc"

    "bdyT_sal_ORCA20_eos10_y2007_L75.nc => My_bdy_so_y2006.nc"
    "bdyT_sal_ORCA20_eos10_y2007_L75.nc => My_bdy_so_y2007.nc"
    "bdyT_sal_ORCA20_eos10_y2008_L75.nc => My_bdy_so_y2008.nc"
    "bdyT_sal_ORCA20_eos10_y2009_L75.nc => My_bdy_so_y2009.nc"
    "bdyT_sal_ORCA20_eos10_y2010_L75.nc => My_bdy_so_y2010.nc"
    "bdyT_sal_ORCA20_eos10_y2011_L75.nc => My_bdy_so_y2011.nc"
    "bdyT_sal_ORCA20_eos10_y2012_L75.nc => My_bdy_so_y2012.nc"
    "bdyT_sal_ORCA20_eos10_y2013_L75.nc => My_bdy_so_y2013.nc"
    "bdyT_sal_ORCA20_eos10_y2013_L75.nc => My_bdy_so_y2014.nc"

    "bdyU_u3d_ORCA20_y2007_L75.nc => My_bdy_uo_y2006.nc"
    "bdyU_u3d_ORCA20_y2007_L75.nc => My_bdy_uo_y2007.nc"
    "bdyU_u3d_ORCA20_y2008_L75.nc => My_bdy_uo_y2008.nc"
    "bdyU_u3d_ORCA20_y2009_L75.nc => My_bdy_uo_y2009.nc"
    "bdyU_u3d_ORCA20_y2010_L75.nc => My_bdy_uo_y2010.nc"
    "bdyU_u3d_ORCA20_y2011_L75.nc => My_bdy_uo_y2011.nc"
    "bdyU_u3d_ORCA20_y2012_L75.nc => My_bdy_uo_y2012.nc"
    "bdyU_u3d_ORCA20_y2013_L75.nc => My_bdy_uo_y2013.nc"
    "bdyU_u3d_ORCA20_y2013_L75.nc => My_bdy_uo_y2014.nc"

    "bdyV_v3d_ORCA20_y2007_L75.nc => My_bdy_vo_y2006.nc"
    "bdyV_v3d_ORCA20_y2007_L75.nc => My_bdy_vo_y2007.nc"
    "bdyV_v3d_ORCA20_y2008_L75.nc => My_bdy_vo_y2008.nc"
    "bdyV_v3d_ORCA20_y2009_L75.nc => My_bdy_vo_y2009.nc"
    "bdyV_v3d_ORCA20_y2010_L75.nc => My_bdy_vo_y2010.nc"
    "bdyV_v3d_ORCA20_y2011_L75.nc => My_bdy_vo_y2011.nc"
    "bdyV_v3d_ORCA20_y2012_L75.nc => My_bdy_vo_y2012.nc"
    "bdyV_v3d_ORCA20_y2013_L75.nc => My_bdy_vo_y2013.nc"
    "bdyV_v3d_ORCA20_y2013_L75.nc => My_bdy_vo_y2014.nc"

    "bdyV_v2d_ORCA20_y2010.nc => My_oldbdy_v2_y2010.nc"

    "bdyU_u2d_ORCA20_y2010.nc => My_oldbdy_u2_y2010.nc"

    "bdyV_v2d_ORCA20_y2007.nc => My_bdy_v2_y2006.nc"
    "bdyV_v2d_ORCA20_y2007.nc => My_bdy_v2_y2007.nc"
    "bdyV_v2d_ORCA20_y2008.nc => My_bdy_v2_y2008.nc"
    "bdyV_v2d_ORCA20_y2009.nc => My_bdy_v2_y2009.nc"
    "bdyV_v2d_ORCA20_y2010.nc => My_bdy_v2_y2010.nc"
    "bdyV_v2d_ORCA20_y2011.nc => My_bdy_v2_y2011.nc"
    "bdyV_v2d_ORCA20_y2012.nc => My_bdy_v2_y2012.nc"
    "bdyV_v2d_ORCA20_y2013.nc => My_bdy_v2_y2013.nc"

    "bdyU_u2d_ORCA20_y2007.nc => My_bdy_u2_y2006.nc"
    "bdyU_u2d_ORCA20_y2007.nc => My_bdy_u2_y2007.nc"
    "bdyU_u2d_ORCA20_y2008.nc => My_bdy_u2_y2008.nc"
    "bdyU_u2d_ORCA20_y2009.nc => My_bdy_u2_y2009.nc"
    "bdyU_u2d_ORCA20_y2010.nc => My_bdy_u2_y2010.nc"
    "bdyU_u2d_ORCA20_y2011.nc => My_bdy_u2_y2011.nc"
    "bdyU_u2d_ORCA20_y2012.nc => My_bdy_u2_y2012.nc"
    "bdyU_u2d_ORCA20_y2013.nc => My_bdy_u2_y2013.nc"


    "ORCA20_coordinates_bdyr1_ORCA20.nc => My_bdy_coordinates.nc"
    "ORCA20_mask_bdy.nc => bdy_msk.nc" 

    "ORCA20_bdytide_K1_grid_U.nc"
    "ORCA20_bdytide_K1_grid_V.nc"
    "ORCA20_bdytide_K1_grid_T.nc"
    "ORCA20_bdytide_K2_grid_U.nc"
    "ORCA20_bdytide_K2_grid_V.nc"
    "ORCA20_bdytide_K2_grid_T.nc"
    "ORCA20_bdytide_M2_grid_U.nc"
    "ORCA20_bdytide_M2_grid_V.nc"
    "ORCA20_bdytide_M2_grid_T.nc"
    "ORCA20_bdytide_O1_grid_U.nc"
    "ORCA20_bdytide_O1_grid_V.nc"
    "ORCA20_bdytide_O1_grid_T.nc"
    "ORCA20_bdytide_P1_grid_U.nc"
    "ORCA20_bdytide_P1_grid_V.nc"
    "ORCA20_bdytide_P1_grid_T.nc"
    "ORCA20_bdytide_S2_grid_U.nc"
    "ORCA20_bdytide_S2_grid_V.nc"
    "ORCA20_bdytide_S2_grid_T.nc"

)

shared_files=(
    "namelist_ice_cfg"
    "domain_def_nemo.xml"
    "field_def_nemo-opa.xml"
    "field_def_nemo-lim.xml"
    "file_def_nemo-opa.xml"
    "file_def_nemo-lim.xml"
    "context.xml"
    "iodef.xml"
    "file_def.xml"
    "${nem_exe_file}"
    "${xio_exe_file}"
    "namelist_ref"
    "namelist_ice_ref"
)

oa_files=(
   "start_atmos_cpl_new.nc"
   "start_ocean_cpl_24.nc"
   "areas.nc"
   "masks.nc"
   "grids.nc"
   "masks.nc"
   "namelist_ice_lim3_ref => namelist_ice_ref"
   "namelist_ref"
)

#
# link / gather files
#

cd "${run_dir}"

# Nemo and xios .exe


# Boundaries, grid, etc ...

for file in "${ic_files[@]}"; do
    [[ ! -e ${file#*> } ]] && ln -sf $(sed 's/ *=> */ /' <<< "${ini_data_dir}/$file")
done
for file in "${shared_files[@]}"; do
    [[ ! -e ${file#*> } ]] && cp ${code_dir}/$file .
done
for file in "${oa_files[@]}"; do
    [[ ! -e ${file#*> } ]] && cp ${oasis_dir}/$file .
done

[ -f nemo.exe ] && echo nemo.exe exists || echo nemo.exe does not exist

echo Preparation completed succesfully


cp ${homedir}/namelist_ice_cfg ${run_dir}

