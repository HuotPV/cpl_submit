#!/bin/bash

#
# Inputs files
#

oasis_dir=/scratch/ucl/elic/phuot/oasis
code_dir=/scratch/ucl/elic/phuot/codes
ini_data_dir=/scratch/ucl/elic/phuot/data
ic_files=(    
    "start025_fixed_y2007.nc => start025.nc"

    "ISF_ade025_01.nc => bathy_meter.nc"
    "ISF_ade025_01.nc => isf_draft_meter.nc"
    "1_ORCA1_coordinates_cl.nc => coordinates.nc"

    "ADE025_merino_icbflux.nc => icb_flx.nc"

    "bdyT_2D_PSY025_y2011.nc => My_bdy_zos_y2011.nc"
    "bdyT_2D_PSY025_y2012.nc => My_bdy_zos_y2012.nc"
    "bdyT_2D_PSY025_y2013.nc => My_bdy_zos_y2013.nc"

    "bdyT_sic_PSY025_y2011.nc => My_bdy_siconc_y2011.nc"
    "bdyT_sic_PSY025_y2012.nc => My_bdy_siconc_y2012.nc"
    "bdyT_sic_PSY025_y2013.nc => My_bdy_siconc_y2013.nc"

    "bdyT_ssit_PSY025_y2011.nc => My_bdy_sithic_y2011.nc"
    "bdyT_ssit_PSY025_y2012.nc => My_bdy_sithic_y2012.nc"
    "bdyT_ssit_PSY025_y2013.nc => My_bdy_sithic_y2013.nc"

    "bdyT_sno_PSY025_y2011.nc => My_bdy_snthic_y2011.nc"
    "bdyT_sno_PSY025_y2012.nc => My_bdy_snthic_y2012.nc"
    "bdyT_sno_PSY025_y2013.nc => My_bdy_snthic_y2013.nc"

    "bdyT_tem_PSY025_eos10_y2011.nc => My_bdy_thetao_y2011.nc"
    "bdyT_tem_PSY025_eos10_y2012.nc => My_bdy_thetao_y2012.nc"
    "bdyT_tem_PSY025_eos10_y2013.nc => My_bdy_thetao_y2013.nc"

    "bdyT_sal_PSY025_eos10_y2011.nc => My_bdy_so_y2011.nc"
    "bdyT_sal_PSY025_eos10_y2012.nc => My_bdy_so_y2012.nc"
    "bdyT_sal_PSY025_eos10_y2013.nc => My_bdy_so_y2013.nc"

    "bdyU_u3d_FIX_PSY025_y2011.nc => My_bdy_uo_y2011.nc"
    "bdyU_u3d_FIX_PSY025_y2012.nc => My_bdy_uo_y2012.nc"
    "bdyU_u3d_FIX_PSY025_y2013.nc => My_bdy_uo_y2013.nc"

    "bdyV_u3d_FIX_PSY025_y2011.nc => My_bdy_vo_y2011.nc"
    "bdyV_u3d_FIX_PSY025_y2012.nc => My_bdy_vo_y2012.nc"
    "bdyV_u3d_FIX_PSY025_y2013.nc => My_bdy_vo_y2013.nc"

    "bdyU_u2d_PSY025_y2011.nc => My_bdy_u2_y2011.nc"
    "bdyU_u2d_PSY025_y2012.nc => My_bdy_u2_y2012.nc"
    "bdyU_u2d_PSY025_y2013.nc => My_bdy_u2_y2013.nc"

    "bdyV_v2d_PSY025_y2011.nc => My_bdy_v2_y2011.nc"
    "bdyV_v2d_PSY025_y2012.nc => My_bdy_v2_y2012.nc"
    "bdyV_v2d_PSY025_y2013.nc => My_bdy_v2_y2013.nc"

    "ORCA025_coordinates_bdyr1_ORCA025.nc => My_bdy_coordinates.nc"
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
    "nemo.exe"
    "xios_server.exe"
    "namelist_ref"
    "namelist_ice_ref"
)

oa_files=(
   "start_atmos_cpl_new.nc"
   "start_ocean_cpl_025.nc"
   "areas.nc"
   "masks.nc"
   "grids.nc"
   "masks.nc"
   "rmp_lmdz_to_torc_BILINEAR.nc"
   "rmp_mara_to_nemo_BILINEAR.nc"
   "rmp_nemo_to_mara_BILINEAR.nc"
   "rmp_torc_to_lmdz_BILINEAR.nc"
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


