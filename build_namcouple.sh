cat << EOF

# This is a typical input file for OASIS3-MCT.
# Keywords used in previous versions of OASIS3 
# but now obsolete are marked "Not used"
# Don't hesitate to ask precisions or make suggestions (oasishelp@cerfacs.fr). 
#
# Any line beginning with # is ignored. Blank lines are not allowed.
#
#########################################################################
#### Pour ce qui est marqué au dessus, je l'ai trouvé sur le forum d'Oasis
 \$NFIELDS
# The number of fields described in the second part of the namcouple.
#
            27
 \$END
###########################################################################
 \$RUNTIME
# The total simulated time for this run in seconds
#
   ${leg_length_sec}
 \$END
###########################################################################
 \$NLOGPRT
# Amount of information written to OASIS3-MCT log files (see User Guide)
  0
 \$END
###########################################################################
 \$STRINGS
#
# The above variables are the general parameters for the experiment.
# Everything below has to do with the fields being exchanged.
#
######################################################
#
#   First line:
# 1) and 2) Symbolic names for the field before and after interpolation
#           (8 characters maximum)
# 3) Index of field in cf_name_table.txt
# 4) Exchange frequency for the field in seconds (here 1 day)
# 5) Number of analysis to be performed
# 6) Restart input NetCDF file names
# 7) Field status: EXPOUT, EXPORTED, INPUT, OUTPUT
#
######################################
#      FROM OCEAN TO ATMOSPHERE      #
######################################
#
#------- Ocean sea surface temp ----#
O_SSTSST SISUTESW 1 ${o2afreq} ${ntranst} ${cpl_oce_rst} ${cploutopt}
${ndx} ${ndy} ${mdx} ${mdy} torc lmdz LAG=+${o2afreq}
R  0  R  0
${transt1}
${transt2}
${transt3}
#------- Sea ice concentration ----#
OIceFrc SIICECOV 1 ${o2afreq} ${ntranst} ${cpl_oce_rst} ${cploutopt}
${ndx} ${ndy} ${mdx} ${mdy} torc lmdz LAG=+${o2afreq}
R  0  R  0
${transt1}
${transt2}
${transt3}
#------- Sea ice / snow albedo ----#
O_AlbIce SIICEALW 1 ${o2afreq} ${ntranst} ${cpl_oce_rst} ${cploutopt}
${ndx} ${ndy} ${mdx} ${mdy} torc lmdz LAG=+${o2afreq}
R  0  R  0
${transt1}
${transt2}
${transt3}
#------- Sea ice surface temperature ----#
O_TepIce SIICTEMW 1 ${o2afreq} ${ntranst} ${cpl_oce_rst} ${cploutopt}
${ndx} ${ndy} ${mdx} ${mdy} torc lmdz LAG=+${o2afreq}
R  0  R  0
${transt1}
${transt2}
${transt3}
#------- Sea ice thicjness  ----#
OIceTck SIHEIGHT 1 ${o2afreq} ${ntranst} ${cpl_oce_rst} ${cploutopt}
${ndx} ${ndy} ${mdx} ${mdy} torc lmdz LAG=+${o2afreq}
R  0  R  0
${transt1}
${transt2}
${transt3}
#------- Snow layer thickness ----#
OSnwTck SISNOWHT 1 ${o2afreq} ${ntranst} ${cpl_oce_rst} ${cploutopt}
${ndx} ${ndy} ${mdx} ${mdy} torc lmdz LAG=+${o2afreq}
R  0  R  0
${transt1}
${transt2}
${transt3}
#------- Ocean U velocity ----#
O_OCurx1 OCECURTU 1 ${o2afreq} ${ntransd} ${cpl_oce_rst} ${cploutopt}
${ndx} ${ndy} ${mdx} ${mdy} torc lmdz LAG=+${o2afreq}
R  0  R  0
${transd1}
${transd2}
${transd3}
#------- Ocean V velocity  ----#
O_OCury1 OCECURTV 1 ${o2afreq} ${ntransd} ${cpl_oce_rst} ${cploutopt}
${ndx} ${ndy} ${mdx} ${mdy} torc lmdz LAG=+${o2afreq}
R  0  R  0
${transd1}
${transd2}
${transd3}
#------- Sea ice U velocity ----#
O_IVelx1 ICECURTU 1 ${o2afreq} ${ntransd} ${cpl_oce_rst} ${cploutopt}
${ndx} ${ndy} ${mdx} ${mdy} torc lmdz LAG=+${o2afreq}
R  0  R  0
${transd1}
${transd2}
${transd3}
#------- Sea ice V velocity ----#
O_IVely1 ICECURTV 1 ${o2afreq} ${ntransd} ${cpl_oce_rst} ${cploutopt}
${ndx} ${ndy} ${mdx} ${mdy} torc lmdz LAG=+${o2afreq}
R  0  R  0
${transd1}
${transd2}
${transd3}
######################################
#      FROM ATMOSPHERE TO OCEAN      #
######################################
#------- Non solar heat flux sensitivity ----#
CODFLXDT O_dQnsdT 1 ${a2ofreq} ${ntranst} ${cpl_atm_rst} ${cploutopt}
${mdx} ${mdy} ${ndx} ${ndy} lmdz torc LAG=+${o2afreq}
R 0 R 0
${transt1}
${transt2}
${transt3}
#------- Net solar heat flux over ocean ----#
COSHFOCE O_QsrOce 1 ${a2ofreq} ${ntranst} ${cpl_atm_rst} ${cploutopt}
${mdx} ${mdy} ${ndx} ${ndy} lmdz torc LAG=+${a2ofreq}
R 0 R 0
${transt1}
${transt2}
${transt3}
#------- Net solar heat flux over sea ice ----#
COSHFICE O_QsrIce 1 ${a2ofreq} ${ntranst} ${cpl_atm_rst} ${cploutopt}
${mdx} ${mdy} ${ndx} ${ndy} lmdz torc LAG=+${a2ofreq}
R 0 R 0
${transt1}
${transt2}
${transt3}
#------- Net non solar heat flux over ocean ----#
CONSFOCE O_QnsOce 1 ${a2ofreq} ${ntranst} ${cpl_atm_rst} ${cploutopt}
${mdx} ${mdy} ${ndx} ${ndy} lmdz torc LAG=+${a2ofreq}
R 0 R 0
${transt1}
${transt2}
${transt3}
#------- Net non solar heat flux over sea ice ----#
CONSFICE O_QnsIce 1 ${a2ofreq} ${ntranst} ${cpl_atm_rst} ${cploutopt}
${mdx} ${mdy} ${ndx} ${ndy} lmdz torc LAG=+${a2ofreq}
R 0 R 0
${transt1}
${transt2}
${transt3}
#------- Total evaporation (oce + ice)  ----#
COEVATOT OTotEvap 1 ${a2ofreq} ${ntranst} ${cpl_atm_rst} ${cploutopt}
${mdx} ${mdy} ${ndx} ${ndy} lmdz torc LAG=+${a2ofreq}
R 0 R 0
${transt1}
${transt2}
${transt3}
#------- Liquid precipitation ----#
COLIQPRE OTotRain 1 ${a2ofreq} ${ntranst} ${cpl_atm_rst} ${cploutopt}
${mdx} ${mdy} ${ndx} ${ndy} lmdz torc LAG=+${a2ofreq}
R 0 R 0
${transt1}
${transt2}
${transt3}
#------- Solid precipitation ----#
COSOLPRE OTotSnow 1 ${a2ofreq} ${ntranst} ${cpl_atm_rst} ${cploutopt}
${mdx} ${mdy} ${ndx} ${ndy} lmdz torc LAG=+${a2ofreq}
R 0 R 0
${transt1}
${transt2}
${transt3}
#------- Sea ice sublimation  ----#
COEVAICE OIceEvap 1 ${a2ofreq} ${ntranst} ${cpl_atm_rst} ${cploutopt}
${mdx} ${mdy} ${ndx} ${ndy} lmdz torc LAG=+${a2ofreq}
R 0 R 0
${transt1}
${transt2}
${transt3}
#------- Wind stress over ocean XU ----#
COTAUXUW O_OTaux1 1 ${a2ofreq} ${ntransd} ${cpl_atm_rst} ${cploutopt}
${mdx} ${mdy} ${ndx} ${ndy} lmdz torc LAG=+${a2ofreq}
R 0 R 0
${transd1}
${transd2}
${transd3}
#------- Wind stress over ocean YU ----#
COTAUYUW O_OTauy1 1 ${a2ofreq} ${ntransd} ${cpl_atm_rst} ${cploutopt}
${mdx} ${mdy} ${ndx} ${ndy} lmdz torc LAG=+${a2ofreq}
R 0 R 0
${transd1}
${transd2}
${transd3}
#------- Wind stress over ocean XV ----#
COTAUXVW O_OTaux2 1 ${a2ofreq} ${ntransd} ${cpl_atm_rst} ${cploutopt}
${mdx} ${mdy} ${ndx} ${ndy} lmdz torc LAG=+${a2ofreq}
R 0 R 0
${transd1}
${transd2}
${transd3}
#------- Wind stress over ocean YV ----#
COTAUYVW O_OTauy2 1 ${a2ofreq} ${ntransd} ${cpl_atm_rst} ${cploutopt}
${mdx} ${mdy} ${ndx} ${ndy} lmdz torc LAG=+${a2ofreq}
R 0 R 0
${transd1}
${transd2}
${transd3}
#------- Wind stress over sea ice XU ----#
COTAUXUI O_ITaux1 1 ${a2ofreq} ${ntransd} ${cpl_atm_rst} ${cploutopt}
${mdx} ${mdy} ${ndx} ${ndy} lmdz torc LAG=+${a2ofreq}
R 0 R 0
${transd1}
${transd2}
${transd3}
#------- Wind stress over sea ice YU ----#
COTAUYUI O_ITauy1 1 ${a2ofreq} ${ntransd} ${cpl_atm_rst} ${cploutopt}
${mdx} ${mdy} ${ndx} ${ndy} lmdz torc LAG=+${a2ofreq}
R 0 R 0
${transd1}
${transd2}
${transd3}
#------- Wind stress over sea ice XV ----#
COTAUXVI O_ITaux2 1 ${a2ofreq} ${ntransd} ${cpl_atm_rst} ${cploutopt}
${mdx} ${mdy} ${ndx} ${ndy} lmdz torc LAG=+${a2ofreq}
R 0 R 0
${transd1}
${transd2}
${transd3}
#------- Wind stress over sea ice YV ----#
COTAUYVI O_ITauy2 1 ${a2ofreq} ${ntransd} ${cpl_atm_rst} ${cploutopt}
${mdx} ${mdy} ${ndx} ${ndy} lmdz torc LAG=+${a2ofreq}
R 0 R 0
${transd1}
${transd2}
${transd3}
#
\$END
EOF
