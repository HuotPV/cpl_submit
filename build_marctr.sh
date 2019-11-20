cat <<EOF
           T  reaVAR=.F. => Input INI: Prev.Dyn.Simulat. (MAR, GCM) |
           T  reaLBC=.F. => Input LBC: Prev.Dyn.Simulat. (MAR, GCM) |
           T  safVAR=.T. => Full Output on Saving Files MARxxx.DAT  |
           F  hamfil=.T. => Initial Filtered Fields (Time, Hamming) |
           T  conmas=.T. => Mass       Conserv. Constraint on Init. |
           F  potvor=.T. => P.-Vortic. Conserv. Constraint on Init. |
           T  brocam=.T. => Brown and Campana Time Scheme (Fast W.) |
           T  center=.T. => Pressure       Spatial Scheme  centered |
           4  nordps= 4  :  Pressure       Spatial Scheme Precision |
           T  staggr=.T. =>                Vertical  Grid staggered |
           T  turhor=.T. => Horizontal Diffusion (Smagorinsky)      |
           F  chimod=.F. => Atmospheric Chemical Model turned OFF   |
           T  convec=.T. => Convective  Adjustment     turned ON    |
           T  micphy=.T. => Cloud       Microphysics   turned ON    |
           T  fracld=.F. => Fractional  Cloudiness     turned OFF   |
       100.0  rhcrHY     :  Critical Relative Humidity Value        |
         0.0  tim_HY=18.0:  Cloud Microphys. start after 18h        |
  0.0001D+00  czmnGE     :  cos(Z) minimal value                    |
           T  physic=.T. => Atmospheric/Surface Physics included    |
           T  vegmod=.F. => Interactive SVAT           turned OFF   |
           T  snomod=.F. => Interactive Snow Model is  included     |
           F  polmod=.F. => Interactive Polynya    is  included     |
        1.00  hic0       :  Initial Thickness      of Sea Ice       |
        0.10  fxlead     :  Initial Lead Fraction  in Sea Ice       |
           F  qsolSL=.T. => Soil Humidity is interactive            |
        $dt.0  dt         =>    Time Step of Slow Dynamics       (s) |
        $nbo   nboucl     => Nb Time Steps between  each  Print      |
        -$np  np         => Nb Prints                               |
           3  ntfast     :  Nb Fast Time Steps over dt  (Lamb)      |
        $dt.0  dtDiff     :     Time Step of Diffusion               |
        $dt.0  dtPhys     :     Time Step of Surface Physics     (s) |
      3600.0  dtRadi     :     Time Step of Radiat. Transfert   (s) |
  5.0000D-03  rxbase     :  Nudging Coefficient (Anthes et al. 1989)|
  1.0000D+02  rxfact     :  Lateral Sponge Coefficient         (A89)|
+-------------------------------------------------------------------+
|                                                                   |
| Simulation     q10                                                |
| Periode        $DDs/$MM/$YYYY -> $DD/$MM/$YYYY                           |
| Time Step      $dt s                                               |
| Conv. adjust.  T                                                  |
| Surface model  SISVAT                                             |
| Snow    model  T                                                  |
| Polynya model  F                                                  |
|                                                                   |
+-------------------------------------------------------------------+
EOF
