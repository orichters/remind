*** |  (C) 2006-2020 Potsdam Institute for Climate Impact Research (PIK)
*** |  authors, and contributors see CITATION.cff file. This file is part
*** |  of REMIND and licensed under AGPL-3.0-or-later. Under Section 7 of
*** |  AGPL-3.0, you are granted additional permissions described in the
*** |  REMIND License Exception, version 1.0 (see LICENSE file).
*** |  Contact: remind@pik-potsdam.de
*** SOF ./modules/47_regipol/NDC/datainput.gms

*** load NDC data
Table f47_factor_targetyear(ttot,all_regi,NDC_version,all_GDPscen) "Table for all NDC versions with multiplier for target year emissions vs 2005 emissions, as weighted average for all countries with quantifyable emissions under NDC in particular region"
$offlisting
$ondelim
$include "./modules/47_regipol/NDC/input/f45_factor_targetyear.cs3r"
$offdelim
$onlisting
;

Parameter p47_factor_targetyear(ttot,all_regi) "Multiplier for target year emissions vs 2005 emissions, as weighted average for all countries with quantifyable emissions under NDC in particular region";
p47_factor_targetyear(ttot,all_regi) = f47_factor_targetyear(ttot,all_regi,"%cm_NDC_version%","%cm_GDPscen%");

display p47_factor_targetyear;

Table f47_2005share_target(ttot,all_regi,NDC_version,all_GDPscen) "Table for all NDC versions with 2005 GHG emission share of countries with quantifyable emissions under NDC in particular region, time dimension specifies alternative future target years"
$offlisting
$ondelim
$include "./modules/47_regipol/NDC/input/f45_2005share_target.cs3r"
$offdelim
$onlisting
;

Parameter p47_2005share_target(ttot,all_regi) "2005 GHG emission share of countries with quantifyable emissions under NDC in particular region, time dimension specifies alternative future target years";
p47_2005share_target(ttot,all_regi) = f47_2005share_target(ttot,all_regi,"%cm_NDC_version%","%cm_GDPscen%");

display p47_2005share_target;

Table f47_hist_share(tall,all_regi,NDC_version) "Table for all NDC versions with GHG emissions share of countries with quantifyable 2030 target, time dimension specifies historic record"
$offlisting
$ondelim
$include "./modules/47_regipol/NDC/input/f45_hist_share.cs3r"
$offdelim
$onlisting
;

Parameter p47_hist_share(tall,all_regi) "GHG emissions share of countries with quantifyable 2030 target, time dimension specifies historic record";
p47_hist_share(tall,all_regi) = f47_hist_share(tall,all_regi,"%cm_NDC_version%");

display p47_hist_share;

Parameter p47_BAU_reg_emi_wo_LU_bunkers(ttot,all_regi) "regional GHG emissions (without LU and bunkers) in BAU scenario"
  /
$ondelim
$include "./modules/47_regipol/NDC/input/p45_BAU_reg_emi_wo_LU_bunkers.cs4r"
$offdelim
  /             ;

*** parameters for selecting NDC years
Scalar p47_ignore_NDC_before             "NDC targets before this years are ignored, for example to exclude 2030 targets" /2050/;
Scalar p47_ignore_NDC_after              "NDC targets after  this years are ignored, for example to exclude 2050 net zero targets" /2070/;
Scalar p47_min_ratio_of_coverage_to_max  "only targets whose coverage is this times p47_best_NDC_coverage are considered. Use 1 for only best." /1.0/;
Scalar p47_use_single_year_close_to      "if 0: use all. If > 0: use only one single NDC target per country closest to this year (use 2030.4 to prefer 2030 over 2035 over 2025)" /2050.4/;

Set p47_NDC_year_set(ttot,all_regi)               "YES for years whose NDC targets is used";
Parameter p47_best_NDC_coverage(all_regi)         "highest coverage of NDC targets within region";
Parameter p47_distance_to_optyear(ttot,all_regi)  "distance to p47_use_single_year_close_to to favor years in case of multiple equally good targets";
Parameter p47_min_distance_to_optyear(all_regi)   "minimal distance to p47_use_single_year_close_to per region";

p47_best_NDC_coverage(regi) = smax(ttot$(ttot.val <= p47_ignore_NDC_after AND ttot.val >= p47_ignore_NDC_before), p47_2005share_target(ttot,regi));
display p47_best_NDC_coverage;

p47_NDC_year_set(ttot,regi)$(ttot.val <= p47_ignore_NDC_after AND ttot.val >= p47_ignore_NDC_before) = p47_2005share_target(ttot,regi) >= p47_min_ratio_of_coverage_to_max * p47_best_NDC_coverage(regi);

if(p47_use_single_year_close_to > 0,
  p47_distance_to_optyear(p47_NDC_year_set(ttot,regi)) = abs(ttot.val - p47_use_single_year_close_to);
  p47_min_distance_to_optyear(regi) = smin(ttot$(p47_NDC_year_set(ttot,regi)), p47_distance_to_optyear(ttot,regi));
  p47_NDC_year_set(ttot,regi) = p47_distance_to_optyear(ttot,regi) = p47_min_distance_to_optyear(regi);
);

*** first and last NDC year as a number
Parameter p47_first_NDC_year(all_regi) "last year with NDC coverage within region";
p47_first_NDC_year(regi) = smin( p47_NDC_year_set(ttot, regi), ttot.val );
Parameter p47_last_NDC_year(all_regi)  "last year with NDC coverage within region";
p47_last_NDC_year(regi)  = smax( p47_NDC_year_set(ttot, regi), ttot.val );

display p47_NDC_year_set,p47_first_NDC_year,p47_last_NDC_year;

*** adjust reduction value for LAM based on the assumption that Brazilian reduction targets are only from landuse.
*** the adjustment were taken such that Brazil is assumed to maintain its 2015 non-landuse emissions

p47_factor_targetyear(ttot,regi)$(sameas(regi,"LAM") AND sameas(ttot,"2025")) = p47_factor_targetyear(ttot,regi) + 0.46;
p47_factor_targetyear(ttot,regi)$(sameas(regi,"LAM") AND sameas(ttot,"2030")) = p47_factor_targetyear(ttot,regi) + 0.205;

*** EOF ./modules/47_regipol/NDC/datainput.gms
