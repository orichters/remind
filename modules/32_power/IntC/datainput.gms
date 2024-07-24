*** |  (C) 2006-2024 Potsdam Institute for Climate Impact Research (PIK)
*** |  authors, and contributors see CITATION.cff file. This file is part
*** |  of REMIND and licensed under AGPL-3.0-or-later. Under Section 7 of
*** |  AGPL-3.0, you are granted additional permissions described in the
*** |  REMIND License Exception, version 1.0 (see LICENSE file).
*** |  Contact: remind@pik-potsdam.de
*** SOF ./modules/32_power/IntC/datainput.gms

*------------------------------------------------------------------------------------
***                        IntC specific data input
*------------------------------------------------------------------------------------

parameter f32_shCHP(ttot,all_regi)  "upper boundary of chp electricity generation"
/
$ondelim
$include "./modules/32_power/IntC/input/f32_shCHP.cs4r"
$offdelim
/
;
p32_shCHP(ttot,all_regi) = f32_shCHP(ttot,all_regi) + 0.05;
p32_shCHP(ttot,all_regi)$(ttot.val ge 2050) = min(p32_shCHP("2020",all_regi) + 0.15, 0.75);
p32_shCHP(ttot,all_regi)$((ttot.val gt 2020) and (ttot.val lt 2050)) = p32_shCHP("2020",all_regi) + ((p32_shCHP("2050",all_regi) - p32_shCHP("2020",all_regi)) / 30 * (ttot.val - 2020));

***parameter p32_grid_factor(all_regi) - multiplicative factor that scales total grid requirements down in comparatively small or homogeneous regions like Japan, Europe or India
parameter p32_grid_factor(all_regi)                "multiplicative factor that scales total grid requirements down in comparatively small or homogeneous regions like Japan, Europe or India"
/
$ondelim
$include "./modules/32_power/IntC/input/p32_grid_factor.cs4r"
$offdelim
/
;

***parameter p32_factorStorage(all_regi,all_te) - multiplicative factor that scales total curtailment and storage requirements up or down in different regions for different technologies (e.g. down for PV in regions where high solar radiation coincides with high electricity demand)
parameter f32_factorStorage(all_regi,all_te)                  "multiplicative factor that scales total curtailment and storage requirements up or down in different regions for different technologies (e.g. down for PV in regions where high solar radiation coincides with high electricity demand)"
/
$ondelim
$include "./modules/32_power/IntC/input/f32_factorStorage.cs4r"
$offdelim
/
;

$IFTHEN.WindOff %cm_wind_offshore% == "1"
f32_factorStorage(all_regi,"windoff") = f32_factorStorage(all_regi,"wind");
f32_factorStorage(all_regi,"wind")      = 1.35 * f32_factorStorage(all_regi,"wind"); 
$ENDIF.WindOff
p32_factorStorage(all_regi,all_te) = f32_factorStorage(all_regi,all_te);

$if not "%cm_storageFactor%" == "off" p32_factorStorage(all_regi,all_te)=%cm_storageFactor%*p32_factorStorage(all_regi,all_te);

***parameter p32_storexp(all_regi,all_te) - exponent that determines how curtailment and storage requirements per kW increase with market share of wind and solar. 1 means specific marginal costs increase linearly
p32_storexp(regi,"spv")     = 1;
p32_storexp(regi,"csp")     = 1;
p32_storexp(regi,"wind")    = 1;
$IFTHEN.WindOff %cm_wind_offshore% == "1"
p32_storexp(regi,"windoff")    = 1;
$ENDIF.WindOff


***parameter p32_gridexp(all_regi,all_te) - exponent that determines how grid requirement per kW increases with market share of wind and solar. 1 means specific marginal costs increase linearly
p32_gridexp(regi,"spv")     = 1;
p32_gridexp(regi,"csp")     = 1;
p32_gridexp(regi,"wind")    = 1;


table f32_storageCap(char, all_te)  "multiplicative factor between dummy seel<-->h2 technologies and storXXX technologies"
$include "./modules/32_power/IntC/input/f32_storageCap.prn"
;

$IFTHEN.WindOff %cm_wind_offshore% == "1"
f32_storageCap(char,"windoff") = f32_storageCap(char,"wind");
$ENDIF.WindOff 

p32_storageCap(te,char) = f32_storageCap(char,te);
display p32_storageCap;

*** set total VRE share threshold above which additional integration challenges arise: 
p32_shThresholdTotVREAddIntCost(t)$(t.val < 2030) = 50;
p32_shThresholdTotVREAddIntCost("2030") = 60;
p32_shThresholdTotVREAddIntCost("2035") = 70;
p32_shThresholdTotVREAddIntCost("2040") = 80;
p32_shThresholdTotVREAddIntCost("2045") = 90;
p32_shThresholdTotVREAddIntCost(t)$(t.val > 2045) = 95;

p32_FactorAddIntCostTotVRE = 1.5;

$ontext
parameter p32_flex_maxdiscount(all_regi,all_te) "maximum electricity price discount for flexible technologies reached at high VRE shares"
/
$ondelim
$include "./modules/32_power/IntC/input/p32_flex_maxdiscount.cs4r"
$offdelim
/
; 
*** convert from USD2015/MWh to trUSD2005/TWa
p32_flex_maxdiscount(regi,te) = p32_flex_maxdiscount(regi,te) * sm_TWa_2_MWh * s_D2015_2_D2005 * 1e-12;
display p32_flex_maxdiscount;
$offtext

*** Flexibility Tax Parameter

*** Both flexibility tax parameters are based on a regression analysis with hourly dispatch data from high-VRE scenarios of the Langfristszenarien
*** for Germany provided by the Enertile power system model.
*** See: https://langfristszenarien.de/enertile-explorer-de/szenario-explorer/angebot.php

*** This parameter determines by the maximum electricity price reduction for electrolysis at 100% VRE share and 0% share of electrolysis in total electricity demand.
*** Standard value is derived based on the regression of the German Langfristzenarien.
parameter f32_cm_PriceDurSlope_elh2(ext_regi) "slope of price duration curve for electrolysis [#]" / %cm_PriceDurSlope_elh2% /;
p32_PriceDurSlope(regi,"elh2") = f32_cm_PriceDurSlope_elh2("GLO");
loop(ext_regi$f32_cm_PriceDurSlope_elh2(ext_regi),
  loop(regi$regi_groupExt(ext_regi,regi),
    p32_PriceDurSlope(regi,"elh2") = f32_cm_PriceDurSlope_elh2(ext_regi);
  );
); 

*** Slope of increase of electricity price for electrolysis with increasing share of electrolysis in power system
*** The value of 1.1 is derived from the regression of the German Langfristzenarien.
p32_flexSeelShare_slope(t,regi,"elh2") = 1.1;

*** EOF ./modules/32_power/IntC/datainput.gms
