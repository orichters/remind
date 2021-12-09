
*** |  (C) 2006-2020 Potsdam Institute for Climate Impact Research (PIK)
*** |  authors, and contributors see CITATION.cff file. This file is part
*** |  of REMIND and licensed under AGPL-3.0-or-later. Under Section 7 of
*** |  AGPL-3.0, you are granted additional permissions described in the
*** |  REMIND License Exception, version 1.0 (see LICENSE file).
*** |  Contact: remind@pik-potsdam.de
*** SOF ./modules/47_regipol/netZero/declarations.gms

Parameter 
p47_actual_co2eq_regi(all_regi )        "greenhouse gas emissions in target year"
p47_2020_regi(all_regi)                 "2020 reference emissions value for normalization of deviation from zero"
p47_taxCO2eq_regi_factor(all_regi)      "factor of change for additional carbon price"
*pm_taxCO2eq_regi(tall,all_regi)        "additional carbon price to reach net-zero target"
p47_taxCO2eq_regi_last(tall,all_regi)   "additional carbon price to reach net-zero target in last iteration"
p47_taxCO2eq_last(tall,all_regi)        "general carbon price in last iteration"
p47_taxCO2eq_factor(all_regi)           "required change of overall tax rate to assure net-zero emission";

*** EOF ./modules/47_regipol/netZero/declarations.gms


