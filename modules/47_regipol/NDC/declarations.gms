*** |  (C) 2006-2020 Potsdam Institute for Climate Impact Research (PIK)
*** |  authors, and contributors see CITATION.cff file. This file is part
*** |  of REMIND and licensed under AGPL-3.0-or-later. Under Section 7 of
*** |  AGPL-3.0, you are granted additional permissions described in the
*** |  REMIND License Exception, version 1.0 (see LICENSE file).
*** |  Contact: remind@pik-potsdam.de
*** SOF ./modules/47_regipol/NDC/declarations.gms

Parameter p47_actual_co2eq_woLU_regi(ttot,all_regi)                "actual level of regional 2020/2025/2030 GHG emissions in previous iteration";
Parameter p47_ref_co2eq_woLU_regi(ttot,all_regi)                   "regional NDC target level of GHG - with different temporal meanings depending on NDC target year";
Parameter p47_factorRescaleCO2Tax(ttot,all_regi)                   "multiplicative factor to rescale CO2 taxes to achieve the climate targets";
Parameter p47_factorRescaleCO2TaxTrack(iteration,ttot,all_regi)    "Track the changes of p47_factorRescaleCO2Tax over the iterations";
Parameter p47_taxCO2eq_first_NDC_year(all_regi)                    "CO2eq tax in p47_first_NDC_year";
Parameter p47_taxCO2eq_last_NDC_year(all_regi)                     "CO2eq tax in p47_last_NDC_year";
Scalar    p47_adjust_exponent                                      "exponent in tax adjustment process";
Scalar    p47_previous_year_in_loop                                "previous year in loop, required for linear interpolation in postsolve";
Scalar    p47_tax_previous_year_in_loop                            "tax of previous year in loop, required for linear interpolation in postsolve";

*** EOF ./modules/47_regipol/NDC/declarations.gms
