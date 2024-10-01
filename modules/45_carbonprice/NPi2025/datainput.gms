*** |  (C) 2006-2023 Potsdam Institute for Climate Impact Research (PIK)
*** |  authors, and contributors see CITATION.cff file. This file is part
*** |  of REMIND and licensed under AGPL-3.0-or-later. Under Section 7 of
*** |  AGPL-3.0, you are granted additional permissions described in the
*** |  REMIND License Exception, version 1.0 (see LICENSE file).
*** |  Contact: remind@pik-potsdam.de
*** SOF ./modules/45_carbonprice/NPi2025/datainput.gms

***----------------------------
*** CO2 Tax level growing exponentially from 2025 value taken from input data
***----------------------------

pm_taxCO2eq(ttot,regi)$(ttot.val le 2025) = fm_taxCO2eqHist(ttot,regi) * sm_DptCO2_2_TDpGtC;
pm_taxCO2eq(ttot,regi)$(ttot.val eq 2020 AND sameas(regi, "EUR")) = 30 * sm_DptCO2_2_TDpGtC;

pm_taxCO2eq(t,regi)$(t.val gt 2025) = sum(ttot, pm_taxCO2eq(ttot,regi)$(ttot.val eq 2025)) * cm_co2_tax_growth**(t.val - 2025);
pm_taxCO2eq(t,regi)$(t.val gt 2110) = pm_taxCO2eq("2110",regi); !! to prevent huge taxes after 2110 and the resulting convergence problems, set taxes after 2110 equal to 2110 value

*** EOF ./modules/45_carbonprice/NPi2025/datainput.gms