*** |  (C) 2006-2023 Potsdam Institute for Climate Impact Research (PIK)
*** |  authors, and contributors see CITATION.cff file. This file is part
*** |  of REMIND and licensed under AGPL-3.0-or-later. Under Section 7 of
*** |  AGPL-3.0, you are granted additional permissions described in the
*** |  REMIND License Exception, version 1.0 (see LICENSE file).
*** |  Contact: remind@pik-potsdam.de
*** SOF ./modules/16_downscaleTemperature/CMIP5/declarations.gms

parameters
pm_regionalTemperature(tall,all_regi) "regional temperature"
pm_tempScaleGlob2Reg(tall,all_regi)   "scaling factor from global to regional temperature"
p16_tempRegionalCMIP5(tall,all_regi)  "regional temperature"
p16_tempGlobalCMIP5(tall)             "global temperature"
;

*** EOF ./modules/16_downscaleTemperature/CMIP5/declarations.gms
