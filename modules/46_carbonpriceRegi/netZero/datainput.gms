*** |  (C) 2006-2020 Potsdam Institute for Climate Impact Research (PIK)
*** |  authors, and contributors see CITATION.cff file. This file is part
*** |  of REMIND and licensed under AGPL-3.0-or-later. Under Section 7 of
*** |  AGPL-3.0, you are granted additional permissions described in the
*** |  REMIND License Exception, version 1.0 (see LICENSE file).
*** |  Contact: remind@pik-potsdam.de
*** SOF ./modules/46_carbonpriceRegi/netZero/datainput.gms

***profile for countries with 2050 target
pm_taxCO2eqRegi("2035",nz_reg2050)=5;
pm_taxCO2eqRegi("2040",nz_reg2050)=10;
pm_taxCO2eqRegi("2045",nz_reg2050)=15;
pm_taxCO2eqRegi("2050",nz_reg2050)=20;
pm_taxCO2eqRegi("2055",nz_reg2050)=18;
pm_taxCO2eqRegi("2060",nz_reg2050)=16;
pm_taxCO2eqRegi("2065",nz_reg2050)=14;
pm_taxCO2eqRegi("2070",nz_reg2050)=12;
pm_taxCO2eqRegi("2075",nz_reg2050)=10;
pm_taxCO2eqRegi("2080",nz_reg2050)=8;
pm_taxCO2eqRegi("2085",nz_reg2050)=6;
pm_taxCO2eqRegi("2090",nz_reg2050)=4;
pm_taxCO2eqRegi("2095",nz_reg2050)=2;

*** profile for countries with 2055 CO2 target
pm_taxCO2eqRegi("2035",nz_reg2055CO2)=3;
pm_taxCO2eqRegi("2040",nz_reg2055CO2)=6;
pm_taxCO2eqRegi("2045",nz_reg2055CO2)=10;
pm_taxCO2eqRegi("2050",nz_reg2055CO2)=16;
pm_taxCO2eqRegi("2055",nz_reg2055CO2)=24;
pm_taxCO2eqRegi("2060",nz_reg2055CO2)=21;
pm_taxCO2eqRegi("2065",nz_reg2055CO2)=19;
pm_taxCO2eqRegi("2070",nz_reg2055CO2)=16;
pm_taxCO2eqRegi("2075",nz_reg2055CO2)=13;
pm_taxCO2eqRegi("2080",nz_reg2055CO2)=10;
pm_taxCO2eqRegi("2085",nz_reg2055CO2)=7;
pm_taxCO2eqRegi("2090",nz_reg2055CO2)=4;
pm_taxCO2eqRegi("2095",nz_reg2055CO2)=2;

*** profile for countries with 2060 target
pm_taxCO2eqRegi("2035",nz_reg2060)=2;
pm_taxCO2eqRegi("2040",nz_reg2060)=5;
pm_taxCO2eqRegi("2045",nz_reg2060)=9;
pm_taxCO2eqRegi("2050",nz_reg2060)=14;
pm_taxCO2eqRegi("2055",nz_reg2060)=20;
pm_taxCO2eqRegi("2060",nz_reg2060)=28;
pm_taxCO2eqRegi("2065",nz_reg2060)=22;
pm_taxCO2eqRegi("2070",nz_reg2060)=17;
pm_taxCO2eqRegi("2075",nz_reg2060)=13;
pm_taxCO2eqRegi("2080",nz_reg2060)=10;
pm_taxCO2eqRegi("2085",nz_reg2060)=7;
pm_taxCO2eqRegi("2090",nz_reg2060)=4;
pm_taxCO2eqRegi("2095",nz_reg2060)=2;

***rescale
pm_taxCO2eqRegi(ttot,regi) = sm_DptCO2_2_TDpGtC * pm_taxCO2eqRegi(ttot,regi);

***initialize parameter
p46_taxCO2eqRegiLast(t,regi) =0;
p46_taxCO2eqLast(t,regi)     =0;

*** EOF ./modules/46_carbonpriceRegi/netZero/datainput.gms


