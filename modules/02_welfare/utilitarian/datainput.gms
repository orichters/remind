*** |  (C) 2006-2020 Potsdam Institute for Climate Impact Research (PIK)
*** |  authors, and contributors see CITATION.cff file. This file is part
*** |  of REMIND and licensed under AGPL-3.0-or-later. Under Section 7 of
*** |  AGPL-3.0, you are granted additional permissions described in the
*** |  REMIND License Exception, version 1.0 (see LICENSE file).
*** |  Contact: remind@pik-potsdam.de
*** SOF ./modules/02_welfare/utilitarian/datainput.gms

*** standard procedure, artificially reducing utility in 2060 to reduce jumps
pm_welf(ttot)$(ttot.val ge 2005) = 1;
$if %cm_less_TS% == "on"  pm_welf("2060") = 0.9;

if( cm_dtscen < 10,
pm_welf_oli(ttot,regi) = pm_welf(ttot) * pm_ts(ttot);
);

*** intertemporal optimization within time step, but with wrong discounting (1-d) instead of 1/(1+d)
*** using Wolfram Alpha with n = pm_dt(ttot), N = pm_dt(ttot+1), p = pm_prtp(regi)
*** Sum[ t/n * (1/(1-p))**(n-t) ,{t,1,n}]  =  ((p - 1) (n p + 1 - (1/(1 - p))^n))/(n p^2)
*** Sum[ (N-t)/N * (1-p)**t ,{t,1,N}]      = -((p - 1) ((1 - p)^N + N p - 1))/(N p^2)
if( cm_dtscen ge 10 AND cm_dtscen < 20,
pm_welf_oli(ttot,regi)$(ttot.val ge 2005 AND ord(ttot) < card(ttot)) = 
  (pm_prtp(regi) - 1) * (pm_dt(ttot)   * pm_prtp(regi) + 1 - (1/(1 - pm_prtp(regi)))**pm_dt(ttot)) / (sqr( pm_prtp(regi) ) * pm_dt(ttot  ) )
- (pm_prtp(regi) - 1) * (pm_dt(ttot+1) * pm_prtp(regi) - 1 + (1 - pm_prtp(regi) )**pm_dt(ttot+1) ) / (sqr( pm_prtp(regi) ) * pm_dt(ttot+1) );
pm_welf_oli(ttot,regi)$(ttot.val ge 2005 AND ord(ttot) eq card(ttot)) =  pm_ts(ttot);
*** not completely precise for the last time step
);

*** standard procedure without the jump reduction
if( cm_dtscen ge 20 AND cm_dtscen < 30,
pm_welf_oli(ttot,regi)$(ttot.val ge 2005) = pm_ts(ttot);
);

*** intertemporal optimization within time step with correct discounting 1/(1+d)
*** using Wolfram Alpha with n = pm_dt(ttot), N = pm_dt(ttot+1), p = pm_prtp(regi)
*** Sum[t/n * (1+p)**(n-t) ,{t,1,n-1}]   = -(p + 1) (n p + 1 -(p + 1)^n)/(n p^2)
*** Sum[(N-t)/N * (1/(1+p))**t ,{t,0,N}] =  (p + 1) (N p - 1 + (1/(p + 1))^N)/(N p^2)
if( cm_dtscen ge 30 AND cm_dtscen < 40,
pm_welf_oli(ttot,regi)$(ttot.val ge 2005 AND ord(ttot) < card(ttot)) = 
  - (pm_prtp(regi) + 1) * (pm_dt(ttot)   * pm_prtp(regi) + 1 - (pm_prtp(regi) + 1)**pm_dt(ttot)      ) / (sqr( pm_prtp(regi) ) * pm_dt(ttot  ) )
  + (pm_prtp(regi) + 1) * (pm_dt(ttot+1) * pm_prtp(regi) - 1 + (1/(1 + pm_prtp(regi)))**pm_dt(ttot+1)) / (sqr( pm_prtp(regi) ) * pm_dt(ttot+1) );
pm_welf_oli(ttot,regi)$(ttot.val ge 2005 AND ord(ttot) eq card(ttot)) =  pm_ts(ttot);
*** not completely precise for the last time step
);


*RP* 2012-03-06: Inconvenience costs on seprod
$IFTHEN.INCONV %cm_INCONV_PENALTY% == "on"
p02_inconvpen_lap(ttot,regi,"coaltr")$(ttot.val ge 2005)      = 0.5;   !! In dollar per GJ seprod at 1.000$/cap GDP, or 10$/GJ at 10.000$_GDP/cap
p02_inconvpen_lap(ttot,regi,"biotr")$(ttot.val ge 2005)       = 1.0;   !! In dollar per GJ seprod
p02_inconvpen_lap(ttot,regi,"biotrmod")$(ttot.val ge 2005)    = 0.25;    !! In dollar per GJ seprod. Biotrmod is a mix of wood stoves and automated wood pellets for heating, which has lower air pollution and other discomfort effects
*' Transformation of coal to liquids/gases/H2 brings local pollution, which is less accepted at higher incomes -> use the inconvenience cost channel
p02_inconvpen_lap(ttot,regi,"coalftrec")$(ttot.val ge 2005)   = 0.1;    !! In dollar per GJ seprod
p02_inconvpen_lap(ttot,regi,"coalftcrec")$(ttot.val ge 2005)  = 0.1;    !!  equivalent to 4$/GJ at 40.000$_GDP/cap, or 10$/GJ at 100.000$_GDP/cap
p02_inconvpen_lap(ttot,regi,"coalgas")$(ttot.val ge 2005)   = 0.1;    !!
p02_inconvpen_lap(ttot,regi,"coalh2")$(ttot.val ge 2005)   = 0.1;    !!
p02_inconvpen_lap(ttot,regi,"coalh2c")$(ttot.val ge 2005)  = 0.1;    !!
p02_inconvpen_lap(ttot,regi,te)$(ttot.val ge 2005) = p02_inconvpen_lap(ttot,regi,te) * 4.3 * 1E-4;            !! this is now equivalent to 1$/GJ at 1000$/per Capita in the welfare logarithm
p02_inconvpen_lap(ttot,regi,te)$(ttot.val ge 2005) = p02_inconvpen_lap(ttot,regi,te) * (1/sm_giga_2_non) / sm_GJ_2_TWa; !! conversion util/(GJ/cap) -> util/(TWa/Gcap)
*RP* these values are all calculated on seprod level.
display p02_inconvpen_lap;
$ENDIF.INCONV

*** EOF ./modules/02_welfare/utilitarian/datainput.gms
