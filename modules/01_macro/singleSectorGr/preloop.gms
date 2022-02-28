*** |  (C) 2006-2020 Potsdam Institute for Climate Impact Research (PIK)
*** |  authors, and contributors see CITATION.cff file. This file is part
*** |  of REMIND and licensed under AGPL-3.0-or-later. Under Section 7 of
*** |  AGPL-3.0, you are granted additional permissions described in the
*** |  REMIND License Exception, version 1.0 (see LICENSE file).
*** |  Contact: remind@pik-potsdam.de
*** SOF ./modules/01_macro/singleSectorGr/preloop.gms

*** Calculate cumulative depreciation factors. old version
*** differentiates even and uneven time steps
*** has a wrong shift in my opinion. The 1/2 parts should be for even, not for uneven
*** should be unfit for yearly models

loop ((t,counter),
if ( pm_dt(t) eq 2 * counter.val,
pm_cumDeprecFactor_old(t,regi,in)$(ppfKap(in) OR in_putty(in))
=   ((1 - pm_delta_kap(regi,in)) ** (pm_dt(t)/2 )
      - (1 - pm_delta_kap(regi,in)) ** (pm_dt(t) ))
     /  pm_delta_kap(regi,in)
    ;

pm_cumDeprecFactor_new(t,regi,in)$(ppfKap(in) OR in_putty(in))
=   ( 1
     - (1 - pm_delta_kap(regi,in)) ** (pm_dt(t)/2)
      )
     /  pm_delta_kap(regi,in)
    ;

);
if ( pm_dt(t) eq (2 * counter.val -1),
pm_cumDeprecFactor_old(t,regi,in)$(ppfKap(in) OR in_putty(in))
=   ((1 - pm_delta_kap(regi,in)) ** (pm_dt(t)/2 - 0.5)
      - (1 - pm_delta_kap(regi,in)) ** (pm_dt(t)))
     /  pm_delta_kap(regi,in)
    - 1/2 * (1 - pm_delta_kap(regi,in)) ** (pm_dt(t)/2 - 0.5 )
    ;

pm_cumDeprecFactor_new(t,regi,in)$(ppfKap(in) OR in_putty(in))
=   ( 1
     - (1 - pm_delta_kap(regi,in)) ** (pm_dt(t)/2 - 0.5 + 1)
      )
     /  pm_delta_kap(regi,in)
    - 1/2 * (1 - pm_delta_kap(regi,in)) ** (pm_dt(t)/2 - 0.5)
    ;

);
);
display "test Deprec old", pm_cumDeprecFactor_new,pm_cumDeprecFactor_old;



*** Calculate cumulative depreciation factors. first new version
*** differentiates even and uneven time timesteps
*** corrected the 1/2 parts
*** h_ndc_bIT_2022-02-24_18.47.09

loop ((t,counter),
if ( pm_dt(t) eq 2 * counter.val,
pm_cumDeprecFactor_old(t,regi,in)$(ppfKap(in) OR in_putty(in))
=   ((1 - pm_delta_kap(regi,in)) ** (pm_dt(t)/2 )
      - (1 - pm_delta_kap(regi,in)) ** (pm_dt(t) ))
     /  pm_delta_kap(regi,in)
    - 1/2 * (1 - pm_delta_kap(regi,in)) ** (pm_dt(t)/2)
    ;

pm_cumDeprecFactor_new(t,regi,in)$(ppfKap(in) OR in_putty(in))
=   ( 1
     - (1 - pm_delta_kap(regi,in)) ** (pm_dt(t)/2)
      )
     /  pm_delta_kap(regi,in)
     - 1/2 * (1 - pm_delta_kap(regi,in)) ** (pm_dt(t)/2)
    ;

);
if ( pm_dt(t) eq (2 * counter.val -1),
pm_cumDeprecFactor_old(t,regi,in)$(ppfKap(in) OR in_putty(in))
=   ((1 - pm_delta_kap(regi,in)) ** (pm_dt(t)/2 + 0.5)
      - (1 - pm_delta_kap(regi,in)) ** (pm_dt(t)))
     /  pm_delta_kap(regi,in)
    ;

pm_cumDeprecFactor_new(t,regi,in)$(ppfKap(in) OR in_putty(in))
=   ( 1
     - (1 - pm_delta_kap(regi,in)) ** (pm_dt(t)/2 + 0.5)
      )
     /  pm_delta_kap(regi,in)
    ;

);
);
display "test Deprec first new, 1/2 shifted to even", pm_cumDeprecFactor_new,pm_cumDeprecFactor_old;




*** Calculate cumulative depreciation factors. second new version
*** makes a linear regression between old and new investment
*** CODE for wolfram alpha with A: old, N: new, t: time, n: time steps, d: depreciation
*** Sum[t/n * N * Power[(1-d),(n-t)]+(n-t)/n * A * Power[(1-d),(n-t)] ,{t,1,n}]
*** = (A ((1 - d)^n + d (-(1 - d)^n + n + 1) - 1) - B (-d (1 - d)^n + d n (1 - d)^n + (1 - d)^n + d - 1))/(d^2 n)
*** h_ndc_bIT_2022-02-28

loop ((t,counter),

pm_cumDeprecFactor_old(t,regi,in)$(ppfKap(in) OR in_putty(in))
  = (1 - pm_delta_kap(regi,in) - (1 + pm_delta_kap(regi,in) * (pm_dt(t) - 1)) * (1 - pm_delta_kap(regi,in))**pm_dt(t))
    /(sqr(pm_delta_kap(regi,in)) * pm_dt(t));

pm_cumDeprecFactor_new(t,regi,in)$(ppfKap(in) OR in_putty(in))
  = ((1 - pm_delta_kap(regi,in))**(pm_dt(t) + 1) + pm_delta_kap(regi,in) * pm_dt(t) + pm_delta_kap(regi,in) - 1)
    /(sqr(pm_delta_kap(regi,in)) * pm_dt(t));

);
display "test Deprec first new, 1/2 shifted to even", pm_cumDeprecFactor_new,pm_cumDeprecFactor_old;





display "test Deprec", pm_cumDeprecFactor_new,pm_cumDeprecFactor_old;

*** EOF ./modules/01_macro/singleSectorGr/preloop.gms
