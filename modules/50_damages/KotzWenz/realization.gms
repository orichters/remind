
*####################### R SECTION START (PHASES) ##############################
$Ifi "%phase%" == "sets" $include "./modules/50_damages/KotzWenz/sets.gms"
$Ifi "%phase%" == "declarations" $include "./modules/50_damages/KotzWenz/declarations.gms"
$Ifi "%phase%" == "datainput" $include "./modules/50_damages/KotzWenz/datainput.gms"
$Ifi "%phase%" == "equations" $include "./modules/50_damages/KotzWenz/equations.gms"
$Ifi "%phase%" == "preloop" $include "./modules/50_damages/KotzWenz/preloop.gms"
$Ifi "%phase%" == "bounds" $include "./modules/50_damages/KotzWenz/bounds.gms"
$Ifi "%phase%" == "presolve" $include "./modules/50_damages/KotzWenz/presolve.gms"
$Ifi "%phase%" == "solve" $include "./modules/50_damages/KotzWenz/solve.gms"
$Ifi "%phase%" == "postsolve" $include "./modules/50_damages/KotzWenz/postsolve.gms"
$Ifi "%phase%" == "output" $include "./modules/50_damages/KotzWenz/output.gms"
*######################## R SECTION END (PHASES) ###############################
