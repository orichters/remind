
*####################### R SECTION START (PHASES) ##############################
$Ifi "%phase%" == "declarations" $include "./modules/50_damages/exogenous/declarations.gms"
$Ifi "%phase%" == "datainput" $include "./modules/50_damages/exogenous/datainput.gms"
$Ifi "%phase%" == "bounds" $include "./modules/50_damages/exogenous/bounds.gms"
*######################## R SECTION END (PHASES) ###############################
