
*####################### R SECTION START (PHASES) ##############################
$Ifi "%phase%" == "sets" $include "./modules/47_regipol/netZero/sets.gms"
$Ifi "%phase%" == "declarations" $include "./modules/47_regipol/netZero/declarations.gms"
$Ifi "%phase%" == "datainput" $include "./modules/47_regipol/netZero/datainput.gms"
$Ifi "%phase%" == "postsolve" $include "./modules/47_regipol/netZero/postsolve.gms"
*######################## R SECTION END (PHASES) ###############################
