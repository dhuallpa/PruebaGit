data Pais = Pais {
 ipc :: Float,
 pobPub :: Float,
 pobPriv ::Float,
 recursos :: [String],
 deuda :: Float
 } deriving (Show)


namibia:: Pais
namibia = Pais  4140 400000 650000 ["mineria","ecoturismo","Petroleo"] 50000000
ghana :: Pais 
ghana = Pais  4140 400000 650000 ["mineria","Petroleo"] 50000000
prestar :: Float -> Pais -> Pais
prestar n pais  = pais {deuda =deuda pais + n*1.5}

reducir :: Float -> Pais -> Pais
reducir x (Pais ipc pobPub pobPriv recursos deuda)
 |x>100 = (Pais (ipc*0.8) (pobPub-x) pobPriv recursos deuda)
 |otherwise = (Pais (ipc*0.85) (pobPub-x) pobPriv recursos deuda)

explotarRecurso :: String -> Pais -> Pais
explotarRecurso recurso (Pais ipc pobPub pobPriv recursos deuda)= (Pais ipc pobPub pobPriv (filter(/=recurso) recursos) (deuda - 2000000) )

establecerBlindaje (Pais ipc pobPub pobPriv recursos deuda) = prestar (prodBruto ipc pobPub pobPriv) (Pais ipc (pobPub-500) pobPriv recursos deuda)

prodBruto i p1 p2= (i * (p1+p2)) / 2

puedenZafar  = filter ((elem "Petroleo").recursos)

calcularDeuda = sum.map deuda 
