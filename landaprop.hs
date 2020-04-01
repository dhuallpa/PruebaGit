type Requisito = Propiedad -> Bool
type Busqueda = [Requisito]
data Propiedad = Depto {
ambientes::Int, 
superficie::Int, 
precio::Float, 
barrio::String}

data Usuario = Persona {
mail::String, 
busqueda::Busqueda }

ordenarSegun :: (a -> a -> Bool) -> [a] -> [a]
ordenarSegun _ [] = []
ordenarSegun criterio (x:xs) = (ordenarSegun criterio.filter (not.criterio x)) xs ++ [x] ++ (ordenarSegun criterio.filter (criterio x)) xs

between x y z = x <= z && y >= z

propiedadesDeEjemplo = [Depto 3 80 12500 "Palermo", Depto 1 45 10000 "Villa Urquiza", Depto 2 50 16000 "Palermo", Depto 1 45 10500 "Recoleta"]
casa = Depto 3 80 12500 "Palermo"
mayorSegun func x y = func x > func y

-- ejemplo de uso    ordenarSegun (mayorSegun length ) ["qt","sds","s","sas"]

ubicadoEn barrios = any(estaEn propiedadesDeEjemplo) barrios 
estaEn prop lugar = elem lugar (map barrio prop)

precioDep dep= precio dep 
cumpleRango func x y = between x y (func casa)

cumpleBusqueda :: Propiedad-> Busqueda -> Bool
cumpleBusqueda dep busq  = map ($ dep) Bus

