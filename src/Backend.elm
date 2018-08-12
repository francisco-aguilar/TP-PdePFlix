module Backend exposing(..)
import Models exposing(Movie, Preferences)

completaAca = identity

-- **************
-- Requerimiento: filtrar películas por su título a medida que se escribe en el buscador;
-- **************

filtrarPeliculasPorPalabrasClave : String -> List Movie -> List Movie
filtrarPeliculasPorPalabrasClave palabras = List.filter (peliculaTienePalabrasClave palabras)

-- esta función la dejamos casi lista, pero tiene un pequeño bug. ¡Corregilo!
--
-- Además tiene dos problemas, que también deberías corregir:
--
-- * distingue mayúsculas de minúsculas, pero debería encontrar a "Lion King" aunque escriba "kINg"
-- * busca una coincidencia exacta, pero si escribís "Avengers Ultron" debería encontrar a "Avengers: Age Of Ultron"
--

peliculaTienePalabrasClave : String -> Movie -> Bool
peliculaTienePalabrasClave palabras pelicula =  List.all (flip contieneUnaPalabra pelicula.title) (String.words palabras)

contieneUnaPalabra : String -> String -> Bool
contieneUnaPalabra unaPalabra otraPalabra = String.contains (String.toLower unaPalabra) (String.toLower otraPalabra) && not(String.isEmpty unaPalabra) && not(String.isEmpty otraPalabra)

-- **************
-- Requerimiento: visualizar las películas según el género elegido en un selector;
-- **************

filtrarPeliculasPorGenero : String -> List Movie -> List Movie
filtrarPeliculasPorGenero genero = List.filter (esGeneroDeseado genero)

esGeneroDeseado : String -> Movie -> Bool
esGeneroDeseado genero pelicula = List.any (contieneUnaPalabra genero) (pelicula.genre)

-- **************
-- Requerimiento: filtrar las películas que sean aptas para menores de edad,
--                usando un checkbox;
-- **************

filtrarPeliculasPorMenoresDeEdad : Bool -> List Movie -> List Movie
filtrarPeliculasPorMenoresDeEdad mostrarSoloMenores peliculas = if mostrarSoloMenores then soloMenores peliculas else peliculas

soloMenores : List Movie -> List Movie
soloMenores peliculas = List.filter (.forKids) peliculas

-- **************
-- Requerimiento: ordenar las películas por su rating;
-- **************

ordenarPeliculasPorRating : List Movie -> List Movie
ordenarPeliculasPorRating = List.reverse << List.sortBy (.matchPercentage)

-- **************
-- Requerimiento: dar like a una película
-- **************

darLikeAPelicula : Int -> List Movie -> List Movie
darLikeAPelicula id = List.map (likear id)

likear : Int -> Movie -> Movie
likear id pelicula = if (pelicula.id == id) then (incrementarLikes pelicula) else pelicula

incrementarLikes : Movie -> Movie
incrementarLikes pelicula = {pelicula | likes = pelicula.likes + 1}

-- **************
-- Requerimiento: cargar preferencias a través de un popup modal,
--                calcular índice de coincidencia de cada película y
--                mostrarlo junto a la misma;
-- **************

calcularPorcentajeDeCoincidencia : Preferences -> List Movie -> List Movie
calcularPorcentajeDeCoincidencia preferencias = List.map (calcularPorcentaje preferencias)

calcularPorcentaje : Preferences -> Movie -> Movie
calcularPorcentaje preferencias pelicula = {pelicula | matchPercentage = porcentajePelicula preferencias pelicula}

porcentajePelicula : Preferences -> Movie -> Int
porcentajePelicula preferencias pelicula = if (porcentajeTotal preferencias pelicula) > 100 then 100 else (porcentajeTotal preferencias pelicula)

porcentajeTotal : Preferences -> Movie -> Int
porcentajeTotal preferencias pelicula = (porcentajePorActor preferencias pelicula) + (porcentajePorGenero preferencias pelicula) + (porcentajePorPalabras preferencias pelicula) + (porcentajePorGeneroAsociado preferencias pelicula)

--Porcentaje Actor

porcentajePorActor : Preferences -> Movie -> Int
porcentajePorActor preferencias peliculas = if trabajaActor pelicula preferencias.favoriteActor then 50 else 0

trabajaActor : Movie -> String -> Bool
trabajaActor pelicula actor = member actor (pelicula.actors)

--Porcentaje Genero

porcentajePorGenero : Preferences -> Movie -> Int
porcentajePorGenero preferencias pelicula = if (member (preferencias.genre) pelicula.genre) then 60 else 0

--Porcentaje Palabras Claves

porcentajePorPalabras : Preferences -> Movie -> Int
porcentajePorPalabras preferencias pelicula = (*)20<<(List.length)<<listaPalabrasClaves preferencias

listaPalabrasClaves : Preferences -> Movie -> List String
listaPalabrasClaves preferencias pelicula = List.filter (flip contieneUnaPalabra pelicula.title) (String.words preferencias.keywords)

