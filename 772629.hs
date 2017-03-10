---------------------------------------
-- Functional Programming Coursework --
-- Haskell Task: Film Review Website --
-- Created by: 772629 				 --
---------------------------------------

import Data.List

-- |#########################|
-- |  		                 |
-- |		  Vars			 |
-- |					     |
-- |#########################|

type Title = String
type Director = String
type Year = Int
type Fanname = String

type Fans = [Fanname]
type Film = (Title, Director, Year, Fans)


-- testDatabase used for all demo functional code (Does not change)
testDatabase :: [Film]
testDatabase = [("Blade Runner", "Ridley Scott", 1982, ["Zoe", "Heidi", "Jo", "Kate", "Emma", "Liz", "Sam", "Olga", "Tim"]),
				("The Fly", "David Cronenberg", 1986, ["Garry", "Dave", "Zoe", "Kevin", "Emma"]),
				("Body Of Lies", "Ridley Scott", 2008, ["Bill", "Olga", "Tim", "Zoe", "Paula"]),			
				("Avatar", "James Cameron", 2009, ["Dave", "Amy", "Liz"]),
				("Titanic", "James Cameron", 1997, ["Zoe", "Emma", "Paula", "Liz", "Olga", "Dave"]),
				("The Departed", "Martin Scorsese", 2006, ["Wally", "Liz", "Kevin", "Tim", "Emma"]),
				("Aliens", "Ridley Scott", 1986, ["Dave", "Garry", "Liz", "Sam", "Wally", "Kate", "Zoe"]),
				("Kingdom Of Heaven", "Ridley Scott", 2005, ["Jo", "Wally", "Emma"]),
				("Prometheus", "Ridley Scott", 2012, ["Kevin", "Tim", "Emma", "Jo", "Liz"]),
				("E.T. The Extra-Terrestrial", "Steven Spielberg", 1982, ["Dave", "Amy", "Garry", "Ian", "Neal"]),
				("Bridge of Spies", "Steven Spielberg", 2015, ["Wally", "Sam", "Dave", "Neal"]),
				("Jaws", "Steven Spielberg", 1975, ["Dave", "Jo", "Zoe", "Wally", "Emma", "Kate"]),
				("The Martian", "Ridley Scott", 2015, ["Wally", "Sam", "Dave", "Jo", "Jenny", "Kate", "Emma", "Olga"]),
				("The BFG", "Steven Spielberg", 2016, ["Sam", "Wally", "Dave", "Jo", "Kate"]),
				("The Shawshank Redemption", "Frank Darabont", 1994, ["Dave", "Amy", "Bill", "Garry", "Ian", "Neal", "Kate", "Jenny", "Zoe"]),
				("Gladiator", "Ridley Scott", 2000, ["Olga", "Neal", "Kate", "Heidi", "Bill", "Sam", "Zoe"]),
				("The Green Mile", "Frank Darabont", 1999, ["Kevin", "Tim", "Emma", "Heidi"]),
				("True Lies", "James Cameron", 1994, ["Sam", "Dave"]),
				("Super 8", "J J Abrams", 2011, ["Kevin", "Tim", "Emma", "Olga", "Heidi"]),
				("Minority Report", "Steven Spielberg", 2002, ["Kevin", "Kate", "Tim", "Emma", "Olga", "Jenny", "Zoe"]),
				("War Horse", "Steven Spielberg", 2011, ["Garry", "Bill", "Olga", "Jo", "Wally", "Emma", "Tim", "Kate", "Zoe"]),
				("Silence", "Martin Scorsese", 2016, ["Wally", "Emma", "Tim", "Heidi", "Bill", "Olga", "Jo"]),
				("The Terminal", "Steven Spielberg", 2004, ["Kate", "Dave", "Jo", "Wally", "Emma"]),
				("Star Wars: The Force Awakens", "J J Abrams", 2015, ["Emma", "Wally", "Zoe", "Kate", "Bill", "Dave", "Liz", "Jo"]),
				("Hugo", "Martin Scorsese", 2011, ["Wally", "Sam"])]	

-- |#########################|
-- |  		                 |
-- |	Core  Functionality	 |
-- |					     |
-- |#########################|
		
-- Returns all the films available in the database
getAllFilms :: [Film]
getAllFilms = testDatabase

-- Adds a new film to the database
addNewFilm :: String -> String -> Int -> [Film] -> [Film]
addNewFilm ti di ye db = (ti, di, ye, []):db

-- Returns all films as a formatted string
filmsAsString :: [Film] -> String
filmsAsString [] = ""
filmsAsString ((ti, di, yr, fa):xs) = "| " ++ ti ++ " \n| Director: " ++ di  ++ " \n| Year: " ++ show ( yr ) ++ " \n| Fans: " ++ show( length ( fa ) ) ++ "\n\n" ++ filmsAsString xs

-- Converts all fans to a formatted string
fansAsString :: [Fans] -> String
fansAsString fan = " | Fan Name: " ++ intercalate "\n | Fan Name: " (head fan)

-- Returns all films released after the year (Not Including)
filmsReleasedAfterYear :: Int -> [Film]
filmsReleasedAfterYear year = [ (ti, di, yr, fan) | (ti, di, yr, fan) <- testDatabase, yr > year ]

-- Returns all films that a user is a fan of
fanFilms :: String -> [Film]
fanFilms user = [(ti, di, yr, fan) | (ti, di, yr, fan) <- testDatabase, elem user fan ]

-- Returns all fans of a particular film
fansOfFilm :: String -> [Fans]
fansOfFilm film = [ fan | (ti, di, yr, fan) <- testDatabase, film == ti ]

-- Returns all film once a user has become a fan
addFan :: String -> String -> [Film] -> [Film]
addFan title name ((ti, di, yr, fa):xs)
	| length xs == 0 = []
	| title == ti && elem name fa == False = (ti, di, yr, name : fa) : addFan title name xs 
	| otherwise		= (ti, di, yr, fa) : addFan title name xs

-- Fans of all films directed by a particular directors
fansOfDirector :: String -> [Film] -> [Fans]
fansOfDirector director ((ti, di, yr, fa):xs)
	| length xs == 0 = []
	| director == di = fa : fansOfDirector director xs
	| otherwise		 = fansOfDirector director xs	 

-- Checks if user is a fan of a specific film
checkIfFan :: String -> [Film] -> Bool
checkIfFan user [(ti, di, yr, fa)] = elem user fa
	
-- Gets all the directors with number of films that have a particular fan by name


-- |#########################|
-- |  		                 |
-- |	Demo Functionality	 |
-- |					     |
-- |#########################|

demo :: Int -> IO ()
-- All films after adding "Alien: Covenant" by "Ridley Scott" 2017
demo 1 = putStrLn( filmsAsString( addNewFilm "Alien: Covenant" "Ridley Scott" 2017 testDatabase ) )

-- Returning filmsAsString testDatabase
demo 2 = putStrLn( filmsAsString testDatabase )

-- All films that were released after 2008
demo 3 = putStrLn( filmsAsString( filmsReleasedAfterYear 2008 ) )

-- All films that "Liz" is a fan of
demo 4 = putStrLn( filmsAsString( fanFilms "Liz" ) )

-- All fans of the movie "Jaws"
demo 5 = putStrLn( fansAsString ( fansOfFilm "Jaws" ) )

-- All films after "Liz" says she becomes fan of "The Fly"
demo 6 = putStrLn( filmsAsString ( addFan "The Fly" "Liz" testDatabase ) )

-- All films after "Liz" says she becomes fan of "Avatar"
demo 66 = putStrLn( filmsAsString ( addFan "Avatar" "Liz" testDatabase ) )

-- All fans of films directed by "James Cameron"
demo 7 = putStrLn( fansAsString ( fansOfDirector "James Cameron" testDatabase ) )

-- All directors & no. of their films that "Liz" is a fan of
demo 8 = putStrLn "Function WIP"

-- |#########################|
-- |  		                 |
-- | Interface Functionality |
-- |					     |
-- |#########################|