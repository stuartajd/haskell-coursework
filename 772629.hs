-- Functional Programming Coursework --
-- Haskell Task: Film Review Website --
-- Created by: 772629 				 --

-- |#########################|
-- |  		                 |
-- |		  Vars			 |
-- |					     |
-- |#########################|

type Title = String
type Director = String
type Year = Int
type Fanname = String

type Film = (Title, Director, Year, [Fanname])

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
		
-- Returns all the films available in the database --
-- @return Database of all the films --
getAllFilms :: [Film]
getAllFilms = testDatabase

-- Adds a new film to the database --
-- @param: Title, Director, Year & Database --
-- @return: Database --
addNewFilm :: String -> String -> Int -> [Film] -> [Film]
addNewFilm ti di ye db = (ti, di, ye, []):db


-- |#########################|
-- |  		                 |
-- |	Demo Functionality	 |
-- |					     |
-- |#########################|

demo :: Int -> IO ()
-- All films after adding "Alien: Covenant" by "Ridley Scott" 2017
demo 1 = putStrLn "Function WIP"
-- Returning filmsAsString testDatabase
demo 2 = putStrLn "Function WIP"
-- All films that were released after 2008
demo 3 = putStrLn "Function WIP"
-- All films that "Liz" is a fan of
demo 4 = putStrLn "Function WIP"
-- All fans of the movie "Jaws"
demo 5 = putStrLn "Function WIP"
-- All films after "Liz" says she becomes fan of "The Fly"
demo 6 = putStrLn "Function WIP"
-- All films after "Liz" says she becomes fan of "Avatar"
demo 66 = putStrLn "Function WIP"
-- All fans of films directed by "James Cameron"
demo 7 = putStrLn "Function WIP"
-- All directors & no. of their films that "Liz" is a fan of
demo 8 = putStrLn "Function WIP"

-- |#########################|
-- |  		                 |
-- | Interface Functionality |
-- |					     |
-- |#########################|