
-- ||
-- || Discrete Mathematics and Functional Programming
-- || Haskell Coursework: Film Review
-- || Student Number: 772629
-- ||

-- ||
-- || Variables
-- ||

import Data.List

-- Film Type Defines
type Title = String
type Director = String
type Year = Int
type Fanname = String

type Fans = [Fanname]
type Film = (Title, Director, Year, Fans)


	
-- ||
-- || Core Functionality
-- ||
		
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
fansAsString [] = ""
fansAsString (x:xs) = text ++ intercalate text x ++ "\n" ++ fansAsString xs
				where text = "\n | Fan Name: "

-- Returns all films released after the year (Not Including)
filmsReleasedAfterYear :: Int -> [Film] -> [Film]
filmsReleasedAfterYear year db = [ (ti, di, yr, fan) | (ti, di, yr, fan) <- db, yr > year ]

-- Returns all films that a user is a fan of
fanFilms :: String -> [Film] -> [Film]
fanFilms user db = [(ti, di, yr, fan) | (ti, di, yr, fan) <- db, elem user fan ]

-- Returns all fans of a particular film
fansOfFilm :: String -> [Film] -> [Fans]
fansOfFilm film db = [ fan | (ti, di, yr, fan) <- db, film == ti ]

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
	
-- Gets the count of number of films by a director that a specific fan is fan of
filmsByDirectorWithFan :: String -> String -> [Film] -> Int
filmsByDirectorWithFan fan direc ((ti, di, yr, fa):xs)
	| length xs == 0 = 0
	| direc == di && checkIfFan fan [(ti, di, yr, fa)] = 1 + filmsByDirectorWithFan fan direc xs
	| otherwise	= filmsByDirectorWithFan fan direc xs	
	
-- Gets all the directors with number of films that have a particular fan by name
filmsByAllDirectorsWithFan :: String -> [Film] -> String
filmsByAllDirectorsWithFan _ [] = ""
filmsByAllDirectorsWithFan fan ((ti, di, yr, fa):xs)
	| length xs > 0 = text ++ di ++ " - " ++ show(filmsByDirectorWithFan fan di ((ti, di, yr, fa):xs)) ++ filmsByAllDirectorsWithFan fan xs
	| otherwise = filmsByAllDirectorsWithFan fan xs
		where text = "\n | Director: "

		

-- ||
-- || Demo Functionality
-- ||

demo :: Int -> IO ()
-- All films after adding "Alien: Covenant" by "Ridley Scott" 2017
demo 1 = putStrLn( filmsAsString( addNewFilm "Alien: Covenant" "Ridley Scott" 2017 testDatabase ) )
-- Returning filmsAsString testDatabase
demo 2 = putStrLn( filmsAsString testDatabase )
-- All films that were released after 2008
demo 3 = putStrLn( filmsAsString( filmsReleasedAfterYear 2008 testDatabase ) )
-- All films that "Liz" is a fan of
demo 4 = putStrLn( filmsAsString( fanFilms "Liz" testDatabase ) )
-- All fans of the movie "Jaws"
demo 5 = putStrLn( fansAsString ( fansOfFilm "Jaws" testDatabase ) )
-- All films after "Liz" says she becomes fan of "The Fly"
demo 6 = putStrLn( filmsAsString ( addFan "The Fly" "Liz" testDatabase ) )
-- All films after "Liz" says she becomes fan of "Avatar"
demo 66 = putStrLn( filmsAsString ( addFan "Avatar" "Liz" testDatabase ) )
-- All fans of films directed by "James Cameron"
demo 7 = putStrLn( fansAsString ( fansOfDirector "James Cameron" testDatabase ) )
-- All directors & no. of their films that "Liz" is a fan of
demo 8 = putStrLn( filmsByAllDirectorsWithFan "Liz" testDatabase )



-- ||
-- || Interface Functionality
-- ||

main :: IO ()
main = do 
	putStrLn ("")
	putStrLn (" Film Database")
	putStrLn (" Loading database file")
	putStrLn ("")
	loadedFile <- readFile "test-data.txt"
	let filmsDatabase = read loadedFile
	putStrLn ("Please input your name: ")
	name <- getLine
	putStrLn ("")
	inMainMenu name filmsDatabase		
		
inMainMenu :: String -> [Film] -> IO()
inMainMenu user filmDB = do 
	putStrLn("Welcome to the Film Database, please enter the number for the action you wish to perform.\n")
	putStrLn(" 1 | Add a film")
	putStrLn(" 2 | Display all films")
	putStrLn(" 3 | Display films released after a certain date")
	putStrLn(" 4 | Get all films you're a fan of")
	putStrLn(" 5 | Get all the fans of a particular film")
	putStrLn(" 6 | Set yourself as a fan of a film")
	putStrLn(" 7 | Check all fans of a particular director")
	putStrLn(" 8 | Get all directors that you're a fan of with count")
	putStrLn(" 0 | Save and Exit\n")
	putStr("Please insert your option: ")
	option <- getLine
	inAction option user filmDB
		
inAction :: String -> String -> [Film] -> IO ()
inAction "0" user filmDB = inSaveAndExit user filmDB
inAction "1" user filmDB = inAddNewFilm user filmDB
inAction "2" user filmDB = inGetAllFilms user filmDB
inAction _ user filmDB = inErrorMessage user filmDB "Incorrect Option Selected"

inGetAllFilms :: String -> [Film] -> IO()
inGetAllFilms user filmDB =
	do
		putStrLn( filmsAsString filmDB )
		inMainMenu user filmDB
			
inErrorMessage :: String -> [Film] -> String -> IO()
inErrorMessage user filmDB error =
	do
		putStrLn(error)
		inMainMenu user filmDB

inSaveAndExit :: String -> [Film] -> IO()
inSaveAndExit user filmDB =
	do
		putStrLn(" Saving the Database")
		length filmDB `seq` writeFile "test-data.txt" ( show filmDB )
		putStrLn(" Saved the Database successfully")
		putStrLn("Exiting the system")
		
inAddNewFilm :: String -> [Film] -> IO()
inAddNewFilm user filmDB = 
	do 
		putStrLn(" Add new film to database\n\n")
		putStrLn(" Enter the film title: ")
		filmTitl <- getLine
		putStrLn(" Enter the film director: ")
		filmDire <- getLine
		putStrLn(" Enter the film release date: ")
		filmYear <- getLine
		putStrLn(" Adding your film to the database")
		let filmDB = addNewFilm filmTitl filmDire (read filmYear :: Int) filmDB
		inMainMenu user filmDB



-- ||
-- || Database used to demonstrate the functionality of the system
-- ||

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