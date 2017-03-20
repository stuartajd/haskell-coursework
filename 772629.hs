
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
addNewFilm ti di ye db = db++[(ti, di, ye, [])]

-- Returns all films as a formatted string
filmsAsString :: [Film] -> String
filmsAsString [] = ""
filmsAsString ((ti, di, yr, fa):xs) = "\nTitle: " ++ ti ++ "\nDirector: " ++ di  ++ "\nYear: " ++ show ( yr ) ++ "\nFan Total: " ++ show( length ( fa ) ) ++ "\n" ++ filmsAsString xs

-- Converts all fans to a formatted string (different data type [Fans])
fansAsString :: [Fans] -> String
fansAsString [] = ""
fansAsString (x:xs) = "\nFan Name: " ++ intercalate "\nFan Name: " x ++ fansAsString xs

-- Converts all fans to a formatted string (different data type [Fanname])
fanNameAsString :: [Fanname] -> String
fanNameAsString [] = ""
fanNameAsString (x:xs) = "\nFan Name: " ++ intercalate "\n" [x] ++ fanNameAsString xs

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
addFan title name db = [ if ti == title && elem name fa == False then (ti, di, yr, fa++[name]) else (ti, di, yr, fa) | (ti, di, yr, fa) <- db ]

-- Fans of all films directed by a particular directors
fansOfDirector :: String -> [Film] -> [Fans]
fansOfDirector director ((ti, di, yr, fa):xs)
 | length xs == 0 = []
 | director == di = fa : fansOfDirector director xs
 | otherwise = fansOfDirector director xs

-- Returns a list of fans without their duplicates
fansOfDirectorNoRepeats:: String -> [Film] -> [Fanname]
fansOfDirectorNoRepeats director db = nub(concat (fansOfDirector director db))

-- Checks if user is a fan of a specific film
checkIfFan :: String -> [Film] -> Bool
checkIfFan user [(ti, di, yr, fa)] = elem user fa

-- Gets the count of number of films by a director that a specific fan is fan of
getCountFilmsByDirWithFan :: String -> Director -> [Film] -> Int
getCountFilmsByDirWithFan fan director ((ti, di, yr, fa):xs)
 | director == di && checkIfFan fan [(ti, di, yr, fa)] = 1 + getCountFilmsByDirWithFan fan di xs
 | length xs > 0 = getCountFilmsByDirWithFan fan director xs
 | otherwise = 0

-- Returns a list of directors without their duplicates
getAllDirectorsWithoutRepeats :: [Film] -> [Director]
getAllDirectorsWithoutRepeats db = nub(getAllDirectors db)

-- Returns a list of all the directors within a database 
getAllDirectors :: [Film] -> [Director]
getAllDirectors ((ti, di, yr, fa):xs)
 | length xs > 0 = [di] ++ getAllDirectors xs
 | otherwise = []

-- Returns a list of tuples with director & count of times a user is a fan 
directorsWithFanCounter :: String -> [Film] -> [(Director, Int)]
directorsWithFanCounter fan db = [ (dir, (getCountFilmsByDirWithFan fan dir db)) | dir <- getAllDirectorsWithoutRepeats(db) ]

-- Returns a formatted list of the tuples of director and count of times a user is a fan
directorsWithFanToString :: [(Director, Int)] -> String
directorsWithFanToString ((dir, cou):xs)
 | length xs > 0 = "Director: " ++ dir ++ " - Count: "++ show(cou) ++ "\n" ++ directorsWithFanToString xs
 | length xs == 0 = "Director: " ++ dir ++ " - Count: "++ show(cou)
 | otherwise = ""
 
-- Check if film exists
checkFilmExists :: String -> [Film] -> Bool
checkFilmExists film ((ti, di, yr, fa):xs)
 | ti == film = True
 | xs == [] = False
 | otherwise = checkFilmExists film xs

-- ||
-- || Interface Functionality
-- ||

-- Main Functionality
main :: IO ()
main = do 
 putStrLn("========================================================")
 putStrLn("Film Database")
 loadedFile <- readFile "test-data.txt"
 let filmsDatabase = read loadedFile
 putStrLn("Successfully Loaded Database")
 putStrLn("========================================================\n")
 putStr("Please input your name: ")
 name <- getLine
 putStrLn ("\n")
 inMainMenu name filmsDatabase

-- Returns an error message for incorrect input
inSendErrorInput :: String -> String
inSendErrorInput "int" = "[Error]: The value entered was not the expected value (Number / Int)\n[Error]: Option is being reset, please try again."
inSendErrorInput "string" = "[Error]: The value entered was not the expected value (Word / String)\n[Error]: Option is being reset, please try again."
inSendErrorInput _ = ""

-- Main Menu Options
inMainMenu :: String -> [Film] -> IO()
inMainMenu user filmDB = do 
 putStrLn("========================================================")
 putStrLn("Welcome to the Film Database, " ++ user ++".\nEnter the number for the required option.")
 putStrLn(" 1 | Add a film")
 putStrLn(" 2 | Display all films")
 putStrLn(" 3 | Display films released after a certain date")
 putStrLn(" 4 | Get all films with you're a fan of")
 putStrLn(" 5 | Get all the fans of a particular film")
 putStrLn(" 6 | Assign yourself as a fan to a particular film")
 putStrLn(" 7 | Check all fans of a particular director")
 putStrLn(" 8 | Get all directors that you're a fan of with count")
 putStrLn(" 0 | Save and Exit")
 putStrLn("========================================================\n")
 putStr("Please insert your option: ")
 option <- getLine
 putStrLn("\n")
 inAction option user filmDB
  
-- Actions for the system
inAction :: String -> String -> [Film] -> IO ()
-- Save the database and exit the UI
inAction "0" user filmDB = inSaveAndExit user filmDB
-- Add a new film to the database
inAction "1" user filmDB = inAddNewFilm user filmDB
-- Display all the films within the database
inAction "2" user filmDB = inGetAllFilms user filmDB
-- Get all the films that were released after a certain date
inAction "3" user filmDB = inGetAllFilmsAfter user filmDB
-- Get all the films with a particular fan
inAction "4" user filmDB = inGetFanFilms user filmDB
-- Get all the fans of a particular film
inAction "5" user filmDB = inGetFansOfFilm user filmDB
-- Assign a fan to a particular film
inAction "6" user filmDB = inAddFan user filmDB
-- All fans of films directed by a particular director
inAction "7" user filmDB = inFansOfDirector user filmDB
-- All directors & no. of their films that a particular fan is is a fan of
inAction "8" user filmDB = inFilmsByAllDirectors user filmDB
-- Display errors if the input is invalid!
inAction _ user filmDB = inErrorMessage user filmDB "Incorrect Option Selected"

-- UI Function for returning the fans of a director
inFansOfDirector :: String -> [Film] -> IO()
inFansOfDirector user filmDB =
 do
  putStrLn("========================================================")
  putStrLn("View all the fans of a particular director.")
  putStr("Director Name: ")
  director <- getLine
  putStrLn("\nAll fans of director "++ director ++"\n")
  putStrLn( fanNameAsString ( fansOfDirectorNoRepeats director filmDB ) )
  putStrLn("========================================================\n")
  inMainMenu user filmDB
   
-- UI Function for returning the directors with the total fans that like their films
inFilmsByAllDirectors :: String -> [Film] -> IO()
inFilmsByAllDirectors user filmDB =
 do
  putStrLn("========================================================")
  putStrLn("View all directors with number of a fan is liking their films.")
  putStrLn("\nCount of times for "++ user  ++"\n")
  putStrLn(  directorsWithFanToString( directorsWithFanCounter user filmDB )) 
  putStrLn("========================================================\n")
  inMainMenu user filmDB

-- UI Function for add a user as a fan to a specific film
inAddFan :: String -> [Film] -> IO()
inAddFan user filmDB =
 do
  putStrLn("========================================================")
  putStrLn("Add a particular user as a fan for a film.")
  putStr("Film Name: ")
  film <- getLine
  putStrLn("\nAdded "++ user ++" to the fan list for "++ film ++"\n")
  let filmDBnew = addFan film user filmDB
  putStrLn( filmsAsString filmDBnew )
  putStrLn("========================================================\n")
  inMainMenu user filmDBnew

-- UI Function for returning all the fans of a specific film
inGetFansOfFilm :: String -> [Film] -> IO()
inGetFansOfFilm user filmDB =
 do
  putStrLn("========================================================")
  putStrLn("View all the fans of a particular film.")
  putStr("Film Name: ")
  film <- getLine
  putStrLn("\nAll fans of a particular film "++ film ++"\n")
  putStrLn( fansAsString ( fansOfFilm film filmDB ) )
  putStrLn("========================================================\n")
  inMainMenu user filmDB

-- UI Function to return all the fans of a specific film
inGetFanFilms :: String -> [Film] -> IO()
inGetFanFilms user filmDB =
 do
  putStrLn("========================================================")
  putStrLn("View all the films with a particular fan.")
  putStrLn("\nAll films with "++ user ++" as a fan\n")
  putStrLn( filmsAsString( fanFilms user filmDB ) )
  putStrLn("========================================================\n")
  inMainMenu user filmDB

-- UI Function to return all the films as a string
inGetAllFilms :: String -> [Film] -> IO()
inGetAllFilms user filmDB =
 do
  putStrLn("========================================================")
  putStrLn("View all the films within the database")
  putStrLn( filmsAsString filmDB )
  putStrLn("========================================================\n")
  inMainMenu user filmDB
  
-- UI Functions to get all the films released after a certain date
inGetAllFilmsAfter :: String -> [Film] -> IO()
inGetAllFilmsAfter user filmDB =
 do
  putStrLn("========================================================")
  putStrLn("View all films released after a certain date.")
  putStr("Which year: ")
  year <- getLine
  putStrLn("All films released after "++ year)
  putStrLn(filmsAsString( filmsReleasedAfterYear (read year :: Int) filmDB ))
  putStrLn("========================================================\n")
  inMainMenu user filmDB
   
-- UI functions to return a custom error message
inErrorMessage :: String -> [Film] -> String -> IO()
inErrorMessage user filmDB error =
 do
  putStrLn(error)
  inMainMenu user filmDB

-- UI Function to save and exit the UI
inSaveAndExit :: String -> [Film] -> IO()
inSaveAndExit user filmDB =
 do
  putStrLn("========================================================")
  putStrLn("Saving the Database")
  seq ( length filmDB ) writeFile "test-data.txt" ( show filmDB )
  putStrLn("Saved the Database successfully")
  putStrLn("Exiting the system")
  putStrLn("========================================================\n")
  
-- UI function to add a new film to the database
inAddNewFilm :: String -> [Film] -> IO()
inAddNewFilm user filmDB = 
 do 
  putStrLn("========================================================")
  putStrLn("Add new film to database\n")
  putStr("Enter the film title: ")
  filmTitl <- getLine
  putStr("Enter the film director: ")
  filmDire <- getLine
  putStr("Enter the film release date: ")
  filmYear <- getLine
  putStrLn("Adding your film to the database")
  let filmDBnew = addNewFilm filmTitl filmDire (read filmYear :: Int) filmDB
  putStrLn("========================================================\n")
  inMainMenu user filmDBnew

-- ||
-- || Demo Functionality
-- ||

demo :: Int -> IO ()
-- All films after adding "Alien: Covenant" by "Ridley Scott" 2017
demo 1 = putStrLn( filmsAsString( addNewFilm "Alien: Covenant" "Ridley Scott" 2017 testDatabase ) )
-- Returning all films in a formatted way
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
-- All fans of films directed by "James Cameron" (Without Duplicates)
demo 7 = putStrLn( fanNameAsString ( fansOfDirectorNoRepeats "James Cameron" testDatabase ))
-- All directors & no. of their films that "Liz" is a fan of (Without Duplicates)
demo 8 = putStrLn( directorsWithFanToString( directorsWithFanCounter "Liz" testDatabase  ))

demo _ = putStrLn "Invalid Demo Requested"


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