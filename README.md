# QMUL-FunctionalProgrammingGroupProject
This application is downloading all recent movies from the TheMovieDB api and
stores it into a SQLite database. The user types in an actor and this application
is looking this actor up and print out all movies he's playing in. For a given
location it suggests cinemas in the areas that play this movie.

Only movies that are not already in the database will be downloaded and stored to
save time.

## Example Input

> Please enter the actore name you want to search for:   
> Ben Affleck    
> The given actor plays in the following movies    
> (1) Justice League    
> Please selecte a movie now:    
> 1    
> Please enter your location:    
> Stratford    
> The following cinemas in your area show this film:    
> Cinema: "Stratford Picturehouse, London" Distance: 0.25    
> Cinema: "VUE Leamington Spa, Leamington Spa" Distance: 9.67    
> Cinema: "VUE Redditch, Redditch" Distance: 12.81   
> Cinema: "Cineworld Solihull, Solihull" Distance: 15.66    
> Cinema: "Odeon Coventry, " Distance: 17.19    
> Cinema: "Cineworld Birmingham - NEC, Bickenhill" Distance: 18.42   
> Cinema: "Empire Birmingham Great Park, Birmingham" Distance: 19.18    

## Build an execute
To build this project just execute in the main folder:
`stack build`
To execute:
`stack exec groupproject-exe`

Note: If you run the programm for the first type, it may take a while. The programm
has to download all movies and store the into the database.

## Used API's
[1] https://www.themoviedb.org/documentation/api

[2] https://api.cinelist.co.uk/
