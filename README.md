# Tabletennis tournament organizer


This project is a tournament organizer for table tennis tournaments.
The project is written in Elm. It includes various modes in which a tournament can be played.
There are currently two different modes ("Einfaches KO" and "Alle Gegen Alle").

## Modi

### Enter player

Before the tournament mode is selected, the participants are entered individually with names and their playing strength.



### Einfaches KO

At "Einfaches K.O." People will be distributed in a tournament tree according to their playing strength, so that the strongest players can only play against each other at the end of the tournament.



### Alle Gegen Alle

In "Alle Gegen Alle" mode, each player competes against all others. Based on the results, a table is calculated which shows how many games a player has already made, how many victories and defeats he has and how the sentence ratio is.



### Enter results

After each game the result is entered. The winner and the sentence ratio are determined using the registered sentences.



## Starting the project
The project can be started in the terminal via the command `elm reactor` on a local host.
In the folder `src/` the data Main.elm has to be opened, then the project starts and the organizer can be started with a click on the button.




