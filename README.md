# snake-in-racket
The classic game Snake, programmed in Racket (Intermediate Student Language).

## To run the program
1. Install the newest version of DrRacket here: https://download.racket-lang.org.
2. Open the snake.rkt file.
3. Go to Language -> Choose Language (or command/control L) and select Teaching languages (or press control/command T).
4. Set the language to be one of Intermediate Student/Intermediate Student with Lambda/Advanced Student.
5. Run the file to start the game!

## Game Settings
1. The cell size (in pixels) of the game can be changed by modifying the CELL-SIZE constant. A cell size of 10 would correspond to snake/food squares of 10 pixels, for example.
2. The width/height of the game screen can be changed by modifiying the WIDTH/HEIGHT constants. Note that in order for the game to work, both width and height must be a multiple of 2 x CELL-SIZE.
3. The speed of the game can be changed by modifying the TICK-SPEED constant. The game will move forward once every TICK-SPEED seconds.
