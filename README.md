# Featherweight Java Type Checker

The main program can be found on ./FeatherweightJavaTypeChecker. Two files are located on that directory, the type checker and a parser.
In order to run the program, happy and the haskell platform are needed.

The first step is to compile the parser and generate the parsing and type checking program.  
In order to do so, the following command is needed: happy parser.y && ghc parser.hs -o [OUTPUT_NAME]

The next step is to create a Featherweight Java Code. After this, just run ./[OUTPUT_NAME] < [FEATHERWEIGHT_FILE_NAME] 2>/dev/null
and the type checking will be performed.

Note: Adding the redirection of the standard error is useful since a message is shown when the end of file is reached. If there are any parsing errors the program won't
generate any output. If no parsing errors are found then the program will print either True or False (a boolean representing if the program is well typed or not).
