module Main where 
import Root
import Eqparser
import Sim
import Diff
import Eval
import Infix

main = do 
       putStrLn "\n\nChoose your choice ?"
       putStrLn "1. Differentiate \n2. Solve or Evaluate \n3. Exit\n0. Demo\n"
       inputStr <- getLine 
       doAction inputStr


doAction "3" = putStrLn "Exiting Bye !!!"

doAction "2" = do
               putStrLn "Format Solve (Equation)"
               putStrLn "Format Evaluate (Equation) At x=4,y=2"
               inputString <- getLine
               let outputString = resultOutput $ parseContainer inputString
               putStrLn outputString
               main

doAction "1" = do
               putStrLn "Format diff(Equation,x)"
               inputString <- getLine
               let outputEquation = eval $ getRight $ parseEquation inputString
               putStrLn $ show  outputEquation  
               main

doAction "0" = demo
doAction _  = main 

getRight (Right val) = val
getRight (Left _) = F "Error" [] 

demo = do
       let string1 = "Solve (cos(x))^2-x^2" 
       putStrLn string1
       let outputString1 = resultOutput $ parseContainer string1
       putStrLn outputString1
       dummy <- getLine
       let string2 = "Solve (sin(x)-x*cos(x))" 
       putStrLn string2
       let outputString2 = resultOutput $ parseContainer string2
       putStrLn outputString2
       dummy <- getLine
       let string3 = "Evaluate (2^ln(x)) At x=2" 
       putStrLn string3
       let outputString3 = resultOutput $ parseContainer string3
       putStrLn outputString3
       dummy <- getLine
       let equation1 = "diff(sin(x),x)" 
       putStrLn $ show  equation1
       let outputEquation = eval $ getRight $ parseEquation equation1
       putStrLn $ show  outputEquation
       main
       
       
       
