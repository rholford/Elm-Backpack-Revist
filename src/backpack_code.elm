module Main exposing (..)
import Html exposing (text)
import Array
--elm install tortus/elm-array-2d
import Array2D
--elm install elm-community/maybe-extra
import Maybe.Extra

--deprecated testing file for the algorithim
-- NOT USED in the final version of the project

--problems:
-- 0 indexing vs 1 indexing?

--These are the values that store the information for the problem
-- The reason they are initialized to these values was so we could test the code
-- They are reassigned later on when the user enters data
vals =    Array.fromList [2,4,5,8,4]
weights = Array.fromList [2,4,5,6,3]
maxWeight = 9
testTable =  emptyTableInit (maxWeight+1) (Array.length vals + 1)


-- this is fine
-- initialize an empty table. Given ints n and m it returns an empty nxm array
emptyTableInit : Int -> Int -> (Array2D.Array2D Int)
emptyTableInit col row = Array2D.fromList (List.repeat row (List.repeat col 0))

--broken
zeroOneBackpack : (Array2D.Array2D Int) -> (Array.Array Int) -> (Array.Array Int) -> Int -> Int -> Int -> (Array2D.Array2D Int)
zeroOneBackpack table valVector weightVector maxW w i = 
  --base case
  if (w == 0 || i == 0) then 
    if (w < maxW) then zeroOneBackpack (Array2D.set i w 0 table) valVector weightVector maxW (w+1) i
    else if (w >= maxW && i < ((Array.length valVector) )) then zeroOneBackpack (Array2D.set i w 0 table) valVector weightVector maxW 0 (i+1)
    else (Array2D.set i w 0 table)
  --iterative step
  else if ((Maybe.Extra.unwrap 0 (test) (Array.get (i - 1) weightVector)) > w) then 
    if (w < maxW) then zeroOneBackpack (Array2D.set i w (Maybe.Extra.unwrap 0 (test) (Array2D.get (i - 1) w table)) table) valVector weightVector maxW (w+1) i
    else if (w >= maxW && i <= ((Array.length valVector) )) then zeroOneBackpack (Array2D.set i w (Maybe.Extra.unwrap 0 (test) (Array2D.get (i - 1) w table)) table) valVector weightVector maxW 0 (i+1)
    else (Array2D.set i w (Maybe.Extra.unwrap 0 (test) (Array2D.get (i - 1) w table)) table)

  else
    if (w < maxW) then zeroOneBackpack (Array2D.set i w 
                                            (max (Maybe.Extra.unwrap 0 (test) (Array2D.get (i - 1) w table)) 
                                                ((Maybe.Extra.unwrap 0 (test) (Array.get (i - 1) valVector)) + (Maybe.Extra.unwrap 0 (test) (Array2D.get (i - 1) (w - (Maybe.Extra.unwrap 0 (test) (Array.get (i - 1) weightVector)) ) table))) ) table) 
                                            valVector weightVector maxW (w+1) i
    else if (w >= maxW && i <= ((Array.length valVector) )) then
                                          zeroOneBackpack (Array2D.set i w 
                                            (max (Maybe.Extra.unwrap 0 (test) (Array2D.get (i - 1) w table)) 
                                                ((Maybe.Extra.unwrap 0 (test) (Array.get (i - 1) valVector)) + (Maybe.Extra.unwrap 0 (test) (Array2D.get (i - 1) (w - (Maybe.Extra.unwrap 0 (test) (Array.get (i - 1) weightVector)) ) table))) ) table) 
                                            valVector weightVector maxW 0 (i+1)

    else (Array2D.set i w (max (Maybe.Extra.unwrap 0 (test) (Array2D.get (i - 1) w table)) 
                                     ((Maybe.Extra.unwrap 0 (test) (Array.get (i - 1) valVector)) + (Maybe.Extra.unwrap 0 (test) (Array2D.get (i - 1) (w - (Maybe.Extra.unwrap 0 (test) (Array.get (i - 1) weightVector)) ) table))) 
                                ) table)

test : Int -> Int
test num = if (num >= 0) then num else 0

--testing = zeroOneBackpack testTable vals weights maxWeight 0 0

--WORKING
printTest : Array2D.Array2D Int -> Int -> Int -> String
printTest inp row col = 
  if (col < ((Array2D.columns inp)- 1)) then (String.fromInt (Maybe.Extra.unwrap 0 (test) (Array2D.get row col inp))) ++ " " ++ printTest inp row (col+1)
  else if (col >= ((Array2D.columns inp) - 1) && (row < ((Array2D.rows inp) - 1) )) then (String.fromInt (Maybe.Extra.unwrap 0 (test) (Array2D.get row col inp))) ++ " -- " ++ printTest inp (row+1) 0
  else (String.fromInt (Maybe.Extra.unwrap -1 (test) (Array2D.get row col inp)))

test2 = Array2D.fromList [[2,4,5,8,4],[2,4,5,6,3]]
vi = Array.fromList [ 3,7,4,1,2 ]
wi = Array.fromList [ 5,2,4,2,3 ]
bigW = 5
--max([i-1, w-wi] + vi, [i-1, w]) something doesn't work
backpackV2 : (Array2D.Array2D Int) -> (Array.Array Int) -> (Array.Array Int) -> Int -> Int -> Int -> Int
backpackV2 table valVector weightVector maxW w i = 
  if (w == 0 || i == 0) then 0
  else if ((Maybe.Extra.unwrap 0 (test) (Array.get (i- 1) weightVector))> w) then (Maybe.Extra.unwrap 0 (test) (Array2D.get (i - 1) w table))
  else    (max (Maybe.Extra.unwrap 0 (test) (Array2D.get (i - 1) w table)) --works for 4 3 fails for 5 3 thinks is 0
           ((Maybe.Extra.unwrap 0 (test) (Array.get (i - 1) valVector)) + (Maybe.Extra.unwrap 0 (test) (Array2D.get (i - 1) (w - (Maybe.Extra.unwrap 0 (test) (Array.get (i - 1) weightVector)) ) table))) ) 


semiComplete =  Array2D.fromList [[0,0,0,0,0],[0,0,0,0,0]]
testTable2 =  emptyTableInit (bigW+1) (Array.length vi + 1)

semiCompleteTestTable2 = Array2D.fromList[[0,0,0,0,0,0],[0,0,0,0,3], [0,0,7,7,7,7], [0,0,7,7,7,0]]

testing  = zeroOneBackpack testTable vals weights maxWeight 0 0
testing2 = backpackV2 testTable2 vi wi bigW 5 2
testing5 = backpackV2 semiCompleteTestTable2 vi wi bigW 5 3
testing3 = zeroOneBackpack testTable2 vi wi bigW 0 0
testing4 = backpackV2  testTable vals weights maxWeight 6 2

main =
  --table 
  --text (String.fromInt (Maybe.Extra.unwrap 0 (test) (Array2D.get 3 3 testing)) )
  --text (String.fromInt testing2)
  --text (String.fromInt testing5)
  --text (String.fromInt testing4)
  --text (printTest test2 0 0)
  --text (printTest testing 0 0)
  text (printTest testing3 0 0) --this works?????
  --text (String.fromInt (Maybe.Extra.unwrap 0 (test) (Array2D.get 0 3 test2))) gets Array2D.fromList [[2,4,5,<8>,4],[2,4,5,6,3]]