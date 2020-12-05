module Main exposing (..)

import Browser
import Html exposing (Html, button, div, pre, text, table, thead,th,tr,td)
import Html.Events exposing (onClick)
import Tachyons exposing (classes, tachyons)
import Tachyons.Classes exposing (..)

--backpack stuff
import Array
--elm install tortus/elm-array-2d
import Array2D
--elm install elm-community/maybe-extra
import Maybe.Extra

-- Defines all types of msgs that can be sent to the update function
type Msg = Increment | Decrement | Increment2 | Decrement2 | AddItem | MaxUp | MaxDown | Reset | Submit | UpdateValue | MagicButton

-- Converts a list if items to a comma seperated list
listToString : (List Int) -> String
listToString l = 
    case l of
        [] -> ""
        (x::[]) -> (String.fromInt x)
        (x::xs) -> (String.fromInt x ) ++ "," ++ (listToString xs)


-- creates the table showing the final solution. Initially filled with zeros
craeteSolveTable : (List (List Int)) -> Int -> Int -> (List (List Int))
craeteSolveTable l n m =
    case n of
        0 -> l 
        otherwise -> craeteSolveTable ((List.repeat m 0)::l) (n-1) m
        --zeroOneBackpack testTable2 vi wi bigW 0 0

--converts a list to an Array2D object
solveTableToArray : (List (List Int)) -> (Array2D.Array2D Int)
solveTableToArray inputList = Array2D.fromList  inputList

-- hardcoded, uses on magic button
-- gets wrong results with 5,5, correct with 6,6 for items, max weight
--^^^^^^^
vi = Array.fromList [ 3,7,4,1,2 ]
wi = Array.fromList [ 5,2,4,2,3 ]
bigW = 5

test : Int -> Int
test num = if (num >= 0) then num else 0

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

printTest : Array2D.Array2D Int -> Int -> Int -> String
printTest inp row col = 
  if (col < ((Array2D.columns inp)- 1)) then (String.fromInt (Maybe.Extra.unwrap 0 (test) (Array2D.get row col inp))) ++ " " ++ printTest inp row (col+1)
  else if (col >= ((Array2D.columns inp) - 1) && (row < ((Array2D.rows inp) - 1) )) then (String.fromInt (Maybe.Extra.unwrap 0 (test) (Array2D.get row col inp))) ++ " \n" ++ printTest inp (row+1) 0
  else (String.fromInt (Maybe.Extra.unwrap -1 (test) (Array2D.get row col inp)))

-- converts the solution table to something that can be rendered in html
renderSolTable : (List(List Int)) -> Html Msg
renderSolTable l = 
    case l of
        [] -> text ""
        (x::xs) -> div [][text (listToString x)
                        ,(renderSolTable xs)]

--gets a value from a 2D list
getValueFromIndex : (List(List Int)) -> Int -> Int -> Int
getValueFromIndex l i j=
    case ((i == 1),l) of 
        (_,[]) -> 0
        (True,(x::xs)) -> getValueHelper1 x j 
        (False,(x::xs)) -> getValueFromIndex xs (i-1) j

getValueHelper1 : (List Int) -> Int -> Int
getValueHelper1 l j =
    case ((j==1),l) of
        (_,[]) -> 0
        (True,(x::xs)) -> x
        (False,(x::xs)) -> getValueHelper1 xs (j-1)

--sets the value of an index in s 2D list
setValueFromIndex : (List(List Int)) -> Int -> Int -> Int -> (List(List Int))
setValueFromIndex l i j s=
    case ((i == 1),l) of 
        (_,[]) -> []
        (True,(x::xs)) -> (setValueHelper1 x j s)::xs
        (False,(x::xs)) -> x::(setValueFromIndex xs (i-1) j s)

setValueHelper1 : (List Int) -> Int -> Int -> (List Int)
setValueHelper1 l j s=
    case ((j==1),l) of
        (_,[]) -> []
        (True,(x::xs)) -> s::xs
        (False,(x::xs)) -> x::(setValueHelper1 xs (j-1)s)

--This is the function that places items on the screen. If a is true then it renders the input screen, and if it is false,
-- it renders the solve screen
state : Int -> Model -> Html Msg
state a model=
    case (a==1) of
        True -> (div [ classes [ f1, pointer, b, pl3 ] ]
            [ -- `tachyons.css` this should only be used for demo pourposes, it's better included as a cdn in HTML: 
            -- <link rel="stylesheet" href="https://unpkg.com/tachyons@4.10.0/css/tachyons.min.css"/>
            tachyons.css 
            ,div[classes [flex, justify_between]][
                text "New Item"
                ,button [onClick Reset] [text "Reset"]

            ]
            , div [classes [flex, pb2,pt2,pl5, f2]]
                [div [classes [pr3, flex_column]] [ text ("Value: "++(String.fromInt model.one)) ]
                ,div [classes [flex, flex_column, f4]]
                    [button [ onClick Increment ] [ text "+" ]
                    ,button [ onClick Decrement ] [ text "-" ]
                    ]
                ]
            , div [classes [flex,f2, pl5]]
                [div [classes [pr3]] [ text ("Weight: " ++ (String.fromInt model.two)) ]
                ,div [classes [flex, flex_column, f4]]
                    [button [ onClick Increment2 ] [ text "+" ]
                    ,button [ onClick Decrement2 ] [ text "-" ]
                    ]
                ]
            , div []
                [button [onClick AddItem][text "Add Item"]
                ,text " "
                ,button [][text "Remove Item"]
                ,text " "
                ,button [onClick Submit][text "Submit Question"]]
            , div [classes [flex]] [
                div [classes [pr3]] model.table
                ,div [classes [flex]] [
                    text ("Max Weight: " ++ (String.fromInt model.maxWeight))
                    ,div [classes [flex, flex_column, f4]]
                    [button [ onClick MaxUp ] [ text "+" ]
                    ,button [ onClick MaxDown ] [ text "-" ]
                    ]  
                ]
            ]
            
                
            ])
        False -> (div [ classes [ f1, pointer, b, pl3 ] ]
            [ -- `tachyons.css` this should only be used for demo pourposes, it's better included as a cdn in HTML: 
            -- <link rel="stylesheet" href="https://unpkg.com/tachyons@4.10.0/css/tachyons.min.css"/>
            tachyons.css 
            ,div[classes [flex, justify_between]][
                text "Problem Solution"
                ,button [onClick Reset] [text "Reset"]

            ]
            ,div [classes [flex, flex_column]][
                div [][text ("Values: " ++ (listToString model.values) )]
                ,div [][text ("Weights: " ++ (listToString model.weights) )]
                ,div [][text ("Max Weight: " ++ (String.fromInt model.maxWeight))]
            ]
            ,div [][
                --renderSolTable model.solveTable
                Html.pre []  [text (printTest model.solveTable 0 0)]
            ]
            ,div [][
                button [] [text "Previous Step"]
               ,button [onClick UpdateValue] [text "Next Step"]  
            ]
            ,div [][
              button [onClick MagicButton] [text "Solve"]               
            ]
            ])
        
--adding fuction
adder: Int -> Int
adder a = a+1

--subtraction function
sub: Int -> Int
sub a = 
    case (a<2) of
        True ->
            a
        False ->
            a-1

-- update function. depending on the msg it gets, it performs a function on items in the model
update msg model=
  case msg of
    -- adds to the 'one' value
    Increment ->
      {model | one = adder model.one}
    --subtracts from the 'one' value
    Decrement ->
      {model | one = sub model.one}

    -- adds to the 'two' value
    Increment2 ->
      {model | two = adder model.two}

    --subtracts from the 'two' value
    Decrement2 ->
      {model | two = sub model.two}

    -- Adds a value and a weight to their respective values
    AddItem ->
        {model | weights = model.two :: model.weights
        , values = model.one :: model.values
        , table = List.reverse((Html.tr []
                        [ td [classes [pl2, pr5]] [text ("Item" ++ (String.fromInt (List.length model.table)))]
                        , td [classes [pr6]] [text (String.fromInt model.one)]
                        , td [classes [pl4]] [text (String.fromInt model.two)]
                        ]) :: List.reverse model.table)
        , one = 1
        , two = 1
        }
    --adds to the max weight value
    MaxUp ->
        {model | maxWeight = adder model.maxWeight}
    -- subtracts from the max weight value
    MaxDown ->
        {model | maxWeight = sub model.maxWeight}
    --resets all model values to their original values
    Reset ->
        {model | one = 1
        , two = 1
        , weights = []
        , values = []
        , table = [table []
                
                    [thead []
                        [ th [classes [pl2, pr5]] [text "Item"]
                        , th [classes [pr5]] [text "Value"]
                        , th [] [text "Weight"]
                        ]
                    ]  
                ]
        , maxWeight = 1
        , currentState = 1
        }
    --updates the model to show the solution screen
    Submit ->
        {model | weights = List.reverse model.weights
        , values = List.reverse model.values
        , currentState = 2
        , solveTable = solveTableToArray (craeteSolveTable [] (List.length model.values + 1) (model.maxWeight + 1)  ) }
    UpdateValue ->
        --{model | solveTable = Array2D.set (model.i) (model.j) (Maybe.Extra.unwrap 0 (test) (Array2D.get (model.i) (model.w) table))  model.solveTable }
        {model | solveTable = Array2D.set 2 2 7 model.solveTable } --(Maybe.Extra.unwrap 0 (test) (Array2D.get (i - 1) w table))
    MagicButton ->
        --todo: make new state var, solved table
        {model | solveTable =  zeroOneBackpack model.solveTable (Array.fromList model.values) (Array.fromList model.weights) model.maxWeight 0 0
        ,i = List.length (model.values) + 1 --symbolic update to last position?
        ,j = List.length (model.weights) + 1 }  --zeroOneBackpack model.solveTable vi wi bigW 0 0       

        
--sets the model of the view
view model= state model.currentState model

-- sets all value types in the Model
type alias Model =
    { one: Int
    , two: Int
    , weights: List Int
    , values: List Int
    , table: List (Html Msg)
    , maxWeight: Int
    , currentState: Int
    , solveTable: Array2D.Array2D Int--solveTable: List(List(Int))
    , i: Int
    , j: Int
    }

--sets the initial model values
initModel : Model
initModel = 
    { one = 1
    , two = 1
    , weights = []
    , values = []
    , table = [table []
                
                    [thead []
                        [ th [classes [pl2, pr5]] [text "Item"]
                        , th [classes [pr5]] [text "Value"]
                        , th [] [text "Weight"]
                        ]
                    ]
                    
                
            ]
    , maxWeight = 1
    , currentState = 1
    , solveTable = Array2D.fromList []
    , i = 0
    , j = 0
    }
--starts the view of the webpage
main = 
    Browser.sandbox {init = initModel, update = update, view = view}
    



