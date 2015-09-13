import Color exposing (hsl)
import Graphics.Collage exposing (collage, square, filled, move, Form, text)
import Graphics.Element exposing (Element)
import Time
import Maybe
import Text

-- constants
size = 700
gridSize = 16

type alias State =
  { iteration: Int
  , size : Int
  , cells : List Float
  }
initialState = State 0 gridSize (List.map toFloat (List.repeat (gridSize*gridSize) 50))

posFromIndex : Int -> (Int,Int)
posFromIndex index =
  let
    x = index % gridSize
    y = index // gridSize
  in
    (x,y)

sum : List Float -> Float
sum list = (List.foldl (+) 0.0 list) / toFloat (List.length list)

-- update
update : Float -> State -> State
update timeDelta state =
  { state
  | iteration <- state.iteration+1
  , cells <- List.indexedMap (\index cell ->
      let
        (x,y) = posFromIndex index
        mid = toFloat (gridSize - 1) / 2
      in
        if  | (x == 0 || x == gridSize-1) && (y == 0 || y == gridSize-1) -> 0
            | (x == floor mid || x == ceiling mid) && (y == floor mid || y == ceiling mid) -> 100
            | otherwise -> updateCell index state.cells
    ) state.cells
  }

updateCell : Int -> List Float -> Float
updateCell pos cells =
  let
    (x,y) = posFromIndex pos
  in
    cells
      |> List.indexedMap (\i c -> { index = i, cell = c })
      |> List.filter (\{index, cell} ->
        let
          (x',y') = posFromIndex index
          distance = sqrt (toFloat ((x-x')^2 + (y-y')^2))
        in
          distance <= 1
      )
      |> List.map .cell
      |> sum


-- view
view : State -> Element
view state =
  List.indexedMap viewCell state.cells
    |> List.concat
    |> (::) (state.iteration |> toString |> Text.fromString |> text |> move (0,0))
    |> List.reverse
    |> collage size size

viewCell : Int -> Float -> List Form
viewCell index temp =
  let
    cellSize = size / gridSize
    (x',y') = posFromIndex(index)
    x = toFloat x' * cellSize - size / 2 + cellSize / 2
    y = toFloat y' * cellSize - size / 2 + cellSize / 2
  in
    [ toFloat (round (temp * 100)) / 100
        |> toString
        |> Text.fromString
        |> text
        |> move (x,y)
    , square cellSize
        |> filled (hsl (degrees (240 - temp/100*240)) 1 0.5)
        |> move (x,y)
    ]
  


-- signals

main : Signal Element
main = Signal.map view state

state : Signal State
state = Signal.foldp update initialState timeDelta

timeDelta : Signal Float
timeDelta = Signal.map Time.inSeconds (Time.fps 60)
