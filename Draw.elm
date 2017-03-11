import Bitwise
import Html
import Html.Attributes
import List exposing (map, head, append, take, tail)
import Mouse exposing (moves, clicks)
import Random
import Svg exposing (polyline, svg, line)
import Svg.Attributes
import Svg.Events
import Task
import WebSocket
import Window

server = "wss://owen.cafe:8000"
lineWeight = 5
sketchWeight = lineWeight // 2
lineWeightStr = toString lineWeight
sketchWeightStr = toString sketchWeight
zeroDims = {width = 0, height = 0}
zeroPoint = {x = 0, y = 0}
defaultColor = "#ccc"
zeroPointCol = {x = zeroPoint.x, y = zeroPoint.y, col = defaultColor}

toHexDig: Int -> String
toHexDig num = case num % 16 of
        0 -> "0"
        1 -> "1"
        2 -> "2"
        3 -> "3"
        4 -> "4"
        5 -> "5"
        6 -> "6"
        7 -> "7"
        8 -> "8"
        9 -> "9"
        10 -> "a"
        11 -> "b"
        12 -> "c"
        13 -> "d"
        14 -> "e"
        15 -> "f"
        _  -> "x"

dimToPoint: Dimension -> Point
dimToPoint {width, height} = {x = width, y = height}

posToHex: Int -> String
posToHex n = case n of
    0 -> ""
    a -> posToHex (Bitwise.shiftRightBy 4 a) ++ toHexDig a

toHex: Int -> String
toHex n = if n < 0 then String.cons '-' (posToHex (negate n))
    else if n == 0 then "0" else posToHex n

chanFromInt: Int -> Int -> String
chanFromInt i c = toHex ((Bitwise.shiftRightBy (c * 8) i) % 256)

chansFromInt: Int -> String
chansFromInt i = String.concat (map (chanFromInt i) [0, 1, 2])

colorFromInt: Int -> String
colorFromInt i = chansFromInt i

genColor: Random.Generator String
genColor = Random.map colorFromInt (Random.int 0 16777216)

evenChars: String -> String
evenChars str = Tuple.second (String.foldl (\c (n, s) -> case n of
        True  -> (False, String.cons c s)
        False -> (True , s)
    ) (True, "") str)

genShortColor: Random.Generator String
genShortColor = Random.map evenChars genColor

defColAndStr: Point -> PointStrCol
defColAndStr p = makeDefColPointStr (toPointStr p)

colPointToStr: PointCol -> PointStrCol
colPointToStr {x, y, col} = {x = toString x, y = toString y, col = col}

makeDefColPoint: Point -> PointCol
makeDefColPoint p = {x = p.x, y = p.y, col = defaultColor}

makeDefColPointStr: PointStr -> PointStrCol
makeDefColPointStr p = {x = p.x, y = p.y, col = defaultColor}

numPixels: a -> String
numPixels a = toString a ++ "px"

linePoint: Point -> String -> String
linePoint {x, y} c = (toString x) ++ "," ++ (toString y) ++ "," ++ c

elN: Int -> List a -> Maybe a
elN a lst = case a of 
    0 -> head lst
    _ -> case tail lst of
        Just t -> elN (a - 1) t
        _ -> Nothing

strToPointCol: String -> Maybe PointCol
strToPointCol s = let 
        split = String.split "," s
        nums  = map String.toInt split
    in
    case nums of
    Ok a :: Ok b :: _ :: [] -> let
        col = elN 2 split
    in case col of
        Nothing -> Nothing
        Just color -> Just {x = a, y = b, col = String.cons '#' color}
    _ -> Nothing

addPointsOneCol: Point -> PointCol -> PointCol
addPointsOneCol p1 p2 = {x = p1.x + p2.x, y = p1.y + p2.y, col = p2.col}

addPointsCol: PointCol -> PointCol -> String -> PointCol
addPointsCol p1 p2 col = {x = p1.x + p2.x, y = p1.y + p2.y, col = col}

addPoints: Point -> Point -> Point
addPoints p1 p2 = {x = p1.x + p2.x, y = p1.y + p2.y}
            
subPoints: Point -> Point -> Point
subPoints p1 p2 = {x = p1.x - p2.x, y = p1.y - p2.y}

viewbox: Model -> String
viewbox model = String.join " " (map toString [0, 0, model.window.dims.width, model.window.dims.height])

headAndTail: a -> List a -> List a
headAndTail a lst = a :: lst

rawsToPoints: Point -> List PointCol -> List PointStrCol
rawsToPoints a p = map (\b -> toPointStrCol (addPointsOneCol a b)) p

toPointStr: Point -> PointStr
toPointStr {x, y} = {x = toString x, y = toString y}

toPointStrCols: List PointCol -> List PointStrCol
toPointStrCols p = map toPointStrCol p

toPointStrCol: PointCol -> PointStrCol
toPointStrCol {x, y, col} = {x = toString x, y = toString y, col = col} 


lines: String -> List PointStrCol -> List (Svg.Svg Msg)
lines width points = 
    case points of 
        a :: b :: xs -> 
                line [
                    Svg.Attributes.stroke b.col,
                    Svg.Attributes.strokeLinecap "round",
                    Svg.Attributes.strokeLinejoin "round",
                    Svg.Attributes.fill "none",
                    Svg.Attributes.x1 a.x,
                    Svg.Attributes.x2 b.x,
                    Svg.Attributes.y1 a.y,
                    Svg.Attributes.y2 b.y,
                    Svg.Attributes.strokeWidth width
                ] [] :: (lines width (b :: xs))
        _ -> []


-- MAIN

main =
    Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }


-- MODEL

type alias Point = {x: Int, y: Int}
type alias PointStr = {x: String, y: String}
type alias PointCol = {x: Int, y: Int, col: String}
type alias PointStrCol= {x: String, y: String, col: String}
type alias Dimension = {width: Int, height: Int}

type alias Model = 
    { window: {
        dims: Dimension
      , halfway: Point
      }
    , loaded: Bool
    , color : Maybe String
    , mouse : Maybe Point
    , rawPoints: List PointCol
    , points: List PointStrCol
    , lastPoint: PointCol
    }

initialModel: Model
initialModel = 
    { window = {dims = zeroDims, halfway = zeroPoint}
    , mouse = Nothing
    , rawPoints = []
    , points = []
    , loaded = False
    , color  = Nothing
    , lastPoint = zeroPointCol
    }

init : (Model, Cmd Msg)
init =
    (initialModel, Cmd.batch 
        [ Random.generate NewColor genShortColor
        , Window.size |> Task.perform WindowSize
        ])


-- UPDATE

type Msg = 
    WindowSize Window.Size
    | NewColor String
    | Position Mouse.Position
    | AddPoint (Maybe PointCol)
    | SendPoint Point

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        SendPoint p -> (model, WebSocket.send server (linePoint p (Debug.log "String" (Maybe.withDefault defaultColor model.color))))
        AddPoint a -> case a of 
                Just p -> 
                    let newPoint = addPointsOneCol model.window.halfway p
                    in ({model | rawPoints = p :: model.rawPoints, 
                            lastPoint = newPoint, 
                            points = toPointStrCol newPoint :: model.points}, Cmd.none)
                _ -> (model, Cmd.none)
        NewColor c -> ({model | color = Just c}, Cmd.none)
        Position p -> ({model | mouse = Just p}, Cmd.none)
        WindowSize {width, height} ->
            let newHalf = {x = width // 2, y = height // 2}
            in ({model | window = {dims = {width = width, height = height}, 
                halfway = newHalf},
                points = rawsToPoints newHalf model.rawPoints} , Cmd.none)


-- SUBSCRIPTIONS

subscriptions: Model -> Sub Msg
subscriptions model =
    Sub.batch [
        Window.resizes WindowSize,
        Mouse.moves Position,
        WebSocket.listen server (\x -> strToPointCol x |> AddPoint)
    ]


-- VIEW

view: Model -> Html.Html Msg
view model =
    let
        str = toString model
        mouseRel = subPoints (Maybe.withDefault model.window.halfway model.mouse) model.window.halfway
        col = Maybe.withDefault "#ccc" model.color
    in
        Html.div [
            Html.Attributes.style [
                ("background-color", "#222"),
                ("overflow", "hidden"),
                ("width" , "100%"),
                ("height", "100%")
            ]
        ] [
            svg [
                Svg.Attributes.width  (numPixels model.window.dims.width ),
                Svg.Attributes.height (numPixels model.window.dims.height),
                Svg.Attributes.viewBox (viewbox model),
                Svg.Events.onMouseDown (SendPoint mouseRel)
            ] (List.concat [
                lines lineWeightStr model.points,
                lines sketchWeightStr (map colPointToStr [model.lastPoint, makeDefColPoint (Maybe.withDefault model.window.halfway model.mouse)])
            ])
        ]
