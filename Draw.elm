import Random
import Html
import Html.Attributes
import Window
import Task
import Svg exposing (polyline, svg, line)
import Svg.Attributes
import Svg.Events
import Mouse exposing (moves, clicks)
import List exposing (map, head, append, take)
import WebSocket

server = "wss://owen.cafe:8000"
lineWeight = 5
sketchWeight = lineWeight // 2

main =
    Html.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

numPixels: a -> String
numPixels a = toString a ++ "px"

linePoint: Point -> String
linePoint {x, y} = (toString x) ++ "," ++ (toString y)

numCommaNum: String -> List (Result String Int)
numCommaNum s = map String.toInt (String.split "," s)

strToPoint: String -> Maybe Point
strToPoint s = case (numCommaNum s) of
    Ok a :: Ok b ::[] -> Just {x = a, y = b}
    _ -> Nothing

pointsToStr: List Point -> String
pointsToStr lst = String.join " " (map linePoint lst)

addPoints: Point -> Point -> Point
addPoints p1 p2 = {x = p1.x + p2.x, y = p1.y + p2.y}
            
subPoints: Point -> Point -> Point
subPoints p1 p2 = {x = p1.x - p2.x, y = p1.y - p2.y}

viewbox: Model -> String
viewbox model = String.join " " (map toString [0, 0, model.window.dims.width, model.window.dims.height])

headAndTail: a -> List a -> List a
headAndTail a lst = a :: lst

lines: String -> List PointStr -> List (Svg.Svg Msg)
lines width points = 
    case points of 
        a :: b :: xs -> 
                line [
                    Svg.Attributes.stroke "#ccc",
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


-- MODEL

type alias Point = {x: Int, y: Int}
type alias PointStr = {x: String, y: String}
type alias Dimension = {width: Int, height: Int}

rawsToPoints: Point -> List Point -> List PointStr
rawsToPoints a p = map (\b -> toPointStr (addPoints a b)) p

toPointStrs: List Point -> List PointStr
toPointStrs p = map toPointStr p

toPointStr: Point -> PointStr
toPointStr {x, y} = {x = toString x, y = toString y} 

type alias Model = 
    { window: {
        dims: Dimension
      , halfway: Point
      }
    , loaded: Bool
    , color : Maybe String
    , mouse : Maybe Point
    , rawPoints: List Point
    , points: List PointStr
    , lastPoint: Point
    }

zeroDims = {width = 0, height = 0}
zeroPoint = {x = 0, y = 0}

dimToPoint {width, height} = {x = width, y = height}

initialModel: Model
initialModel = 
    { window = {dims = zeroDims, halfway = zeroPoint}
    , mouse = Nothing
    , rawPoints = []
    , points = []
    , loaded = False
    , color  = Nothing
    , lastPoint = zeroPoint
    }

init : (Model, Cmd Msg)
init =
    (initialModel, Window.size |> Task.perform WindowSize)


-- UPDATE

type Msg = 
    WindowSize Window.Size
    | Position Mouse.Position
    | AddPoint (Maybe Point)
    | SendPoint Point

update: Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        SendPoint p -> (model, WebSocket.send server (linePoint p))
        AddPoint a -> 
            case a of 
                Just p -> 
                    let newPoint = addPoints model.window.halfway p
                    in ({model | rawPoints = p :: model.rawPoints, 
                            lastPoint = newPoint, 
                            points = toPointStr newPoint :: model.points}, Cmd.none)
                _      -> (model, Cmd.none)
        Position p -> ({model | mouse  = Just p}, Cmd.none)
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
        WebSocket.listen server (\x -> strToPoint x |> AddPoint)
    ]


-- VIEW

view: Model -> Html.Html Msg
view model =
    let
        str = toString model
        mouseRel = subPoints (Maybe.withDefault model.window.halfway model.mouse) model.window.halfway
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
                lines (toString lineWeight) model.points,
                lines (toString sketchWeight) [toPointStr model.lastPoint, toPointStr (Maybe.withDefault model.window.halfway model.mouse)]
            ])
        ]
