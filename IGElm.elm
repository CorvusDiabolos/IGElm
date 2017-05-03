import Audio exposing (PlaybackOptions, defaultPlaybackOptions, Sound)
import Task exposing (Task, andThen)
import Html exposing (Html, button, div, input, program)
import String
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Html.Attributes exposing (style)
{-score in the game is based on the passage of time; trying to prepare for keeping score and possibly using time as my means of generating the objects in the level, but still not working quite right, hoping for the next release because I apparently don't fully understand how to utilize time-}
import Time exposing (Time, second, millisecond) 
import Keyboard


{-halfSecond : Time
halfSecond = millisecond * 500-}
screenY : Float --for the gameplay area size; helpful in positioning other elements
screenY = 400
screenX : Float
screenX = 700

cubeX : Float --the object players use
cubeX = 200
cubeSize : Float
cubeSize = 40

{-platforms being weird-}
platformSize : Float
platformSize = cubeSize

cubeAcceleration : Float --the gravity and velocity shiz
cubeAcceleration = 10
triangleSpeed : Float
triangleSpeed = 11
maxJump : Float
maxJump = 260

floor : Float
floor = screenY - cubeSize

loadSound : Task String Sound
loadSound = Audio.loadSound "music/The Impossible Game Soundtrack.mp3"

main = program { 
        init = init,
    view = view, 
    update = update,
    subscriptions = subscriptions
     }

type alias Position ={ 
        x: Float, 
        y: Float 
        }

type alias Platform = {
        platformHeight : Float
        }

type alias Cube = { 
        cubeHeight : Float, 
        jumping : Bool 
        }

type alias Triangle = { 
        position : Position 
        }

type alias Score = --score not quite working with Time not being my friend EDIT: now is more my friend
    Int

playbackOptions = {
  defaultPlaybackOptions 
  | loop = True, 
  startAt = Nothing 
  }



--more time stuff; may be a tool with generating the obstacles; after explaination of randomness this may not be viable if I understand Elm
type alias Delay = 
    Int

type Model =
    InsertCoin Score 
    | Started Cube (List Triangle) Score 


init : (Model, Cmd Msg)
init = (InsertCoin 0, Cmd.none)


type Msg = Tick Time 
 | SpaceBar Keyboard.KeyCode 
 | AddTriangle 
 | MusicLoaded Sound
 | Error String
 | Noop

checkCollision : Cube -> Triangle -> Bool --Very, very rough, but mostly ok
checkCollision cube triangle =
    let
        collisionX = (cubeX > triangle.position.x) && (cubeX < triangle.position.x + cubeSize)
        collisionY = (cube.cubeHeight + cubeSize > triangle.position.y) && (cube.cubeHeight < triangle.position.y + cubeSize)
    in
        collisionX && collisionY

addTriangle : List Triangle -> List Triangle --recursive list to keep the game going until the player dies
addTriangle triangles1 =
    let
        triangles = List.map (\triangle -> { 
            position = { 
                x = triangle.position.x-triangleSpeed, 
        y=triangle.position.y } }) triangles1
        previousTriangle = case List.head triangles of
            Nothing -> { 
                    position = 
                    { 
                        x=0, 
                        y=0 
                        } 
                        }
            Just val ->
                val
        previousTriangleX = previousTriangle.position.x
    in
        if previousTriangleX < screenX-cubeSize-400 
        {-to give some space between triangles otherwise you get a long train of them and instantly die, but the game is called Impossible Game so maybe that is the point....-} 
            then 
            List.append [
                { 
                    position = 
                    { 
                        x=screenX, 
                        y=floor 
                        }
                        }
                        ] 
                        triangles1
        else
       triangles

-- playSound : Sound -> PlaybackOptions -> Cmd Msg
-- playSound sound options =
--   Task.perform Error (always Noop) <| Audio.playSound options sound

-- stopSound : Sound -> Cmd Msg
-- stopSound sound =
--   Task.perform (always Noop) (always Noop) <| Audio.stopSound sound

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case model of
        InsertCoin score ->
            case msg of
                Tick _ ->
                    ( model, Cmd.none )
                SpaceBar key -> --32 is Spacebar
                    if key == 32 then ( Started (Cube floor False) [] 0, Cmd.none )
                    else ( model, Cmd.none )
                addTriangle ->
                    ( model, Cmd.none )
        Started cube triangles score ->
            let
                checkCubeCollision = checkCollision cube
                collisions = List.map checkCubeCollision triangles
                isGameOver = List.member True collisions

                cubeHeight = cube.cubeHeight
                jumping = (
                    if not cube.jumping || cube.cubeHeight <= maxJump then
                        False
                    else True
                )
                newHeight = (
                    if jumping then cubeHeight - cubeAcceleration
                    else if cubeHeight < floor then cubeHeight + cubeAcceleration
                    else cubeHeight
                )
            in
                case msg of
                    Tick _ ->
                        if isGameOver then
                            ( InsertCoin score, Cmd.none )
                        else
                        (
                            Started { cube | cubeHeight = newHeight, jumping = jumping }
                            (addTriangle triangles) (score + 1),
                            Cmd.none
                        )
                    SpaceBar key ->
                        if key == 32 && cubeHeight == floor then
                            ( Started { cube | jumping = True } triangles score, Cmd.none )
                        else (Started cube triangles score, Cmd.none)
                    addTriangle ->
                        ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch(
        [
        Keyboard.presses SpaceBar,
        Time.every (millisecond*20) Tick
    ]
    )


containerStyle : List (String, String)
containerStyle =
    [
        ("display", "flex"),
        ("align-items", "center"),
        ("justify-content", "center"),
        ("height", "100%")
    ]

triangleTriangle : Triangle -> Html a
triangleTriangle triangle =
    let
        leftPoint = "0," ++ toString cubeSize
        rightPoint = toString cubeSize ++ "," ++ toString cubeSize
        topPoint = toString (cubeSize / 2) ++ ",0"
        pointList = [leftPoint, topPoint, rightPoint]
    in
        polygon 
        [width (toString cubeSize), 
        height (toString cubeSize), 
        fill "black", 
        stroke "white",
                 transform ("translate(" ++ toString(triangle.position.x) ++ "," ++ toString(triangle.position.y) ++ ")"),
                 points (String.join " " pointList)] []

getRotation : Float -> Float -> Float -> String
getRotation deg x y =
    "rotate(" ++ toString(deg) ++ " " ++ toString(x) ++ " " ++ toString(y) ++ ")"

cubeRect : Cube -> Html a --need to figure out the jump/twist animation; lord help me EDIT: yayay
cubeRect cube = 
    let
        cubeCenterX = cubeX + cubeSize / 2
        cubeCenterY = cube.cubeHeight + cubeSize / 2
        rotationValue = ( 90 / (floor - (maxJump)) ) * (floor - cube.cubeHeight)
        rotation =
            if cube.jumping 
            then 
            rotationValue
            else 
            -rotationValue

    in
        rect 
        [width (toString cubeSize), 
        height (toString cubeSize), 
        fill "orange",
        stroke "black",
        transform ( getRotation rotation cubeCenterX cubeCenterY ),
              y 
              (toString cube.cubeHeight), 
              x (toString cubeX)] 
              []

gameContainer : List (Html msg) -> Html msg
gameContainer children =
    div [ Html.Attributes.style containerStyle ]
        [
            svg [
                width (toString screenX),
                height (toString screenY),
                fontFamily "Courier New" --hey! I can change the font! neat!
            ]
            (List.concat [
                [rect 
                [width "100%", 
                height "100%", 
                fill "teal"] 
                []
                ],
                children
            ]
            )
        ]

scoreText : Score -> Bool -> Html a
scoreText score started =
    let
        anchor = if started then
                "start"
            else
                "middle"
        xVal = if started then
                toString 10
            else
                toString (screenX/2)
        yVal = if started then
                toString 20
            else
                toString (screenY/2 + 50)
    in
        text_ [x xVal,
               y yVal,
               textAnchor anchor,
               fill "white"
              ] [ text ("Score: " ++ toString score) ]

view : Model -> Html Msg
view model =
    case model of
        InsertCoin score ->
            gameContainer [
                text_ [x (toString (screenX / 2)),
                       y (toString (screenY / 2)),
                       fill "white",
                       textAnchor "middle"
                      ] [ text "Press Spacebar to insert coin or jump" ],
                scoreText score False
            ]
        Started cube triangles score ->
                gameContainer
                    (List.concat [
                        [
                            cubeRect cube,
                            scoreText score True
                        ],
                        List.map triangleTriangle triangles
                    ])