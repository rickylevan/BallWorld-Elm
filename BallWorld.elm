import Html.App as App
import Time exposing (Time, millisecond, inSeconds)
import Color
import Element
import Collage
import Html exposing (Html)
import AnimationFrame


-- MODEL

(gameWidth, gameHeight) = (500, 500)
(halfWidth, halfHeight) = (gameWidth/2, gameHeight/2)

type alias Ball = 
    { x : Float
    , y : Float
    , dx : Float
    , dy : Float
    , r : Float
    , c : Color.Color
    }

defaultBall1 = 
    { x = 0
    , y = 0
    , dx = 84
    , dy = 35
    , r = 45
    , c = lightBlue
    }

defaultBall2 = 
    { x = 80
    , y = 0
    , dx = 4
    , dy = 35
    , r = 30
    , c = lightRed
    }

defaultBall3 = 
    { x = -80
    , y = 0
    , dx = -100
    , dy = 235
    , r = 35
    , c = lightGreen
    }

defaultBalls = [defaultBall1, defaultBall2, defaultBall3]

type alias Model = List Ball


init : (Model, Cmd Msg)
init =
  (defaultBalls, Cmd.none)


-- UPDATE

updateBall : Time.Time -> Ball -> Ball
updateBall delta ball = 
    let 
        rightBound = ball.x + ball.r
        leftBound = ball.x - ball.r
        topBound = ball.y + ball.r
        bottomBound = ball.y - ball.r
        rightFlab = Basics.max 0 (rightBound - halfWidth)
        leftFlab = Basics.min 0 (leftBound + halfWidth)
        topFlab = Basics.max 0 (topBound - halfWidth)
        bottomFlab = Basics.min 0 (bottomBound + halfWidth)
        xVelFlip = if (rightFlab > 0 || leftFlab < 0) then -1 else 1
        yVelFlip = if (topFlab > 0 || bottomFlab < 0) then -1 else 1
    in
        {ball | 
        x = ball.x + xVelFlip*ball.dx*delta - 2*(rightFlab+leftFlab),
        y = ball.y + yVelFlip*ball.dy*delta - 2*(topFlab+bottomFlab),
        dx = xVelFlip * ball.dx,
        dy = yVelFlip * ball.dy
        }


type Msg
  = Tick Time

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick delta -> ((List.map (updateBall delta) model), Cmd.none)



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions = (\_ -> Sub.batch 
        [AnimationFrame.diffs (Tick<<inSeconds)])



-- VIEW

lightBlue = Color.rgb 160 160 255
lightRed = Color.rgb 255 160 160
lightGreen = Color.rgb 160 255 160
lightGray = Color.rgb 200 200 200
darkBlue = Color.rgb 0 0 50

viewBalls : List Ball -> List Collage.Form
viewBalls ballList = 
    let f ball = Collage.circle ball.r
        |> Collage.filled ball.c
        |> Collage.move (ball.x, ball.y)
    in List.map f ballList

view : Model -> Html Msg
view model = 
    Element.toHtml (Element.container 500 500 Element.middle <|
    Collage.collage gameWidth gameHeight
    ([ Collage.rect gameWidth gameHeight
        |> Collage.filled darkBlue
     ] ++ (viewBalls model)))


-- MAIN

main =
  App.program
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }
