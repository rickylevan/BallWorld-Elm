import Html.App as App
import Time exposing (Time, millisecond, inSeconds)
import Color
import Element
import Collage
import Html exposing (..)
import AnimationFrame
import Math.Vector2 exposing (..)


-- MODEL

(gameWidth, gameHeight) = (500, 500)
(halfWidth, halfHeight) = (gameWidth/2, gameHeight/2)

type alias Ball =
    { x : Float, y : Float, dx : Float, dy : Float
    , r : Float, c : Color.Color}

defaultBall1 =
    { x = 0, y = 0, dx = 184, dy = 335
    , r = 25, c = lightBlue}

defaultBall2 =
    { x = 80, y = 0, dx = 200, dy = 0
    , r = 30, c = lightRed}

defaultBall3 =
    { x = -80, y = 0, dx = -400, dy = 235
    , r = 35, c = lightGreen}

defaultBalls = [defaultBall1, defaultBall2, defaultBall3]

type alias Model = List Ball


init : (Model, Cmd Msg)
init =
  (defaultBalls, Cmd.none)


-- UPDATE

moveBounceBall : Time.Time -> Ball -> Ball
moveBounceBall delta ball =
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

moveBounceBalls : Time.Time -> List Ball -> List Ball
moveBounceBalls delta ballList =
    List.map (moveBounceBall delta) ballList


type Msg
  = Tick Time

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Tick delta ->
        let moveBounce = moveBounceBalls delta
        in (model |> moveBounce |> collideBalls, Cmd.none)

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

-- PHYSICS

mass : Ball -> Float
mass b = b.r ^ 2

distance : Ball -> Ball -> Float
distance b0 b1 =
    let vec0 = vec2 b0.x b0.y
        vec1 = vec2 b1.x b1.y
    in
        (Math.Vector2.sub vec0 vec1) |> length


collisionTime : Ball -> Ball -> Float
collisionTime b0 b1 =
    let dvel = deltaVel b0 b1
        dpos = deltaPos b0 b1
        r = b0.r + b1.r
        phys = ((dot dvel dpos) ^ 2) -
            (dot dvel dvel) *
            ((dot dpos dpos) - (r ^ 2))
    in ((neg (dot dvel dpos)) + (neg (sqrt phys))) /
        (dot dvel dvel)

deltaVel : Ball -> Ball -> Vec2
deltaVel b0 b1 =
    vec2 (b1.dx - b0.dx) (b1.dy - b0.dy)

deltaPos : Ball -> Ball -> Vec2
deltaPos b0 b1 =
    vec2 (b1.x - b0.x) (b1.y - b0.y)

neg : number -> number
neg x = -x

overlapping : Ball -> Ball -> Bool
overlapping b0 b1 =
    (distance b0 b1) < b0.r + b1.r

nudge = 1.1

reducedMass : Ball -> Ball -> Float
reducedMass b0 b1 =
    let m0 = mass b0
        m1 = mass b1
    in (m0 * m1) / (m0 + m1)

impulse : Ball -> Ball -> Vec2
impulse b0 b1 =
    let dist = (distance b0 b1)
        nx = (b1.x - b0.x) / dist
        ny = (b1.y - b0.y) / dist
        dvn = (nx * (b1.dx - b0.dx)) +
              (ny * (b1.dy - b0.dy))
        rm = reducedMass b0 b1
    in vec2 (2 * rm * dvn * nx) (2 * rm * dvn * ny)


collideBalls : List Ball -> List Ball
collideBalls ballList =
    let collideWithOthers subList b =
        case subList of
            x :: xs ->
                if b /= x && overlapping b x then collide b x
                else collideWithOthers xs b
            [] -> b
    in List.map (collideWithOthers ballList) ballList

-- Update b0 in collision with perfectly fixed b1
collide : Ball -> Ball -> Ball
collide b0 b1 =
    let dxNext = b0.dx + (((impulse b0 b1) |> getX) / (mass b0))
        dyNext = b0.dy + (((impulse b0 b1) |> getY) / (mass b0))
        tstar = collisionTime b0 b1
        xNext = b0.x + tstar*b0.dx + neg (nudge * tstar * dxNext)
        yNext = b0.y + tstar*b0.dy + neg (nudge * tstar * dyNext)
    in { b0 | x = xNext, y = yNext, dx = dxNext, dy = dyNext}
