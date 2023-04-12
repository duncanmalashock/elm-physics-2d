module Physics2d.Time exposing (framesPerSecond, step)

import Duration
import Quantity


framesPerSecond : Float
framesPerSecond =
    60


step : Duration.Duration
step =
    Duration.seconds 1
        |> Quantity.divideBy framesPerSecond
