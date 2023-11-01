module NewtonIteration exposing (iterate)


iterate : (Float -> Float) -> (Float -> Float) -> Float -> Float -> Int -> Result String Float
iterate f fp startValue maxIterationDifference maxIterations =
    iterationStep f fp startValue maxIterationDifference maxIterations 0


iterationStep : (Float -> Float) -> (Float -> Float) -> Float -> Float -> Int -> Int -> Result String Float
iterationStep f fp previousIteration maxIterationDifference maxIterations iterationCount =
    if iterationCount > maxIterations then
        Err "too many iterations"

    else
        let
            iteration : Float
            iteration =
                previousIteration - f previousIteration / fp previousIteration
        in
        if abs (iteration - previousIteration) <= maxIterationDifference then
            Ok iteration

        else
            iterationStep f fp iteration maxIterationDifference maxIterations (iterationCount + 1)
