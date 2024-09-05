let f x = log((1.0 + x) / (1.0 - x))

let a = 0.0
let b = 0.5

let n = 10

let eps = 0.000001

let rec raiseDegree num degree =
    if degree = 0.0 then 
        1.0
    else 
        num * (raiseDegree num (degree - 1.0))

let rec taylorSmart x prev s n =
    let memberSeries = prev / 2.0*(x * ((raiseDegree x (2.0*n + 1.0)) / (2.0*n + 1.0)))
    if memberSeries < eps then
        s + memberSeries
    else
        s + taylorSmart x memberSeries memberSeries (n + 1.0)

let rec taylorDumb x s n =
    let memberSeries = 2.0*((raiseDegree x (2.0*n + 1.0)) / ( 2.0*n + 1.0))
    if memberSeries < eps then
        s + memberSeries
    else
        s + taylorDumb x memberSeries (n + 1.0)

let rec terms x n = 
    let memberSeries = 2.0*((raiseDegree x (2.0*n + 1.0)) / ( 2.0*n + 1.0)) 
    if memberSeries < eps then
        1.0
    else
        n + terms x (n + 1.0)

let main =
   printfn "  x     Builtin     SmartTaylor     terms     DumbTaylor     terms"
   
   for i = 0 to n do
     let x = a + (float i) / (float n) * (b-a)
     let start = 2.0*x
     
     printfn "%5.2f  %10.6f  %10.6f   %10.6f   %10.6f   %10.6f" x (f x) (taylorSmart x start start 1.0) (terms x 0.0) (taylorDumb x 0.0 0.0) (terms x 0.0)

main
