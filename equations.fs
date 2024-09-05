let eps = 0.001
let e = 2.71828

let rec dichotomy f a b = 
    let x0 = (a + b) / 2.0
    
    if (abs b - a) <= eps then 
        x0
    else
        if ((f x0) * (f b))  < 0.0 then                        
            dichotomy f x0 b
        else
            dichotomy f a x0 

let rec iterations phi x0 = 
    let xCurent = x0 - phi x0
    
    if abs xCurent <= eps then
        phi x0
    else
        iterations phi (phi x0)       

let newthon f f' x0 = 
   let xCurent x = x - f x / f' x
   
   iterations xCurent x0
    
let f1 x = cos(x) - e**((-x**2.0) / 2.0) + x - 1.0 
let f2 x = 1.0 - x + sin(x) - log(1.0 + x)
let f3 x = 3.0*x - 14.0 + e**x - e**x

let f1' x = -sin(x) + x * e**((-x**2.0) / 2.0) + 1.0
let f2' x = -1.0 + cos(x) - (1.0 / 1.0 + x)
let f3' x = 3.0 + e**x - e**x

let phi1 x = x - f1 x / f1' x
let phi2 x = x - f2 x / f2' x
let phi3 x = x - f3 x / f3' x 

let main = 
    printfn "%10.5f  %10.5f  %10.5f" (dichotomy f1 1.0 2.0) (iterations phi1 1.0) (newthon f1 f1' 1.0)
    printfn "%10.5f  %10.5f  %10.5f" (dichotomy f2 1.0 1.5) (iterations phi2 1.0) (newthon f2 f2' 1.0)
    printfn "%10.5f  %10.5f  %10.5f" (dichotomy f3 1.0 3.0) (iterations phi3 1.0) (newthon f3 f3' 1.0)
