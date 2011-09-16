// Learn more about F# at http://fsharp.net
// Learn more about F# at http://fsharp.net
// Tuples

let printCity (cityInfo) =
    printfn "Population of %s is %d."
            (fst cityInfo) (snd cityInfo)

let prague = ("Prague",18340)
let NewYork = ("Prague",18340.1)
let seattle = ("Seattle",12345678)

let withItem2 newItem2 tuple =
    let (original1,o2) = tuple
    (original1,newItem2)

let withItem3 newItem2 (original1,_) = (original1,newItem2) 

let withItem4 newItem2 tuple = 
    match tuple with
    | (o1,_) -> (o1,newItem2)


let setPopulation newItem2 tuple = 
    match tuple with
    | ("Seattle",_) -> ("Seattle",newItem2+100)
    | (cityname,_) -> (cityname,newItem2)

let setPopulation1 tuple = 
    match tuple with
    | ("Seattle",pop) -> ("Seattle",pop+100)
    |  _-> tuple 
    
let newPrague = withItem3 ((snd prague) + 1000) prague

printCity newPrague


//List and recursions
// 3.3



let rec factorial n =
    if (n<=1) then
        1
    else
         n* factorial(n-1) 

let rec fac n =
    match n <=1 with
    |true -> 1
    |_ -> n * fac(n-1) 

fac 5 |> ignore


let lst = []

let lst1 = 1::2::3::4:: []

let ls2= [1;2;3;4;5]

let ls3 = [1..5]

let ls4 = 10::ls3


let rec aggregateList op init intList=
    match intList with
    |[] -> init
    |h::t ->
            let result = aggregateList op init t
            op result h

let x = aggregateList (*) 1 [1..5] 

let max = aggregateList max (-1) [1..5] 



