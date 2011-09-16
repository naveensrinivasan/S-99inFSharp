open System;;

let data = [1..10];;
// last P01   Find the last element of a list. 
let last l = List.fold (fun acc ele -> ele::acc) [] l 
             |>  List.head
last data 

//P02  Find the last but one element of a list.
let lastButOne l = List.fold (fun acc ele -> ele:: acc) [] l 
                |> List.tail 
                |> List.head
lastButOne data
 
//P03  Find the Kth element of a list.
let kth l n = l |> Seq.take n 
                |> Seq.skip (n-1)
                |> Seq.head
kth data 8
              
//P04  Find the number of elements of a list.
let count l = l |> List.map(fun i -> 1)
                |> List.fold (+) 0
count data

//P05  Reverse a list.
let rev l = List.fold (fun acc ele -> ele::acc) [] l
rev data

//P06  Find out whether a list is a palindrome.
let isPalindrome d =
    let (dataarr,len ) = d.ToString().ToCharArray() , d.ToString().Length /2
    let (first, second) = ( dataarr|> Seq.take(len) , dataarr |> Array.rev |> Seq.take(len))
    Seq.forall2( fun x y -> x = y) first second 
let result = isPalindrome  12218 

 //P07 Flatten a nested list structure
let (x:List<Object>) = ["a";"a";1;[box 1;box "p"]]
List.foldBack (fun x acc ->  if x.GetType() = typeof<List<Object>> then (box x :?> List<Object>) @ acc else x::acc) x []

//P08 :- Eliminate consecutive duplicates of list elements.
let data1 = [1;1;2;2;1;1;3;3;4] 
List.foldBack(fun x (acc:List<int>) -> if (acc.IsEmpty || acc.Head <> x)  then x::acc else acc) data []

//P09  Pack consecutive duplicates of list elements into sublists.
let packDups l =
    let rec removedups listitem mainlist filteredlist innerlist=
            match mainlist with
            |head::tail when head = listitem -> removedups listitem tail (listitem::filteredlist) innerlist
            |head::tail -> removedups head tail [head]  (filteredlist::innerlist)
            |[] -> filteredlist::innerlist |> List.rev
    removedups (List.head l) l [] []
packDups data1           

//P10  Run-length encoding of a list.
let encode l = packDups l |> List.collect (fun x ->[(List.length x, List.head x)]) 
encode data1

//P11  Modified run-length encoding.
let modEncode l = encode l |> List.map(fun x-> if fst x = 1 then box(snd x) else box [for i in 1..fst x -> snd x ])
modEncode data1

//P12  Decode a run-length encodedlist 
let decode l = encode l |> List.collect(fun x -> [for i in 1 .. fst x -> snd x])
decode data1

//p14  duplicate elements in the list
let duplicate l = l |> List.collect(fun x -> [x;x])
duplicate [1..10]

//P15  Duplicate elements by a given number of times
let duplicateN n l = l |> List.collect(fun x -> [for i in 1..n -> x])
duplicateN 3 [1..5]

//P16  Drop every  Nth element from a list
let dropeverynth n l = l |> List.mapi (fun i x -> if (i % n) <> 0 then [x] else [])  
                       |> List.collect(fun x -> x)
dropeverynth 3 [0..30]

//P17  Split a list into two parts
let split n l = (l|> Seq.take(n) |> Seq.toList,l |> Seq.skip(n) |> Seq.toList) 
split 3 [0..10]

//P18  Split a list into two parts
let slice a b l = l |> Seq.take(b) |> Seq.skip(a) |> Seq.toList 
slice 3 7 [1..10]

//P19  Rotate N places
let rotate n l =  if n > -1 then (l |>Seq.skip(n) |> Seq.toList) @ (l |>Seq.take(n) |> Seq.toList)
                        else (l |> List.rev |> Seq.take(-n) |> Seq.toList) @ (l |> Seq.take( (l |> Seq.length) + n ) |> Seq.toList)
 
rotate -2 [1..10]
rotate 2 [1..10]

// P20  Remove Kth element from the list
let removenth n l = 
    let (h,t) = split n l 
    (h|>Seq.take (n-1) |> Seq.toList) @ t
removenth 9 [1..10]
