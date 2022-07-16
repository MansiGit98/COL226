
fun interpret(str: string) = 
let 
fun readlist(infile: string) = 
let val ins = TextIO.openIn infile
fun loop ins =     
case TextIO.inputLine ins of
SOME line => line::loop ins
|NONE => []
in loop ins before TextIO.closeIn ins end
 
val kList = readlist(str)
val n = length kList
val code = Array.array(n, (0,0,0,0))
val ct = ref 0

fun fed(nil) = nil
  |fed(h::t) = (
     let
        fun topair s =
        let val s' = String.substring(s, 1, size s-2)
        in List.mapPartial Int.fromString(String.tokens(fn c=> c = #",")s')
       end
       fun to_Tuple[w:int,x:int,y:int,z:int] = (w,x,y,z)
      in (Array.update(code, !ct, to_Tuple(topair(h)));
         ct := !ct + 1;
         fed(t))
end)

val arr = Array.array(20,0);

exception Division_By_Zero;

fun right(x:int, i:int, j:int, k:int) = 
(if x = 1 then (
print("Give Input: ");
let val str = valOf (TextIO.inputLine TextIO.stdIn) 
  val e : int = valOf (Int.fromString str)
in Array.update(arr, k, e) end)
else if x = 2 then Array.update(arr, k, Array.sub(arr,i))
else if x = 3 then
if Array.sub(arr,i) = 0 then Array.update(arr, k, 1)
else Array.update(arr, k, 0)
else if x = 4 then
if Array.sub(arr,i) = 0 andalso Array.sub(arr,j) = 0 then Array.update(arr, k, 0)
else Array.update(arr, k, 1)
else if x = 5 then
if Array.sub(arr,i) = 0 orelse Array.sub(arr,j) = 0 then Array.update(arr, k, 0)
else Array.update(arr, k, 1)
else if x = 6 then Array.update(arr, k, Array.sub(arr,i)+Array.sub(arr,j))
else if x = 7 then Array.update(arr, k, Array.sub(arr,i)-Array.sub(arr,j))
else if x = 8 then Array.update(arr, k, Array.sub(arr,i)*Array.sub(arr,j))
else if x = 9 then 
if Array.sub(arr,j) = 0 then raise Division_By_Zero
else Array.update(arr, k, Array.sub(arr,i) div Array.sub(arr,j))
else if x = 10 then 
if Array.sub(arr,j) = 0 then raise Division_By_Zero
else Array.update(arr, k, Array.sub(arr,i) mod Array.sub(arr,j))
else if x = 11 then
if Array.sub(arr,i) = Array.sub(arr,j) then Array.update(arr, k, 1)
else Array.update(arr, k, 0)
else if x = 12 then
if Array.sub(arr,i) > Array.sub(arr,j) then Array.update(arr, k, 1)
else Array.update(arr, k, 0)
else if x = 15 then print("Output: "^Int.toString(Array.sub(arr,i))^"\n")
else if x = 16 then Array.update(arr, k, i)
else print("Error>>..Error"))

fun Loop(x:int) =
let
  val count = ref 0
in
  while !count <> x do (
    if #1(Array.sub(code, !count)) = 0 then count := x 
    else if #1(Array.sub(code, !count)) = 13 then
       if Array.sub(arr, #2(Array.sub(code, !count))) <> 0
         then count := #4(Array.sub(code, !count)) - 1
       else count := !count + 1
     else if #1(Array.sub(code, !count)) = 14 
         then count := #4(Array.sub(code, !count)) - 1
     else(
         right(Array.sub(code, !count));
         count := !count + 1 
     )
  )
end
in fed(kList);
   Loop(n)
end;
