
type opt_type = Call | Put

(* option data *)
type opt = { mutable s : float
           ; mutable strike : float
           ; mutable r : float
           ; mutable divq : float
           ; mutable v : float
           ; mutable t : float
           ; mutable typ : opt_type
           ; mutable divs : float
           ; mutable dgrefval : float
           }


let data = ref [||]
let prices = ref [||]
let numOptions = ref 0

let otype = ref [||]
let sptprice = ref [||]
let strike = ref [||]
let rate = ref [||]
let volatility = ref [||]
let otime = ref [||]

let inv_sqrt_2xPI = 0.39894228040143270286

let num_runs = 100

let cndf _x =
  let inputX = ref _x in
  let sign = ref 0 in
  let outputX = ref 0. in
  let xInput = ref 0. in
  let xNPrimeofX = ref 0. in
  let expValues = ref 0. in
  let xK2 = ref 0. in
  let xK2_2 = ref 0. in
  let xK2_3 = ref 0. in
  let xK2_4 = ref 0. in
  let xK2_5 = ref 0. in
  let xLocal = ref 0. in
  let xLocal_1 = ref 0. in
  let xLocal_2 = ref 0. in
  let xLocal_3 = ref 0. in

  if !inputX < 0. then
    (inputX := -. !inputX ; sign := 1)
  else
    sign := 0 ;

  xInput := !inputX ;

  (* Compute NPrimeX term common to both four & six decimal accuracy calcs *)
  expValues := exp @@ -0.5 *. !inputX *. !inputX ;
  xNPrimeofX := !expValues ;
  xNPrimeofX := !xNPrimeofX *. inv_sqrt_2xPI ;

  xK2 := 0.2316419 *. !xInput ;
  xK2 := 1.0 +. !xK2 ;
  xK2 := 1.0 /. !xK2 ;
  xK2_2 := !xK2 *. !xK2 ;
  xK2_3 := !xK2_2 *. !xK2 ;
  xK2_4 := !xK2_3 *. !xK2 ;
  xK2_5 := !xK2_4 *. !xK2 ;

  xLocal_1 := !xK2 *. 0.319381530 ;
  xLocal_2 := !xK2_2 *. (-0.356563782) ;
  xLocal_3 := !xK2_3 *. 1.781477937 ;
  xLocal_2 := !xLocal_2 +. !xLocal_3 ;
  xLocal_3 := !xK2_4 *. (-1.821255978) ;
  xLocal_2 := !xLocal_2 +. !xLocal_3 ;
  xLocal_3 := !xK2_5 *. 1.330274429 ;
  xLocal_2 := !xLocal_2 +. !xLocal_3 ;

  xLocal_1 := !xLocal_2 +. !xLocal_1 ;
  xLocal   := !xLocal_1 *. !xNPrimeofX ;
  xLocal   := 1.0 -. !xLocal ;

  outputX  := !xLocal ;

  if !sign = 1 then
    outputX := 1.0 -. !outputX ;

  !outputX


let blkSchlsEqEuroNoDiv sptprice strike rate volatility time otype timet =
    let optionPrice = ref 0. in

    (* local private working variables for the calculation *)
    let xStockPrice = ref 0. in
    let xStrikePrice = ref 0. in
    let xRiskFreeRate = ref 0. in
    let xVolatility = ref 0. in
    let xTime = ref 0. in
    let xSqrtTime = ref 0. in

    let logValues = ref 0. in
    let xLogTerm = ref 0. in
    let xD1 = ref 0. in
    let xD2 = ref 0. in
    let xPowerTerm = ref 0. in
    let xDen = ref 0. in
    let d1 = ref 0. in
    let d2 = ref 0. in
    let futureValueX = ref 0. in
    let nofXd1 = ref 0. in
    let nofXd2 = ref 0. in
    let negNofXd1 = ref 0. in
    let negNofXd2 = ref 0. in

    xStockPrice := sptprice ;
    xStrikePrice := strike ;
    xRiskFreeRate := rate ;
    xVolatility := volatility ;

    xTime := time ;
    xSqrtTime := sqrt !xTime ;

    logValues := log (sptprice /. strike) ;

    xLogTerm := !logValues ;


    xPowerTerm := !xVolatility *. !xVolatility ;
    xPowerTerm := !xPowerTerm *. 0.5 ;

    xD1 := !xRiskFreeRate +. !xPowerTerm ;
    xD1 := !xD1 *. !xTime ;
    xD1 := !xD1 +. !xLogTerm ;

    xDen := !xVolatility *. !xSqrtTime ;
    xD1 := !xD1 /. !xDen ;
    xD2 := !xD1 -.  !xDen ;

    d1 := !xD1 ;
    d2 := !xD2 ;

    nofXd1 := cndf !d1 ;
    nofXd2 := cndf !d2 ;

    futureValueX := strike *. exp ((-.rate)*.time) ;
    if otype = Call then
        optionPrice := (sptprice *. !nofXd1) -. (!futureValueX *. !nofXd2)
    else
      begin
        negNofXd1 := 1.0 -. !nofXd1 ;
        negNofXd2 := 1.0 -. !nofXd2 ;
        optionPrice := (!futureValueX *. !negNofXd2) -. (sptprice *. !negNofXd1)
      end ;

    !optionPrice;

type range = int * int

let mainWork ran =
  let price = ref 0. in
  let beg = fst ran in
  let en = snd ran in

  for i = beg to (en-1) do

    (* Calling main function to calculate option value based on Black & Scholes's equation. *)
    price := blkSchlsEqEuroNoDiv !sptprice.(i) !strike.(i) !rate.(i) !volatility.(i) !otime.(i) !otype.(i) 0 ;
    !prices.(i) <- !price ;

  done

let bs_thread () =
  for j = 0 to num_runs - 1 do
    mainWork (0, !numOptions)
  done


let main () =
  (* FILE *file; *)
  (* int i; *)
  (* int loopnum; *)
  (* fptype * buffer; *)
  (* int * buffer2; *)
  (* int rv; *)

  let fsin = Scanf.Scanning.from_channel stdin in

  Scanf.bscanf fsin "%i\n" (fun i -> numOptions := i) ;

  (* alloc spaces for the option data *)
  data := Array.init !numOptions (fun _ -> { s = 0. ; strike = 0. ; r = 0. ; divq = 0. ; v = 0. ; t = 0. ; typ = Call ; divs = 0. ; dgrefval = 0. }) ;
  prices := Array.make !numOptions 0. ;

  for loopnum = 0 to (!numOptions - 1) do
    Scanf.bscanf fsin "%f %f %f %f %f %f %c %f %f\n"
      (fun a b c d e f g h i ->
         !data.(loopnum).s <- a ;
         !data.(loopnum).strike <- b ;
         !data.(loopnum).r <- c ;
         !data.(loopnum).divq <- d ;
         !data.(loopnum).v <- e ;
         !data.(loopnum).t <- f ;
         !data.(loopnum).typ <- if g = 'C' then Call else Put ;
         !data.(loopnum).divs <- h ;
         !data.(loopnum).dgrefval <- i)
  done ;

  Printf.eprintf "input done\n" ;


  Printf.eprintf "Num of Options: %d\n" !numOptions ;
  Printf.eprintf "Num of Runs: %d\n" num_runs ;

  sptprice := Array.make !numOptions 0. ;
  strike := Array.make !numOptions 0. ;
  rate := Array.make !numOptions 0. ;
  volatility := Array.make !numOptions 0. ;
  otime := Array.make !numOptions 0. ;
  otype := Array.make !numOptions Call ;


  for i = 0 to (!numOptions-1) do
    !otype.(i) <- !data.(i).typ ;
    !sptprice.(i) <- !data.(i).s ;
    !strike.(i) <- !data.(i).strike ;
    !rate.(i) <- !data.(i).r ;
    !volatility.(i) <- !data.(i).v ;
    !otime.(i) <- !data.(i).t ;
  done ;

  bs_thread () ;

  (* Write prices to output file *)

  Printf.printf "%i\n" !numOptions ;

  for i = 0 to (!numOptions-1) do
      Printf.printf "%.18f\n" !prices.(i) ;
  done

let () = main ()
