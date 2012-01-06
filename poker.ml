type suit = HEART | DIAMOND | SPADE | CLOVER;;
type rank = Num of int | J | Q | K | A
type card = Card of suit * rank;;

exception Unknown_Rank_Descr
exception Unknown_Rank_Int

let encode_rank (r: rank) : int = 
        match r with 
        | Num 2 -> 3
        | Num 3 -> 5
        | Num 4 -> 7
        | Num 5 -> 11
        | Num 6 -> 13
        | Num 7 -> 17
        | Num 8 -> 19
        | Num 9 -> 23
        | Num 10 -> 29
        | J -> 31 
        | Q -> 37
        | K -> 41 
        | A -> 2
        | _ -> raise Unknown_Rank_Descr


let decode_rank (i: int) -> rank = 
        match i with
        | 3 -> Num 2
        | 5 -> Num 3
        | 7 -> Num 4
        | 11 -> Num 5
        | 13 -> Num 6
        | 17 -> Num 7
        | 19 -> Num 8
        | 23 -> Num 9
        | 29 -> Num 10
        | 31 -> J
        | 37 -> Q
        | 41 -> K
        | 2 -> A
        | raise Unknown_Rank_Int
      
