module Loewe.Misc.Result

let get res =
    match res with
    | Ok v -> v
    | Error _ -> raise (System.ArgumentException "Value was Error, expected Ok")