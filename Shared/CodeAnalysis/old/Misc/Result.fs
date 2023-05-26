module Loewe.Misc.Result

let get res =
    match res with
    | Ok v -> v
    | Error _ -> failwith "Value was Error, expected Ok"