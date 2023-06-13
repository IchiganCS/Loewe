module Loewe.Shared.Utility.ReadRef

type ReadRef<'a> = { mutable Value: 'a option }

let (|Init|Uninit|) ref =
    match ref.Value with
    | Some x -> Init x
    | None -> Uninit

module ReadRef =
    let createInit value = { Value = Some value }
    let createUninit () = { Value = None }

    let isUninit ref =
        match ref with
        | Uninit -> true
        | _ -> false

    let isInit ref =
        match ref with
        | Init _ -> true
        | Uninit -> false

    let initWith value ref =
        match ref with
        | Init _ -> failwith "cannot initialize an already initialized read reference"
        | Uninit -> ref.Value <- Some value

    let get value =
        match value with
        | Init x -> x
        | Uninit -> failwith "tried to get the value of an uninitialized read reference"
