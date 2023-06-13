module Loewe.Shared.Utility.SpikeList

[<RequireQualifiedAccess>]
type FilledSpikeList<'entry, 'spike> =
    | OneEntry of 'entry
    | SpikeEntry of 'entry * 'spike * FilledSpikeList<'entry, 'spike>

[<RequireQualifiedAccess>]
type SpikeList<'entry, 'spike> =
    | NotFilled
    | Filled of FilledSpikeList<'entry, 'spike>

let (|Empty|Single|Multiple|) ls =
    match ls with
    | SpikeList.NotFilled -> Empty
    | SpikeList.Filled (FilledSpikeList.OneEntry e) -> Single e
    | SpikeList.Filled (FilledSpikeList.SpikeEntry (e, s, tail)) -> Multiple (e, s, tail)


module SpikeList =
    let rec createRev lastElem revEntries =
        let rec helper entriesAndSpikes =
            match entriesAndSpikes with
            | [] -> FilledSpikeList.OneEntry lastElem
            | (e, s) :: tail -> FilledSpikeList.SpikeEntry (e, s, (helper tail))

        SpikeList.Filled (helper revEntries)

    let rec create firstElem entries =
        // first reverse the data
        // then call createRev
        let rec revData entries =
            match entries with
            | [] -> firstElem, []
            | (s, e) :: tail ->
                let last, newEntries = revData tail
                last, newEntries @ [ e, s ]

        let lastElem, newEntries = revData entries

        createRev lastElem newEntries

    let rec entries ls =
        match ls with
        | Empty -> []
        | Single e -> [ e ]
        | Multiple (e, _, tail) -> e :: (entries (SpikeList.Filled tail))


    let rec spikes ls =
        match ls with
        | Empty -> []
        | Single _ -> []
        | Multiple (_, s, tail) -> s :: (spikes (SpikeList.Filled tail))
