module Tests

open System
open Xunit
open FsUnit.Xunit
open Kobalt.Core


let rules: Rules =
  [ Rule.create @"^(.*?)\s-\s(\b(?!Opening|Ending|OVA\b|\d))" "$1: $2" |> Rule.setRegex 
    Rule.create "Fate stay night" "Fate/stay night"
    Rule.create "Kaguya-sama wa Kokurasetai2" "Kaguya-sama wa Kokurasetai?"
    Rule.create "Re-Zero" "Re:Zero"
    Rule.create "Uzaki-chan wa Asobitai! Double" "Uzaki-chan wa Asobitai! ω"
    Rule.create "NieR-Automata" "NieR:Automata" ]


[<Fact>]
let ``Apply rules match single regular`` () =

    "Uzaki-chan wa Asobitai! Double - 11"
    |> Rules.apply rules
    |> should equal "Uzaki-chan wa Asobitai! ω - 11"

    "NieR-Automata Ver1.1a - 04"
    |> Rules.apply rules
    |> should equal "NieR:Automata Ver1.1a - 04"

    "Re-Zero kara Hajimeru Isekai Seikatsu - 03B"
    |> Rules.apply rules
    |> should equal "Re:Zero kara Hajimeru Isekai Seikatsu - 03B"

    "Re-Zero kara Hajimeru Isekai Seikatsu - Opening 01"
    |> Rules.apply rules
    |> should equal "Re:Zero kara Hajimeru Isekai Seikatsu - Opening 01"


[<Fact>]
let ``Apply rules match single regex`` () =

    "Ginga Eiyuu Densetsu - Die Neue These - Ending"
    |> Rules.apply rules
    |> should equal "Ginga Eiyuu Densetsu: Die Neue These - Ending"

    "Ginga Eiyuu Densetsu - Die Neue These - Sakubou - 03"
    |> Rules.apply rules
    |> should equal "Ginga Eiyuu Densetsu: Die Neue These - Sakubou - 03"


[<Fact>]
let ``Apply rules match regex and regular`` () =

    "Fate stay night - Unlimited Blade Works - Opening"
    |> Rules.apply rules
    |> should equal "Fate/stay night: Unlimited Blade Works - Opening"

    "Kaguya-sama wa Kokurasetai2 - Tensai-tachi no Renai Zunousen - Opening"
    |> Rules.apply rules
    |> should equal "Kaguya-sama wa Kokurasetai?: Tensai-tachi no Renai Zunousen - Opening"
