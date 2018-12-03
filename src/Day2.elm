module Day2 exposing (part1, part2)

import Dict exposing (Dict)


part1 =
    let
        x =
            puzzleInput
                |> String.lines
                |> List.filter (hasCharWithFrequency 2)
                |> List.length

        y =
            puzzleInput
                |> String.lines
                |> List.filter (hasCharWithFrequency 3)
                |> List.length
    in
    x * y


part2 =
    matchLines 0 (String.lines puzzleInput)


matchLines : Int -> List String -> String
matchLines n lines =
    let
        h =
            lines
                |> List.map (String.dropLeft 1)
                |> createHistogram Dict.empty
                |> Dict.toList
                |> List.filter (\( k, v ) -> v > 1)
    in
    case h of
        [] ->
            matchLines (n + 1) (List.map rotateLeft lines)

        ( k, v ) :: xs ->
            String.right n k ++ String.dropRight n k


rotateLeft : String -> String
rotateLeft line =
    let
        l =
            String.toList line
    in
    case l of
        x :: xs ->
            String.fromList (xs ++ [ x ])

        [] ->
            ""


hasCharWithFrequency : Int -> String -> Bool
hasCharWithFrequency n line =
    createHistogram Dict.empty (String.toList line)
        |> Dict.values
        |> List.filter ((==) n)
        |> List.isEmpty
        |> not


createHistogram : Dict comparable Int -> List comparable -> Dict comparable Int
createHistogram d chars =
    case chars of
        x :: xs ->
            case Dict.get x d of
                Just count ->
                    createHistogram (Dict.insert x (count + 1) d) xs

                Nothing ->
                    createHistogram (Dict.insert x 1 d) xs

        [] ->
            d


puzzleInput =
    """ohvflkatysoimjxbunazgwcdpr
ohoflkctysmiqjxbufezgwcdpr
ohvflkatysciqwxfunezgwcdpr
fhvflyatysmiqjxbunazgwcdpr
ohvhlkatysmiqjxbunhzgwcdxr
ohvflbatykmiqjxbunezgscdpr
ohvflkatasaiqjxbbnezgwcdpr
ohvflkatyymiqjxrunetgwcdpr
ohvflkatbsmiqhxbunezgwcdpw
oheflkytysmiqjxbuntzgwcdpr
ohvflkatrsmiqjibunezgwcupr
ohvflkaiysmiqjxbunkzgwkdpr
ohvilkutysmiqjxbuoezgwcdpr
phvflkatysmkqjxbulezgwcdpr
ohvflkatnsmiqjxbznezgpcdpr
ohvylkatysriqjobunezgwcdpr
ohvflkatytmiqjxbunezrwcypr
ohvonkatysmiqjxbunezgwxdpr
ohvflkatgsmoqjxyunezgwcdpr
ohvflkbtqsmicjxbunezgwcdpr
ohvflkatysmgqjqbunezgwcdvr
ohvtlkatyrmiqjxbunezgwcdpi
ohvflkatyskovjxbunezgwcdpr
ohvflkayysmipjxbunezgwcdpu
ohvalkltysmiqjxbunezgecdpr
ohvflkatysmiqjxiunezgnndpr
ohvflkatyomiqjxbbnezgwcdpp
ohvflkatysmiqjxbuoezgncdpy
omvflkvtysmiqjxwunezgwcdpr
ohvflkatynmicjxbunezgwpdpr
ohvflkatyqmaqjxbunezvwcdpr
ohbfhkatysmiqjxbunezgwcdqr
ohvflkatesmiqjvbunezpwcdpr
ohvflkatysmsqjxiunezgwcdhr
ohvfjkatysmwqjxbunezgwcddr
ohvflkanysmiqjxbunwkgwcdpr
ohqflkatysmiqjxbuuezgwcddr
ohvflkatysmvqjxbznlzgwcdpr
ohvflkatysmiqjxbunjzwwqdpr
ohvfjkatysmiqxxbunezgwcupr
chvfxkatysmiqjxxunezgwcdpr
uhvflkatitmiqjxbunezgwcdpr
ohvflbatysmiqjxbuntzgwcdor
ohvflkmtysmmqjxbunexgwcdpr
ohvflsatysmyqjxjunezgwcdpr
ohvfskatysmiqjjbunezgwcdpg
ohvflkatysniqjxbunexgwcrpr
ohvfekatysmiqjxbunedswcdpr
ohvfltatysmjqjxbunezghcdpr
ohvflkatydmiqjxvunezggcdpr
oavflkatysmiqjxtunazgwcdpr
ohvflkltysmiqjxbuzeugwcdpr
ohbflkatysmiqjybuuezgwcdpr
ehvfzkatysmiqjxbuhezgwcdpr
odvflkatssmiqjxbunezgwcdpj
ohvflkatysmiqjzbufezgwbdpr
jhvflkdtysmiqqxbunezgwcdpr
ohvflkatysmiqjwbunengwcnpr
ohvfskatysmiqjxbxuezgwcdpr
ohvflkatysmiqjobvnezgwcrpr
ohvrlkatysmiqjxbwnezgrcdpr
ofvflkatysmiqjxbunezpwcdwr
ohvfxdatyomiqjxbunezgwcdpr
yhvflkatydmiqjxbubezgwcdpr
ohvflkatysdiqjxbuneztwcspr
ohvflkatydmiquxbunezgwcbpr
ohvflkatysmiqcxbukezgwcdwr
ohvflkntasmiqjxbunezghcdpr
lhvflkatysmiqjxbunezqwckpr
ehifikatysmiqjxbunezgwcdpr
ohvflkatysmiqjcbutezgwcdpm
ohvflkatjssiqrxbunezgwcdpr
oyvflkavysmiqjxlunezgwcdpr
orvflkgtysmiqjxbukezgwcdpr
ihvflkatysmiqaxbunpzgwcdpr
ohvflkatusmiqjxbbnezgwchpr
ohvflkatysbiqjxvuneugwcdpr
ohvflkatysmiqjcbungzgwcwpr
ovvflkatysmidjxbunezgscdpr
ohvflqatysmiljxbunfzgwcdpr
ghvfokatysmiqjxbunqzgwcdpr
nxvflkatysmxqjxbunezgwcdpr
ohvflkatysmiqjxbexezgwrdpr
ohvfrkatysmhqjxbuntzgwcdpr
ohvflkvtysmiqjxocnezgwcdpr
ohvglkgtysmiqjxnunezgwcdpr
ohvflkatysmnqjxbunecgwqdpr
oyvflkatysgiqjxbcnezgwcdpr
ofvflkatysmiqjxbunfzgwcdpg
otvflkttysmiqjxbunezgwmdpr
ohvflkvtysmiqjbbunezgzcdpr
ahvflkatysyiqjxbunezvwcdpr
ohiflkatysmydjxbunezgwcdpr
ohvfwkatysmvqjxbunezwwcdpr
ohvflkatysbiqjxbunergwodpr
hhvsdkatysmiqjxbunezgwcdpr
ihvflkwtysmiqjxbunezgacdpr
ohvfljatysmiqcxbunuzgwcdpr
ohvflkatysqiqlwbunezgwcdpr
ohvflkauysmkqjxwunezgwcdpr
ohvflkatysmoqjqbunezgwodpr
ohvslkvtysmipjxbunezgwcdpr
olvflkatysmiujxbunezgwctpr
osvflxatysmiqjxbenezgwcdpr
orvflkhtysmiqjxbinezgwcdpr
ohcflkatystiqjxbunezbwcdpr
ohcflkatyfmifjxbunezgwcdpr
ohvflkatdsmiqjxbrnezgwcdpt
ohvflkatysmiqjxbwnqzawcdpr
oevflkakysmiqjxbunezgwcdpt
ofvflkatysmiqjxbunbqgwcdpr
ohvflkatysmdqjxbunefqwcdpr
ohvklkalysmiqjxbunezgwcepr
ocvflhatysmiqjxbunezzwcdpr
uhvflkatysmiqmxbunezgwcxpr
ohvflkatyshikjhbunezgwcdpr
lbvflkatysmoqjxbunezgwcdpr
ohvflkatssmuqjxbunezgscdpr
ohvflkatysmifyxbuvezgwcdpr
ohvfikatysmiqjxbunezgwfupr
ohvmlkaiysmiqjxqunezgwcdpr
ohvflkatysmiqjxiunpzgwcdpo
lhvflkatysmpqjxbenezgwcdpr
ohvflkatysmiqjobunengwczpr
ohoflkatysniqjxbunezgccdpr
ohvfxkatysmiqjgbunyzgwcdpr
ohvflkytysmiljxbubezgwcdpr
hhvsdkatysmiqjxjunezgwcdpr
ohvflkatysmiqjtuunezgwcdpt
ohvfdkxtysmiqjubunezgwcdpr
ohxflkatysmiyjxbunezgwcdhr
ohvflkatysmiqjibunezgwcppd
ohvflkatysmihjxbunezgwcdhj
ohvflkatysmiqjxronezgwcdvr
ofrflxatysmiqjxbunezgwcdpr
ohvwlkatysmiqjxounezgscdpr
ohvflkatcodiqjxbunezgwcdpr
oqvflkatysmiqjxbunebgwmdpr
ohvflmatysmisjxbunezqwcdpr
ovvflkatysmiqjxbuxezgwcdpe
ohvflkatysmdejxbuneztwcdpr
hhvflkathsmiqjxbwnezgwcdpr
ohkflkatlsmsqjxbunezgwcdpr
ohvflkktysmizjxhunezgwcdpr
ohzflkatysmiqjrbunezgwcdpj
ohuflwatysmiqjxbunezgwcdgr
ohvflkatysmiqvxmunpzgwcdpr
xhvflkwtysmiqjxbunezgwjdpr
whvflkatysmiqjxbunezgzcopr
ohvflkayysmiqjxuznezgwcdpr
khvflkasysmiqjxbunezgwcdpv
ohvflkatylmiqjxbpnozgwcdpr
ohvflkgtysziqjxbunezgwgdpr
ohvfljaiysmiqjxbuvezgwcdpr
ohvflkxtyslizjxbunezgwcdpr
ohzflkatysmiqjxbcnezgwcdar
ohvflkatysmiqjxbisecgwcdpr
shvflkatyjmiqjkbunezgwcdpr
mhvflkatysmiqjxvunezgwcdpk
ohfflkatysmiqjxbunczgwcppr
ohvflkatysmiqjkzunezgwcdpc
ohvflkatysmifjxbuneygwctpr
ohvflkatysmimjbbunezgwcdpe
ohvflkatjsciqjxbunezgwcdpa
ohvxlkatysmitjxbunezswcdpr
ohvslkatfsmiqjxbunezgwudpr
ohvflkatysmiqexbugezgwcdnr
onvflkatysmiqjxkunezgtcdpr
fhsflkalysmiqjxbunezgwcdpr
oyvflkatysmiqjobxnezgwcdpr
ohvflkatysmiqjxbunezswgdvr
phvflkatyymiqjxvunezgwcdpr
oivflzutysmiqjxbunezgwcdpr
ohvflkftysmiqjxbunezkwcopr
ohvflkatysmwnjxbunezgwcdpp
ohvflkatysmiqkxcunezgwndpr
phvklkatysmiqjhbunezgwcdpr
ohvflrawysmiqjxbunhzgwcdpr
ohvflkatysmiqjxbunecgwcdig
ohvflpakysmiqjxbunezgwrdpr
odvflkatykmiqjxbunezglcdpr
ohtflkatysiiqjxblnezgwcdpr
lhvfpkatysmiqjxbupezgwcdpr
ohvflkatdsmiqjpbunezgwcdps
ohvflkztysmiqjxvunezgwjdpr
ohvflbatysmxqoxbunezgwcdpr
ohvklkaigsmiqjxbunezgwcdpr
ohvfgkawysmiqjxbunezgwcdur
ohvflkatyskpqjlbunezgwcdpr
ohvflkatyqmiqjhbupezgwcdpr
ohqflkatysmiqjxzonezgwcdpr
ohxfnkatyymiqjxbunezgwcdpr
ohmflkatpsmiqjxbunezgwcdpw
ohvflkatysmiqjibnnewgwcdpr
vevflkatysmiqjxbunezgwcypr
ohvflkatydmiqwxbungzgwcdpr
ohsrlkatysmiqjxbcnezgwcdpr
ohvflkptyvmiqexbunezgwcdpr
opzflkatysmiqjxrunezgwcdpr
ohvflkitysmiqjxcunezgwcmpr
ohvflkatysmhhjxblnezgwcdpr
ohvflkatysfiqjxbunrzgwmdpr
ohvflkatyamibjxbunezgwcdpf
ohvflkalysmigjxbunezggcdpr
ohvflkatwsmisjxbunezgdcdpr
dhvflkatysmlqjxbunszgwcdpr
ohvflkatysmiqjxbueeygwcbpr
ohvflkatgsmiqjnbunezhwcdpr
svvflkatysmiqjxbunezgwckpr
opvflkatysmiqpxbufezgwcdpr
ohnvlkatysmiqjxbunezglcdpr
phvflkutysjiqjxbunezgwcdpr
ohvflabtysmiqjjbunezgwcdpr
ouvflkatysmiqjsbunezgwcdpk
osvflkatysmijjxbunezgwcypr
owvflkatysmiqjxbukxzgwcdpr
ohvfliatvsmiljxbunezgwcdpr
ohvflkatysmiqjxbumezbwtdpr
ohvflkatyfcicjxbunezgwcdpr
ohvflkatysmiqldbunezgfcdpr
oqvflkatysmiqixkunezgwcdpr
ohvflkatysmiqjxbulezgicdpe
ohvflkatysmiqjxbuniegwcdpl
ohvflkatysmiqjwbunbzgwcdhr
ohvflkatysmiqjdbunezgwwdkr
ohqflkytysmiqjxbunezgwcdpc
ohvflkatysmigjxbunezqwwdpr
ohvfloatysmiqjpbumezgwcdpr
ohvklkathkmiqjxbunezgwcdpr
ohvflkstjsmiqjxbunezgwctpr
ohvvlkatysmiqjxbunewgwcdir
ohnflkatysmiqjxbunszgwcdlr
ohvflkatysmnqjxbunezgxcdlr
ohvfrkatysmiqjxbonezgwcdor
ihvflkatysmiqjxbuneogwcxpr
ohvflkatysmiqjxbunecgwcccr
owvflkatysmivjxbunezgwjdpr
ohvflkgtysmiqjxbunczhwcdpr
ohyqlkatysmiqjxbunezgwcypr
ohvflkatysmiqjvbunezuwcdpw
ohvflkathsmiqmxbuoezgwcdpr
ehvjlkajysmiqjxbunezgwcdpr
ohvflkltysmiqjxblnezgwjdpr
oovflkvtfsmiqjxbunezgwcdpr
olvfzkatysmiqjxyunezgwcdpr
ohvflkatysqitjxbunezgncdpr
yhvflkatysmkqjxbunazgwcdpr
zlvolkatysmiqjxbunezgwcdpr
ohvflpatysmiqjxbunezgwcapb
ohvflkatysmuqjxbunezgfcdur"""
