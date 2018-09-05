let fs = require("fs");
let moment = require("moment-timezone");

// `names` contains a mapping from "flat" zone names to their
// corresponding IANA/Olsen name, e.g. europe_london -> 'Europe/London'.
// There are entries for canonical zone names as well as aliases and
// deprecated names.
let names = moment.tz._names;
// `links` contains a mapping from flat zone aliases and deprecated
// names to their canonical sanitized name, e.g. europe_belfast ->
// 'europe_london'. HOWEVER, there are also mappings from SOME canonical
// sanitized names to other names, like europe_london -> 'gb-eire'. It's
// not clear why these latter entries exist.
let links = moment.tz._links;
// `zones` contains a mapping from flat zone names to actual zone data,
// e.g. europe_london -> 'Europe/London|GMT BST BDST|0 -1 ...'. It
// appears to only contain mappings for canonical zone names.
let zones = moment.tz._zones;

let sanitize = (name) => {
  return name.
    toLowerCase().
    replace(/gmt\+/, "gmt_plus_").
    replace(/gmt\-/, "gmt_minus_").
    replace(/gmt0/, "gmt_0").
    replace(/-/g, "_");
};

// Data file
let timeZones = [], timeZoneData = [], tests = [];

// Date file: Populate canonical zone data.
for (let name in zones) {
  let zone = zones[name];
  let link = links[name];
  let fullName = names[name];

  name = sanitize(name);
  timeZoneData.push(`${name}_l = unpack "${zone}"`);
  timeZones.push({name, fullName, link: false});
  tests.push(`
        , test "time zone ${name}" <|
            \\() ->
                let
                    tzName =
                        name Data.${name}_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
`);
}

// Data file: Populate aliases.
for (let name in links) {
  let link = links[name];
  // Only add an alias when it refers to a canonical zone.
  if (zones[link]) {
    timeZones.push({
      name: sanitize(name),
      fullName: names[name],
      timeZone: sanitize(link),
      link: true
    });
  }
}

timeZones.sort((a, b) => {
  return a.name > b.name ? 1 : -1;
});
timeZoneData.sort();

// TimeZones file
let docs = [], fns = [], all = [];
for (let i = 0; i < timeZones.length; i++) {
  let timeZone = timeZones[i];

  docs.push(timeZone.name);
  all.push(`("${timeZone.fullName}", ${timeZone.name})`);

  if (timeZone.link) {
    fns.push(`
{-| ${timeZone.fullName} -}
${timeZone.name} : () -> TimeZone
${timeZone.name} () = link "${timeZone.fullName}" ${timeZone.timeZone}_l`);
  } else {
    fns.push(`
{-| ${timeZone.fullName} -}
${timeZone.name} : () -> TimeZone
${timeZone.name} () = ${timeZone.name}_l`);
  }
}

fs.open("../src/Time/TimeZoneData.elm", "w", (err, fd) => {
  let content = `
module Time.TimeZoneData exposing (..)

import String
import Parser exposing (deadEndsToString)
import Time.TimeZone exposing (TimeZone, setName, errorZone)


unpack : String -> TimeZone
unpack data =
    case Time.TimeZone.unpack data of
        Err errors ->
            Time.TimeZone.errorZone <| "failed to parse zone '" ++ data ++ "': " ++ (deadEndsToString errors)

        Ok zone ->
            zone


link : String -> TimeZone -> TimeZone
link name tz =
    setName name tz


-- Data
-- ----
${timeZoneData.join("\n")}`.trim();

  fs.write(fd, content, (err) => {
    if (err) throw new err;
  });
});

// Chunk `all` to prevent stack overflow in Elm 0.18 with large list literals.
let all_chunks = [], all_chunk_size = 50;
for (let i = 0; i < all.length; i += all_chunk_size) {
  let slice = all.slice(i, i + all_chunk_size);
  all_chunks.push("[ " + slice.join("\n              , ") + "\n              ]");
}

fs.open("../src/Time/TimeZones.elm", "w", (err, fd) => {
  let content = `
module Time.TimeZones exposing (..)

{-| This module contains TimeZone definitions for all Timezones as they
are defined in the IANA zoneinfo database.

TimeZone data is parsed lazily so, in order to retrieve a zone's value you
must apply \`()\` to it.  For example:

    import Time.DateTime exposing (epoch, toTimestamp)
    import Time.TimeZone exposing (abbreviation)
    import Time.TimeZones exposing (europe_bucharest)

    let
        timezone = europe_bucharest ()
    in
        abbreviation (toTimestamp epoch) timezone

@docs all, fromName, ${docs.join(", ")}
-}

import Dict exposing (Dict)
import Time.TimeZone exposing (TimeZone)
import Time.TimeZoneData exposing (..)


-- TimeZones
-- ---------
${fns.join("\n")}


-- Utils
-- -----
{-| A mapping from TimeZone names to their respective functions.  Use
this to look up TimeZones by name. -}
all : Dict String (() -> TimeZone)
all =
    Dict.fromList <|
        List.concat
            [ ${all_chunks.join("\n            , ")}
            ]


{-| Look up a TimeZone by name. -}
fromName : String -> Maybe TimeZone
fromName name =
  case Dict.get name all of
    Nothing ->
      Nothing

    Just f ->
      Just (f ())
`.trim();

  fs.write(fd, content, (err) => {
    if (err) throw new err;
  });
});

fs.open("../tests/TestTimeZoneData.elm", "w", (err, fd) => {
  let content = `
module TestTimeZoneData exposing (checkAllValid)

import Expect exposing (Expectation)
import Test exposing (..)
import Time.TimeZone exposing (name)
import Time.TimeZoneData as Data


checkAllValid : Test
checkAllValid =
    describe "Time.TimeZoneData" <|
        [ test "skip" <| \\() -> Expect.true "skip" True
        ${tests.join('')}
        ]
`.trim();

  fs.write(fd, content, (err) => {
    if(err) throw new err;
  });
});
