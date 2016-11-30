let fs = require("fs");
let moment = require("moment-timezone");
let names = moment.tz._names;
let links = moment.tz._links;
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
let timeZones = [], timeZoneData = [];
for (let name in zones) {
  let zone = zones[name];
  let link = links[name];
  let fullName = names[name];

  name = sanitize(name);
  timeZoneData.push(`${name}_l = unpack "${zone}"`);
  timeZones.push({name, fullName, link: false});
}

for (let name in links) {
  let link = links[name];
  if (zones[link]) {
    timeZones.push({
      name: sanitize(name),
      fullName: names[link],
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
${timeZone.name} () = force (link "${timeZone.fullName}" ${timeZone.timeZone}_l)`);
  } else {
    fns.push(`
{-| ${timeZone.fullName} -}
${timeZone.name} : () -> TimeZone
${timeZone.name} () = force ${timeZone.name}_l`);
  }
}

fs.open("../src/Time/TimeZoneData.elm", "w", (err, fd) => {
  let content = `
module Time.TimeZoneData exposing (..)

import Lazy exposing (Lazy, force, lazy)
import String
import Time.TimeZone exposing (TimeZone, setName)


unpack : String -> Lazy TimeZone
unpack data =
    let
        helper () =
            case Time.TimeZone.unpack data of
                Err errors ->
                    let
                        messages =
                            String.join " or " errors
                    in
                        Debug.crash ("failed to parse zone '" ++ data ++ "': " ++ messages)

                Ok zone ->
                    zone
    in
        lazy helper


link : String -> Lazy TimeZone -> Lazy TimeZone
link link lz =
    Lazy.map (setName link) lz


-- Data
-- ----
${timeZoneData.join("\n")}`.trim();

  fs.write(fd, content, (err) => {
    if (err) throw new err;
  });
});

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
import Lazy exposing (Lazy, force)
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
all = Dict.fromList [${all.join(", ")}]


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
