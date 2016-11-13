let fs = require("fs");
let moment = require("moment-timezone");
let zones = moment.tz._zones;
let lines = [`
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
-- ----`];


for (let name in zones) {
  let zone = zones[name];

  name = name.
    toLowerCase().
    replace(/gmt\+/, "gmt_plus_").
    replace(/gmt\-/, "gmt_minus_").
    replace(/-/g, "_");

  lines.push(`${name}_l = unpack "${zone}"`);
}

lines.sort();

fs.open("../src/Time/TimeZoneData.elm", "w", (err, fd) => {
  fs.write(fd, lines.join("\n"), (err) => {
    if (err) throw new err;
  });
});
