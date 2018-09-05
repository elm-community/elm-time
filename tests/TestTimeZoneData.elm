module TestTimeZoneData exposing (checkAllValid)

import Expect exposing (Expectation)
import Test exposing (..)
import Time.TimeZone exposing (name)
import Time.TimeZoneData as Data


checkAllValid : Test
checkAllValid =
    describe "Time.TimeZoneData" <|
        [ test "skip" <| \() -> Expect.true "skip" True
        , test "time zone africa_abidjan" <|
            \() ->
                let
                    tzName =
                        name Data.africa_abidjan_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone africa_accra" <|
            \() ->
                let
                    tzName =
                        name Data.africa_accra_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone africa_nairobi" <|
            \() ->
                let
                    tzName =
                        name Data.africa_nairobi_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone africa_algiers" <|
            \() ->
                let
                    tzName =
                        name Data.africa_algiers_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone africa_lagos" <|
            \() ->
                let
                    tzName =
                        name Data.africa_lagos_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone africa_bissau" <|
            \() ->
                let
                    tzName =
                        name Data.africa_bissau_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone africa_maputo" <|
            \() ->
                let
                    tzName =
                        name Data.africa_maputo_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone africa_cairo" <|
            \() ->
                let
                    tzName =
                        name Data.africa_cairo_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone africa_casablanca" <|
            \() ->
                let
                    tzName =
                        name Data.africa_casablanca_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone africa_ceuta" <|
            \() ->
                let
                    tzName =
                        name Data.africa_ceuta_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone africa_el_aaiun" <|
            \() ->
                let
                    tzName =
                        name Data.africa_el_aaiun_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone africa_johannesburg" <|
            \() ->
                let
                    tzName =
                        name Data.africa_johannesburg_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone africa_khartoum" <|
            \() ->
                let
                    tzName =
                        name Data.africa_khartoum_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone africa_monrovia" <|
            \() ->
                let
                    tzName =
                        name Data.africa_monrovia_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone africa_ndjamena" <|
            \() ->
                let
                    tzName =
                        name Data.africa_ndjamena_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone africa_tripoli" <|
            \() ->
                let
                    tzName =
                        name Data.africa_tripoli_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone africa_tunis" <|
            \() ->
                let
                    tzName =
                        name Data.africa_tunis_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone africa_windhoek" <|
            \() ->
                let
                    tzName =
                        name Data.africa_windhoek_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_adak" <|
            \() ->
                let
                    tzName =
                        name Data.america_adak_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_anchorage" <|
            \() ->
                let
                    tzName =
                        name Data.america_anchorage_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_port_of_spain" <|
            \() ->
                let
                    tzName =
                        name Data.america_port_of_spain_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_araguaina" <|
            \() ->
                let
                    tzName =
                        name Data.america_araguaina_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_argentina_buenos_aires" <|
            \() ->
                let
                    tzName =
                        name Data.america_argentina_buenos_aires_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_argentina_catamarca" <|
            \() ->
                let
                    tzName =
                        name Data.america_argentina_catamarca_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_argentina_cordoba" <|
            \() ->
                let
                    tzName =
                        name Data.america_argentina_cordoba_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_argentina_jujuy" <|
            \() ->
                let
                    tzName =
                        name Data.america_argentina_jujuy_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_argentina_la_rioja" <|
            \() ->
                let
                    tzName =
                        name Data.america_argentina_la_rioja_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_argentina_mendoza" <|
            \() ->
                let
                    tzName =
                        name Data.america_argentina_mendoza_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_argentina_rio_gallegos" <|
            \() ->
                let
                    tzName =
                        name Data.america_argentina_rio_gallegos_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_argentina_salta" <|
            \() ->
                let
                    tzName =
                        name Data.america_argentina_salta_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_argentina_san_juan" <|
            \() ->
                let
                    tzName =
                        name Data.america_argentina_san_juan_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_argentina_san_luis" <|
            \() ->
                let
                    tzName =
                        name Data.america_argentina_san_luis_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_argentina_tucuman" <|
            \() ->
                let
                    tzName =
                        name Data.america_argentina_tucuman_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_argentina_ushuaia" <|
            \() ->
                let
                    tzName =
                        name Data.america_argentina_ushuaia_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_curacao" <|
            \() ->
                let
                    tzName =
                        name Data.america_curacao_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_asuncion" <|
            \() ->
                let
                    tzName =
                        name Data.america_asuncion_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_atikokan" <|
            \() ->
                let
                    tzName =
                        name Data.america_atikokan_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_bahia" <|
            \() ->
                let
                    tzName =
                        name Data.america_bahia_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_bahia_banderas" <|
            \() ->
                let
                    tzName =
                        name Data.america_bahia_banderas_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_barbados" <|
            \() ->
                let
                    tzName =
                        name Data.america_barbados_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_belem" <|
            \() ->
                let
                    tzName =
                        name Data.america_belem_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_belize" <|
            \() ->
                let
                    tzName =
                        name Data.america_belize_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_blanc_sablon" <|
            \() ->
                let
                    tzName =
                        name Data.america_blanc_sablon_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_boa_vista" <|
            \() ->
                let
                    tzName =
                        name Data.america_boa_vista_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_bogota" <|
            \() ->
                let
                    tzName =
                        name Data.america_bogota_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_boise" <|
            \() ->
                let
                    tzName =
                        name Data.america_boise_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_cambridge_bay" <|
            \() ->
                let
                    tzName =
                        name Data.america_cambridge_bay_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_campo_grande" <|
            \() ->
                let
                    tzName =
                        name Data.america_campo_grande_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_cancun" <|
            \() ->
                let
                    tzName =
                        name Data.america_cancun_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_caracas" <|
            \() ->
                let
                    tzName =
                        name Data.america_caracas_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_cayenne" <|
            \() ->
                let
                    tzName =
                        name Data.america_cayenne_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_panama" <|
            \() ->
                let
                    tzName =
                        name Data.america_panama_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_chicago" <|
            \() ->
                let
                    tzName =
                        name Data.america_chicago_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_chihuahua" <|
            \() ->
                let
                    tzName =
                        name Data.america_chihuahua_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_costa_rica" <|
            \() ->
                let
                    tzName =
                        name Data.america_costa_rica_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_creston" <|
            \() ->
                let
                    tzName =
                        name Data.america_creston_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_cuiaba" <|
            \() ->
                let
                    tzName =
                        name Data.america_cuiaba_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_danmarkshavn" <|
            \() ->
                let
                    tzName =
                        name Data.america_danmarkshavn_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_dawson" <|
            \() ->
                let
                    tzName =
                        name Data.america_dawson_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_dawson_creek" <|
            \() ->
                let
                    tzName =
                        name Data.america_dawson_creek_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_denver" <|
            \() ->
                let
                    tzName =
                        name Data.america_denver_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_detroit" <|
            \() ->
                let
                    tzName =
                        name Data.america_detroit_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_edmonton" <|
            \() ->
                let
                    tzName =
                        name Data.america_edmonton_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_eirunepe" <|
            \() ->
                let
                    tzName =
                        name Data.america_eirunepe_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_el_salvador" <|
            \() ->
                let
                    tzName =
                        name Data.america_el_salvador_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_tijuana" <|
            \() ->
                let
                    tzName =
                        name Data.america_tijuana_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_fort_nelson" <|
            \() ->
                let
                    tzName =
                        name Data.america_fort_nelson_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_fort_wayne" <|
            \() ->
                let
                    tzName =
                        name Data.america_fort_wayne_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_fortaleza" <|
            \() ->
                let
                    tzName =
                        name Data.america_fortaleza_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_glace_bay" <|
            \() ->
                let
                    tzName =
                        name Data.america_glace_bay_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_godthab" <|
            \() ->
                let
                    tzName =
                        name Data.america_godthab_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_goose_bay" <|
            \() ->
                let
                    tzName =
                        name Data.america_goose_bay_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_grand_turk" <|
            \() ->
                let
                    tzName =
                        name Data.america_grand_turk_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_guatemala" <|
            \() ->
                let
                    tzName =
                        name Data.america_guatemala_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_guayaquil" <|
            \() ->
                let
                    tzName =
                        name Data.america_guayaquil_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_guyana" <|
            \() ->
                let
                    tzName =
                        name Data.america_guyana_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_halifax" <|
            \() ->
                let
                    tzName =
                        name Data.america_halifax_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_havana" <|
            \() ->
                let
                    tzName =
                        name Data.america_havana_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_hermosillo" <|
            \() ->
                let
                    tzName =
                        name Data.america_hermosillo_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_indiana_knox" <|
            \() ->
                let
                    tzName =
                        name Data.america_indiana_knox_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_indiana_marengo" <|
            \() ->
                let
                    tzName =
                        name Data.america_indiana_marengo_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_indiana_petersburg" <|
            \() ->
                let
                    tzName =
                        name Data.america_indiana_petersburg_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_indiana_tell_city" <|
            \() ->
                let
                    tzName =
                        name Data.america_indiana_tell_city_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_indiana_vevay" <|
            \() ->
                let
                    tzName =
                        name Data.america_indiana_vevay_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_indiana_vincennes" <|
            \() ->
                let
                    tzName =
                        name Data.america_indiana_vincennes_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_indiana_winamac" <|
            \() ->
                let
                    tzName =
                        name Data.america_indiana_winamac_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_inuvik" <|
            \() ->
                let
                    tzName =
                        name Data.america_inuvik_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_iqaluit" <|
            \() ->
                let
                    tzName =
                        name Data.america_iqaluit_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_jamaica" <|
            \() ->
                let
                    tzName =
                        name Data.america_jamaica_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_juneau" <|
            \() ->
                let
                    tzName =
                        name Data.america_juneau_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_kentucky_louisville" <|
            \() ->
                let
                    tzName =
                        name Data.america_kentucky_louisville_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_kentucky_monticello" <|
            \() ->
                let
                    tzName =
                        name Data.america_kentucky_monticello_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_la_paz" <|
            \() ->
                let
                    tzName =
                        name Data.america_la_paz_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_lima" <|
            \() ->
                let
                    tzName =
                        name Data.america_lima_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_los_angeles" <|
            \() ->
                let
                    tzName =
                        name Data.america_los_angeles_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_maceio" <|
            \() ->
                let
                    tzName =
                        name Data.america_maceio_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_managua" <|
            \() ->
                let
                    tzName =
                        name Data.america_managua_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_manaus" <|
            \() ->
                let
                    tzName =
                        name Data.america_manaus_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_martinique" <|
            \() ->
                let
                    tzName =
                        name Data.america_martinique_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_matamoros" <|
            \() ->
                let
                    tzName =
                        name Data.america_matamoros_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_mazatlan" <|
            \() ->
                let
                    tzName =
                        name Data.america_mazatlan_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_menominee" <|
            \() ->
                let
                    tzName =
                        name Data.america_menominee_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_merida" <|
            \() ->
                let
                    tzName =
                        name Data.america_merida_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_metlakatla" <|
            \() ->
                let
                    tzName =
                        name Data.america_metlakatla_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_mexico_city" <|
            \() ->
                let
                    tzName =
                        name Data.america_mexico_city_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_miquelon" <|
            \() ->
                let
                    tzName =
                        name Data.america_miquelon_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_moncton" <|
            \() ->
                let
                    tzName =
                        name Data.america_moncton_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_monterrey" <|
            \() ->
                let
                    tzName =
                        name Data.america_monterrey_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_montevideo" <|
            \() ->
                let
                    tzName =
                        name Data.america_montevideo_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_toronto" <|
            \() ->
                let
                    tzName =
                        name Data.america_toronto_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_nassau" <|
            \() ->
                let
                    tzName =
                        name Data.america_nassau_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_new_york" <|
            \() ->
                let
                    tzName =
                        name Data.america_new_york_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_nipigon" <|
            \() ->
                let
                    tzName =
                        name Data.america_nipigon_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_nome" <|
            \() ->
                let
                    tzName =
                        name Data.america_nome_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_noronha" <|
            \() ->
                let
                    tzName =
                        name Data.america_noronha_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_north_dakota_beulah" <|
            \() ->
                let
                    tzName =
                        name Data.america_north_dakota_beulah_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_north_dakota_center" <|
            \() ->
                let
                    tzName =
                        name Data.america_north_dakota_center_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_north_dakota_new_salem" <|
            \() ->
                let
                    tzName =
                        name Data.america_north_dakota_new_salem_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_ojinaga" <|
            \() ->
                let
                    tzName =
                        name Data.america_ojinaga_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_pangnirtung" <|
            \() ->
                let
                    tzName =
                        name Data.america_pangnirtung_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_paramaribo" <|
            \() ->
                let
                    tzName =
                        name Data.america_paramaribo_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_phoenix" <|
            \() ->
                let
                    tzName =
                        name Data.america_phoenix_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_port_au_prince" <|
            \() ->
                let
                    tzName =
                        name Data.america_port_au_prince_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_rio_branco" <|
            \() ->
                let
                    tzName =
                        name Data.america_rio_branco_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_porto_velho" <|
            \() ->
                let
                    tzName =
                        name Data.america_porto_velho_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_puerto_rico" <|
            \() ->
                let
                    tzName =
                        name Data.america_puerto_rico_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_rainy_river" <|
            \() ->
                let
                    tzName =
                        name Data.america_rainy_river_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_rankin_inlet" <|
            \() ->
                let
                    tzName =
                        name Data.america_rankin_inlet_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_recife" <|
            \() ->
                let
                    tzName =
                        name Data.america_recife_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_regina" <|
            \() ->
                let
                    tzName =
                        name Data.america_regina_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_resolute" <|
            \() ->
                let
                    tzName =
                        name Data.america_resolute_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_santarem" <|
            \() ->
                let
                    tzName =
                        name Data.america_santarem_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_santiago" <|
            \() ->
                let
                    tzName =
                        name Data.america_santiago_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_santo_domingo" <|
            \() ->
                let
                    tzName =
                        name Data.america_santo_domingo_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_sao_paulo" <|
            \() ->
                let
                    tzName =
                        name Data.america_sao_paulo_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_scoresbysund" <|
            \() ->
                let
                    tzName =
                        name Data.america_scoresbysund_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_sitka" <|
            \() ->
                let
                    tzName =
                        name Data.america_sitka_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_st_johns" <|
            \() ->
                let
                    tzName =
                        name Data.america_st_johns_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_swift_current" <|
            \() ->
                let
                    tzName =
                        name Data.america_swift_current_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_tegucigalpa" <|
            \() ->
                let
                    tzName =
                        name Data.america_tegucigalpa_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_thule" <|
            \() ->
                let
                    tzName =
                        name Data.america_thule_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_thunder_bay" <|
            \() ->
                let
                    tzName =
                        name Data.america_thunder_bay_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_vancouver" <|
            \() ->
                let
                    tzName =
                        name Data.america_vancouver_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_whitehorse" <|
            \() ->
                let
                    tzName =
                        name Data.america_whitehorse_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_winnipeg" <|
            \() ->
                let
                    tzName =
                        name Data.america_winnipeg_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_yakutat" <|
            \() ->
                let
                    tzName =
                        name Data.america_yakutat_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone america_yellowknife" <|
            \() ->
                let
                    tzName =
                        name Data.america_yellowknife_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone antarctica_casey" <|
            \() ->
                let
                    tzName =
                        name Data.antarctica_casey_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone antarctica_davis" <|
            \() ->
                let
                    tzName =
                        name Data.antarctica_davis_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone antarctica_dumontdurville" <|
            \() ->
                let
                    tzName =
                        name Data.antarctica_dumontdurville_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone antarctica_macquarie" <|
            \() ->
                let
                    tzName =
                        name Data.antarctica_macquarie_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone antarctica_mawson" <|
            \() ->
                let
                    tzName =
                        name Data.antarctica_mawson_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone pacific_auckland" <|
            \() ->
                let
                    tzName =
                        name Data.pacific_auckland_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone antarctica_palmer" <|
            \() ->
                let
                    tzName =
                        name Data.antarctica_palmer_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone antarctica_rothera" <|
            \() ->
                let
                    tzName =
                        name Data.antarctica_rothera_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone antarctica_syowa" <|
            \() ->
                let
                    tzName =
                        name Data.antarctica_syowa_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone antarctica_troll" <|
            \() ->
                let
                    tzName =
                        name Data.antarctica_troll_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone antarctica_vostok" <|
            \() ->
                let
                    tzName =
                        name Data.antarctica_vostok_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone europe_oslo" <|
            \() ->
                let
                    tzName =
                        name Data.europe_oslo_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone asia_riyadh" <|
            \() ->
                let
                    tzName =
                        name Data.asia_riyadh_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone asia_almaty" <|
            \() ->
                let
                    tzName =
                        name Data.asia_almaty_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone asia_amman" <|
            \() ->
                let
                    tzName =
                        name Data.asia_amman_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone asia_anadyr" <|
            \() ->
                let
                    tzName =
                        name Data.asia_anadyr_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone asia_aqtau" <|
            \() ->
                let
                    tzName =
                        name Data.asia_aqtau_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone asia_aqtobe" <|
            \() ->
                let
                    tzName =
                        name Data.asia_aqtobe_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone asia_ashgabat" <|
            \() ->
                let
                    tzName =
                        name Data.asia_ashgabat_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone asia_baghdad" <|
            \() ->
                let
                    tzName =
                        name Data.asia_baghdad_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone asia_qatar" <|
            \() ->
                let
                    tzName =
                        name Data.asia_qatar_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone asia_baku" <|
            \() ->
                let
                    tzName =
                        name Data.asia_baku_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone asia_bangkok" <|
            \() ->
                let
                    tzName =
                        name Data.asia_bangkok_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone asia_barnaul" <|
            \() ->
                let
                    tzName =
                        name Data.asia_barnaul_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone asia_beirut" <|
            \() ->
                let
                    tzName =
                        name Data.asia_beirut_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone asia_bishkek" <|
            \() ->
                let
                    tzName =
                        name Data.asia_bishkek_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone asia_brunei" <|
            \() ->
                let
                    tzName =
                        name Data.asia_brunei_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone asia_kolkata" <|
            \() ->
                let
                    tzName =
                        name Data.asia_kolkata_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone asia_chita" <|
            \() ->
                let
                    tzName =
                        name Data.asia_chita_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone asia_choibalsan" <|
            \() ->
                let
                    tzName =
                        name Data.asia_choibalsan_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone asia_shanghai" <|
            \() ->
                let
                    tzName =
                        name Data.asia_shanghai_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone asia_colombo" <|
            \() ->
                let
                    tzName =
                        name Data.asia_colombo_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone asia_dhaka" <|
            \() ->
                let
                    tzName =
                        name Data.asia_dhaka_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone asia_damascus" <|
            \() ->
                let
                    tzName =
                        name Data.asia_damascus_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone asia_dili" <|
            \() ->
                let
                    tzName =
                        name Data.asia_dili_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone asia_dubai" <|
            \() ->
                let
                    tzName =
                        name Data.asia_dubai_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone asia_dushanbe" <|
            \() ->
                let
                    tzName =
                        name Data.asia_dushanbe_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone asia_famagusta" <|
            \() ->
                let
                    tzName =
                        name Data.asia_famagusta_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone asia_gaza" <|
            \() ->
                let
                    tzName =
                        name Data.asia_gaza_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone asia_hebron" <|
            \() ->
                let
                    tzName =
                        name Data.asia_hebron_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone asia_ho_chi_minh" <|
            \() ->
                let
                    tzName =
                        name Data.asia_ho_chi_minh_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone asia_hong_kong" <|
            \() ->
                let
                    tzName =
                        name Data.asia_hong_kong_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone asia_hovd" <|
            \() ->
                let
                    tzName =
                        name Data.asia_hovd_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone asia_irkutsk" <|
            \() ->
                let
                    tzName =
                        name Data.asia_irkutsk_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone europe_istanbul" <|
            \() ->
                let
                    tzName =
                        name Data.europe_istanbul_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone asia_jakarta" <|
            \() ->
                let
                    tzName =
                        name Data.asia_jakarta_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone asia_jayapura" <|
            \() ->
                let
                    tzName =
                        name Data.asia_jayapura_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone asia_jerusalem" <|
            \() ->
                let
                    tzName =
                        name Data.asia_jerusalem_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone asia_kabul" <|
            \() ->
                let
                    tzName =
                        name Data.asia_kabul_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone asia_kamchatka" <|
            \() ->
                let
                    tzName =
                        name Data.asia_kamchatka_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone asia_karachi" <|
            \() ->
                let
                    tzName =
                        name Data.asia_karachi_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone asia_urumqi" <|
            \() ->
                let
                    tzName =
                        name Data.asia_urumqi_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone asia_kathmandu" <|
            \() ->
                let
                    tzName =
                        name Data.asia_kathmandu_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone asia_khandyga" <|
            \() ->
                let
                    tzName =
                        name Data.asia_khandyga_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone asia_krasnoyarsk" <|
            \() ->
                let
                    tzName =
                        name Data.asia_krasnoyarsk_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone asia_kuala_lumpur" <|
            \() ->
                let
                    tzName =
                        name Data.asia_kuala_lumpur_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone asia_kuching" <|
            \() ->
                let
                    tzName =
                        name Data.asia_kuching_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone asia_macau" <|
            \() ->
                let
                    tzName =
                        name Data.asia_macau_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone asia_magadan" <|
            \() ->
                let
                    tzName =
                        name Data.asia_magadan_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone asia_makassar" <|
            \() ->
                let
                    tzName =
                        name Data.asia_makassar_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone asia_manila" <|
            \() ->
                let
                    tzName =
                        name Data.asia_manila_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone asia_nicosia" <|
            \() ->
                let
                    tzName =
                        name Data.asia_nicosia_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone asia_novokuznetsk" <|
            \() ->
                let
                    tzName =
                        name Data.asia_novokuznetsk_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone asia_novosibirsk" <|
            \() ->
                let
                    tzName =
                        name Data.asia_novosibirsk_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone asia_omsk" <|
            \() ->
                let
                    tzName =
                        name Data.asia_omsk_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone asia_oral" <|
            \() ->
                let
                    tzName =
                        name Data.asia_oral_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone asia_pontianak" <|
            \() ->
                let
                    tzName =
                        name Data.asia_pontianak_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone asia_pyongyang" <|
            \() ->
                let
                    tzName =
                        name Data.asia_pyongyang_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone asia_qyzylorda" <|
            \() ->
                let
                    tzName =
                        name Data.asia_qyzylorda_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone asia_rangoon" <|
            \() ->
                let
                    tzName =
                        name Data.asia_rangoon_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone asia_sakhalin" <|
            \() ->
                let
                    tzName =
                        name Data.asia_sakhalin_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone asia_samarkand" <|
            \() ->
                let
                    tzName =
                        name Data.asia_samarkand_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone asia_seoul" <|
            \() ->
                let
                    tzName =
                        name Data.asia_seoul_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone asia_singapore" <|
            \() ->
                let
                    tzName =
                        name Data.asia_singapore_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone asia_srednekolymsk" <|
            \() ->
                let
                    tzName =
                        name Data.asia_srednekolymsk_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone asia_taipei" <|
            \() ->
                let
                    tzName =
                        name Data.asia_taipei_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone asia_tashkent" <|
            \() ->
                let
                    tzName =
                        name Data.asia_tashkent_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone asia_tbilisi" <|
            \() ->
                let
                    tzName =
                        name Data.asia_tbilisi_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone asia_tehran" <|
            \() ->
                let
                    tzName =
                        name Data.asia_tehran_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone asia_thimphu" <|
            \() ->
                let
                    tzName =
                        name Data.asia_thimphu_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone asia_tokyo" <|
            \() ->
                let
                    tzName =
                        name Data.asia_tokyo_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone asia_tomsk" <|
            \() ->
                let
                    tzName =
                        name Data.asia_tomsk_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone asia_ulaanbaatar" <|
            \() ->
                let
                    tzName =
                        name Data.asia_ulaanbaatar_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone asia_ust_nera" <|
            \() ->
                let
                    tzName =
                        name Data.asia_ust_nera_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone asia_vladivostok" <|
            \() ->
                let
                    tzName =
                        name Data.asia_vladivostok_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone asia_yakutsk" <|
            \() ->
                let
                    tzName =
                        name Data.asia_yakutsk_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone asia_yekaterinburg" <|
            \() ->
                let
                    tzName =
                        name Data.asia_yekaterinburg_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone asia_yerevan" <|
            \() ->
                let
                    tzName =
                        name Data.asia_yerevan_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone atlantic_azores" <|
            \() ->
                let
                    tzName =
                        name Data.atlantic_azores_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone atlantic_bermuda" <|
            \() ->
                let
                    tzName =
                        name Data.atlantic_bermuda_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone atlantic_canary" <|
            \() ->
                let
                    tzName =
                        name Data.atlantic_canary_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone atlantic_cape_verde" <|
            \() ->
                let
                    tzName =
                        name Data.atlantic_cape_verde_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone atlantic_faroe" <|
            \() ->
                let
                    tzName =
                        name Data.atlantic_faroe_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone atlantic_madeira" <|
            \() ->
                let
                    tzName =
                        name Data.atlantic_madeira_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone atlantic_reykjavik" <|
            \() ->
                let
                    tzName =
                        name Data.atlantic_reykjavik_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone atlantic_south_georgia" <|
            \() ->
                let
                    tzName =
                        name Data.atlantic_south_georgia_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone atlantic_stanley" <|
            \() ->
                let
                    tzName =
                        name Data.atlantic_stanley_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone australia_sydney" <|
            \() ->
                let
                    tzName =
                        name Data.australia_sydney_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone australia_adelaide" <|
            \() ->
                let
                    tzName =
                        name Data.australia_adelaide_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone australia_brisbane" <|
            \() ->
                let
                    tzName =
                        name Data.australia_brisbane_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone australia_broken_hill" <|
            \() ->
                let
                    tzName =
                        name Data.australia_broken_hill_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone australia_currie" <|
            \() ->
                let
                    tzName =
                        name Data.australia_currie_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone australia_darwin" <|
            \() ->
                let
                    tzName =
                        name Data.australia_darwin_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone australia_eucla" <|
            \() ->
                let
                    tzName =
                        name Data.australia_eucla_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone australia_hobart" <|
            \() ->
                let
                    tzName =
                        name Data.australia_hobart_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone australia_lord_howe" <|
            \() ->
                let
                    tzName =
                        name Data.australia_lord_howe_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone australia_lindeman" <|
            \() ->
                let
                    tzName =
                        name Data.australia_lindeman_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone australia_melbourne" <|
            \() ->
                let
                    tzName =
                        name Data.australia_melbourne_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone australia_perth" <|
            \() ->
                let
                    tzName =
                        name Data.australia_perth_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone cet" <|
            \() ->
                let
                    tzName =
                        name Data.cet_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone cst6cdt" <|
            \() ->
                let
                    tzName =
                        name Data.cst6cdt_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone pacific_easter" <|
            \() ->
                let
                    tzName =
                        name Data.pacific_easter_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone eet" <|
            \() ->
                let
                    tzName =
                        name Data.eet_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone est" <|
            \() ->
                let
                    tzName =
                        name Data.est_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone est5edt" <|
            \() ->
                let
                    tzName =
                        name Data.est5edt_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone europe_dublin" <|
            \() ->
                let
                    tzName =
                        name Data.europe_dublin_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone etc_gmt_plus_0" <|
            \() ->
                let
                    tzName =
                        name Data.etc_gmt_plus_0_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone etc_gmt_plus_1" <|
            \() ->
                let
                    tzName =
                        name Data.etc_gmt_plus_1_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone etc_gmt_plus_10" <|
            \() ->
                let
                    tzName =
                        name Data.etc_gmt_plus_10_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone etc_gmt_plus_11" <|
            \() ->
                let
                    tzName =
                        name Data.etc_gmt_plus_11_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone etc_gmt_plus_12" <|
            \() ->
                let
                    tzName =
                        name Data.etc_gmt_plus_12_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone etc_gmt_plus_2" <|
            \() ->
                let
                    tzName =
                        name Data.etc_gmt_plus_2_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone etc_gmt_plus_3" <|
            \() ->
                let
                    tzName =
                        name Data.etc_gmt_plus_3_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone etc_gmt_plus_4" <|
            \() ->
                let
                    tzName =
                        name Data.etc_gmt_plus_4_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone etc_gmt_plus_5" <|
            \() ->
                let
                    tzName =
                        name Data.etc_gmt_plus_5_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone etc_gmt_plus_6" <|
            \() ->
                let
                    tzName =
                        name Data.etc_gmt_plus_6_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone etc_gmt_plus_7" <|
            \() ->
                let
                    tzName =
                        name Data.etc_gmt_plus_7_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone etc_gmt_plus_8" <|
            \() ->
                let
                    tzName =
                        name Data.etc_gmt_plus_8_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone etc_gmt_plus_9" <|
            \() ->
                let
                    tzName =
                        name Data.etc_gmt_plus_9_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone etc_gmt_minus_1" <|
            \() ->
                let
                    tzName =
                        name Data.etc_gmt_minus_1_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone etc_gmt_minus_10" <|
            \() ->
                let
                    tzName =
                        name Data.etc_gmt_minus_10_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone etc_gmt_minus_11" <|
            \() ->
                let
                    tzName =
                        name Data.etc_gmt_minus_11_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone etc_gmt_minus_12" <|
            \() ->
                let
                    tzName =
                        name Data.etc_gmt_minus_12_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone etc_gmt_minus_13" <|
            \() ->
                let
                    tzName =
                        name Data.etc_gmt_minus_13_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone etc_gmt_minus_14" <|
            \() ->
                let
                    tzName =
                        name Data.etc_gmt_minus_14_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone etc_gmt_minus_2" <|
            \() ->
                let
                    tzName =
                        name Data.etc_gmt_minus_2_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone etc_gmt_minus_3" <|
            \() ->
                let
                    tzName =
                        name Data.etc_gmt_minus_3_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone etc_gmt_minus_4" <|
            \() ->
                let
                    tzName =
                        name Data.etc_gmt_minus_4_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone etc_gmt_minus_5" <|
            \() ->
                let
                    tzName =
                        name Data.etc_gmt_minus_5_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone etc_gmt_minus_6" <|
            \() ->
                let
                    tzName =
                        name Data.etc_gmt_minus_6_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone etc_gmt_minus_7" <|
            \() ->
                let
                    tzName =
                        name Data.etc_gmt_minus_7_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone etc_gmt_minus_8" <|
            \() ->
                let
                    tzName =
                        name Data.etc_gmt_minus_8_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone etc_gmt_minus_9" <|
            \() ->
                let
                    tzName =
                        name Data.etc_gmt_minus_9_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone etc_uct" <|
            \() ->
                let
                    tzName =
                        name Data.etc_uct_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone etc_utc" <|
            \() ->
                let
                    tzName =
                        name Data.etc_utc_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone europe_amsterdam" <|
            \() ->
                let
                    tzName =
                        name Data.europe_amsterdam_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone europe_andorra" <|
            \() ->
                let
                    tzName =
                        name Data.europe_andorra_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone europe_astrakhan" <|
            \() ->
                let
                    tzName =
                        name Data.europe_astrakhan_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone europe_athens" <|
            \() ->
                let
                    tzName =
                        name Data.europe_athens_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone europe_london" <|
            \() ->
                let
                    tzName =
                        name Data.europe_london_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone europe_belgrade" <|
            \() ->
                let
                    tzName =
                        name Data.europe_belgrade_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone europe_berlin" <|
            \() ->
                let
                    tzName =
                        name Data.europe_berlin_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone europe_prague" <|
            \() ->
                let
                    tzName =
                        name Data.europe_prague_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone europe_brussels" <|
            \() ->
                let
                    tzName =
                        name Data.europe_brussels_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone europe_bucharest" <|
            \() ->
                let
                    tzName =
                        name Data.europe_bucharest_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone europe_budapest" <|
            \() ->
                let
                    tzName =
                        name Data.europe_budapest_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone europe_zurich" <|
            \() ->
                let
                    tzName =
                        name Data.europe_zurich_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone europe_chisinau" <|
            \() ->
                let
                    tzName =
                        name Data.europe_chisinau_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone europe_copenhagen" <|
            \() ->
                let
                    tzName =
                        name Data.europe_copenhagen_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone europe_gibraltar" <|
            \() ->
                let
                    tzName =
                        name Data.europe_gibraltar_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone europe_helsinki" <|
            \() ->
                let
                    tzName =
                        name Data.europe_helsinki_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone europe_kaliningrad" <|
            \() ->
                let
                    tzName =
                        name Data.europe_kaliningrad_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone europe_kiev" <|
            \() ->
                let
                    tzName =
                        name Data.europe_kiev_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone europe_kirov" <|
            \() ->
                let
                    tzName =
                        name Data.europe_kirov_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone europe_lisbon" <|
            \() ->
                let
                    tzName =
                        name Data.europe_lisbon_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone europe_luxembourg" <|
            \() ->
                let
                    tzName =
                        name Data.europe_luxembourg_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone europe_madrid" <|
            \() ->
                let
                    tzName =
                        name Data.europe_madrid_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone europe_malta" <|
            \() ->
                let
                    tzName =
                        name Data.europe_malta_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone europe_minsk" <|
            \() ->
                let
                    tzName =
                        name Data.europe_minsk_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone europe_monaco" <|
            \() ->
                let
                    tzName =
                        name Data.europe_monaco_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone europe_moscow" <|
            \() ->
                let
                    tzName =
                        name Data.europe_moscow_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone europe_paris" <|
            \() ->
                let
                    tzName =
                        name Data.europe_paris_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone europe_riga" <|
            \() ->
                let
                    tzName =
                        name Data.europe_riga_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone europe_rome" <|
            \() ->
                let
                    tzName =
                        name Data.europe_rome_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone europe_samara" <|
            \() ->
                let
                    tzName =
                        name Data.europe_samara_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone europe_simferopol" <|
            \() ->
                let
                    tzName =
                        name Data.europe_simferopol_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone europe_sofia" <|
            \() ->
                let
                    tzName =
                        name Data.europe_sofia_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone europe_stockholm" <|
            \() ->
                let
                    tzName =
                        name Data.europe_stockholm_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone europe_tallinn" <|
            \() ->
                let
                    tzName =
                        name Data.europe_tallinn_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone europe_tirane" <|
            \() ->
                let
                    tzName =
                        name Data.europe_tirane_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone europe_ulyanovsk" <|
            \() ->
                let
                    tzName =
                        name Data.europe_ulyanovsk_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone europe_uzhgorod" <|
            \() ->
                let
                    tzName =
                        name Data.europe_uzhgorod_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone europe_vienna" <|
            \() ->
                let
                    tzName =
                        name Data.europe_vienna_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone europe_vilnius" <|
            \() ->
                let
                    tzName =
                        name Data.europe_vilnius_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone europe_volgograd" <|
            \() ->
                let
                    tzName =
                        name Data.europe_volgograd_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone europe_warsaw" <|
            \() ->
                let
                    tzName =
                        name Data.europe_warsaw_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone europe_zaporozhye" <|
            \() ->
                let
                    tzName =
                        name Data.europe_zaporozhye_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone hst" <|
            \() ->
                let
                    tzName =
                        name Data.hst_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone indian_chagos" <|
            \() ->
                let
                    tzName =
                        name Data.indian_chagos_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone indian_christmas" <|
            \() ->
                let
                    tzName =
                        name Data.indian_christmas_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone indian_cocos" <|
            \() ->
                let
                    tzName =
                        name Data.indian_cocos_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone indian_kerguelen" <|
            \() ->
                let
                    tzName =
                        name Data.indian_kerguelen_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone indian_mahe" <|
            \() ->
                let
                    tzName =
                        name Data.indian_mahe_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone indian_maldives" <|
            \() ->
                let
                    tzName =
                        name Data.indian_maldives_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone indian_mauritius" <|
            \() ->
                let
                    tzName =
                        name Data.indian_mauritius_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone indian_reunion" <|
            \() ->
                let
                    tzName =
                        name Data.indian_reunion_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone pacific_kwajalein" <|
            \() ->
                let
                    tzName =
                        name Data.pacific_kwajalein_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone met" <|
            \() ->
                let
                    tzName =
                        name Data.met_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone mst" <|
            \() ->
                let
                    tzName =
                        name Data.mst_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone mst7mdt" <|
            \() ->
                let
                    tzName =
                        name Data.mst7mdt_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone pacific_chatham" <|
            \() ->
                let
                    tzName =
                        name Data.pacific_chatham_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone pst8pdt" <|
            \() ->
                let
                    tzName =
                        name Data.pst8pdt_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone pacific_apia" <|
            \() ->
                let
                    tzName =
                        name Data.pacific_apia_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone pacific_bougainville" <|
            \() ->
                let
                    tzName =
                        name Data.pacific_bougainville_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone pacific_chuuk" <|
            \() ->
                let
                    tzName =
                        name Data.pacific_chuuk_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone pacific_efate" <|
            \() ->
                let
                    tzName =
                        name Data.pacific_efate_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone pacific_enderbury" <|
            \() ->
                let
                    tzName =
                        name Data.pacific_enderbury_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone pacific_fakaofo" <|
            \() ->
                let
                    tzName =
                        name Data.pacific_fakaofo_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone pacific_fiji" <|
            \() ->
                let
                    tzName =
                        name Data.pacific_fiji_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone pacific_funafuti" <|
            \() ->
                let
                    tzName =
                        name Data.pacific_funafuti_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone pacific_galapagos" <|
            \() ->
                let
                    tzName =
                        name Data.pacific_galapagos_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone pacific_gambier" <|
            \() ->
                let
                    tzName =
                        name Data.pacific_gambier_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone pacific_guadalcanal" <|
            \() ->
                let
                    tzName =
                        name Data.pacific_guadalcanal_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone pacific_guam" <|
            \() ->
                let
                    tzName =
                        name Data.pacific_guam_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone pacific_honolulu" <|
            \() ->
                let
                    tzName =
                        name Data.pacific_honolulu_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone pacific_kiritimati" <|
            \() ->
                let
                    tzName =
                        name Data.pacific_kiritimati_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone pacific_kosrae" <|
            \() ->
                let
                    tzName =
                        name Data.pacific_kosrae_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone pacific_majuro" <|
            \() ->
                let
                    tzName =
                        name Data.pacific_majuro_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone pacific_marquesas" <|
            \() ->
                let
                    tzName =
                        name Data.pacific_marquesas_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone pacific_pago_pago" <|
            \() ->
                let
                    tzName =
                        name Data.pacific_pago_pago_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone pacific_nauru" <|
            \() ->
                let
                    tzName =
                        name Data.pacific_nauru_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone pacific_niue" <|
            \() ->
                let
                    tzName =
                        name Data.pacific_niue_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone pacific_norfolk" <|
            \() ->
                let
                    tzName =
                        name Data.pacific_norfolk_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone pacific_noumea" <|
            \() ->
                let
                    tzName =
                        name Data.pacific_noumea_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone pacific_palau" <|
            \() ->
                let
                    tzName =
                        name Data.pacific_palau_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone pacific_pitcairn" <|
            \() ->
                let
                    tzName =
                        name Data.pacific_pitcairn_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone pacific_pohnpei" <|
            \() ->
                let
                    tzName =
                        name Data.pacific_pohnpei_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone pacific_port_moresby" <|
            \() ->
                let
                    tzName =
                        name Data.pacific_port_moresby_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone pacific_rarotonga" <|
            \() ->
                let
                    tzName =
                        name Data.pacific_rarotonga_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone pacific_tahiti" <|
            \() ->
                let
                    tzName =
                        name Data.pacific_tahiti_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone pacific_tarawa" <|
            \() ->
                let
                    tzName =
                        name Data.pacific_tarawa_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone pacific_tongatapu" <|
            \() ->
                let
                    tzName =
                        name Data.pacific_tongatapu_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone pacific_wake" <|
            \() ->
                let
                    tzName =
                        name Data.pacific_wake_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone pacific_wallis" <|
            \() ->
                let
                    tzName =
                        name Data.pacific_wallis_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        , test "time zone wet" <|
            \() ->
                let
                    tzName =
                        name Data.wet_l
                in
                tzName
                    |> String.contains "error"
                    |> Expect.false (tzName ++ " should not include error")
        ]
