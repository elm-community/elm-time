module UTC.Zones exposing (..)

{-| This module contains Zone definitions for all Timezones as they
are defined in the IANA zoneinfo database.

Zone data is parsed lazily so, in order to retrieve a zone's value you
must apply `()` to it.  For example:

    import UTC.DateTime exposing (epoch, toTimestamp)
    import UTC.Zone exposing (abbreviation)
    import UTC.Zones exposing (europe_bucharest)

    let
        timezone = europe_bucharest ()
    in
        abbreviation (toTimestamp epoch) timezone

@docs africa_abidjan, africa_accra, africa_addis_ababa, africa_algiers, africa_asmara, africa_asmera, africa_bamako, africa_bangui, africa_banjul, africa_bissau, africa_blantyre, africa_brazzaville, africa_bujumbura, africa_cairo, africa_casablanca, africa_ceuta, africa_conakry, africa_dakar, africa_dar_es_salaam, africa_djibouti, africa_douala, africa_el_aaiun, africa_freetown, africa_gaborone, africa_harare, africa_johannesburg, africa_juba, africa_kampala, africa_khartoum, africa_kigali, africa_kinshasa, africa_lagos, africa_libreville, africa_lome, africa_luanda, africa_lubumbashi, africa_lusaka, africa_malabo, africa_maputo, africa_maseru, africa_mbabane, africa_mogadishu, africa_monrovia, africa_nairobi, africa_ndjamena, africa_niamey, africa_nouakchott, africa_ouagadougou, africa_porto_novo, africa_sao_tome, africa_timbuktu, africa_tripoli, africa_tunis, africa_windhoek, america_adak, america_anchorage, america_anguilla, america_antigua, america_araguaina, america_argentina_buenos_aires, america_argentina_catamarca, america_argentina_comodrivadavia, america_argentina_cordoba, america_argentina_jujuy, america_argentina_la_rioja, america_argentina_mendoza, america_argentina_rio_gallegos, america_argentina_salta, america_argentina_san_juan, america_argentina_san_luis, america_argentina_tucuman, america_argentina_ushuaia, america_aruba, america_asuncion, america_atikokan, america_atka, america_bahia, america_bahia_banderas, america_barbados, america_belem, america_belize, america_blanc_sablon, america_boa_vista, america_bogota, america_boise, america_buenos_aires, america_cambridge_bay, america_campo_grande, america_cancun, america_caracas, america_catamarca, america_cayenne, america_cayman, america_chicago, america_chihuahua, america_coral_harbour, america_cordoba, america_costa_rica, america_creston, america_cuiaba, america_curacao, america_danmarkshavn, america_dawson, america_dawson_creek, america_denver, america_detroit, america_dominica, america_edmonton, america_eirunepe, america_el_salvador, america_ensenada, america_fort_nelson, america_fort_wayne, america_fortaleza, america_glace_bay, america_godthab, america_goose_bay, america_grand_turk, america_grenada, america_guadeloupe, america_guatemala, america_guayaquil, america_guyana, america_halifax, america_havana, america_hermosillo, america_indiana_indianapolis, america_indiana_knox, america_indiana_marengo, america_indiana_petersburg, america_indiana_tell_city, america_indiana_vevay, america_indiana_vincennes, america_indiana_winamac, america_indianapolis, america_inuvik, america_iqaluit, america_jamaica, america_jujuy, america_juneau, america_kentucky_louisville, america_kentucky_monticello, america_knox_in, america_kralendijk, america_la_paz, america_lima, america_los_angeles, america_louisville, america_lower_princes, america_maceio, america_managua, america_manaus, america_marigot, america_martinique, america_matamoros, america_mazatlan, america_mendoza, america_menominee, america_merida, america_metlakatla, america_mexico_city, america_miquelon, america_moncton, america_monterrey, america_montevideo, america_montreal, america_montserrat, america_nassau, america_new_york, america_nipigon, america_nome, america_noronha, america_north_dakota_beulah, america_north_dakota_center, america_north_dakota_new_salem, america_ojinaga, america_panama, america_pangnirtung, america_paramaribo, america_phoenix, america_port_au_prince, america_port_of_spain, america_porto_acre, america_porto_velho, america_puerto_rico, america_rainy_river, america_rankin_inlet, america_recife, america_regina, america_resolute, america_rio_branco, america_rosario, america_santa_isabel, america_santarem, america_santiago, america_santo_domingo, america_sao_paulo, america_scoresbysund, america_shiprock, america_sitka, america_st_barthelemy, america_st_johns, america_st_kitts, america_st_lucia, america_st_thomas, america_st_vincent, america_swift_current, america_tegucigalpa, america_thule, america_thunder_bay, america_tijuana, america_toronto, america_tortola, america_vancouver, america_virgin, america_whitehorse, america_winnipeg, america_yakutat, america_yellowknife, antarctica_casey, antarctica_davis, antarctica_dumontdurville, antarctica_macquarie, antarctica_mawson, antarctica_mcmurdo, antarctica_palmer, antarctica_rothera, antarctica_south_pole, antarctica_syowa, antarctica_troll, antarctica_vostok, arctic_longyearbyen, asia_aden, asia_almaty, asia_amman, asia_anadyr, asia_aqtau, asia_aqtobe, asia_ashgabat, asia_ashkhabad, asia_baghdad, asia_bahrain, asia_baku, asia_bangkok, asia_barnaul, asia_beirut, asia_bishkek, asia_brunei, asia_calcutta, asia_chita, asia_choibalsan, asia_chongqing, asia_chungking, asia_colombo, asia_dacca, asia_damascus, asia_dhaka, asia_dili, asia_dubai, asia_dushanbe, asia_gaza, asia_harbin, asia_hebron, asia_ho_chi_minh, asia_hong_kong, asia_hovd, asia_irkutsk, asia_istanbul, asia_jakarta, asia_jayapura, asia_jerusalem, asia_kabul, asia_kamchatka, asia_karachi, asia_kashgar, asia_kathmandu, asia_katmandu, asia_khandyga, asia_kolkata, asia_krasnoyarsk, asia_kuala_lumpur, asia_kuching, asia_kuwait, asia_macao, asia_macau, asia_magadan, asia_makassar, asia_manila, asia_muscat, asia_nicosia, asia_novokuznetsk, asia_novosibirsk, asia_omsk, asia_oral, asia_phnom_penh, asia_pontianak, asia_pyongyang, asia_qatar, asia_qyzylorda, asia_rangoon, asia_riyadh, asia_saigon, asia_sakhalin, asia_samarkand, asia_seoul, asia_shanghai, asia_singapore, asia_srednekolymsk, asia_taipei, asia_tashkent, asia_tbilisi, asia_tehran, asia_tel_aviv, asia_thimbu, asia_thimphu, asia_tokyo, asia_tomsk, asia_ujung_pandang, asia_ulaanbaatar, asia_ulan_bator, asia_urumqi, asia_ust_nera, asia_vientiane, asia_vladivostok, asia_yakutsk, asia_yekaterinburg, asia_yerevan, atlantic_azores, atlantic_bermuda, atlantic_canary, atlantic_cape_verde, atlantic_faeroe, atlantic_faroe, atlantic_jan_mayen, atlantic_madeira, atlantic_reykjavik, atlantic_south_georgia, atlantic_st_helena, atlantic_stanley, australia_act, australia_adelaide, australia_brisbane, australia_broken_hill, australia_canberra, australia_currie, australia_darwin, australia_eucla, australia_hobart, australia_lhi, australia_lindeman, australia_lord_howe, australia_melbourne, australia_north, australia_nsw, australia_perth, australia_queensland, australia_south, australia_sydney, australia_tasmania, australia_victoria, australia_west, australia_yancowinna, brazil_acre, brazil_denoronha, brazil_east, brazil_west, canada_atlantic, canada_central, canada_east_saskatchewan, canada_eastern, canada_mountain, canada_newfoundland, canada_pacific, canada_saskatchewan, canada_yukon, cet, chile_continental, chile_easterisland, cst6cdt, cuba, eet, egypt, eire, est, est5edt, etc_gmt, etc_gmt_0, etc_gmt_minus_0, etc_gmt_minus_1, etc_gmt_minus_10, etc_gmt_minus_11, etc_gmt_minus_12, etc_gmt_minus_13, etc_gmt_minus_14, etc_gmt_minus_2, etc_gmt_minus_3, etc_gmt_minus_4, etc_gmt_minus_5, etc_gmt_minus_6, etc_gmt_minus_7, etc_gmt_minus_8, etc_gmt_minus_9, etc_gmt_plus_0, etc_gmt_plus_1, etc_gmt_plus_10, etc_gmt_plus_11, etc_gmt_plus_12, etc_gmt_plus_2, etc_gmt_plus_3, etc_gmt_plus_4, etc_gmt_plus_5, etc_gmt_plus_6, etc_gmt_plus_7, etc_gmt_plus_8, etc_gmt_plus_9, etc_greenwich, etc_uct, etc_universal, etc_utc, etc_zulu, europe_amsterdam, europe_andorra, europe_astrakhan, europe_athens, europe_belfast, europe_belgrade, europe_berlin, europe_bratislava, europe_brussels, europe_bucharest, europe_budapest, europe_busingen, europe_chisinau, europe_copenhagen, europe_dublin, europe_gibraltar, europe_guernsey, europe_helsinki, europe_isle_of_man, europe_istanbul, europe_jersey, europe_kaliningrad, europe_kiev, europe_kirov, europe_lisbon, europe_ljubljana, europe_london, europe_luxembourg, europe_madrid, europe_malta, europe_mariehamn, europe_minsk, europe_monaco, europe_moscow, europe_nicosia, europe_oslo, europe_paris, europe_podgorica, europe_prague, europe_riga, europe_rome, europe_samara, europe_san_marino, europe_sarajevo, europe_simferopol, europe_skopje, europe_sofia, europe_stockholm, europe_tallinn, europe_tirane, europe_tiraspol, europe_ulyanovsk, europe_uzhgorod, europe_vaduz, europe_vatican, europe_vienna, europe_vilnius, europe_volgograd, europe_warsaw, europe_zagreb, europe_zaporozhye, europe_zurich, gb, gb_eire, gmt, gmt_0, gmt_minus_0, gmt_plus_0, greenwich, hongkong, hst, iceland, indian_antananarivo, indian_chagos, indian_christmas, indian_cocos, indian_comoro, indian_kerguelen, indian_mahe, indian_maldives, indian_mauritius, indian_mayotte, indian_reunion, iran, israel, jamaica, japan, kwajalein, libya, met, mexico_bajanorte, mexico_bajasur, mexico_general, mst, mst7mdt, navajo, nz, nz_chat, pacific_apia, pacific_auckland, pacific_bougainville, pacific_chatham, pacific_chuuk, pacific_easter, pacific_efate, pacific_enderbury, pacific_fakaofo, pacific_fiji, pacific_funafuti, pacific_galapagos, pacific_gambier, pacific_guadalcanal, pacific_guam, pacific_honolulu, pacific_johnston, pacific_kiritimati, pacific_kosrae, pacific_kwajalein, pacific_majuro, pacific_marquesas, pacific_midway, pacific_nauru, pacific_niue, pacific_norfolk, pacific_noumea, pacific_pago_pago, pacific_palau, pacific_pitcairn, pacific_pohnpei, pacific_ponape, pacific_port_moresby, pacific_rarotonga, pacific_saipan, pacific_samoa, pacific_tahiti, pacific_tarawa, pacific_tongatapu, pacific_truk, pacific_wake, pacific_wallis, pacific_yap, poland, portugal, prc, pst8pdt, roc, rok, singapore, turkey, uct, universal, us_alaska, us_aleutian, us_arizona, us_central, us_east_indiana, us_eastern, us_hawaii, us_indiana_starke, us_michigan, us_mountain, us_pacific, us_pacific_new, us_samoa, utc, w_su, wet, zulu
-}

import Lazy exposing (Lazy, force)
import UTC.Zone exposing (Zone)
import UTC.ZoneData exposing (..)


-- Zones
-- -----

{-| Africa/Abidjan -}
africa_abidjan : () -> Zone
africa_abidjan () = force africa_abidjan_l

{-| Africa/Accra -}
africa_accra : () -> Zone
africa_accra () = force africa_accra_l

{-| Africa/Algiers -}
africa_algiers : () -> Zone
africa_algiers () = force africa_algiers_l

{-| Africa/Bissau -}
africa_bissau : () -> Zone
africa_bissau () = force africa_bissau_l

{-| Africa/Cairo -}
africa_cairo : () -> Zone
africa_cairo () = force africa_cairo_l

{-| Africa/Casablanca -}
africa_casablanca : () -> Zone
africa_casablanca () = force africa_casablanca_l

{-| Africa/Ceuta -}
africa_ceuta : () -> Zone
africa_ceuta () = force africa_ceuta_l

{-| Africa/El_Aaiun -}
africa_el_aaiun : () -> Zone
africa_el_aaiun () = force africa_el_aaiun_l

{-| Africa/Johannesburg -}
africa_johannesburg : () -> Zone
africa_johannesburg () = force africa_johannesburg_l

{-| Africa/Khartoum -}
africa_khartoum : () -> Zone
africa_khartoum () = force africa_khartoum_l

{-| Africa/Lagos -}
africa_lagos : () -> Zone
africa_lagos () = force africa_lagos_l

{-| Africa/Maputo -}
africa_maputo : () -> Zone
africa_maputo () = force africa_maputo_l

{-| Africa/Monrovia -}
africa_monrovia : () -> Zone
africa_monrovia () = force africa_monrovia_l

{-| Africa/Nairobi -}
africa_nairobi : () -> Zone
africa_nairobi () = force africa_nairobi_l

{-| Africa/Ndjamena -}
africa_ndjamena : () -> Zone
africa_ndjamena () = force africa_ndjamena_l

{-| Africa/Tripoli -}
africa_tripoli : () -> Zone
africa_tripoli () = force africa_tripoli_l

{-| Africa/Tunis -}
africa_tunis : () -> Zone
africa_tunis () = force africa_tunis_l

{-| Africa/Windhoek -}
africa_windhoek : () -> Zone
africa_windhoek () = force africa_windhoek_l

{-| America/Adak -}
america_adak : () -> Zone
america_adak () = force america_adak_l

{-| America/Anchorage -}
america_anchorage : () -> Zone
america_anchorage () = force america_anchorage_l

{-| America/Araguaina -}
america_araguaina : () -> Zone
america_araguaina () = force america_araguaina_l

{-| America/Argentina/Buenos_Aires -}
america_argentina_buenos_aires : () -> Zone
america_argentina_buenos_aires () = force america_argentina_buenos_aires_l

{-| America/Argentina/Catamarca -}
america_argentina_catamarca : () -> Zone
america_argentina_catamarca () = force america_argentina_catamarca_l

{-| America/Argentina/Cordoba -}
america_argentina_cordoba : () -> Zone
america_argentina_cordoba () = force america_argentina_cordoba_l

{-| America/Argentina/Jujuy -}
america_argentina_jujuy : () -> Zone
america_argentina_jujuy () = force america_argentina_jujuy_l

{-| America/Argentina/La_Rioja -}
america_argentina_la_rioja : () -> Zone
america_argentina_la_rioja () = force america_argentina_la_rioja_l

{-| America/Argentina/Mendoza -}
america_argentina_mendoza : () -> Zone
america_argentina_mendoza () = force america_argentina_mendoza_l

{-| America/Argentina/Rio_Gallegos -}
america_argentina_rio_gallegos : () -> Zone
america_argentina_rio_gallegos () = force america_argentina_rio_gallegos_l

{-| America/Argentina/Salta -}
america_argentina_salta : () -> Zone
america_argentina_salta () = force america_argentina_salta_l

{-| America/Argentina/San_Juan -}
america_argentina_san_juan : () -> Zone
america_argentina_san_juan () = force america_argentina_san_juan_l

{-| America/Argentina/San_Luis -}
america_argentina_san_luis : () -> Zone
america_argentina_san_luis () = force america_argentina_san_luis_l

{-| America/Argentina/Tucuman -}
america_argentina_tucuman : () -> Zone
america_argentina_tucuman () = force america_argentina_tucuman_l

{-| America/Argentina/Ushuaia -}
america_argentina_ushuaia : () -> Zone
america_argentina_ushuaia () = force america_argentina_ushuaia_l

{-| America/Asuncion -}
america_asuncion : () -> Zone
america_asuncion () = force america_asuncion_l

{-| America/Atikokan -}
america_atikokan : () -> Zone
america_atikokan () = force america_atikokan_l

{-| America/Bahia -}
america_bahia : () -> Zone
america_bahia () = force america_bahia_l

{-| America/Bahia_Banderas -}
america_bahia_banderas : () -> Zone
america_bahia_banderas () = force america_bahia_banderas_l

{-| America/Barbados -}
america_barbados : () -> Zone
america_barbados () = force america_barbados_l

{-| America/Belem -}
america_belem : () -> Zone
america_belem () = force america_belem_l

{-| America/Belize -}
america_belize : () -> Zone
america_belize () = force america_belize_l

{-| America/Blanc-Sablon -}
america_blanc_sablon : () -> Zone
america_blanc_sablon () = force america_blanc_sablon_l

{-| America/Boa_Vista -}
america_boa_vista : () -> Zone
america_boa_vista () = force america_boa_vista_l

{-| America/Bogota -}
america_bogota : () -> Zone
america_bogota () = force america_bogota_l

{-| America/Boise -}
america_boise : () -> Zone
america_boise () = force america_boise_l

{-| America/Cambridge_Bay -}
america_cambridge_bay : () -> Zone
america_cambridge_bay () = force america_cambridge_bay_l

{-| America/Campo_Grande -}
america_campo_grande : () -> Zone
america_campo_grande () = force america_campo_grande_l

{-| America/Cancun -}
america_cancun : () -> Zone
america_cancun () = force america_cancun_l

{-| America/Caracas -}
america_caracas : () -> Zone
america_caracas () = force america_caracas_l

{-| America/Cayenne -}
america_cayenne : () -> Zone
america_cayenne () = force america_cayenne_l

{-| America/Chicago -}
america_chicago : () -> Zone
america_chicago () = force america_chicago_l

{-| America/Chihuahua -}
america_chihuahua : () -> Zone
america_chihuahua () = force america_chihuahua_l

{-| America/Costa_Rica -}
america_costa_rica : () -> Zone
america_costa_rica () = force america_costa_rica_l

{-| America/Creston -}
america_creston : () -> Zone
america_creston () = force america_creston_l

{-| America/Cuiaba -}
america_cuiaba : () -> Zone
america_cuiaba () = force america_cuiaba_l

{-| America/Curacao -}
america_curacao : () -> Zone
america_curacao () = force america_curacao_l

{-| America/Danmarkshavn -}
america_danmarkshavn : () -> Zone
america_danmarkshavn () = force america_danmarkshavn_l

{-| America/Dawson -}
america_dawson : () -> Zone
america_dawson () = force america_dawson_l

{-| America/Dawson_Creek -}
america_dawson_creek : () -> Zone
america_dawson_creek () = force america_dawson_creek_l

{-| America/Denver -}
america_denver : () -> Zone
america_denver () = force america_denver_l

{-| America/Detroit -}
america_detroit : () -> Zone
america_detroit () = force america_detroit_l

{-| America/Edmonton -}
america_edmonton : () -> Zone
america_edmonton () = force america_edmonton_l

{-| America/Eirunepe -}
america_eirunepe : () -> Zone
america_eirunepe () = force america_eirunepe_l

{-| America/El_Salvador -}
america_el_salvador : () -> Zone
america_el_salvador () = force america_el_salvador_l

{-| America/Fort_Nelson -}
america_fort_nelson : () -> Zone
america_fort_nelson () = force america_fort_nelson_l

{-| America/Fort_Wayne -}
america_fort_wayne : () -> Zone
america_fort_wayne () = force america_fort_wayne_l

{-| America/Fortaleza -}
america_fortaleza : () -> Zone
america_fortaleza () = force america_fortaleza_l

{-| America/Glace_Bay -}
america_glace_bay : () -> Zone
america_glace_bay () = force america_glace_bay_l

{-| America/Godthab -}
america_godthab : () -> Zone
america_godthab () = force america_godthab_l

{-| America/Goose_Bay -}
america_goose_bay : () -> Zone
america_goose_bay () = force america_goose_bay_l

{-| America/Grand_Turk -}
america_grand_turk : () -> Zone
america_grand_turk () = force america_grand_turk_l

{-| America/Guatemala -}
america_guatemala : () -> Zone
america_guatemala () = force america_guatemala_l

{-| America/Guayaquil -}
america_guayaquil : () -> Zone
america_guayaquil () = force america_guayaquil_l

{-| America/Guyana -}
america_guyana : () -> Zone
america_guyana () = force america_guyana_l

{-| America/Halifax -}
america_halifax : () -> Zone
america_halifax () = force america_halifax_l

{-| America/Havana -}
america_havana : () -> Zone
america_havana () = force america_havana_l

{-| America/Hermosillo -}
america_hermosillo : () -> Zone
america_hermosillo () = force america_hermosillo_l

{-| America/Indiana/Knox -}
america_indiana_knox : () -> Zone
america_indiana_knox () = force america_indiana_knox_l

{-| America/Indiana/Marengo -}
america_indiana_marengo : () -> Zone
america_indiana_marengo () = force america_indiana_marengo_l

{-| America/Indiana/Petersburg -}
america_indiana_petersburg : () -> Zone
america_indiana_petersburg () = force america_indiana_petersburg_l

{-| America/Indiana/Tell_City -}
america_indiana_tell_city : () -> Zone
america_indiana_tell_city () = force america_indiana_tell_city_l

{-| America/Indiana/Vevay -}
america_indiana_vevay : () -> Zone
america_indiana_vevay () = force america_indiana_vevay_l

{-| America/Indiana/Vincennes -}
america_indiana_vincennes : () -> Zone
america_indiana_vincennes () = force america_indiana_vincennes_l

{-| America/Indiana/Winamac -}
america_indiana_winamac : () -> Zone
america_indiana_winamac () = force america_indiana_winamac_l

{-| America/Inuvik -}
america_inuvik : () -> Zone
america_inuvik () = force america_inuvik_l

{-| America/Iqaluit -}
america_iqaluit : () -> Zone
america_iqaluit () = force america_iqaluit_l

{-| America/Jamaica -}
america_jamaica : () -> Zone
america_jamaica () = force america_jamaica_l

{-| America/Juneau -}
america_juneau : () -> Zone
america_juneau () = force america_juneau_l

{-| America/Kentucky/Louisville -}
america_kentucky_louisville : () -> Zone
america_kentucky_louisville () = force america_kentucky_louisville_l

{-| America/Kentucky/Monticello -}
america_kentucky_monticello : () -> Zone
america_kentucky_monticello () = force america_kentucky_monticello_l

{-| America/La_Paz -}
america_la_paz : () -> Zone
america_la_paz () = force america_la_paz_l

{-| America/Lima -}
america_lima : () -> Zone
america_lima () = force america_lima_l

{-| America/Los_Angeles -}
america_los_angeles : () -> Zone
america_los_angeles () = force america_los_angeles_l

{-| America/Maceio -}
america_maceio : () -> Zone
america_maceio () = force america_maceio_l

{-| America/Managua -}
america_managua : () -> Zone
america_managua () = force america_managua_l

{-| America/Manaus -}
america_manaus : () -> Zone
america_manaus () = force america_manaus_l

{-| America/Martinique -}
america_martinique : () -> Zone
america_martinique () = force america_martinique_l

{-| America/Matamoros -}
america_matamoros : () -> Zone
america_matamoros () = force america_matamoros_l

{-| America/Mazatlan -}
america_mazatlan : () -> Zone
america_mazatlan () = force america_mazatlan_l

{-| America/Menominee -}
america_menominee : () -> Zone
america_menominee () = force america_menominee_l

{-| America/Merida -}
america_merida : () -> Zone
america_merida () = force america_merida_l

{-| America/Metlakatla -}
america_metlakatla : () -> Zone
america_metlakatla () = force america_metlakatla_l

{-| America/Mexico_City -}
america_mexico_city : () -> Zone
america_mexico_city () = force america_mexico_city_l

{-| America/Miquelon -}
america_miquelon : () -> Zone
america_miquelon () = force america_miquelon_l

{-| America/Moncton -}
america_moncton : () -> Zone
america_moncton () = force america_moncton_l

{-| America/Monterrey -}
america_monterrey : () -> Zone
america_monterrey () = force america_monterrey_l

{-| America/Montevideo -}
america_montevideo : () -> Zone
america_montevideo () = force america_montevideo_l

{-| America/Nassau -}
america_nassau : () -> Zone
america_nassau () = force america_nassau_l

{-| America/New_York -}
america_new_york : () -> Zone
america_new_york () = force america_new_york_l

{-| America/Nipigon -}
america_nipigon : () -> Zone
america_nipigon () = force america_nipigon_l

{-| America/Nome -}
america_nome : () -> Zone
america_nome () = force america_nome_l

{-| America/Noronha -}
america_noronha : () -> Zone
america_noronha () = force america_noronha_l

{-| America/North_Dakota/Beulah -}
america_north_dakota_beulah : () -> Zone
america_north_dakota_beulah () = force america_north_dakota_beulah_l

{-| America/North_Dakota/Center -}
america_north_dakota_center : () -> Zone
america_north_dakota_center () = force america_north_dakota_center_l

{-| America/North_Dakota/New_Salem -}
america_north_dakota_new_salem : () -> Zone
america_north_dakota_new_salem () = force america_north_dakota_new_salem_l

{-| America/Ojinaga -}
america_ojinaga : () -> Zone
america_ojinaga () = force america_ojinaga_l

{-| America/Panama -}
america_panama : () -> Zone
america_panama () = force america_panama_l

{-| America/Pangnirtung -}
america_pangnirtung : () -> Zone
america_pangnirtung () = force america_pangnirtung_l

{-| America/Paramaribo -}
america_paramaribo : () -> Zone
america_paramaribo () = force america_paramaribo_l

{-| America/Phoenix -}
america_phoenix : () -> Zone
america_phoenix () = force america_phoenix_l

{-| America/Port-au-Prince -}
america_port_au_prince : () -> Zone
america_port_au_prince () = force america_port_au_prince_l

{-| America/Port_of_Spain -}
america_port_of_spain : () -> Zone
america_port_of_spain () = force america_port_of_spain_l

{-| America/Porto_Velho -}
america_porto_velho : () -> Zone
america_porto_velho () = force america_porto_velho_l

{-| America/Puerto_Rico -}
america_puerto_rico : () -> Zone
america_puerto_rico () = force america_puerto_rico_l

{-| America/Rainy_River -}
america_rainy_river : () -> Zone
america_rainy_river () = force america_rainy_river_l

{-| America/Rankin_Inlet -}
america_rankin_inlet : () -> Zone
america_rankin_inlet () = force america_rankin_inlet_l

{-| America/Recife -}
america_recife : () -> Zone
america_recife () = force america_recife_l

{-| America/Regina -}
america_regina : () -> Zone
america_regina () = force america_regina_l

{-| America/Resolute -}
america_resolute : () -> Zone
america_resolute () = force america_resolute_l

{-| America/Rio_Branco -}
america_rio_branco : () -> Zone
america_rio_branco () = force america_rio_branco_l

{-| America/Santarem -}
america_santarem : () -> Zone
america_santarem () = force america_santarem_l

{-| America/Santiago -}
america_santiago : () -> Zone
america_santiago () = force america_santiago_l

{-| America/Santo_Domingo -}
america_santo_domingo : () -> Zone
america_santo_domingo () = force america_santo_domingo_l

{-| America/Sao_Paulo -}
america_sao_paulo : () -> Zone
america_sao_paulo () = force america_sao_paulo_l

{-| America/Scoresbysund -}
america_scoresbysund : () -> Zone
america_scoresbysund () = force america_scoresbysund_l

{-| America/Sitka -}
america_sitka : () -> Zone
america_sitka () = force america_sitka_l

{-| America/St_Johns -}
america_st_johns : () -> Zone
america_st_johns () = force america_st_johns_l

{-| America/Swift_Current -}
america_swift_current : () -> Zone
america_swift_current () = force america_swift_current_l

{-| America/Tegucigalpa -}
america_tegucigalpa : () -> Zone
america_tegucigalpa () = force america_tegucigalpa_l

{-| America/Thule -}
america_thule : () -> Zone
america_thule () = force america_thule_l

{-| America/Thunder_Bay -}
america_thunder_bay : () -> Zone
america_thunder_bay () = force america_thunder_bay_l

{-| America/Tijuana -}
america_tijuana : () -> Zone
america_tijuana () = force america_tijuana_l

{-| America/Toronto -}
america_toronto : () -> Zone
america_toronto () = force america_toronto_l

{-| America/Vancouver -}
america_vancouver : () -> Zone
america_vancouver () = force america_vancouver_l

{-| America/Whitehorse -}
america_whitehorse : () -> Zone
america_whitehorse () = force america_whitehorse_l

{-| America/Winnipeg -}
america_winnipeg : () -> Zone
america_winnipeg () = force america_winnipeg_l

{-| America/Yakutat -}
america_yakutat : () -> Zone
america_yakutat () = force america_yakutat_l

{-| America/Yellowknife -}
america_yellowknife : () -> Zone
america_yellowknife () = force america_yellowknife_l

{-| Antarctica/Casey -}
antarctica_casey : () -> Zone
antarctica_casey () = force antarctica_casey_l

{-| Antarctica/Davis -}
antarctica_davis : () -> Zone
antarctica_davis () = force antarctica_davis_l

{-| Antarctica/DumontDUrville -}
antarctica_dumontdurville : () -> Zone
antarctica_dumontdurville () = force antarctica_dumontdurville_l

{-| Antarctica/Macquarie -}
antarctica_macquarie : () -> Zone
antarctica_macquarie () = force antarctica_macquarie_l

{-| Antarctica/Mawson -}
antarctica_mawson : () -> Zone
antarctica_mawson () = force antarctica_mawson_l

{-| Antarctica/Palmer -}
antarctica_palmer : () -> Zone
antarctica_palmer () = force antarctica_palmer_l

{-| Antarctica/Rothera -}
antarctica_rothera : () -> Zone
antarctica_rothera () = force antarctica_rothera_l

{-| Antarctica/Syowa -}
antarctica_syowa : () -> Zone
antarctica_syowa () = force antarctica_syowa_l

{-| Antarctica/Troll -}
antarctica_troll : () -> Zone
antarctica_troll () = force antarctica_troll_l

{-| Antarctica/Vostok -}
antarctica_vostok : () -> Zone
antarctica_vostok () = force antarctica_vostok_l

{-| Asia/Almaty -}
asia_almaty : () -> Zone
asia_almaty () = force asia_almaty_l

{-| Asia/Amman -}
asia_amman : () -> Zone
asia_amman () = force asia_amman_l

{-| Asia/Anadyr -}
asia_anadyr : () -> Zone
asia_anadyr () = force asia_anadyr_l

{-| Asia/Aqtau -}
asia_aqtau : () -> Zone
asia_aqtau () = force asia_aqtau_l

{-| Asia/Aqtobe -}
asia_aqtobe : () -> Zone
asia_aqtobe () = force asia_aqtobe_l

{-| Asia/Ashgabat -}
asia_ashgabat : () -> Zone
asia_ashgabat () = force asia_ashgabat_l

{-| Asia/Baghdad -}
asia_baghdad : () -> Zone
asia_baghdad () = force asia_baghdad_l

{-| Asia/Baku -}
asia_baku : () -> Zone
asia_baku () = force asia_baku_l

{-| Asia/Bangkok -}
asia_bangkok : () -> Zone
asia_bangkok () = force asia_bangkok_l

{-| Asia/Barnaul -}
asia_barnaul : () -> Zone
asia_barnaul () = force asia_barnaul_l

{-| Asia/Beirut -}
asia_beirut : () -> Zone
asia_beirut () = force asia_beirut_l

{-| Asia/Bishkek -}
asia_bishkek : () -> Zone
asia_bishkek () = force asia_bishkek_l

{-| Asia/Brunei -}
asia_brunei : () -> Zone
asia_brunei () = force asia_brunei_l

{-| Asia/Chita -}
asia_chita : () -> Zone
asia_chita () = force asia_chita_l

{-| Asia/Choibalsan -}
asia_choibalsan : () -> Zone
asia_choibalsan () = force asia_choibalsan_l

{-| Asia/Colombo -}
asia_colombo : () -> Zone
asia_colombo () = force asia_colombo_l

{-| Asia/Damascus -}
asia_damascus : () -> Zone
asia_damascus () = force asia_damascus_l

{-| Asia/Dhaka -}
asia_dhaka : () -> Zone
asia_dhaka () = force asia_dhaka_l

{-| Asia/Dili -}
asia_dili : () -> Zone
asia_dili () = force asia_dili_l

{-| Asia/Dubai -}
asia_dubai : () -> Zone
asia_dubai () = force asia_dubai_l

{-| Asia/Dushanbe -}
asia_dushanbe : () -> Zone
asia_dushanbe () = force asia_dushanbe_l

{-| Asia/Gaza -}
asia_gaza : () -> Zone
asia_gaza () = force asia_gaza_l

{-| Asia/Hebron -}
asia_hebron : () -> Zone
asia_hebron () = force asia_hebron_l

{-| Asia/Ho_Chi_Minh -}
asia_ho_chi_minh : () -> Zone
asia_ho_chi_minh () = force asia_ho_chi_minh_l

{-| Asia/Hong_Kong -}
asia_hong_kong : () -> Zone
asia_hong_kong () = force asia_hong_kong_l

{-| Asia/Hovd -}
asia_hovd : () -> Zone
asia_hovd () = force asia_hovd_l

{-| Asia/Irkutsk -}
asia_irkutsk : () -> Zone
asia_irkutsk () = force asia_irkutsk_l

{-| Asia/Jakarta -}
asia_jakarta : () -> Zone
asia_jakarta () = force asia_jakarta_l

{-| Asia/Jayapura -}
asia_jayapura : () -> Zone
asia_jayapura () = force asia_jayapura_l

{-| Asia/Jerusalem -}
asia_jerusalem : () -> Zone
asia_jerusalem () = force asia_jerusalem_l

{-| Asia/Kabul -}
asia_kabul : () -> Zone
asia_kabul () = force asia_kabul_l

{-| Asia/Kamchatka -}
asia_kamchatka : () -> Zone
asia_kamchatka () = force asia_kamchatka_l

{-| Asia/Karachi -}
asia_karachi : () -> Zone
asia_karachi () = force asia_karachi_l

{-| Asia/Kathmandu -}
asia_kathmandu : () -> Zone
asia_kathmandu () = force asia_kathmandu_l

{-| Asia/Khandyga -}
asia_khandyga : () -> Zone
asia_khandyga () = force asia_khandyga_l

{-| Asia/Kolkata -}
asia_kolkata : () -> Zone
asia_kolkata () = force asia_kolkata_l

{-| Asia/Krasnoyarsk -}
asia_krasnoyarsk : () -> Zone
asia_krasnoyarsk () = force asia_krasnoyarsk_l

{-| Asia/Kuala_Lumpur -}
asia_kuala_lumpur : () -> Zone
asia_kuala_lumpur () = force asia_kuala_lumpur_l

{-| Asia/Kuching -}
asia_kuching : () -> Zone
asia_kuching () = force asia_kuching_l

{-| Asia/Macau -}
asia_macau : () -> Zone
asia_macau () = force asia_macau_l

{-| Asia/Magadan -}
asia_magadan : () -> Zone
asia_magadan () = force asia_magadan_l

{-| Asia/Makassar -}
asia_makassar : () -> Zone
asia_makassar () = force asia_makassar_l

{-| Asia/Manila -}
asia_manila : () -> Zone
asia_manila () = force asia_manila_l

{-| Asia/Nicosia -}
asia_nicosia : () -> Zone
asia_nicosia () = force asia_nicosia_l

{-| Asia/Novokuznetsk -}
asia_novokuznetsk : () -> Zone
asia_novokuznetsk () = force asia_novokuznetsk_l

{-| Asia/Novosibirsk -}
asia_novosibirsk : () -> Zone
asia_novosibirsk () = force asia_novosibirsk_l

{-| Asia/Omsk -}
asia_omsk : () -> Zone
asia_omsk () = force asia_omsk_l

{-| Asia/Oral -}
asia_oral : () -> Zone
asia_oral () = force asia_oral_l

{-| Asia/Pontianak -}
asia_pontianak : () -> Zone
asia_pontianak () = force asia_pontianak_l

{-| Asia/Pyongyang -}
asia_pyongyang : () -> Zone
asia_pyongyang () = force asia_pyongyang_l

{-| Asia/Qatar -}
asia_qatar : () -> Zone
asia_qatar () = force asia_qatar_l

{-| Asia/Qyzylorda -}
asia_qyzylorda : () -> Zone
asia_qyzylorda () = force asia_qyzylorda_l

{-| Asia/Rangoon -}
asia_rangoon : () -> Zone
asia_rangoon () = force asia_rangoon_l

{-| Asia/Riyadh -}
asia_riyadh : () -> Zone
asia_riyadh () = force asia_riyadh_l

{-| Asia/Sakhalin -}
asia_sakhalin : () -> Zone
asia_sakhalin () = force asia_sakhalin_l

{-| Asia/Samarkand -}
asia_samarkand : () -> Zone
asia_samarkand () = force asia_samarkand_l

{-| Asia/Seoul -}
asia_seoul : () -> Zone
asia_seoul () = force asia_seoul_l

{-| Asia/Shanghai -}
asia_shanghai : () -> Zone
asia_shanghai () = force asia_shanghai_l

{-| Asia/Singapore -}
asia_singapore : () -> Zone
asia_singapore () = force asia_singapore_l

{-| Asia/Srednekolymsk -}
asia_srednekolymsk : () -> Zone
asia_srednekolymsk () = force asia_srednekolymsk_l

{-| Asia/Taipei -}
asia_taipei : () -> Zone
asia_taipei () = force asia_taipei_l

{-| Asia/Tashkent -}
asia_tashkent : () -> Zone
asia_tashkent () = force asia_tashkent_l

{-| Asia/Tbilisi -}
asia_tbilisi : () -> Zone
asia_tbilisi () = force asia_tbilisi_l

{-| Asia/Tehran -}
asia_tehran : () -> Zone
asia_tehran () = force asia_tehran_l

{-| Asia/Thimphu -}
asia_thimphu : () -> Zone
asia_thimphu () = force asia_thimphu_l

{-| Asia/Tokyo -}
asia_tokyo : () -> Zone
asia_tokyo () = force asia_tokyo_l

{-| Asia/Tomsk -}
asia_tomsk : () -> Zone
asia_tomsk () = force asia_tomsk_l

{-| Asia/Ulaanbaatar -}
asia_ulaanbaatar : () -> Zone
asia_ulaanbaatar () = force asia_ulaanbaatar_l

{-| Asia/Urumqi -}
asia_urumqi : () -> Zone
asia_urumqi () = force asia_urumqi_l

{-| Asia/Ust-Nera -}
asia_ust_nera : () -> Zone
asia_ust_nera () = force asia_ust_nera_l

{-| Asia/Vladivostok -}
asia_vladivostok : () -> Zone
asia_vladivostok () = force asia_vladivostok_l

{-| Asia/Yakutsk -}
asia_yakutsk : () -> Zone
asia_yakutsk () = force asia_yakutsk_l

{-| Asia/Yekaterinburg -}
asia_yekaterinburg : () -> Zone
asia_yekaterinburg () = force asia_yekaterinburg_l

{-| Asia/Yerevan -}
asia_yerevan : () -> Zone
asia_yerevan () = force asia_yerevan_l

{-| Atlantic/Azores -}
atlantic_azores : () -> Zone
atlantic_azores () = force atlantic_azores_l

{-| Atlantic/Bermuda -}
atlantic_bermuda : () -> Zone
atlantic_bermuda () = force atlantic_bermuda_l

{-| Atlantic/Canary -}
atlantic_canary : () -> Zone
atlantic_canary () = force atlantic_canary_l

{-| Atlantic/Cape_Verde -}
atlantic_cape_verde : () -> Zone
atlantic_cape_verde () = force atlantic_cape_verde_l

{-| Atlantic/Faroe -}
atlantic_faroe : () -> Zone
atlantic_faroe () = force atlantic_faroe_l

{-| Atlantic/Madeira -}
atlantic_madeira : () -> Zone
atlantic_madeira () = force atlantic_madeira_l

{-| Atlantic/Reykjavik -}
atlantic_reykjavik : () -> Zone
atlantic_reykjavik () = force atlantic_reykjavik_l

{-| Atlantic/South_Georgia -}
atlantic_south_georgia : () -> Zone
atlantic_south_georgia () = force atlantic_south_georgia_l

{-| Atlantic/Stanley -}
atlantic_stanley : () -> Zone
atlantic_stanley () = force atlantic_stanley_l

{-| Australia/Adelaide -}
australia_adelaide : () -> Zone
australia_adelaide () = force australia_adelaide_l

{-| Australia/Brisbane -}
australia_brisbane : () -> Zone
australia_brisbane () = force australia_brisbane_l

{-| Australia/Broken_Hill -}
australia_broken_hill : () -> Zone
australia_broken_hill () = force australia_broken_hill_l

{-| Australia/Currie -}
australia_currie : () -> Zone
australia_currie () = force australia_currie_l

{-| Australia/Darwin -}
australia_darwin : () -> Zone
australia_darwin () = force australia_darwin_l

{-| Australia/Eucla -}
australia_eucla : () -> Zone
australia_eucla () = force australia_eucla_l

{-| Australia/Hobart -}
australia_hobart : () -> Zone
australia_hobart () = force australia_hobart_l

{-| Australia/Lindeman -}
australia_lindeman : () -> Zone
australia_lindeman () = force australia_lindeman_l

{-| Australia/Lord_Howe -}
australia_lord_howe : () -> Zone
australia_lord_howe () = force australia_lord_howe_l

{-| Australia/Melbourne -}
australia_melbourne : () -> Zone
australia_melbourne () = force australia_melbourne_l

{-| Australia/Perth -}
australia_perth : () -> Zone
australia_perth () = force australia_perth_l

{-| Australia/Sydney -}
australia_sydney : () -> Zone
australia_sydney () = force australia_sydney_l

{-| CET -}
cet : () -> Zone
cet () = force cet_l

{-| CST6CDT -}
cst6cdt : () -> Zone
cst6cdt () = force cst6cdt_l

{-| EET -}
eet : () -> Zone
eet () = force eet_l

{-| Europe/Budapest -}
est : () -> Zone
est () = force est_l

{-| EST5EDT -}
est5edt : () -> Zone
est5edt () = force est5edt_l

{-| Etc/GMT-1 -}
etc_gmt_minus_1 : () -> Zone
etc_gmt_minus_1 () = force etc_gmt_minus_1_l

{-| Etc/GMT-10 -}
etc_gmt_minus_10 : () -> Zone
etc_gmt_minus_10 () = force etc_gmt_minus_10_l

{-| Etc/GMT-11 -}
etc_gmt_minus_11 : () -> Zone
etc_gmt_minus_11 () = force etc_gmt_minus_11_l

{-| Etc/GMT-12 -}
etc_gmt_minus_12 : () -> Zone
etc_gmt_minus_12 () = force etc_gmt_minus_12_l

{-| Etc/GMT-13 -}
etc_gmt_minus_13 : () -> Zone
etc_gmt_minus_13 () = force etc_gmt_minus_13_l

{-| Etc/GMT-14 -}
etc_gmt_minus_14 : () -> Zone
etc_gmt_minus_14 () = force etc_gmt_minus_14_l

{-| Etc/GMT-2 -}
etc_gmt_minus_2 : () -> Zone
etc_gmt_minus_2 () = force etc_gmt_minus_2_l

{-| Etc/GMT-3 -}
etc_gmt_minus_3 : () -> Zone
etc_gmt_minus_3 () = force etc_gmt_minus_3_l

{-| Etc/GMT-4 -}
etc_gmt_minus_4 : () -> Zone
etc_gmt_minus_4 () = force etc_gmt_minus_4_l

{-| Etc/GMT-5 -}
etc_gmt_minus_5 : () -> Zone
etc_gmt_minus_5 () = force etc_gmt_minus_5_l

{-| Etc/GMT-6 -}
etc_gmt_minus_6 : () -> Zone
etc_gmt_minus_6 () = force etc_gmt_minus_6_l

{-| Etc/GMT-7 -}
etc_gmt_minus_7 : () -> Zone
etc_gmt_minus_7 () = force etc_gmt_minus_7_l

{-| Etc/GMT-8 -}
etc_gmt_minus_8 : () -> Zone
etc_gmt_minus_8 () = force etc_gmt_minus_8_l

{-| Etc/GMT-9 -}
etc_gmt_minus_9 : () -> Zone
etc_gmt_minus_9 () = force etc_gmt_minus_9_l

{-| Etc/GMT+0 -}
etc_gmt_plus_0 : () -> Zone
etc_gmt_plus_0 () = force etc_gmt_plus_0_l

{-| Etc/GMT+1 -}
etc_gmt_plus_1 : () -> Zone
etc_gmt_plus_1 () = force etc_gmt_plus_1_l

{-| Etc/GMT+10 -}
etc_gmt_plus_10 : () -> Zone
etc_gmt_plus_10 () = force etc_gmt_plus_10_l

{-| Etc/GMT+11 -}
etc_gmt_plus_11 : () -> Zone
etc_gmt_plus_11 () = force etc_gmt_plus_11_l

{-| Etc/GMT+12 -}
etc_gmt_plus_12 : () -> Zone
etc_gmt_plus_12 () = force etc_gmt_plus_12_l

{-| Etc/GMT+2 -}
etc_gmt_plus_2 : () -> Zone
etc_gmt_plus_2 () = force etc_gmt_plus_2_l

{-| Etc/GMT+3 -}
etc_gmt_plus_3 : () -> Zone
etc_gmt_plus_3 () = force etc_gmt_plus_3_l

{-| Etc/GMT+4 -}
etc_gmt_plus_4 : () -> Zone
etc_gmt_plus_4 () = force etc_gmt_plus_4_l

{-| Etc/GMT+5 -}
etc_gmt_plus_5 : () -> Zone
etc_gmt_plus_5 () = force etc_gmt_plus_5_l

{-| Etc/GMT+6 -}
etc_gmt_plus_6 : () -> Zone
etc_gmt_plus_6 () = force etc_gmt_plus_6_l

{-| Etc/GMT+7 -}
etc_gmt_plus_7 : () -> Zone
etc_gmt_plus_7 () = force etc_gmt_plus_7_l

{-| Etc/GMT+8 -}
etc_gmt_plus_8 : () -> Zone
etc_gmt_plus_8 () = force etc_gmt_plus_8_l

{-| Etc/GMT+9 -}
etc_gmt_plus_9 : () -> Zone
etc_gmt_plus_9 () = force etc_gmt_plus_9_l

{-| Etc/UCT -}
etc_uct : () -> Zone
etc_uct () = force etc_uct_l

{-| Etc/UTC -}
etc_utc : () -> Zone
etc_utc () = force etc_utc_l

{-| Europe/Amsterdam -}
europe_amsterdam : () -> Zone
europe_amsterdam () = force europe_amsterdam_l

{-| Europe/Andorra -}
europe_andorra : () -> Zone
europe_andorra () = force europe_andorra_l

{-| Europe/Astrakhan -}
europe_astrakhan : () -> Zone
europe_astrakhan () = force europe_astrakhan_l

{-| Europe/Athens -}
europe_athens : () -> Zone
europe_athens () = force europe_athens_l

{-| Europe/Belgrade -}
europe_belgrade : () -> Zone
europe_belgrade () = force europe_belgrade_l

{-| Europe/Berlin -}
europe_berlin : () -> Zone
europe_berlin () = force europe_berlin_l

{-| Europe/Brussels -}
europe_brussels : () -> Zone
europe_brussels () = force europe_brussels_l

{-| Europe/Bucharest -}
europe_bucharest : () -> Zone
europe_bucharest () = force europe_bucharest_l

{-| Europe/Budapest -}
europe_budapest : () -> Zone
europe_budapest () = force europe_budapest_l

{-| Europe/Chisinau -}
europe_chisinau : () -> Zone
europe_chisinau () = force europe_chisinau_l

{-| Europe/Copenhagen -}
europe_copenhagen : () -> Zone
europe_copenhagen () = force europe_copenhagen_l

{-| Europe/Dublin -}
europe_dublin : () -> Zone
europe_dublin () = force europe_dublin_l

{-| Europe/Gibraltar -}
europe_gibraltar : () -> Zone
europe_gibraltar () = force europe_gibraltar_l

{-| Europe/Helsinki -}
europe_helsinki : () -> Zone
europe_helsinki () = force europe_helsinki_l

{-| Europe/Istanbul -}
europe_istanbul : () -> Zone
europe_istanbul () = force europe_istanbul_l

{-| Europe/Kaliningrad -}
europe_kaliningrad : () -> Zone
europe_kaliningrad () = force europe_kaliningrad_l

{-| Europe/Kiev -}
europe_kiev : () -> Zone
europe_kiev () = force europe_kiev_l

{-| Europe/Kirov -}
europe_kirov : () -> Zone
europe_kirov () = force europe_kirov_l

{-| Europe/Lisbon -}
europe_lisbon : () -> Zone
europe_lisbon () = force europe_lisbon_l

{-| Europe/London -}
europe_london : () -> Zone
europe_london () = force europe_london_l

{-| Europe/Luxembourg -}
europe_luxembourg : () -> Zone
europe_luxembourg () = force europe_luxembourg_l

{-| Europe/Madrid -}
europe_madrid : () -> Zone
europe_madrid () = force europe_madrid_l

{-| Europe/Malta -}
europe_malta : () -> Zone
europe_malta () = force europe_malta_l

{-| Europe/Minsk -}
europe_minsk : () -> Zone
europe_minsk () = force europe_minsk_l

{-| Europe/Monaco -}
europe_monaco : () -> Zone
europe_monaco () = force europe_monaco_l

{-| Europe/Moscow -}
europe_moscow : () -> Zone
europe_moscow () = force europe_moscow_l

{-| Europe/Oslo -}
europe_oslo : () -> Zone
europe_oslo () = force europe_oslo_l

{-| Europe/Paris -}
europe_paris : () -> Zone
europe_paris () = force europe_paris_l

{-| Europe/Prague -}
europe_prague : () -> Zone
europe_prague () = force europe_prague_l

{-| Europe/Riga -}
europe_riga : () -> Zone
europe_riga () = force europe_riga_l

{-| Europe/Rome -}
europe_rome : () -> Zone
europe_rome () = force europe_rome_l

{-| Europe/Samara -}
europe_samara : () -> Zone
europe_samara () = force europe_samara_l

{-| Europe/Simferopol -}
europe_simferopol : () -> Zone
europe_simferopol () = force europe_simferopol_l

{-| Europe/Sofia -}
europe_sofia : () -> Zone
europe_sofia () = force europe_sofia_l

{-| Europe/Stockholm -}
europe_stockholm : () -> Zone
europe_stockholm () = force europe_stockholm_l

{-| Europe/Tallinn -}
europe_tallinn : () -> Zone
europe_tallinn () = force europe_tallinn_l

{-| Europe/Tirane -}
europe_tirane : () -> Zone
europe_tirane () = force europe_tirane_l

{-| Europe/Ulyanovsk -}
europe_ulyanovsk : () -> Zone
europe_ulyanovsk () = force europe_ulyanovsk_l

{-| Europe/Uzhgorod -}
europe_uzhgorod : () -> Zone
europe_uzhgorod () = force europe_uzhgorod_l

{-| Europe/Vienna -}
europe_vienna : () -> Zone
europe_vienna () = force europe_vienna_l

{-| Europe/Vilnius -}
europe_vilnius : () -> Zone
europe_vilnius () = force europe_vilnius_l

{-| Europe/Volgograd -}
europe_volgograd : () -> Zone
europe_volgograd () = force europe_volgograd_l

{-| Europe/Warsaw -}
europe_warsaw : () -> Zone
europe_warsaw () = force europe_warsaw_l

{-| Europe/Zaporozhye -}
europe_zaporozhye : () -> Zone
europe_zaporozhye () = force europe_zaporozhye_l

{-| Europe/Zurich -}
europe_zurich : () -> Zone
europe_zurich () = force europe_zurich_l

{-| HST -}
hst : () -> Zone
hst () = force hst_l

{-| Indian/Chagos -}
indian_chagos : () -> Zone
indian_chagos () = force indian_chagos_l

{-| Indian/Christmas -}
indian_christmas : () -> Zone
indian_christmas () = force indian_christmas_l

{-| Indian/Cocos -}
indian_cocos : () -> Zone
indian_cocos () = force indian_cocos_l

{-| Indian/Kerguelen -}
indian_kerguelen : () -> Zone
indian_kerguelen () = force indian_kerguelen_l

{-| Indian/Mahe -}
indian_mahe : () -> Zone
indian_mahe () = force indian_mahe_l

{-| Indian/Maldives -}
indian_maldives : () -> Zone
indian_maldives () = force indian_maldives_l

{-| Indian/Mauritius -}
indian_mauritius : () -> Zone
indian_mauritius () = force indian_mauritius_l

{-| Indian/Reunion -}
indian_reunion : () -> Zone
indian_reunion () = force indian_reunion_l

{-| MET -}
met : () -> Zone
met () = force met_l

{-| MST -}
mst : () -> Zone
mst () = force mst_l

{-| MST7MDT -}
mst7mdt : () -> Zone
mst7mdt () = force mst7mdt_l

{-| Pacific/Apia -}
pacific_apia : () -> Zone
pacific_apia () = force pacific_apia_l

{-| Pacific/Auckland -}
pacific_auckland : () -> Zone
pacific_auckland () = force pacific_auckland_l

{-| Pacific/Bougainville -}
pacific_bougainville : () -> Zone
pacific_bougainville () = force pacific_bougainville_l

{-| Pacific/Chatham -}
pacific_chatham : () -> Zone
pacific_chatham () = force pacific_chatham_l

{-| Pacific/Chuuk -}
pacific_chuuk : () -> Zone
pacific_chuuk () = force pacific_chuuk_l

{-| Pacific/Easter -}
pacific_easter : () -> Zone
pacific_easter () = force pacific_easter_l

{-| Pacific/Efate -}
pacific_efate : () -> Zone
pacific_efate () = force pacific_efate_l

{-| Pacific/Enderbury -}
pacific_enderbury : () -> Zone
pacific_enderbury () = force pacific_enderbury_l

{-| Pacific/Fakaofo -}
pacific_fakaofo : () -> Zone
pacific_fakaofo () = force pacific_fakaofo_l

{-| Pacific/Fiji -}
pacific_fiji : () -> Zone
pacific_fiji () = force pacific_fiji_l

{-| Pacific/Funafuti -}
pacific_funafuti : () -> Zone
pacific_funafuti () = force pacific_funafuti_l

{-| Pacific/Galapagos -}
pacific_galapagos : () -> Zone
pacific_galapagos () = force pacific_galapagos_l

{-| Pacific/Gambier -}
pacific_gambier : () -> Zone
pacific_gambier () = force pacific_gambier_l

{-| Pacific/Guadalcanal -}
pacific_guadalcanal : () -> Zone
pacific_guadalcanal () = force pacific_guadalcanal_l

{-| Pacific/Guam -}
pacific_guam : () -> Zone
pacific_guam () = force pacific_guam_l

{-| Pacific/Honolulu -}
pacific_honolulu : () -> Zone
pacific_honolulu () = force pacific_honolulu_l

{-| Pacific/Kiritimati -}
pacific_kiritimati : () -> Zone
pacific_kiritimati () = force pacific_kiritimati_l

{-| Pacific/Kosrae -}
pacific_kosrae : () -> Zone
pacific_kosrae () = force pacific_kosrae_l

{-| Pacific/Kwajalein -}
pacific_kwajalein : () -> Zone
pacific_kwajalein () = force pacific_kwajalein_l

{-| Pacific/Majuro -}
pacific_majuro : () -> Zone
pacific_majuro () = force pacific_majuro_l

{-| Pacific/Marquesas -}
pacific_marquesas : () -> Zone
pacific_marquesas () = force pacific_marquesas_l

{-| Pacific/Nauru -}
pacific_nauru : () -> Zone
pacific_nauru () = force pacific_nauru_l

{-| Pacific/Niue -}
pacific_niue : () -> Zone
pacific_niue () = force pacific_niue_l

{-| Pacific/Norfolk -}
pacific_norfolk : () -> Zone
pacific_norfolk () = force pacific_norfolk_l

{-| Pacific/Noumea -}
pacific_noumea : () -> Zone
pacific_noumea () = force pacific_noumea_l

{-| Pacific/Pago_Pago -}
pacific_pago_pago : () -> Zone
pacific_pago_pago () = force pacific_pago_pago_l

{-| Pacific/Palau -}
pacific_palau : () -> Zone
pacific_palau () = force pacific_palau_l

{-| Pacific/Pitcairn -}
pacific_pitcairn : () -> Zone
pacific_pitcairn () = force pacific_pitcairn_l

{-| Pacific/Pohnpei -}
pacific_pohnpei : () -> Zone
pacific_pohnpei () = force pacific_pohnpei_l

{-| Pacific/Port_Moresby -}
pacific_port_moresby : () -> Zone
pacific_port_moresby () = force pacific_port_moresby_l

{-| Pacific/Rarotonga -}
pacific_rarotonga : () -> Zone
pacific_rarotonga () = force pacific_rarotonga_l


{-| Pacific/Tahiti -}
pacific_tahiti : () -> Zone
pacific_tahiti () = force pacific_tahiti_l

{-| Pacific/Tarawa -}
pacific_tarawa : () -> Zone
pacific_tarawa () = force pacific_tarawa_l

{-| Pacific/Tongatapu -}
pacific_tongatapu : () -> Zone
pacific_tongatapu () = force pacific_tongatapu_l

{-| Pacific/Wake -}
pacific_wake : () -> Zone
pacific_wake () = force pacific_wake_l

{-| Pacific/Wallis -}
pacific_wallis : () -> Zone
pacific_wallis () = force pacific_wallis_l

{-| PST8PDT -}
pst8pdt : () -> Zone
pst8pdt () = force pst8pdt_l

{-| WET -}
wet : () -> Zone
wet () = force wet_l


-- Links
-- -----

{-| Africa/Addis_Ababa -}
africa_addis_ababa : () -> Zone
africa_addis_ababa () = force (link "Africa/Addis_Ababa" africa_nairobi_l)

{-| Africa/Asmara -}
africa_asmara : () -> Zone
africa_asmara () = force (link "Africa/Asmara" africa_nairobi_l)

{-| Africa/Asmera -}
africa_asmera : () -> Zone
africa_asmera () = force (link "Africa/Asmera" africa_nairobi_l)

{-| Africa/Bamako -}
africa_bamako : () -> Zone
africa_bamako () = force (link "Africa/Bamako" africa_abidjan_l)

{-| Africa/Bangui -}
africa_bangui : () -> Zone
africa_bangui () = force (link "Africa/Bangui" africa_lagos_l)

{-| Africa/Banjul -}
africa_banjul : () -> Zone
africa_banjul () = force (link "Africa/Banjul" africa_abidjan_l)

{-| Africa/Blantyre -}
africa_blantyre : () -> Zone
africa_blantyre () = force (link "Africa/Blantyre" africa_maputo_l)

{-| Africa/Brazzaville -}
africa_brazzaville : () -> Zone
africa_brazzaville () = force (link "Africa/Brazzaville" africa_lagos_l)

{-| Africa/Bujumbura -}
africa_bujumbura : () -> Zone
africa_bujumbura () = force (link "Africa/Bujumbura" africa_maputo_l)

{-| Africa/Conakry -}
africa_conakry : () -> Zone
africa_conakry () = force (link "Africa/Conakry" africa_abidjan_l)

{-| Africa/Dakar -}
africa_dakar : () -> Zone
africa_dakar () = force (link "Africa/Dakar" africa_abidjan_l)

{-| Africa/Dar_es_Salaam -}
africa_dar_es_salaam : () -> Zone
africa_dar_es_salaam () = force (link "Africa/Dar_es_Salaam" africa_nairobi_l)

{-| Africa/Djibouti -}
africa_djibouti : () -> Zone
africa_djibouti () = force (link "Africa/Djibouti" africa_nairobi_l)

{-| Africa/Douala -}
africa_douala : () -> Zone
africa_douala () = force (link "Africa/Douala" africa_lagos_l)

{-| Africa/Freetown -}
africa_freetown : () -> Zone
africa_freetown () = force (link "Africa/Freetown" africa_abidjan_l)

{-| Africa/Gaborone -}
africa_gaborone : () -> Zone
africa_gaborone () = force (link "Africa/Gaborone" africa_maputo_l)

{-| Africa/Harare -}
africa_harare : () -> Zone
africa_harare () = force (link "Africa/Harare" africa_maputo_l)

{-| Africa/Juba -}
africa_juba : () -> Zone
africa_juba () = force (link "Africa/Juba" africa_khartoum_l)

{-| Africa/Kampala -}
africa_kampala : () -> Zone
africa_kampala () = force (link "Africa/Kampala" africa_nairobi_l)

{-| Africa/Kigali -}
africa_kigali : () -> Zone
africa_kigali () = force (link "Africa/Kigali" africa_maputo_l)

{-| Africa/Kinshasa -}
africa_kinshasa : () -> Zone
africa_kinshasa () = force (link "Africa/Kinshasa" africa_lagos_l)

{-| Africa/Libreville -}
africa_libreville : () -> Zone
africa_libreville () = force (link "Africa/Libreville" africa_lagos_l)

{-| Africa/Lome -}
africa_lome : () -> Zone
africa_lome () = force (link "Africa/Lome" africa_abidjan_l)

{-| Africa/Luanda -}
africa_luanda : () -> Zone
africa_luanda () = force (link "Africa/Luanda" africa_lagos_l)

{-| Africa/Lubumbashi -}
africa_lubumbashi : () -> Zone
africa_lubumbashi () = force (link "Africa/Lubumbashi" africa_maputo_l)

{-| Africa/Lusaka -}
africa_lusaka : () -> Zone
africa_lusaka () = force (link "Africa/Lusaka" africa_maputo_l)

{-| Africa/Malabo -}
africa_malabo : () -> Zone
africa_malabo () = force (link "Africa/Malabo" africa_lagos_l)

{-| Africa/Maseru -}
africa_maseru : () -> Zone
africa_maseru () = force (link "Africa/Maseru" africa_johannesburg_l)

{-| Africa/Mbabane -}
africa_mbabane : () -> Zone
africa_mbabane () = force (link "Africa/Mbabane" africa_johannesburg_l)

{-| Africa/Mogadishu -}
africa_mogadishu : () -> Zone
africa_mogadishu () = force (link "Africa/Mogadishu" africa_nairobi_l)

{-| Africa/Niamey -}
africa_niamey : () -> Zone
africa_niamey () = force (link "Africa/Niamey" africa_lagos_l)

{-| Africa/Nouakchott -}
africa_nouakchott : () -> Zone
africa_nouakchott () = force (link "Africa/Nouakchott" africa_abidjan_l)

{-| Africa/Ouagadougou -}
africa_ouagadougou : () -> Zone
africa_ouagadougou () = force (link "Africa/Ouagadougou" africa_abidjan_l)

{-| Africa/Porto-Novo -}
africa_porto_novo : () -> Zone
africa_porto_novo () = force (link "Africa/Porto-Novo" africa_lagos_l)

{-| Africa/Sao_Tome -}
africa_sao_tome : () -> Zone
africa_sao_tome () = force (link "Africa/Sao_Tome" africa_abidjan_l)

{-| Africa/Timbuktu -}
africa_timbuktu : () -> Zone
africa_timbuktu () = force (link "Africa/Timbuktu" africa_abidjan_l)

{-| America/Anguilla -}
america_anguilla : () -> Zone
america_anguilla () = force (link "America/Anguilla" america_port_of_spain_l)

{-| America/Antigua -}
america_antigua : () -> Zone
america_antigua () = force (link "America/Antigua" america_port_of_spain_l)

{-| America/Argentina/ComodRivadavia -}
america_argentina_comodrivadavia : () -> Zone
america_argentina_comodrivadavia () = force (link "America/Argentina/ComodRivadavia" america_argentina_catamarca_l)

{-| America/Aruba -}
america_aruba : () -> Zone
america_aruba () = force (link "America/Aruba" america_curacao_l)

{-| America/Atka -}
america_atka : () -> Zone
america_atka () = force (link "America/Atka" america_adak_l)

{-| America/Buenos_Aires -}
america_buenos_aires : () -> Zone
america_buenos_aires () = force (link "America/Buenos_Aires" america_argentina_buenos_aires_l)

{-| America/Catamarca -}
america_catamarca : () -> Zone
america_catamarca () = force (link "America/Catamarca" america_argentina_catamarca_l)

{-| America/Cayman -}
america_cayman : () -> Zone
america_cayman () = force (link "America/Cayman" america_panama_l)

{-| America/Coral_Harbour -}
america_coral_harbour : () -> Zone
america_coral_harbour () = force (link "America/Coral_Harbour" america_atikokan_l)

{-| America/Cordoba -}
america_cordoba : () -> Zone
america_cordoba () = force (link "America/Cordoba" america_argentina_cordoba_l)

{-| America/Dominica -}
america_dominica : () -> Zone
america_dominica () = force (link "America/Dominica" america_port_of_spain_l)

{-| America/Ensenada -}
america_ensenada : () -> Zone
america_ensenada () = force (link "America/Ensenada" america_tijuana_l)

{-| America/Grenada -}
america_grenada : () -> Zone
america_grenada () = force (link "America/Grenada" america_port_of_spain_l)

{-| America/Guadeloupe -}
america_guadeloupe : () -> Zone
america_guadeloupe () = force (link "America/Guadeloupe" america_port_of_spain_l)

{-| America/Indiana/Indianapolis -}
america_indiana_indianapolis : () -> Zone
america_indiana_indianapolis () = force (link "America/Indiana/Indianapolis" america_fort_wayne_l)

{-| America/Indianapolis -}
america_indianapolis : () -> Zone
america_indianapolis () = force (link "America/Indianapolis" america_fort_wayne_l)

{-| America/Jujuy -}
america_jujuy : () -> Zone
america_jujuy () = force (link "America/Jujuy" america_argentina_jujuy_l)

{-| America/Knox_IN -}
america_knox_in : () -> Zone
america_knox_in () = force (link "America/Knox_IN" america_indiana_knox_l)

{-| America/Kralendijk -}
america_kralendijk : () -> Zone
america_kralendijk () = force (link "America/Kralendijk" america_curacao_l)

{-| America/Louisville -}
america_louisville : () -> Zone
america_louisville () = force (link "America/Louisville" america_kentucky_louisville_l)

{-| America/Lower_Princes -}
america_lower_princes : () -> Zone
america_lower_princes () = force (link "America/Lower_Princes" america_curacao_l)

{-| America/Marigot -}
america_marigot : () -> Zone
america_marigot () = force (link "America/Marigot" america_port_of_spain_l)

{-| America/Mendoza -}
america_mendoza : () -> Zone
america_mendoza () = force (link "America/Mendoza" america_argentina_mendoza_l)

{-| America/Montreal -}
america_montreal : () -> Zone
america_montreal () = force (link "America/Montreal" america_toronto_l)

{-| America/Montserrat -}
america_montserrat : () -> Zone
america_montserrat () = force (link "America/Montserrat" america_port_of_spain_l)

{-| America/Porto_Acre -}
america_porto_acre : () -> Zone
america_porto_acre () = force (link "America/Porto_Acre" america_rio_branco_l)

{-| America/Rosario -}
america_rosario : () -> Zone
america_rosario () = force (link "America/Rosario" america_argentina_cordoba_l)

{-| America/Santa_Isabel -}
america_santa_isabel : () -> Zone
america_santa_isabel () = force (link "America/Santa_Isabel" america_tijuana_l)

{-| America/Shiprock -}
america_shiprock : () -> Zone
america_shiprock () = force (link "America/Shiprock" america_denver_l)

{-| America/St_Barthelemy -}
america_st_barthelemy : () -> Zone
america_st_barthelemy () = force (link "America/St_Barthelemy" america_port_of_spain_l)

{-| America/St_Kitts -}
america_st_kitts : () -> Zone
america_st_kitts () = force (link "America/St_Kitts" america_port_of_spain_l)

{-| America/St_Lucia -}
america_st_lucia : () -> Zone
america_st_lucia () = force (link "America/St_Lucia" america_port_of_spain_l)

{-| America/St_Thomas -}
america_st_thomas : () -> Zone
america_st_thomas () = force (link "America/St_Thomas" america_port_of_spain_l)

{-| America/St_Vincent -}
america_st_vincent : () -> Zone
america_st_vincent () = force (link "America/St_Vincent" america_port_of_spain_l)

{-| America/Tortola -}
america_tortola : () -> Zone
america_tortola () = force (link "America/Tortola" america_port_of_spain_l)

{-| America/Virgin -}
america_virgin : () -> Zone
america_virgin () = force (link "America/Virgin" america_port_of_spain_l)

{-| Antarctica/McMurdo -}
antarctica_mcmurdo : () -> Zone
antarctica_mcmurdo () = force (link "Antarctica/McMurdo" pacific_auckland_l)

{-| Antarctica/South_Pole -}
antarctica_south_pole : () -> Zone
antarctica_south_pole () = force (link "Antarctica/South_Pole" pacific_auckland_l)

{-| Arctic/Longyearbyen -}
arctic_longyearbyen : () -> Zone
arctic_longyearbyen () = force (link "Arctic/Longyearbyen" europe_oslo_l)

{-| Asia/Aden -}
asia_aden : () -> Zone
asia_aden () = force (link "Asia/Aden" asia_riyadh_l)

{-| Asia/Ashkhabad -}
asia_ashkhabad : () -> Zone
asia_ashkhabad () = force (link "Asia/Ashkhabad" asia_ashgabat_l)

{-| Asia/Bahrain -}
asia_bahrain : () -> Zone
asia_bahrain () = force (link "Asia/Bahrain" asia_qatar_l)

{-| Asia/Calcutta -}
asia_calcutta : () -> Zone
asia_calcutta () = force (link "Asia/Calcutta" asia_kolkata_l)

{-| Asia/Chongqing -}
asia_chongqing : () -> Zone
asia_chongqing () = force (link "Asia/Chongqing" asia_shanghai_l)

{-| Asia/Chungking -}
asia_chungking : () -> Zone
asia_chungking () = force (link "Asia/Chungking" asia_shanghai_l)

{-| Asia/Dacca -}
asia_dacca : () -> Zone
asia_dacca () = force (link "Asia/Dacca" asia_dhaka_l)

{-| Asia/Harbin -}
asia_harbin : () -> Zone
asia_harbin () = force (link "Asia/Harbin" asia_shanghai_l)

{-| Asia/Istanbul -}
asia_istanbul : () -> Zone
asia_istanbul () = force (link "Asia/Istanbul" europe_istanbul_l)

{-| Asia/Kashgar -}
asia_kashgar : () -> Zone
asia_kashgar () = force (link "Asia/Kashgar" asia_urumqi_l)

{-| Asia/Katmandu -}
asia_katmandu : () -> Zone
asia_katmandu () = force (link "Asia/Katmandu" asia_kathmandu_l)

{-| Asia/Kuwait -}
asia_kuwait : () -> Zone
asia_kuwait () = force (link "Asia/Kuwait" asia_riyadh_l)

{-| Asia/Macao -}
asia_macao : () -> Zone
asia_macao () = force (link "Asia/Macao" asia_macau_l)

{-| Asia/Muscat -}
asia_muscat : () -> Zone
asia_muscat () = force (link "Asia/Muscat" asia_dubai_l)

{-| Asia/Phnom_Penh -}
asia_phnom_penh : () -> Zone
asia_phnom_penh () = force (link "Asia/Phnom_Penh" asia_bangkok_l)

{-| Asia/Saigon -}
asia_saigon : () -> Zone
asia_saigon () = force (link "Asia/Saigon" asia_ho_chi_minh_l)

{-| Asia/Tel_Aviv -}
asia_tel_aviv : () -> Zone
asia_tel_aviv () = force (link "Asia/Tel_Aviv" asia_jerusalem_l)

{-| Asia/Thimbu -}
asia_thimbu : () -> Zone
asia_thimbu () = force (link "Asia/Thimbu" asia_thimphu_l)

{-| Asia/Ujung_Pandang -}
asia_ujung_pandang : () -> Zone
asia_ujung_pandang () = force (link "Asia/Ujung_Pandang" asia_makassar_l)

{-| Asia/Ulan_Bator -}
asia_ulan_bator : () -> Zone
asia_ulan_bator () = force (link "Asia/Ulan_Bator" asia_ulaanbaatar_l)

{-| Asia/Vientiane -}
asia_vientiane : () -> Zone
asia_vientiane () = force (link "Asia/Vientiane" asia_bangkok_l)

{-| Atlantic/Faeroe -}
atlantic_faeroe : () -> Zone
atlantic_faeroe () = force (link "Atlantic/Faeroe" atlantic_faroe_l)

{-| Atlantic/Jan_Mayen -}
atlantic_jan_mayen : () -> Zone
atlantic_jan_mayen () = force (link "Atlantic/Jan_Mayen" europe_oslo_l)

{-| Atlantic/St_Helena -}
atlantic_st_helena : () -> Zone
atlantic_st_helena () = force (link "Atlantic/St_Helena" africa_abidjan_l)

{-| Australia/ACT -}
australia_act : () -> Zone
australia_act () = force (link "Australia/ACT" australia_sydney_l)

{-| Australia/Canberra -}
australia_canberra : () -> Zone
australia_canberra () = force (link "Australia/Canberra" australia_sydney_l)

{-| Australia/LHI -}
australia_lhi : () -> Zone
australia_lhi () = force (link "Australia/LHI" australia_lord_howe_l)

{-| Australia/North -}
australia_north : () -> Zone
australia_north () = force (link "Australia/North" australia_darwin_l)

{-| Australia/NSW -}
australia_nsw : () -> Zone
australia_nsw () = force (link "Australia/NSW" australia_sydney_l)

{-| Australia/Queensland -}
australia_queensland : () -> Zone
australia_queensland () = force (link "Australia/Queensland" australia_brisbane_l)

{-| Australia/South -}
australia_south : () -> Zone
australia_south () = force (link "Australia/South" australia_adelaide_l)

{-| Australia/Tasmania -}
australia_tasmania : () -> Zone
australia_tasmania () = force (link "Australia/Tasmania" australia_hobart_l)

{-| Australia/Victoria -}
australia_victoria : () -> Zone
australia_victoria () = force (link "Australia/Victoria" australia_melbourne_l)

{-| Australia/West -}
australia_west : () -> Zone
australia_west () = force (link "Australia/West" australia_perth_l)

{-| Australia/Yancowinna -}
australia_yancowinna : () -> Zone
australia_yancowinna () = force (link "Australia/Yancowinna" australia_broken_hill_l)

{-| Brazil/Acre -}
brazil_acre : () -> Zone
brazil_acre () = force (link "Brazil/Acre" america_rio_branco_l)

{-| Brazil/DeNoronha -}
brazil_denoronha : () -> Zone
brazil_denoronha () = force (link "Brazil/DeNoronha" america_noronha_l)

{-| Brazil/East -}
brazil_east : () -> Zone
brazil_east () = force (link "Brazil/East" america_sao_paulo_l)

{-| Brazil/West -}
brazil_west : () -> Zone
brazil_west () = force (link "Brazil/West" america_manaus_l)

{-| Canada/Atlantic -}
canada_atlantic : () -> Zone
canada_atlantic () = force (link "Canada/Atlantic" america_halifax_l)

{-| Canada/Central -}
canada_central : () -> Zone
canada_central () = force (link "Canada/Central" america_winnipeg_l)

{-| Canada/East-Saskatchewan -}
canada_east_saskatchewan : () -> Zone
canada_east_saskatchewan () = force (link "Canada/East-Saskatchewan" america_regina_l)

{-| Canada/Eastern -}
canada_eastern : () -> Zone
canada_eastern () = force (link "Canada/Eastern" america_toronto_l)

{-| Canada/Mountain -}
canada_mountain : () -> Zone
canada_mountain () = force (link "Canada/Mountain" america_edmonton_l)

{-| Canada/Newfoundland -}
canada_newfoundland : () -> Zone
canada_newfoundland () = force (link "Canada/Newfoundland" america_st_johns_l)

{-| Canada/Pacific -}
canada_pacific : () -> Zone
canada_pacific () = force (link "Canada/Pacific" america_vancouver_l)

{-| Canada/Saskatchewan -}
canada_saskatchewan : () -> Zone
canada_saskatchewan () = force (link "Canada/Saskatchewan" america_regina_l)

{-| Canada/Yukon -}
canada_yukon : () -> Zone
canada_yukon () = force (link "Canada/Yukon" america_whitehorse_l)

{-| Chile/Continental -}
chile_continental : () -> Zone
chile_continental () = force (link "Chile/Continental" america_santiago_l)

{-| Chile/EasterIsland -}
chile_easterisland : () -> Zone
chile_easterisland () = force (link "Chile/EasterIsland" pacific_easter_l)

{-| Cuba -}
cuba : () -> Zone
cuba () = force (link "Cuba" america_havana_l)

{-| Egypt -}
egypt : () -> Zone
egypt () = force (link "Egypt" africa_cairo_l)

{-| Eire -}
eire : () -> Zone
eire () = force (link "Eire" europe_dublin_l)

{-| Etc/GMT -}
etc_gmt : () -> Zone
etc_gmt () = force (link "Etc/GMT" etc_gmt_plus_0_l)

{-| Etc/GMT0 -}
etc_gmt_0 : () -> Zone
etc_gmt_0 () = force (link "Etc/GMT0" etc_gmt_plus_0_l)

{-| Etc/GMT-0 -}
etc_gmt_minus_0 : () -> Zone
etc_gmt_minus_0 () = force (link "Etc/GMT-0" etc_gmt_plus_0_l)

{-| Etc/Greenwich -}
etc_greenwich : () -> Zone
etc_greenwich () = force (link "Etc/Greenwich" etc_gmt_plus_0_l)

{-| Etc/Universal -}
etc_universal : () -> Zone
etc_universal () = force (link "Etc/Universal" etc_utc_l)

{-| Etc/Zulu -}
etc_zulu : () -> Zone
etc_zulu () = force (link "Etc/Zulu" etc_utc_l)

{-| Europe/Belfast -}
europe_belfast : () -> Zone
europe_belfast () = force (link "Europe/Belfast" europe_london_l)

{-| Europe/Bratislava -}
europe_bratislava : () -> Zone
europe_bratislava () = force (link "Europe/Bratislava" europe_prague_l)

{-| Europe/Busingen -}
europe_busingen : () -> Zone
europe_busingen () = force (link "Europe/Busingen" europe_zurich_l)

{-| Europe/Guernsey -}
europe_guernsey : () -> Zone
europe_guernsey () = force (link "Europe/Guernsey" europe_london_l)

{-| Europe/Isle_of_Man -}
europe_isle_of_man : () -> Zone
europe_isle_of_man () = force (link "Europe/Isle_of_Man" europe_london_l)

{-| Europe/Jersey -}
europe_jersey : () -> Zone
europe_jersey () = force (link "Europe/Jersey" europe_london_l)

{-| Europe/Ljubljana -}
europe_ljubljana : () -> Zone
europe_ljubljana () = force (link "Europe/Ljubljana" europe_belgrade_l)

{-| Europe/Mariehamn -}
europe_mariehamn : () -> Zone
europe_mariehamn () = force (link "Europe/Mariehamn" europe_helsinki_l)

{-| Europe/Nicosia -}
europe_nicosia : () -> Zone
europe_nicosia () = force (link "Europe/Nicosia" asia_nicosia_l)

{-| Europe/Podgorica -}
europe_podgorica : () -> Zone
europe_podgorica () = force (link "Europe/Podgorica" europe_belgrade_l)

{-| Europe/San_Marino -}
europe_san_marino : () -> Zone
europe_san_marino () = force (link "Europe/San_Marino" europe_rome_l)

{-| Europe/Sarajevo -}
europe_sarajevo : () -> Zone
europe_sarajevo () = force (link "Europe/Sarajevo" europe_belgrade_l)

{-| Europe/Skopje -}
europe_skopje : () -> Zone
europe_skopje () = force (link "Europe/Skopje" europe_belgrade_l)

{-| Europe/Tiraspol -}
europe_tiraspol : () -> Zone
europe_tiraspol () = force (link "Europe/Tiraspol" europe_chisinau_l)

{-| Europe/Vaduz -}
europe_vaduz : () -> Zone
europe_vaduz () = force (link "Europe/Vaduz" europe_zurich_l)

{-| Europe/Vatican -}
europe_vatican : () -> Zone
europe_vatican () = force (link "Europe/Vatican" europe_rome_l)

{-| Europe/Zagreb -}
europe_zagreb : () -> Zone
europe_zagreb () = force (link "Europe/Zagreb" europe_belgrade_l)

{-| GB -}
gb : () -> Zone
gb () = force (link "GB" europe_london_l)

{-| GB-Eire -}
gb_eire : () -> Zone
gb_eire () = force (link "GB-Eire" europe_london_l)

{-| GMT -}
gmt : () -> Zone
gmt () = force (link "GMT" etc_gmt_plus_0_l)

{-| GMT0 -}
gmt_0 : () -> Zone
gmt_0 () = force (link "GMT0" etc_gmt_plus_0_l)

{-| GMT-0 -}
gmt_minus_0 : () -> Zone
gmt_minus_0 () = force (link "GMT-0" etc_gmt_plus_0_l)

{-| GMT+0 -}
gmt_plus_0 : () -> Zone
gmt_plus_0 () = force (link "GMT+0" etc_gmt_plus_0_l)

{-| Greenwich -}
greenwich : () -> Zone
greenwich () = force (link "Greenwich" etc_gmt_plus_0_l)

{-| Hongkong -}
hongkong : () -> Zone
hongkong () = force (link "Hongkong" asia_hong_kong_l)

{-| Iceland -}
iceland : () -> Zone
iceland () = force (link "Iceland" atlantic_reykjavik_l)

{-| Indian/Antananarivo -}
indian_antananarivo : () -> Zone
indian_antananarivo () = force (link "Indian/Antananarivo" africa_nairobi_l)

{-| Indian/Comoro -}
indian_comoro : () -> Zone
indian_comoro () = force (link "Indian/Comoro" africa_nairobi_l)

{-| Indian/Mayotte -}
indian_mayotte : () -> Zone
indian_mayotte () = force (link "Indian/Mayotte" africa_nairobi_l)

{-| Iran -}
iran : () -> Zone
iran () = force (link "Iran" asia_tehran_l)

{-| Israel -}
israel : () -> Zone
israel () = force (link "Israel" asia_jerusalem_l)

{-| Jamaica -}
jamaica : () -> Zone
jamaica () = force (link "Jamaica" america_jamaica_l)

{-| Japan -}
japan : () -> Zone
japan () = force (link "Japan" asia_tokyo_l)

{-| Kwajalein -}
kwajalein : () -> Zone
kwajalein () = force (link "Kwajalein" pacific_kwajalein_l)

{-| Libya -}
libya : () -> Zone
libya () = force (link "Libya" africa_tripoli_l)

{-| Mexico/BajaNorte -}
mexico_bajanorte : () -> Zone
mexico_bajanorte () = force (link "Mexico/BajaNorte" america_tijuana_l)

{-| Mexico/BajaSur -}
mexico_bajasur : () -> Zone
mexico_bajasur () = force (link "Mexico/BajaSur" america_mazatlan_l)

{-| Mexico/General -}
mexico_general : () -> Zone
mexico_general () = force (link "Mexico/General" america_mexico_city_l)

{-| Navajo -}
navajo : () -> Zone
navajo () = force (link "Navajo" america_denver_l)

{-| NZ -}
nz : () -> Zone
nz () = force (link "NZ" pacific_auckland_l)

{-| NZ-CHAT -}
nz_chat : () -> Zone
nz_chat () = force (link "NZ-CHAT" pacific_chatham_l)

{-| Pacific/Johnston -}
pacific_johnston : () -> Zone
pacific_johnston () = force (link "Pacific/Johnston" pacific_honolulu_l)

{-| Pacific/Midway -}
pacific_midway : () -> Zone
pacific_midway () = force (link "Pacific/Midway" pacific_pago_pago_l)

{-| Pacific/Ponape -}
pacific_ponape : () -> Zone
pacific_ponape () = force (link "Pacific/Ponape" pacific_pohnpei_l)

{-| Pacific/Saipan -}
pacific_saipan : () -> Zone
pacific_saipan () = force (link "Pacific/Saipan" pacific_guam_l)

{-| Pacific/Samoa -}
pacific_samoa : () -> Zone
pacific_samoa () = force (link "Pacific/Samoa" pacific_pago_pago_l)

{-| Pacific/Truk -}
pacific_truk : () -> Zone
pacific_truk () = force (link "Pacific/Truk" pacific_chuuk_l)

{-| Pacific/Yap -}
pacific_yap : () -> Zone
pacific_yap () = force (link "Pacific/Yap" pacific_chuuk_l)

{-| Poland -}
poland : () -> Zone
poland () = force (link "Poland" europe_warsaw_l)

{-| Portugal -}
portugal : () -> Zone
portugal () = force (link "Portugal" europe_lisbon_l)

{-| PRC -}
prc : () -> Zone
prc () = force (link "PRC" asia_shanghai_l)

{-| ROC -}
roc : () -> Zone
roc () = force (link "ROC" asia_taipei_l)

{-| ROK -}
rok : () -> Zone
rok () = force (link "ROK" asia_seoul_l)

{-| Singapore -}
singapore : () -> Zone
singapore () = force (link "Singapore" asia_singapore_l)

{-| Turkey -}
turkey : () -> Zone
turkey () = force (link "Turkey" europe_istanbul_l)

{-| UCT -}
uct : () -> Zone
uct () = force (link "UCT" etc_uct_l)

{-| Universal -}
universal : () -> Zone
universal () = force (link "Universal" etc_utc_l)

{-| US/Alaska -}
us_alaska : () -> Zone
us_alaska () = force (link "US/Alaska" america_anchorage_l)

{-| US/Aleutian -}
us_aleutian : () -> Zone
us_aleutian () = force (link "US/Aleutian" america_adak_l)

{-| US/Arizona -}
us_arizona : () -> Zone
us_arizona () = force (link "US/Arizona" america_phoenix_l)

{-| US/Central -}
us_central : () -> Zone
us_central () = force (link "US/Central" america_chicago_l)

{-| US/East-Indiana -}
us_east_indiana : () -> Zone
us_east_indiana () = force (link "US/East-Indiana" america_fort_wayne_l)

{-| US/Eastern -}
us_eastern : () -> Zone
us_eastern () = force (link "US/Eastern" america_new_york_l)

{-| US/Hawaii -}
us_hawaii : () -> Zone
us_hawaii () = force (link "US/Hawaii" pacific_honolulu_l)

{-| US/Indiana-Starke -}
us_indiana_starke : () -> Zone
us_indiana_starke () = force (link "US/Indiana-Starke" america_indiana_knox_l)

{-| US/Michigan -}
us_michigan : () -> Zone
us_michigan () = force (link "US/Michigan" america_detroit_l)

{-| US/Mountain -}
us_mountain : () -> Zone
us_mountain () = force (link "US/Mountain" america_denver_l)

{-| US/Pacific -}
us_pacific : () -> Zone
us_pacific () = force (link "US/Pacific" america_los_angeles_l)

{-| US/Pacific-New -}
us_pacific_new : () -> Zone
us_pacific_new () = force (link "US/Pacific-New" america_los_angeles_l)

{-| US/Samoa -}
us_samoa : () -> Zone
us_samoa () = force (link "US/Samoa" pacific_pago_pago_l)

{-| UTC -}
utc : () -> Zone
utc () = force (link "UTC" etc_utc_l)

{-| W-SU -}
w_su : () -> Zone
w_su () = force (link "W-SU" europe_moscow_l)

{-| Zulu -}
zulu : () -> Zone
zulu () = force (link "Zulu" etc_utc_l)
