module Time.TimeZones exposing (..)

{-| This module contains TimeZone definitions for all Timezones as they
are defined in the IANA zoneinfo database.

TimeZone data is parsed lazily so, in order to retrieve a zone's value you
must apply `()` to it.  For example:

    import Time.DateTime exposing (epoch, toTimestamp)
    import Time.TimeZone exposing (abbreviation)
    import Time.TimeZones exposing (europe_bucharest)

    let
        timezone = europe_bucharest ()
    in
        abbreviation (toTimestamp epoch) timezone

@docs all, fromName, africa_abidjan, africa_accra, africa_addis_ababa, africa_algiers, africa_asmara, africa_asmera, africa_bamako, africa_bangui, africa_banjul, africa_bissau, africa_blantyre, africa_brazzaville, africa_bujumbura, africa_cairo, africa_casablanca, africa_ceuta, africa_conakry, africa_dakar, africa_dar_es_salaam, africa_djibouti, africa_douala, africa_el_aaiun, africa_freetown, africa_gaborone, africa_harare, africa_johannesburg, africa_juba, africa_kampala, africa_khartoum, africa_kigali, africa_kinshasa, africa_lagos, africa_libreville, africa_lome, africa_luanda, africa_lubumbashi, africa_lusaka, africa_malabo, africa_maputo, africa_maseru, africa_mbabane, africa_mogadishu, africa_monrovia, africa_nairobi, africa_ndjamena, africa_niamey, africa_nouakchott, africa_ouagadougou, africa_porto_novo, africa_sao_tome, africa_timbuktu, africa_tripoli, africa_tunis, africa_windhoek, america_adak, america_anchorage, america_anguilla, america_antigua, america_araguaina, america_argentina_buenos_aires, america_argentina_catamarca, america_argentina_comodrivadavia, america_argentina_cordoba, america_argentina_jujuy, america_argentina_la_rioja, america_argentina_mendoza, america_argentina_rio_gallegos, america_argentina_salta, america_argentina_san_juan, america_argentina_san_luis, america_argentina_tucuman, america_argentina_ushuaia, america_aruba, america_asuncion, america_atikokan, america_atka, america_bahia, america_bahia_banderas, america_barbados, america_belem, america_belize, america_blanc_sablon, america_boa_vista, america_bogota, america_boise, america_buenos_aires, america_cambridge_bay, america_campo_grande, america_cancun, america_caracas, america_catamarca, america_cayenne, america_cayman, america_chicago, america_chihuahua, america_coral_harbour, america_cordoba, america_costa_rica, america_creston, america_cuiaba, america_curacao, america_danmarkshavn, america_dawson, america_dawson_creek, america_denver, america_detroit, america_dominica, america_edmonton, america_eirunepe, america_el_salvador, america_ensenada, america_fort_nelson, america_fort_wayne, america_fortaleza, america_glace_bay, america_godthab, america_goose_bay, america_grand_turk, america_grenada, america_guadeloupe, america_guatemala, america_guayaquil, america_guyana, america_halifax, america_havana, america_hermosillo, america_indiana_indianapolis, america_indiana_knox, america_indiana_marengo, america_indiana_petersburg, america_indiana_tell_city, america_indiana_vevay, america_indiana_vincennes, america_indiana_winamac, america_indianapolis, america_inuvik, america_iqaluit, america_jamaica, america_jujuy, america_juneau, america_kentucky_louisville, america_kentucky_monticello, america_knox_in, america_kralendijk, america_la_paz, america_lima, america_los_angeles, america_louisville, america_lower_princes, america_maceio, america_managua, america_manaus, america_marigot, america_martinique, america_matamoros, america_mazatlan, america_mendoza, america_menominee, america_merida, america_metlakatla, america_mexico_city, america_miquelon, america_moncton, america_monterrey, america_montevideo, america_montreal, america_montserrat, america_nassau, america_new_york, america_nipigon, america_nome, america_noronha, america_north_dakota_beulah, america_north_dakota_center, america_north_dakota_new_salem, america_ojinaga, america_panama, america_pangnirtung, america_paramaribo, america_phoenix, america_port_au_prince, america_port_of_spain, america_porto_acre, america_porto_velho, america_puerto_rico, america_rainy_river, america_rankin_inlet, america_recife, america_regina, america_resolute, america_rio_branco, america_rosario, america_santa_isabel, america_santarem, america_santiago, america_santo_domingo, america_sao_paulo, america_scoresbysund, america_shiprock, america_sitka, america_st_barthelemy, america_st_johns, america_st_kitts, america_st_lucia, america_st_thomas, america_st_vincent, america_swift_current, america_tegucigalpa, america_thule, america_thunder_bay, america_tijuana, america_toronto, america_tortola, america_vancouver, america_virgin, america_whitehorse, america_winnipeg, america_yakutat, america_yellowknife, antarctica_casey, antarctica_davis, antarctica_dumontdurville, antarctica_macquarie, antarctica_mawson, antarctica_mcmurdo, antarctica_palmer, antarctica_rothera, antarctica_south_pole, antarctica_syowa, antarctica_troll, antarctica_vostok, arctic_longyearbyen, asia_aden, asia_almaty, asia_amman, asia_anadyr, asia_aqtau, asia_aqtobe, asia_ashgabat, asia_ashkhabad, asia_atyrau, asia_baghdad, asia_bahrain, asia_baku, asia_bangkok, asia_barnaul, asia_beirut, asia_bishkek, asia_brunei, asia_calcutta, asia_chita, asia_choibalsan, asia_chongqing, asia_chungking, asia_colombo, asia_dacca, asia_damascus, asia_dhaka, asia_dili, asia_dubai, asia_dushanbe, asia_famagusta, asia_gaza, asia_harbin, asia_hebron, asia_ho_chi_minh, asia_hong_kong, asia_hovd, asia_irkutsk, asia_istanbul, asia_jakarta, asia_jayapura, asia_jerusalem, asia_kabul, asia_kamchatka, asia_karachi, asia_kashgar, asia_kathmandu, asia_katmandu, asia_khandyga, asia_kolkata, asia_krasnoyarsk, asia_kuala_lumpur, asia_kuching, asia_kuwait, asia_macao, asia_macau, asia_magadan, asia_makassar, asia_manila, asia_muscat, asia_nicosia, asia_novokuznetsk, asia_novosibirsk, asia_omsk, asia_oral, asia_phnom_penh, asia_pontianak, asia_pyongyang, asia_qatar, asia_qyzylorda, asia_rangoon, asia_riyadh, asia_saigon, asia_sakhalin, asia_samarkand, asia_seoul, asia_shanghai, asia_singapore, asia_srednekolymsk, asia_taipei, asia_tashkent, asia_tbilisi, asia_tehran, asia_tel_aviv, asia_thimbu, asia_thimphu, asia_tokyo, asia_tomsk, asia_ujung_pandang, asia_ulaanbaatar, asia_ulan_bator, asia_urumqi, asia_ust_nera, asia_vientiane, asia_vladivostok, asia_yakutsk, asia_yangon, asia_yekaterinburg, asia_yerevan, atlantic_azores, atlantic_bermuda, atlantic_canary, atlantic_cape_verde, atlantic_faeroe, atlantic_faroe, atlantic_jan_mayen, atlantic_madeira, atlantic_reykjavik, atlantic_south_georgia, atlantic_st_helena, atlantic_stanley, australia_act, australia_adelaide, australia_brisbane, australia_broken_hill, australia_canberra, australia_currie, australia_darwin, australia_eucla, australia_hobart, australia_lhi, australia_lindeman, australia_lord_howe, australia_melbourne, australia_north, australia_nsw, australia_perth, australia_queensland, australia_south, australia_sydney, australia_tasmania, australia_victoria, australia_west, australia_yancowinna, brazil_acre, brazil_denoronha, brazil_east, brazil_west, canada_atlantic, canada_central, canada_east_saskatchewan, canada_eastern, canada_mountain, canada_newfoundland, canada_pacific, canada_saskatchewan, canada_yukon, cet, chile_continental, chile_easterisland, cst6cdt, cuba, eet, egypt, eire, est, est5edt, etc_gmt, etc_gmt_0, etc_gmt_minus_0, etc_gmt_minus_1, etc_gmt_minus_10, etc_gmt_minus_11, etc_gmt_minus_12, etc_gmt_minus_13, etc_gmt_minus_14, etc_gmt_minus_2, etc_gmt_minus_3, etc_gmt_minus_4, etc_gmt_minus_5, etc_gmt_minus_6, etc_gmt_minus_7, etc_gmt_minus_8, etc_gmt_minus_9, etc_gmt_plus_0, etc_gmt_plus_1, etc_gmt_plus_10, etc_gmt_plus_11, etc_gmt_plus_12, etc_gmt_plus_2, etc_gmt_plus_3, etc_gmt_plus_4, etc_gmt_plus_5, etc_gmt_plus_6, etc_gmt_plus_7, etc_gmt_plus_8, etc_gmt_plus_9, etc_greenwich, etc_uct, etc_universal, etc_utc, etc_zulu, europe_amsterdam, europe_andorra, europe_astrakhan, europe_athens, europe_belfast, europe_belgrade, europe_berlin, europe_bratislava, europe_brussels, europe_bucharest, europe_budapest, europe_busingen, europe_chisinau, europe_copenhagen, europe_dublin, europe_gibraltar, europe_guernsey, europe_helsinki, europe_isle_of_man, europe_istanbul, europe_jersey, europe_kaliningrad, europe_kiev, europe_kirov, europe_lisbon, europe_ljubljana, europe_london, europe_luxembourg, europe_madrid, europe_malta, europe_mariehamn, europe_minsk, europe_monaco, europe_moscow, europe_nicosia, europe_oslo, europe_paris, europe_podgorica, europe_prague, europe_riga, europe_rome, europe_samara, europe_san_marino, europe_sarajevo, europe_saratov, europe_simferopol, europe_skopje, europe_sofia, europe_stockholm, europe_tallinn, europe_tirane, europe_tiraspol, europe_ulyanovsk, europe_uzhgorod, europe_vaduz, europe_vatican, europe_vienna, europe_vilnius, europe_volgograd, europe_warsaw, europe_zagreb, europe_zaporozhye, europe_zurich, gb, gb_eire, gmt, gmt_0, gmt_minus_0, gmt_plus_0, greenwich, hongkong, hst, iceland, indian_antananarivo, indian_chagos, indian_christmas, indian_cocos, indian_comoro, indian_kerguelen, indian_mahe, indian_maldives, indian_mauritius, indian_mayotte, indian_reunion, iran, israel, jamaica, japan, kwajalein, libya, met, mexico_bajanorte, mexico_bajasur, mexico_general, mst, mst7mdt, navajo, nz, nz_chat, pacific_apia, pacific_auckland, pacific_bougainville, pacific_chatham, pacific_chuuk, pacific_easter, pacific_efate, pacific_enderbury, pacific_fakaofo, pacific_fiji, pacific_funafuti, pacific_galapagos, pacific_gambier, pacific_guadalcanal, pacific_guam, pacific_honolulu, pacific_johnston, pacific_kiritimati, pacific_kosrae, pacific_kwajalein, pacific_majuro, pacific_marquesas, pacific_midway, pacific_nauru, pacific_niue, pacific_norfolk, pacific_noumea, pacific_pago_pago, pacific_palau, pacific_pitcairn, pacific_pohnpei, pacific_ponape, pacific_port_moresby, pacific_rarotonga, pacific_saipan, pacific_samoa, pacific_tahiti, pacific_tarawa, pacific_tongatapu, pacific_truk, pacific_wake, pacific_wallis, pacific_yap, poland, portugal, prc, pst8pdt, roc, rok, singapore, turkey, uct, universal, us_alaska, us_aleutian, us_arizona, us_central, us_east_indiana, us_eastern, us_hawaii, us_indiana_starke, us_michigan, us_mountain, us_pacific, us_pacific_new, us_samoa, utc, w_su, wet, zulu
-}

import Dict exposing (Dict)
import Lazy exposing (Lazy, force)
import Time.TimeZone exposing (TimeZone)
import Time.TimeZoneData exposing (..)


-- TimeZones
-- ---------

{-| Africa/Abidjan -}
africa_abidjan : () -> TimeZone
africa_abidjan () = force africa_abidjan_l

{-| Africa/Accra -}
africa_accra : () -> TimeZone
africa_accra () = force africa_accra_l

{-| Africa/Nairobi -}
africa_addis_ababa : () -> TimeZone
africa_addis_ababa () = force (link "Africa/Nairobi" africa_nairobi_l)

{-| Africa/Algiers -}
africa_algiers : () -> TimeZone
africa_algiers () = force africa_algiers_l

{-| Africa/Nairobi -}
africa_asmara : () -> TimeZone
africa_asmara () = force (link "Africa/Nairobi" africa_nairobi_l)

{-| Africa/Nairobi -}
africa_asmera : () -> TimeZone
africa_asmera () = force (link "Africa/Nairobi" africa_nairobi_l)

{-| Africa/Abidjan -}
africa_bamako : () -> TimeZone
africa_bamako () = force (link "Africa/Abidjan" africa_abidjan_l)

{-| Africa/Lagos -}
africa_bangui : () -> TimeZone
africa_bangui () = force (link "Africa/Lagos" africa_lagos_l)

{-| Africa/Abidjan -}
africa_banjul : () -> TimeZone
africa_banjul () = force (link "Africa/Abidjan" africa_abidjan_l)

{-| Africa/Bissau -}
africa_bissau : () -> TimeZone
africa_bissau () = force africa_bissau_l

{-| Africa/Maputo -}
africa_blantyre : () -> TimeZone
africa_blantyre () = force (link "Africa/Maputo" africa_maputo_l)

{-| Africa/Lagos -}
africa_brazzaville : () -> TimeZone
africa_brazzaville () = force (link "Africa/Lagos" africa_lagos_l)

{-| Africa/Maputo -}
africa_bujumbura : () -> TimeZone
africa_bujumbura () = force (link "Africa/Maputo" africa_maputo_l)

{-| Africa/Cairo -}
africa_cairo : () -> TimeZone
africa_cairo () = force africa_cairo_l

{-| Africa/Casablanca -}
africa_casablanca : () -> TimeZone
africa_casablanca () = force africa_casablanca_l

{-| Africa/Ceuta -}
africa_ceuta : () -> TimeZone
africa_ceuta () = force africa_ceuta_l

{-| Africa/Abidjan -}
africa_conakry : () -> TimeZone
africa_conakry () = force (link "Africa/Abidjan" africa_abidjan_l)

{-| Africa/Abidjan -}
africa_dakar : () -> TimeZone
africa_dakar () = force (link "Africa/Abidjan" africa_abidjan_l)

{-| Africa/Nairobi -}
africa_dar_es_salaam : () -> TimeZone
africa_dar_es_salaam () = force (link "Africa/Nairobi" africa_nairobi_l)

{-| Africa/Nairobi -}
africa_djibouti : () -> TimeZone
africa_djibouti () = force (link "Africa/Nairobi" africa_nairobi_l)

{-| Africa/Lagos -}
africa_douala : () -> TimeZone
africa_douala () = force (link "Africa/Lagos" africa_lagos_l)

{-| Africa/El_Aaiun -}
africa_el_aaiun : () -> TimeZone
africa_el_aaiun () = force africa_el_aaiun_l

{-| Africa/Abidjan -}
africa_freetown : () -> TimeZone
africa_freetown () = force (link "Africa/Abidjan" africa_abidjan_l)

{-| Africa/Maputo -}
africa_gaborone : () -> TimeZone
africa_gaborone () = force (link "Africa/Maputo" africa_maputo_l)

{-| Africa/Maputo -}
africa_harare : () -> TimeZone
africa_harare () = force (link "Africa/Maputo" africa_maputo_l)

{-| Africa/Johannesburg -}
africa_johannesburg : () -> TimeZone
africa_johannesburg () = force africa_johannesburg_l

{-| Africa/Khartoum -}
africa_juba : () -> TimeZone
africa_juba () = force (link "Africa/Khartoum" africa_khartoum_l)

{-| Africa/Nairobi -}
africa_kampala : () -> TimeZone
africa_kampala () = force (link "Africa/Nairobi" africa_nairobi_l)

{-| Africa/Khartoum -}
africa_khartoum : () -> TimeZone
africa_khartoum () = force africa_khartoum_l

{-| Africa/Maputo -}
africa_kigali : () -> TimeZone
africa_kigali () = force (link "Africa/Maputo" africa_maputo_l)

{-| Africa/Lagos -}
africa_kinshasa : () -> TimeZone
africa_kinshasa () = force (link "Africa/Lagos" africa_lagos_l)

{-| Africa/Lagos -}
africa_lagos : () -> TimeZone
africa_lagos () = force africa_lagos_l

{-| Africa/Lagos -}
africa_libreville : () -> TimeZone
africa_libreville () = force (link "Africa/Lagos" africa_lagos_l)

{-| Africa/Abidjan -}
africa_lome : () -> TimeZone
africa_lome () = force (link "Africa/Abidjan" africa_abidjan_l)

{-| Africa/Lagos -}
africa_luanda : () -> TimeZone
africa_luanda () = force (link "Africa/Lagos" africa_lagos_l)

{-| Africa/Maputo -}
africa_lubumbashi : () -> TimeZone
africa_lubumbashi () = force (link "Africa/Maputo" africa_maputo_l)

{-| Africa/Maputo -}
africa_lusaka : () -> TimeZone
africa_lusaka () = force (link "Africa/Maputo" africa_maputo_l)

{-| Africa/Lagos -}
africa_malabo : () -> TimeZone
africa_malabo () = force (link "Africa/Lagos" africa_lagos_l)

{-| Africa/Maputo -}
africa_maputo : () -> TimeZone
africa_maputo () = force africa_maputo_l

{-| Africa/Johannesburg -}
africa_maseru : () -> TimeZone
africa_maseru () = force (link "Africa/Johannesburg" africa_johannesburg_l)

{-| Africa/Johannesburg -}
africa_mbabane : () -> TimeZone
africa_mbabane () = force (link "Africa/Johannesburg" africa_johannesburg_l)

{-| Africa/Nairobi -}
africa_mogadishu : () -> TimeZone
africa_mogadishu () = force (link "Africa/Nairobi" africa_nairobi_l)

{-| Africa/Monrovia -}
africa_monrovia : () -> TimeZone
africa_monrovia () = force africa_monrovia_l

{-| Africa/Nairobi -}
africa_nairobi : () -> TimeZone
africa_nairobi () = force africa_nairobi_l

{-| Africa/Ndjamena -}
africa_ndjamena : () -> TimeZone
africa_ndjamena () = force africa_ndjamena_l

{-| Africa/Lagos -}
africa_niamey : () -> TimeZone
africa_niamey () = force (link "Africa/Lagos" africa_lagos_l)

{-| Africa/Abidjan -}
africa_nouakchott : () -> TimeZone
africa_nouakchott () = force (link "Africa/Abidjan" africa_abidjan_l)

{-| Africa/Abidjan -}
africa_ouagadougou : () -> TimeZone
africa_ouagadougou () = force (link "Africa/Abidjan" africa_abidjan_l)

{-| Africa/Lagos -}
africa_porto_novo : () -> TimeZone
africa_porto_novo () = force (link "Africa/Lagos" africa_lagos_l)

{-| Africa/Abidjan -}
africa_sao_tome : () -> TimeZone
africa_sao_tome () = force (link "Africa/Abidjan" africa_abidjan_l)

{-| Africa/Abidjan -}
africa_timbuktu : () -> TimeZone
africa_timbuktu () = force (link "Africa/Abidjan" africa_abidjan_l)

{-| Africa/Tripoli -}
africa_tripoli : () -> TimeZone
africa_tripoli () = force africa_tripoli_l

{-| Africa/Tunis -}
africa_tunis : () -> TimeZone
africa_tunis () = force africa_tunis_l

{-| Africa/Windhoek -}
africa_windhoek : () -> TimeZone
africa_windhoek () = force africa_windhoek_l

{-| America/Adak -}
america_adak : () -> TimeZone
america_adak () = force america_adak_l

{-| America/Anchorage -}
america_anchorage : () -> TimeZone
america_anchorage () = force america_anchorage_l

{-| America/Port_of_Spain -}
america_anguilla : () -> TimeZone
america_anguilla () = force (link "America/Port_of_Spain" america_port_of_spain_l)

{-| America/Port_of_Spain -}
america_antigua : () -> TimeZone
america_antigua () = force (link "America/Port_of_Spain" america_port_of_spain_l)

{-| America/Araguaina -}
america_araguaina : () -> TimeZone
america_araguaina () = force america_araguaina_l

{-| America/Argentina/Buenos_Aires -}
america_argentina_buenos_aires : () -> TimeZone
america_argentina_buenos_aires () = force america_argentina_buenos_aires_l

{-| America/Argentina/Catamarca -}
america_argentina_catamarca : () -> TimeZone
america_argentina_catamarca () = force america_argentina_catamarca_l

{-| America/Argentina/Catamarca -}
america_argentina_comodrivadavia : () -> TimeZone
america_argentina_comodrivadavia () = force (link "America/Argentina/Catamarca" america_argentina_catamarca_l)

{-| America/Argentina/Cordoba -}
america_argentina_cordoba : () -> TimeZone
america_argentina_cordoba () = force america_argentina_cordoba_l

{-| America/Argentina/Jujuy -}
america_argentina_jujuy : () -> TimeZone
america_argentina_jujuy () = force america_argentina_jujuy_l

{-| America/Argentina/La_Rioja -}
america_argentina_la_rioja : () -> TimeZone
america_argentina_la_rioja () = force america_argentina_la_rioja_l

{-| America/Argentina/Mendoza -}
america_argentina_mendoza : () -> TimeZone
america_argentina_mendoza () = force america_argentina_mendoza_l

{-| America/Argentina/Rio_Gallegos -}
america_argentina_rio_gallegos : () -> TimeZone
america_argentina_rio_gallegos () = force america_argentina_rio_gallegos_l

{-| America/Argentina/Salta -}
america_argentina_salta : () -> TimeZone
america_argentina_salta () = force america_argentina_salta_l

{-| America/Argentina/San_Juan -}
america_argentina_san_juan : () -> TimeZone
america_argentina_san_juan () = force america_argentina_san_juan_l

{-| America/Argentina/San_Luis -}
america_argentina_san_luis : () -> TimeZone
america_argentina_san_luis () = force america_argentina_san_luis_l

{-| America/Argentina/Tucuman -}
america_argentina_tucuman : () -> TimeZone
america_argentina_tucuman () = force america_argentina_tucuman_l

{-| America/Argentina/Ushuaia -}
america_argentina_ushuaia : () -> TimeZone
america_argentina_ushuaia () = force america_argentina_ushuaia_l

{-| America/Curacao -}
america_aruba : () -> TimeZone
america_aruba () = force (link "America/Curacao" america_curacao_l)

{-| America/Asuncion -}
america_asuncion : () -> TimeZone
america_asuncion () = force america_asuncion_l

{-| America/Atikokan -}
america_atikokan : () -> TimeZone
america_atikokan () = force america_atikokan_l

{-| America/Adak -}
america_atka : () -> TimeZone
america_atka () = force (link "America/Adak" america_adak_l)

{-| America/Bahia -}
america_bahia : () -> TimeZone
america_bahia () = force america_bahia_l

{-| America/Bahia_Banderas -}
america_bahia_banderas : () -> TimeZone
america_bahia_banderas () = force america_bahia_banderas_l

{-| America/Barbados -}
america_barbados : () -> TimeZone
america_barbados () = force america_barbados_l

{-| America/Belem -}
america_belem : () -> TimeZone
america_belem () = force america_belem_l

{-| America/Belize -}
america_belize : () -> TimeZone
america_belize () = force america_belize_l

{-| America/Blanc-Sablon -}
america_blanc_sablon : () -> TimeZone
america_blanc_sablon () = force america_blanc_sablon_l

{-| America/Boa_Vista -}
america_boa_vista : () -> TimeZone
america_boa_vista () = force america_boa_vista_l

{-| America/Bogota -}
america_bogota : () -> TimeZone
america_bogota () = force america_bogota_l

{-| America/Boise -}
america_boise : () -> TimeZone
america_boise () = force america_boise_l

{-| America/Argentina/Buenos_Aires -}
america_buenos_aires : () -> TimeZone
america_buenos_aires () = force (link "America/Argentina/Buenos_Aires" america_argentina_buenos_aires_l)

{-| America/Cambridge_Bay -}
america_cambridge_bay : () -> TimeZone
america_cambridge_bay () = force america_cambridge_bay_l

{-| America/Campo_Grande -}
america_campo_grande : () -> TimeZone
america_campo_grande () = force america_campo_grande_l

{-| America/Cancun -}
america_cancun : () -> TimeZone
america_cancun () = force america_cancun_l

{-| America/Caracas -}
america_caracas : () -> TimeZone
america_caracas () = force america_caracas_l

{-| America/Argentina/Catamarca -}
america_catamarca : () -> TimeZone
america_catamarca () = force (link "America/Argentina/Catamarca" america_argentina_catamarca_l)

{-| America/Cayenne -}
america_cayenne : () -> TimeZone
america_cayenne () = force america_cayenne_l

{-| America/Panama -}
america_cayman : () -> TimeZone
america_cayman () = force (link "America/Panama" america_panama_l)

{-| America/Chicago -}
america_chicago : () -> TimeZone
america_chicago () = force america_chicago_l

{-| America/Chihuahua -}
america_chihuahua : () -> TimeZone
america_chihuahua () = force america_chihuahua_l

{-| America/Atikokan -}
america_coral_harbour : () -> TimeZone
america_coral_harbour () = force (link "America/Atikokan" america_atikokan_l)

{-| America/Argentina/Cordoba -}
america_cordoba : () -> TimeZone
america_cordoba () = force (link "America/Argentina/Cordoba" america_argentina_cordoba_l)

{-| America/Costa_Rica -}
america_costa_rica : () -> TimeZone
america_costa_rica () = force america_costa_rica_l

{-| America/Creston -}
america_creston : () -> TimeZone
america_creston () = force america_creston_l

{-| America/Cuiaba -}
america_cuiaba : () -> TimeZone
america_cuiaba () = force america_cuiaba_l

{-| America/Curacao -}
america_curacao : () -> TimeZone
america_curacao () = force america_curacao_l

{-| America/Danmarkshavn -}
america_danmarkshavn : () -> TimeZone
america_danmarkshavn () = force america_danmarkshavn_l

{-| America/Dawson -}
america_dawson : () -> TimeZone
america_dawson () = force america_dawson_l

{-| America/Dawson_Creek -}
america_dawson_creek : () -> TimeZone
america_dawson_creek () = force america_dawson_creek_l

{-| America/Denver -}
america_denver : () -> TimeZone
america_denver () = force america_denver_l

{-| America/Detroit -}
america_detroit : () -> TimeZone
america_detroit () = force america_detroit_l

{-| America/Port_of_Spain -}
america_dominica : () -> TimeZone
america_dominica () = force (link "America/Port_of_Spain" america_port_of_spain_l)

{-| America/Edmonton -}
america_edmonton : () -> TimeZone
america_edmonton () = force america_edmonton_l

{-| America/Eirunepe -}
america_eirunepe : () -> TimeZone
america_eirunepe () = force america_eirunepe_l

{-| America/El_Salvador -}
america_el_salvador : () -> TimeZone
america_el_salvador () = force america_el_salvador_l

{-| America/Tijuana -}
america_ensenada : () -> TimeZone
america_ensenada () = force (link "America/Tijuana" america_tijuana_l)

{-| America/Fort_Nelson -}
america_fort_nelson : () -> TimeZone
america_fort_nelson () = force america_fort_nelson_l

{-| America/Fort_Wayne -}
america_fort_wayne : () -> TimeZone
america_fort_wayne () = force america_fort_wayne_l

{-| America/Fortaleza -}
america_fortaleza : () -> TimeZone
america_fortaleza () = force america_fortaleza_l

{-| America/Glace_Bay -}
america_glace_bay : () -> TimeZone
america_glace_bay () = force america_glace_bay_l

{-| America/Godthab -}
america_godthab : () -> TimeZone
america_godthab () = force america_godthab_l

{-| America/Goose_Bay -}
america_goose_bay : () -> TimeZone
america_goose_bay () = force america_goose_bay_l

{-| America/Grand_Turk -}
america_grand_turk : () -> TimeZone
america_grand_turk () = force america_grand_turk_l

{-| America/Port_of_Spain -}
america_grenada : () -> TimeZone
america_grenada () = force (link "America/Port_of_Spain" america_port_of_spain_l)

{-| America/Port_of_Spain -}
america_guadeloupe : () -> TimeZone
america_guadeloupe () = force (link "America/Port_of_Spain" america_port_of_spain_l)

{-| America/Guatemala -}
america_guatemala : () -> TimeZone
america_guatemala () = force america_guatemala_l

{-| America/Guayaquil -}
america_guayaquil : () -> TimeZone
america_guayaquil () = force america_guayaquil_l

{-| America/Guyana -}
america_guyana : () -> TimeZone
america_guyana () = force america_guyana_l

{-| America/Halifax -}
america_halifax : () -> TimeZone
america_halifax () = force america_halifax_l

{-| America/Havana -}
america_havana : () -> TimeZone
america_havana () = force america_havana_l

{-| America/Hermosillo -}
america_hermosillo : () -> TimeZone
america_hermosillo () = force america_hermosillo_l

{-| America/Fort_Wayne -}
america_indiana_indianapolis : () -> TimeZone
america_indiana_indianapolis () = force (link "America/Fort_Wayne" america_fort_wayne_l)

{-| America/Indiana/Knox -}
america_indiana_knox : () -> TimeZone
america_indiana_knox () = force america_indiana_knox_l

{-| America/Indiana/Marengo -}
america_indiana_marengo : () -> TimeZone
america_indiana_marengo () = force america_indiana_marengo_l

{-| America/Indiana/Petersburg -}
america_indiana_petersburg : () -> TimeZone
america_indiana_petersburg () = force america_indiana_petersburg_l

{-| America/Indiana/Tell_City -}
america_indiana_tell_city : () -> TimeZone
america_indiana_tell_city () = force america_indiana_tell_city_l

{-| America/Indiana/Vevay -}
america_indiana_vevay : () -> TimeZone
america_indiana_vevay () = force america_indiana_vevay_l

{-| America/Indiana/Vincennes -}
america_indiana_vincennes : () -> TimeZone
america_indiana_vincennes () = force america_indiana_vincennes_l

{-| America/Indiana/Winamac -}
america_indiana_winamac : () -> TimeZone
america_indiana_winamac () = force america_indiana_winamac_l

{-| America/Fort_Wayne -}
america_indianapolis : () -> TimeZone
america_indianapolis () = force (link "America/Fort_Wayne" america_fort_wayne_l)

{-| America/Inuvik -}
america_inuvik : () -> TimeZone
america_inuvik () = force america_inuvik_l

{-| America/Iqaluit -}
america_iqaluit : () -> TimeZone
america_iqaluit () = force america_iqaluit_l

{-| America/Jamaica -}
america_jamaica : () -> TimeZone
america_jamaica () = force america_jamaica_l

{-| America/Argentina/Jujuy -}
america_jujuy : () -> TimeZone
america_jujuy () = force (link "America/Argentina/Jujuy" america_argentina_jujuy_l)

{-| America/Juneau -}
america_juneau : () -> TimeZone
america_juneau () = force america_juneau_l

{-| America/Kentucky/Louisville -}
america_kentucky_louisville : () -> TimeZone
america_kentucky_louisville () = force america_kentucky_louisville_l

{-| America/Kentucky/Monticello -}
america_kentucky_monticello : () -> TimeZone
america_kentucky_monticello () = force america_kentucky_monticello_l

{-| America/Indiana/Knox -}
america_knox_in : () -> TimeZone
america_knox_in () = force (link "America/Indiana/Knox" america_indiana_knox_l)

{-| America/Curacao -}
america_kralendijk : () -> TimeZone
america_kralendijk () = force (link "America/Curacao" america_curacao_l)

{-| America/La_Paz -}
america_la_paz : () -> TimeZone
america_la_paz () = force america_la_paz_l

{-| America/Lima -}
america_lima : () -> TimeZone
america_lima () = force america_lima_l

{-| America/Los_Angeles -}
america_los_angeles : () -> TimeZone
america_los_angeles () = force america_los_angeles_l

{-| America/Kentucky/Louisville -}
america_louisville : () -> TimeZone
america_louisville () = force (link "America/Kentucky/Louisville" america_kentucky_louisville_l)

{-| America/Curacao -}
america_lower_princes : () -> TimeZone
america_lower_princes () = force (link "America/Curacao" america_curacao_l)

{-| America/Maceio -}
america_maceio : () -> TimeZone
america_maceio () = force america_maceio_l

{-| America/Managua -}
america_managua : () -> TimeZone
america_managua () = force america_managua_l

{-| America/Manaus -}
america_manaus : () -> TimeZone
america_manaus () = force america_manaus_l

{-| America/Port_of_Spain -}
america_marigot : () -> TimeZone
america_marigot () = force (link "America/Port_of_Spain" america_port_of_spain_l)

{-| America/Martinique -}
america_martinique : () -> TimeZone
america_martinique () = force america_martinique_l

{-| America/Matamoros -}
america_matamoros : () -> TimeZone
america_matamoros () = force america_matamoros_l

{-| America/Mazatlan -}
america_mazatlan : () -> TimeZone
america_mazatlan () = force america_mazatlan_l

{-| America/Argentina/Mendoza -}
america_mendoza : () -> TimeZone
america_mendoza () = force (link "America/Argentina/Mendoza" america_argentina_mendoza_l)

{-| America/Menominee -}
america_menominee : () -> TimeZone
america_menominee () = force america_menominee_l

{-| America/Merida -}
america_merida : () -> TimeZone
america_merida () = force america_merida_l

{-| America/Metlakatla -}
america_metlakatla : () -> TimeZone
america_metlakatla () = force america_metlakatla_l

{-| America/Mexico_City -}
america_mexico_city : () -> TimeZone
america_mexico_city () = force america_mexico_city_l

{-| America/Miquelon -}
america_miquelon : () -> TimeZone
america_miquelon () = force america_miquelon_l

{-| America/Moncton -}
america_moncton : () -> TimeZone
america_moncton () = force america_moncton_l

{-| America/Monterrey -}
america_monterrey : () -> TimeZone
america_monterrey () = force america_monterrey_l

{-| America/Montevideo -}
america_montevideo : () -> TimeZone
america_montevideo () = force america_montevideo_l

{-| America/Toronto -}
america_montreal : () -> TimeZone
america_montreal () = force (link "America/Toronto" america_toronto_l)

{-| America/Port_of_Spain -}
america_montserrat : () -> TimeZone
america_montserrat () = force (link "America/Port_of_Spain" america_port_of_spain_l)

{-| America/Nassau -}
america_nassau : () -> TimeZone
america_nassau () = force america_nassau_l

{-| America/New_York -}
america_new_york : () -> TimeZone
america_new_york () = force america_new_york_l

{-| America/Nipigon -}
america_nipigon : () -> TimeZone
america_nipigon () = force america_nipigon_l

{-| America/Nome -}
america_nome : () -> TimeZone
america_nome () = force america_nome_l

{-| America/Noronha -}
america_noronha : () -> TimeZone
america_noronha () = force america_noronha_l

{-| America/North_Dakota/Beulah -}
america_north_dakota_beulah : () -> TimeZone
america_north_dakota_beulah () = force america_north_dakota_beulah_l

{-| America/North_Dakota/Center -}
america_north_dakota_center : () -> TimeZone
america_north_dakota_center () = force america_north_dakota_center_l

{-| America/North_Dakota/New_Salem -}
america_north_dakota_new_salem : () -> TimeZone
america_north_dakota_new_salem () = force america_north_dakota_new_salem_l

{-| America/Ojinaga -}
america_ojinaga : () -> TimeZone
america_ojinaga () = force america_ojinaga_l

{-| America/Panama -}
america_panama : () -> TimeZone
america_panama () = force america_panama_l

{-| America/Pangnirtung -}
america_pangnirtung : () -> TimeZone
america_pangnirtung () = force america_pangnirtung_l

{-| America/Paramaribo -}
america_paramaribo : () -> TimeZone
america_paramaribo () = force america_paramaribo_l

{-| America/Phoenix -}
america_phoenix : () -> TimeZone
america_phoenix () = force america_phoenix_l

{-| America/Port-au-Prince -}
america_port_au_prince : () -> TimeZone
america_port_au_prince () = force america_port_au_prince_l

{-| America/Port_of_Spain -}
america_port_of_spain : () -> TimeZone
america_port_of_spain () = force america_port_of_spain_l

{-| America/Rio_Branco -}
america_porto_acre : () -> TimeZone
america_porto_acre () = force (link "America/Rio_Branco" america_rio_branco_l)

{-| America/Porto_Velho -}
america_porto_velho : () -> TimeZone
america_porto_velho () = force america_porto_velho_l

{-| America/Puerto_Rico -}
america_puerto_rico : () -> TimeZone
america_puerto_rico () = force america_puerto_rico_l

{-| America/Rainy_River -}
america_rainy_river : () -> TimeZone
america_rainy_river () = force america_rainy_river_l

{-| America/Rankin_Inlet -}
america_rankin_inlet : () -> TimeZone
america_rankin_inlet () = force america_rankin_inlet_l

{-| America/Recife -}
america_recife : () -> TimeZone
america_recife () = force america_recife_l

{-| America/Regina -}
america_regina : () -> TimeZone
america_regina () = force america_regina_l

{-| America/Resolute -}
america_resolute : () -> TimeZone
america_resolute () = force america_resolute_l

{-| America/Rio_Branco -}
america_rio_branco : () -> TimeZone
america_rio_branco () = force america_rio_branco_l

{-| America/Argentina/Cordoba -}
america_rosario : () -> TimeZone
america_rosario () = force (link "America/Argentina/Cordoba" america_argentina_cordoba_l)

{-| America/Tijuana -}
america_santa_isabel : () -> TimeZone
america_santa_isabel () = force (link "America/Tijuana" america_tijuana_l)

{-| America/Santarem -}
america_santarem : () -> TimeZone
america_santarem () = force america_santarem_l

{-| America/Santiago -}
america_santiago : () -> TimeZone
america_santiago () = force america_santiago_l

{-| America/Santo_Domingo -}
america_santo_domingo : () -> TimeZone
america_santo_domingo () = force america_santo_domingo_l

{-| America/Sao_Paulo -}
america_sao_paulo : () -> TimeZone
america_sao_paulo () = force america_sao_paulo_l

{-| America/Scoresbysund -}
america_scoresbysund : () -> TimeZone
america_scoresbysund () = force america_scoresbysund_l

{-| America/Denver -}
america_shiprock : () -> TimeZone
america_shiprock () = force (link "America/Denver" america_denver_l)

{-| America/Sitka -}
america_sitka : () -> TimeZone
america_sitka () = force america_sitka_l

{-| America/Port_of_Spain -}
america_st_barthelemy : () -> TimeZone
america_st_barthelemy () = force (link "America/Port_of_Spain" america_port_of_spain_l)

{-| America/St_Johns -}
america_st_johns : () -> TimeZone
america_st_johns () = force america_st_johns_l

{-| America/Port_of_Spain -}
america_st_kitts : () -> TimeZone
america_st_kitts () = force (link "America/Port_of_Spain" america_port_of_spain_l)

{-| America/Port_of_Spain -}
america_st_lucia : () -> TimeZone
america_st_lucia () = force (link "America/Port_of_Spain" america_port_of_spain_l)

{-| America/Port_of_Spain -}
america_st_thomas : () -> TimeZone
america_st_thomas () = force (link "America/Port_of_Spain" america_port_of_spain_l)

{-| America/Port_of_Spain -}
america_st_vincent : () -> TimeZone
america_st_vincent () = force (link "America/Port_of_Spain" america_port_of_spain_l)

{-| America/Swift_Current -}
america_swift_current : () -> TimeZone
america_swift_current () = force america_swift_current_l

{-| America/Tegucigalpa -}
america_tegucigalpa : () -> TimeZone
america_tegucigalpa () = force america_tegucigalpa_l

{-| America/Thule -}
america_thule : () -> TimeZone
america_thule () = force america_thule_l

{-| America/Thunder_Bay -}
america_thunder_bay : () -> TimeZone
america_thunder_bay () = force america_thunder_bay_l

{-| America/Tijuana -}
america_tijuana : () -> TimeZone
america_tijuana () = force america_tijuana_l

{-| America/Toronto -}
america_toronto : () -> TimeZone
america_toronto () = force america_toronto_l

{-| America/Port_of_Spain -}
america_tortola : () -> TimeZone
america_tortola () = force (link "America/Port_of_Spain" america_port_of_spain_l)

{-| America/Vancouver -}
america_vancouver : () -> TimeZone
america_vancouver () = force america_vancouver_l

{-| America/Port_of_Spain -}
america_virgin : () -> TimeZone
america_virgin () = force (link "America/Port_of_Spain" america_port_of_spain_l)

{-| America/Whitehorse -}
america_whitehorse : () -> TimeZone
america_whitehorse () = force america_whitehorse_l

{-| America/Winnipeg -}
america_winnipeg : () -> TimeZone
america_winnipeg () = force america_winnipeg_l

{-| America/Yakutat -}
america_yakutat : () -> TimeZone
america_yakutat () = force america_yakutat_l

{-| America/Yellowknife -}
america_yellowknife : () -> TimeZone
america_yellowknife () = force america_yellowknife_l

{-| Antarctica/Casey -}
antarctica_casey : () -> TimeZone
antarctica_casey () = force antarctica_casey_l

{-| Antarctica/Davis -}
antarctica_davis : () -> TimeZone
antarctica_davis () = force antarctica_davis_l

{-| Antarctica/DumontDUrville -}
antarctica_dumontdurville : () -> TimeZone
antarctica_dumontdurville () = force antarctica_dumontdurville_l

{-| Antarctica/Macquarie -}
antarctica_macquarie : () -> TimeZone
antarctica_macquarie () = force antarctica_macquarie_l

{-| Antarctica/Mawson -}
antarctica_mawson : () -> TimeZone
antarctica_mawson () = force antarctica_mawson_l

{-| Pacific/Auckland -}
antarctica_mcmurdo : () -> TimeZone
antarctica_mcmurdo () = force (link "Pacific/Auckland" pacific_auckland_l)

{-| Antarctica/Palmer -}
antarctica_palmer : () -> TimeZone
antarctica_palmer () = force antarctica_palmer_l

{-| Antarctica/Rothera -}
antarctica_rothera : () -> TimeZone
antarctica_rothera () = force antarctica_rothera_l

{-| Pacific/Auckland -}
antarctica_south_pole : () -> TimeZone
antarctica_south_pole () = force (link "Pacific/Auckland" pacific_auckland_l)

{-| Antarctica/Syowa -}
antarctica_syowa : () -> TimeZone
antarctica_syowa () = force antarctica_syowa_l

{-| Antarctica/Troll -}
antarctica_troll : () -> TimeZone
antarctica_troll () = force antarctica_troll_l

{-| Antarctica/Vostok -}
antarctica_vostok : () -> TimeZone
antarctica_vostok () = force antarctica_vostok_l

{-| Europe/Oslo -}
arctic_longyearbyen : () -> TimeZone
arctic_longyearbyen () = force (link "Europe/Oslo" europe_oslo_l)

{-| Asia/Riyadh -}
asia_aden : () -> TimeZone
asia_aden () = force (link "Asia/Riyadh" asia_riyadh_l)

{-| Asia/Almaty -}
asia_almaty : () -> TimeZone
asia_almaty () = force asia_almaty_l

{-| Asia/Amman -}
asia_amman : () -> TimeZone
asia_amman () = force asia_amman_l

{-| Asia/Anadyr -}
asia_anadyr : () -> TimeZone
asia_anadyr () = force asia_anadyr_l

{-| Asia/Aqtau -}
asia_aqtau : () -> TimeZone
asia_aqtau () = force asia_aqtau_l

{-| Asia/Aqtobe -}
asia_aqtobe : () -> TimeZone
asia_aqtobe () = force asia_aqtobe_l

{-| Asia/Ashgabat -}
asia_ashgabat : () -> TimeZone
asia_ashgabat () = force asia_ashgabat_l

{-| Asia/Ashgabat -}
asia_ashkhabad : () -> TimeZone
asia_ashkhabad () = force (link "Asia/Ashgabat" asia_ashgabat_l)

{-| Asia/Atyrau -}
asia_atyrau : () -> TimeZone
asia_atyrau () = force asia_atyrau_l

{-| Asia/Baghdad -}
asia_baghdad : () -> TimeZone
asia_baghdad () = force asia_baghdad_l

{-| Asia/Qatar -}
asia_bahrain : () -> TimeZone
asia_bahrain () = force (link "Asia/Qatar" asia_qatar_l)

{-| Asia/Baku -}
asia_baku : () -> TimeZone
asia_baku () = force asia_baku_l

{-| Asia/Bangkok -}
asia_bangkok : () -> TimeZone
asia_bangkok () = force asia_bangkok_l

{-| Asia/Barnaul -}
asia_barnaul : () -> TimeZone
asia_barnaul () = force asia_barnaul_l

{-| Asia/Beirut -}
asia_beirut : () -> TimeZone
asia_beirut () = force asia_beirut_l

{-| Asia/Bishkek -}
asia_bishkek : () -> TimeZone
asia_bishkek () = force asia_bishkek_l

{-| Asia/Brunei -}
asia_brunei : () -> TimeZone
asia_brunei () = force asia_brunei_l

{-| Asia/Kolkata -}
asia_calcutta : () -> TimeZone
asia_calcutta () = force (link "Asia/Kolkata" asia_kolkata_l)

{-| Asia/Chita -}
asia_chita : () -> TimeZone
asia_chita () = force asia_chita_l

{-| Asia/Choibalsan -}
asia_choibalsan : () -> TimeZone
asia_choibalsan () = force asia_choibalsan_l

{-| Asia/Shanghai -}
asia_chongqing : () -> TimeZone
asia_chongqing () = force (link "Asia/Shanghai" asia_shanghai_l)

{-| Asia/Shanghai -}
asia_chungking : () -> TimeZone
asia_chungking () = force (link "Asia/Shanghai" asia_shanghai_l)

{-| Asia/Colombo -}
asia_colombo : () -> TimeZone
asia_colombo () = force asia_colombo_l

{-| Asia/Dhaka -}
asia_dacca : () -> TimeZone
asia_dacca () = force (link "Asia/Dhaka" asia_dhaka_l)

{-| Asia/Damascus -}
asia_damascus : () -> TimeZone
asia_damascus () = force asia_damascus_l

{-| Asia/Dhaka -}
asia_dhaka : () -> TimeZone
asia_dhaka () = force asia_dhaka_l

{-| Asia/Dili -}
asia_dili : () -> TimeZone
asia_dili () = force asia_dili_l

{-| Asia/Dubai -}
asia_dubai : () -> TimeZone
asia_dubai () = force asia_dubai_l

{-| Asia/Dushanbe -}
asia_dushanbe : () -> TimeZone
asia_dushanbe () = force asia_dushanbe_l

{-| Asia/Famagusta -}
asia_famagusta : () -> TimeZone
asia_famagusta () = force asia_famagusta_l

{-| Asia/Gaza -}
asia_gaza : () -> TimeZone
asia_gaza () = force asia_gaza_l

{-| Asia/Shanghai -}
asia_harbin : () -> TimeZone
asia_harbin () = force (link "Asia/Shanghai" asia_shanghai_l)

{-| Asia/Hebron -}
asia_hebron : () -> TimeZone
asia_hebron () = force asia_hebron_l

{-| Asia/Ho_Chi_Minh -}
asia_ho_chi_minh : () -> TimeZone
asia_ho_chi_minh () = force asia_ho_chi_minh_l

{-| Asia/Hong_Kong -}
asia_hong_kong : () -> TimeZone
asia_hong_kong () = force asia_hong_kong_l

{-| Asia/Hovd -}
asia_hovd : () -> TimeZone
asia_hovd () = force asia_hovd_l

{-| Asia/Irkutsk -}
asia_irkutsk : () -> TimeZone
asia_irkutsk () = force asia_irkutsk_l

{-| Europe/Istanbul -}
asia_istanbul : () -> TimeZone
asia_istanbul () = force (link "Europe/Istanbul" europe_istanbul_l)

{-| Asia/Jakarta -}
asia_jakarta : () -> TimeZone
asia_jakarta () = force asia_jakarta_l

{-| Asia/Jayapura -}
asia_jayapura : () -> TimeZone
asia_jayapura () = force asia_jayapura_l

{-| Asia/Jerusalem -}
asia_jerusalem : () -> TimeZone
asia_jerusalem () = force asia_jerusalem_l

{-| Asia/Kabul -}
asia_kabul : () -> TimeZone
asia_kabul () = force asia_kabul_l

{-| Asia/Kamchatka -}
asia_kamchatka : () -> TimeZone
asia_kamchatka () = force asia_kamchatka_l

{-| Asia/Karachi -}
asia_karachi : () -> TimeZone
asia_karachi () = force asia_karachi_l

{-| Asia/Urumqi -}
asia_kashgar : () -> TimeZone
asia_kashgar () = force (link "Asia/Urumqi" asia_urumqi_l)

{-| Asia/Kathmandu -}
asia_kathmandu : () -> TimeZone
asia_kathmandu () = force asia_kathmandu_l

{-| Asia/Kathmandu -}
asia_katmandu : () -> TimeZone
asia_katmandu () = force (link "Asia/Kathmandu" asia_kathmandu_l)

{-| Asia/Khandyga -}
asia_khandyga : () -> TimeZone
asia_khandyga () = force asia_khandyga_l

{-| Asia/Kolkata -}
asia_kolkata : () -> TimeZone
asia_kolkata () = force asia_kolkata_l

{-| Asia/Krasnoyarsk -}
asia_krasnoyarsk : () -> TimeZone
asia_krasnoyarsk () = force asia_krasnoyarsk_l

{-| Asia/Kuala_Lumpur -}
asia_kuala_lumpur : () -> TimeZone
asia_kuala_lumpur () = force asia_kuala_lumpur_l

{-| Asia/Kuching -}
asia_kuching : () -> TimeZone
asia_kuching () = force asia_kuching_l

{-| Asia/Riyadh -}
asia_kuwait : () -> TimeZone
asia_kuwait () = force (link "Asia/Riyadh" asia_riyadh_l)

{-| Asia/Macau -}
asia_macao : () -> TimeZone
asia_macao () = force (link "Asia/Macau" asia_macau_l)

{-| Asia/Macau -}
asia_macau : () -> TimeZone
asia_macau () = force asia_macau_l

{-| Asia/Magadan -}
asia_magadan : () -> TimeZone
asia_magadan () = force asia_magadan_l

{-| Asia/Makassar -}
asia_makassar : () -> TimeZone
asia_makassar () = force asia_makassar_l

{-| Asia/Manila -}
asia_manila : () -> TimeZone
asia_manila () = force asia_manila_l

{-| Asia/Dubai -}
asia_muscat : () -> TimeZone
asia_muscat () = force (link "Asia/Dubai" asia_dubai_l)

{-| Asia/Nicosia -}
asia_nicosia : () -> TimeZone
asia_nicosia () = force asia_nicosia_l

{-| Asia/Novokuznetsk -}
asia_novokuznetsk : () -> TimeZone
asia_novokuznetsk () = force asia_novokuznetsk_l

{-| Asia/Novosibirsk -}
asia_novosibirsk : () -> TimeZone
asia_novosibirsk () = force asia_novosibirsk_l

{-| Asia/Omsk -}
asia_omsk : () -> TimeZone
asia_omsk () = force asia_omsk_l

{-| Asia/Oral -}
asia_oral : () -> TimeZone
asia_oral () = force asia_oral_l

{-| Asia/Bangkok -}
asia_phnom_penh : () -> TimeZone
asia_phnom_penh () = force (link "Asia/Bangkok" asia_bangkok_l)

{-| Asia/Pontianak -}
asia_pontianak : () -> TimeZone
asia_pontianak () = force asia_pontianak_l

{-| Asia/Pyongyang -}
asia_pyongyang : () -> TimeZone
asia_pyongyang () = force asia_pyongyang_l

{-| Asia/Qatar -}
asia_qatar : () -> TimeZone
asia_qatar () = force asia_qatar_l

{-| Asia/Qyzylorda -}
asia_qyzylorda : () -> TimeZone
asia_qyzylorda () = force asia_qyzylorda_l

{-| Asia/Rangoon -}
asia_rangoon : () -> TimeZone
asia_rangoon () = force asia_rangoon_l

{-| Asia/Riyadh -}
asia_riyadh : () -> TimeZone
asia_riyadh () = force asia_riyadh_l

{-| Asia/Ho_Chi_Minh -}
asia_saigon : () -> TimeZone
asia_saigon () = force (link "Asia/Ho_Chi_Minh" asia_ho_chi_minh_l)

{-| Asia/Sakhalin -}
asia_sakhalin : () -> TimeZone
asia_sakhalin () = force asia_sakhalin_l

{-| Asia/Samarkand -}
asia_samarkand : () -> TimeZone
asia_samarkand () = force asia_samarkand_l

{-| Asia/Seoul -}
asia_seoul : () -> TimeZone
asia_seoul () = force asia_seoul_l

{-| Asia/Shanghai -}
asia_shanghai : () -> TimeZone
asia_shanghai () = force asia_shanghai_l

{-| Asia/Singapore -}
asia_singapore : () -> TimeZone
asia_singapore () = force asia_singapore_l

{-| Asia/Srednekolymsk -}
asia_srednekolymsk : () -> TimeZone
asia_srednekolymsk () = force asia_srednekolymsk_l

{-| Asia/Taipei -}
asia_taipei : () -> TimeZone
asia_taipei () = force asia_taipei_l

{-| Asia/Tashkent -}
asia_tashkent : () -> TimeZone
asia_tashkent () = force asia_tashkent_l

{-| Asia/Tbilisi -}
asia_tbilisi : () -> TimeZone
asia_tbilisi () = force asia_tbilisi_l

{-| Asia/Tehran -}
asia_tehran : () -> TimeZone
asia_tehran () = force asia_tehran_l

{-| Asia/Jerusalem -}
asia_tel_aviv : () -> TimeZone
asia_tel_aviv () = force (link "Asia/Jerusalem" asia_jerusalem_l)

{-| Asia/Thimphu -}
asia_thimbu : () -> TimeZone
asia_thimbu () = force (link "Asia/Thimphu" asia_thimphu_l)

{-| Asia/Thimphu -}
asia_thimphu : () -> TimeZone
asia_thimphu () = force asia_thimphu_l

{-| Asia/Tokyo -}
asia_tokyo : () -> TimeZone
asia_tokyo () = force asia_tokyo_l

{-| Asia/Tomsk -}
asia_tomsk : () -> TimeZone
asia_tomsk () = force asia_tomsk_l

{-| Asia/Makassar -}
asia_ujung_pandang : () -> TimeZone
asia_ujung_pandang () = force (link "Asia/Makassar" asia_makassar_l)

{-| Asia/Ulaanbaatar -}
asia_ulaanbaatar : () -> TimeZone
asia_ulaanbaatar () = force asia_ulaanbaatar_l

{-| Asia/Ulaanbaatar -}
asia_ulan_bator : () -> TimeZone
asia_ulan_bator () = force (link "Asia/Ulaanbaatar" asia_ulaanbaatar_l)

{-| Asia/Urumqi -}
asia_urumqi : () -> TimeZone
asia_urumqi () = force asia_urumqi_l

{-| Asia/Ust-Nera -}
asia_ust_nera : () -> TimeZone
asia_ust_nera () = force asia_ust_nera_l

{-| Asia/Bangkok -}
asia_vientiane : () -> TimeZone
asia_vientiane () = force (link "Asia/Bangkok" asia_bangkok_l)

{-| Asia/Vladivostok -}
asia_vladivostok : () -> TimeZone
asia_vladivostok () = force asia_vladivostok_l

{-| Asia/Yakutsk -}
asia_yakutsk : () -> TimeZone
asia_yakutsk () = force asia_yakutsk_l

{-| Asia/Rangoon -}
asia_yangon : () -> TimeZone
asia_yangon () = force (link "Asia/Rangoon" asia_rangoon_l)

{-| Asia/Yekaterinburg -}
asia_yekaterinburg : () -> TimeZone
asia_yekaterinburg () = force asia_yekaterinburg_l

{-| Asia/Yerevan -}
asia_yerevan : () -> TimeZone
asia_yerevan () = force asia_yerevan_l

{-| Atlantic/Azores -}
atlantic_azores : () -> TimeZone
atlantic_azores () = force atlantic_azores_l

{-| Atlantic/Bermuda -}
atlantic_bermuda : () -> TimeZone
atlantic_bermuda () = force atlantic_bermuda_l

{-| Atlantic/Canary -}
atlantic_canary : () -> TimeZone
atlantic_canary () = force atlantic_canary_l

{-| Atlantic/Cape_Verde -}
atlantic_cape_verde : () -> TimeZone
atlantic_cape_verde () = force atlantic_cape_verde_l

{-| Atlantic/Faroe -}
atlantic_faeroe : () -> TimeZone
atlantic_faeroe () = force (link "Atlantic/Faroe" atlantic_faroe_l)

{-| Atlantic/Faroe -}
atlantic_faroe : () -> TimeZone
atlantic_faroe () = force atlantic_faroe_l

{-| Europe/Oslo -}
atlantic_jan_mayen : () -> TimeZone
atlantic_jan_mayen () = force (link "Europe/Oslo" europe_oslo_l)

{-| Atlantic/Madeira -}
atlantic_madeira : () -> TimeZone
atlantic_madeira () = force atlantic_madeira_l

{-| Atlantic/Reykjavik -}
atlantic_reykjavik : () -> TimeZone
atlantic_reykjavik () = force atlantic_reykjavik_l

{-| Atlantic/South_Georgia -}
atlantic_south_georgia : () -> TimeZone
atlantic_south_georgia () = force atlantic_south_georgia_l

{-| Africa/Abidjan -}
atlantic_st_helena : () -> TimeZone
atlantic_st_helena () = force (link "Africa/Abidjan" africa_abidjan_l)

{-| Atlantic/Stanley -}
atlantic_stanley : () -> TimeZone
atlantic_stanley () = force atlantic_stanley_l

{-| Australia/Sydney -}
australia_act : () -> TimeZone
australia_act () = force (link "Australia/Sydney" australia_sydney_l)

{-| Australia/Adelaide -}
australia_adelaide : () -> TimeZone
australia_adelaide () = force australia_adelaide_l

{-| Australia/Brisbane -}
australia_brisbane : () -> TimeZone
australia_brisbane () = force australia_brisbane_l

{-| Australia/Broken_Hill -}
australia_broken_hill : () -> TimeZone
australia_broken_hill () = force australia_broken_hill_l

{-| Australia/Sydney -}
australia_canberra : () -> TimeZone
australia_canberra () = force (link "Australia/Sydney" australia_sydney_l)

{-| Australia/Currie -}
australia_currie : () -> TimeZone
australia_currie () = force australia_currie_l

{-| Australia/Darwin -}
australia_darwin : () -> TimeZone
australia_darwin () = force australia_darwin_l

{-| Australia/Eucla -}
australia_eucla : () -> TimeZone
australia_eucla () = force australia_eucla_l

{-| Australia/Hobart -}
australia_hobart : () -> TimeZone
australia_hobart () = force australia_hobart_l

{-| Australia/Lord_Howe -}
australia_lhi : () -> TimeZone
australia_lhi () = force (link "Australia/Lord_Howe" australia_lord_howe_l)

{-| Australia/Lindeman -}
australia_lindeman : () -> TimeZone
australia_lindeman () = force australia_lindeman_l

{-| Australia/Lord_Howe -}
australia_lord_howe : () -> TimeZone
australia_lord_howe () = force australia_lord_howe_l

{-| Australia/Melbourne -}
australia_melbourne : () -> TimeZone
australia_melbourne () = force australia_melbourne_l

{-| Australia/Darwin -}
australia_north : () -> TimeZone
australia_north () = force (link "Australia/Darwin" australia_darwin_l)

{-| Australia/Sydney -}
australia_nsw : () -> TimeZone
australia_nsw () = force (link "Australia/Sydney" australia_sydney_l)

{-| Australia/Perth -}
australia_perth : () -> TimeZone
australia_perth () = force australia_perth_l

{-| Australia/Brisbane -}
australia_queensland : () -> TimeZone
australia_queensland () = force (link "Australia/Brisbane" australia_brisbane_l)

{-| Australia/Adelaide -}
australia_south : () -> TimeZone
australia_south () = force (link "Australia/Adelaide" australia_adelaide_l)

{-| Australia/Sydney -}
australia_sydney : () -> TimeZone
australia_sydney () = force australia_sydney_l

{-| Australia/Hobart -}
australia_tasmania : () -> TimeZone
australia_tasmania () = force (link "Australia/Hobart" australia_hobart_l)

{-| Australia/Melbourne -}
australia_victoria : () -> TimeZone
australia_victoria () = force (link "Australia/Melbourne" australia_melbourne_l)

{-| Australia/Perth -}
australia_west : () -> TimeZone
australia_west () = force (link "Australia/Perth" australia_perth_l)

{-| Australia/Broken_Hill -}
australia_yancowinna : () -> TimeZone
australia_yancowinna () = force (link "Australia/Broken_Hill" australia_broken_hill_l)

{-| America/Rio_Branco -}
brazil_acre : () -> TimeZone
brazil_acre () = force (link "America/Rio_Branco" america_rio_branco_l)

{-| America/Noronha -}
brazil_denoronha : () -> TimeZone
brazil_denoronha () = force (link "America/Noronha" america_noronha_l)

{-| America/Sao_Paulo -}
brazil_east : () -> TimeZone
brazil_east () = force (link "America/Sao_Paulo" america_sao_paulo_l)

{-| America/Manaus -}
brazil_west : () -> TimeZone
brazil_west () = force (link "America/Manaus" america_manaus_l)

{-| America/Halifax -}
canada_atlantic : () -> TimeZone
canada_atlantic () = force (link "America/Halifax" america_halifax_l)

{-| America/Winnipeg -}
canada_central : () -> TimeZone
canada_central () = force (link "America/Winnipeg" america_winnipeg_l)

{-| America/Regina -}
canada_east_saskatchewan : () -> TimeZone
canada_east_saskatchewan () = force (link "America/Regina" america_regina_l)

{-| America/Toronto -}
canada_eastern : () -> TimeZone
canada_eastern () = force (link "America/Toronto" america_toronto_l)

{-| America/Edmonton -}
canada_mountain : () -> TimeZone
canada_mountain () = force (link "America/Edmonton" america_edmonton_l)

{-| America/St_Johns -}
canada_newfoundland : () -> TimeZone
canada_newfoundland () = force (link "America/St_Johns" america_st_johns_l)

{-| America/Vancouver -}
canada_pacific : () -> TimeZone
canada_pacific () = force (link "America/Vancouver" america_vancouver_l)

{-| America/Regina -}
canada_saskatchewan : () -> TimeZone
canada_saskatchewan () = force (link "America/Regina" america_regina_l)

{-| America/Whitehorse -}
canada_yukon : () -> TimeZone
canada_yukon () = force (link "America/Whitehorse" america_whitehorse_l)

{-| CET -}
cet : () -> TimeZone
cet () = force cet_l

{-| America/Santiago -}
chile_continental : () -> TimeZone
chile_continental () = force (link "America/Santiago" america_santiago_l)

{-| Pacific/Easter -}
chile_easterisland : () -> TimeZone
chile_easterisland () = force (link "Pacific/Easter" pacific_easter_l)

{-| CST6CDT -}
cst6cdt : () -> TimeZone
cst6cdt () = force cst6cdt_l

{-| America/Havana -}
cuba : () -> TimeZone
cuba () = force (link "America/Havana" america_havana_l)

{-| EET -}
eet : () -> TimeZone
eet () = force eet_l

{-| Africa/Cairo -}
egypt : () -> TimeZone
egypt () = force (link "Africa/Cairo" africa_cairo_l)

{-| Europe/Dublin -}
eire : () -> TimeZone
eire () = force (link "Europe/Dublin" europe_dublin_l)

{-| EST -}
est : () -> TimeZone
est () = force est_l

{-| EST5EDT -}
est5edt : () -> TimeZone
est5edt () = force est5edt_l

{-| Etc/GMT+0 -}
etc_gmt : () -> TimeZone
etc_gmt () = force (link "Etc/GMT+0" etc_gmt_plus_0_l)

{-| Etc/GMT+0 -}
etc_gmt_0 : () -> TimeZone
etc_gmt_0 () = force (link "Etc/GMT+0" etc_gmt_plus_0_l)

{-| Etc/GMT+0 -}
etc_gmt_minus_0 : () -> TimeZone
etc_gmt_minus_0 () = force (link "Etc/GMT+0" etc_gmt_plus_0_l)

{-| Etc/GMT-1 -}
etc_gmt_minus_1 : () -> TimeZone
etc_gmt_minus_1 () = force etc_gmt_minus_1_l

{-| Etc/GMT-10 -}
etc_gmt_minus_10 : () -> TimeZone
etc_gmt_minus_10 () = force etc_gmt_minus_10_l

{-| Etc/GMT-11 -}
etc_gmt_minus_11 : () -> TimeZone
etc_gmt_minus_11 () = force etc_gmt_minus_11_l

{-| Etc/GMT-12 -}
etc_gmt_minus_12 : () -> TimeZone
etc_gmt_minus_12 () = force etc_gmt_minus_12_l

{-| Etc/GMT-13 -}
etc_gmt_minus_13 : () -> TimeZone
etc_gmt_minus_13 () = force etc_gmt_minus_13_l

{-| Etc/GMT-14 -}
etc_gmt_minus_14 : () -> TimeZone
etc_gmt_minus_14 () = force etc_gmt_minus_14_l

{-| Etc/GMT-2 -}
etc_gmt_minus_2 : () -> TimeZone
etc_gmt_minus_2 () = force etc_gmt_minus_2_l

{-| Etc/GMT-3 -}
etc_gmt_minus_3 : () -> TimeZone
etc_gmt_minus_3 () = force etc_gmt_minus_3_l

{-| Etc/GMT-4 -}
etc_gmt_minus_4 : () -> TimeZone
etc_gmt_minus_4 () = force etc_gmt_minus_4_l

{-| Etc/GMT-5 -}
etc_gmt_minus_5 : () -> TimeZone
etc_gmt_minus_5 () = force etc_gmt_minus_5_l

{-| Etc/GMT-6 -}
etc_gmt_minus_6 : () -> TimeZone
etc_gmt_minus_6 () = force etc_gmt_minus_6_l

{-| Etc/GMT-7 -}
etc_gmt_minus_7 : () -> TimeZone
etc_gmt_minus_7 () = force etc_gmt_minus_7_l

{-| Etc/GMT-8 -}
etc_gmt_minus_8 : () -> TimeZone
etc_gmt_minus_8 () = force etc_gmt_minus_8_l

{-| Etc/GMT-9 -}
etc_gmt_minus_9 : () -> TimeZone
etc_gmt_minus_9 () = force etc_gmt_minus_9_l

{-| Etc/GMT+0 -}
etc_gmt_plus_0 : () -> TimeZone
etc_gmt_plus_0 () = force etc_gmt_plus_0_l

{-| Etc/GMT+1 -}
etc_gmt_plus_1 : () -> TimeZone
etc_gmt_plus_1 () = force etc_gmt_plus_1_l

{-| Etc/GMT+10 -}
etc_gmt_plus_10 : () -> TimeZone
etc_gmt_plus_10 () = force etc_gmt_plus_10_l

{-| Etc/GMT+11 -}
etc_gmt_plus_11 : () -> TimeZone
etc_gmt_plus_11 () = force etc_gmt_plus_11_l

{-| Etc/GMT+12 -}
etc_gmt_plus_12 : () -> TimeZone
etc_gmt_plus_12 () = force etc_gmt_plus_12_l

{-| Etc/GMT+2 -}
etc_gmt_plus_2 : () -> TimeZone
etc_gmt_plus_2 () = force etc_gmt_plus_2_l

{-| Etc/GMT+3 -}
etc_gmt_plus_3 : () -> TimeZone
etc_gmt_plus_3 () = force etc_gmt_plus_3_l

{-| Etc/GMT+4 -}
etc_gmt_plus_4 : () -> TimeZone
etc_gmt_plus_4 () = force etc_gmt_plus_4_l

{-| Etc/GMT+5 -}
etc_gmt_plus_5 : () -> TimeZone
etc_gmt_plus_5 () = force etc_gmt_plus_5_l

{-| Etc/GMT+6 -}
etc_gmt_plus_6 : () -> TimeZone
etc_gmt_plus_6 () = force etc_gmt_plus_6_l

{-| Etc/GMT+7 -}
etc_gmt_plus_7 : () -> TimeZone
etc_gmt_plus_7 () = force etc_gmt_plus_7_l

{-| Etc/GMT+8 -}
etc_gmt_plus_8 : () -> TimeZone
etc_gmt_plus_8 () = force etc_gmt_plus_8_l

{-| Etc/GMT+9 -}
etc_gmt_plus_9 : () -> TimeZone
etc_gmt_plus_9 () = force etc_gmt_plus_9_l

{-| Etc/GMT+0 -}
etc_greenwich : () -> TimeZone
etc_greenwich () = force (link "Etc/GMT+0" etc_gmt_plus_0_l)

{-| Etc/UCT -}
etc_uct : () -> TimeZone
etc_uct () = force etc_uct_l

{-| Etc/UTC -}
etc_universal : () -> TimeZone
etc_universal () = force (link "Etc/UTC" etc_utc_l)

{-| Etc/UTC -}
etc_utc : () -> TimeZone
etc_utc () = force etc_utc_l

{-| Etc/UTC -}
etc_zulu : () -> TimeZone
etc_zulu () = force (link "Etc/UTC" etc_utc_l)

{-| Europe/Amsterdam -}
europe_amsterdam : () -> TimeZone
europe_amsterdam () = force europe_amsterdam_l

{-| Europe/Andorra -}
europe_andorra : () -> TimeZone
europe_andorra () = force europe_andorra_l

{-| Europe/Astrakhan -}
europe_astrakhan : () -> TimeZone
europe_astrakhan () = force europe_astrakhan_l

{-| Europe/Athens -}
europe_athens : () -> TimeZone
europe_athens () = force europe_athens_l

{-| Europe/London -}
europe_belfast : () -> TimeZone
europe_belfast () = force (link "Europe/London" europe_london_l)

{-| Europe/Belgrade -}
europe_belgrade : () -> TimeZone
europe_belgrade () = force europe_belgrade_l

{-| Europe/Berlin -}
europe_berlin : () -> TimeZone
europe_berlin () = force europe_berlin_l

{-| Europe/Prague -}
europe_bratislava : () -> TimeZone
europe_bratislava () = force (link "Europe/Prague" europe_prague_l)

{-| Europe/Brussels -}
europe_brussels : () -> TimeZone
europe_brussels () = force europe_brussels_l

{-| Europe/Bucharest -}
europe_bucharest : () -> TimeZone
europe_bucharest () = force europe_bucharest_l

{-| Europe/Budapest -}
europe_budapest : () -> TimeZone
europe_budapest () = force europe_budapest_l

{-| Europe/Zurich -}
europe_busingen : () -> TimeZone
europe_busingen () = force (link "Europe/Zurich" europe_zurich_l)

{-| Europe/Chisinau -}
europe_chisinau : () -> TimeZone
europe_chisinau () = force europe_chisinau_l

{-| Europe/Copenhagen -}
europe_copenhagen : () -> TimeZone
europe_copenhagen () = force europe_copenhagen_l

{-| Europe/Dublin -}
europe_dublin : () -> TimeZone
europe_dublin () = force europe_dublin_l

{-| Europe/Gibraltar -}
europe_gibraltar : () -> TimeZone
europe_gibraltar () = force europe_gibraltar_l

{-| Europe/London -}
europe_guernsey : () -> TimeZone
europe_guernsey () = force (link "Europe/London" europe_london_l)

{-| Europe/Helsinki -}
europe_helsinki : () -> TimeZone
europe_helsinki () = force europe_helsinki_l

{-| Europe/London -}
europe_isle_of_man : () -> TimeZone
europe_isle_of_man () = force (link "Europe/London" europe_london_l)

{-| Europe/Istanbul -}
europe_istanbul : () -> TimeZone
europe_istanbul () = force europe_istanbul_l

{-| Europe/London -}
europe_jersey : () -> TimeZone
europe_jersey () = force (link "Europe/London" europe_london_l)

{-| Europe/Kaliningrad -}
europe_kaliningrad : () -> TimeZone
europe_kaliningrad () = force europe_kaliningrad_l

{-| Europe/Kiev -}
europe_kiev : () -> TimeZone
europe_kiev () = force europe_kiev_l

{-| Europe/Kirov -}
europe_kirov : () -> TimeZone
europe_kirov () = force europe_kirov_l

{-| Europe/Lisbon -}
europe_lisbon : () -> TimeZone
europe_lisbon () = force europe_lisbon_l

{-| Europe/Belgrade -}
europe_ljubljana : () -> TimeZone
europe_ljubljana () = force (link "Europe/Belgrade" europe_belgrade_l)

{-| Europe/London -}
europe_london : () -> TimeZone
europe_london () = force europe_london_l

{-| Europe/Luxembourg -}
europe_luxembourg : () -> TimeZone
europe_luxembourg () = force europe_luxembourg_l

{-| Europe/Madrid -}
europe_madrid : () -> TimeZone
europe_madrid () = force europe_madrid_l

{-| Europe/Malta -}
europe_malta : () -> TimeZone
europe_malta () = force europe_malta_l

{-| Europe/Helsinki -}
europe_mariehamn : () -> TimeZone
europe_mariehamn () = force (link "Europe/Helsinki" europe_helsinki_l)

{-| Europe/Minsk -}
europe_minsk : () -> TimeZone
europe_minsk () = force europe_minsk_l

{-| Europe/Monaco -}
europe_monaco : () -> TimeZone
europe_monaco () = force europe_monaco_l

{-| Europe/Moscow -}
europe_moscow : () -> TimeZone
europe_moscow () = force europe_moscow_l

{-| Asia/Nicosia -}
europe_nicosia : () -> TimeZone
europe_nicosia () = force (link "Asia/Nicosia" asia_nicosia_l)

{-| Europe/Oslo -}
europe_oslo : () -> TimeZone
europe_oslo () = force europe_oslo_l

{-| Europe/Paris -}
europe_paris : () -> TimeZone
europe_paris () = force europe_paris_l

{-| Europe/Belgrade -}
europe_podgorica : () -> TimeZone
europe_podgorica () = force (link "Europe/Belgrade" europe_belgrade_l)

{-| Europe/Prague -}
europe_prague : () -> TimeZone
europe_prague () = force europe_prague_l

{-| Europe/Riga -}
europe_riga : () -> TimeZone
europe_riga () = force europe_riga_l

{-| Europe/Rome -}
europe_rome : () -> TimeZone
europe_rome () = force europe_rome_l

{-| Europe/Samara -}
europe_samara : () -> TimeZone
europe_samara () = force europe_samara_l

{-| Europe/Rome -}
europe_san_marino : () -> TimeZone
europe_san_marino () = force (link "Europe/Rome" europe_rome_l)

{-| Europe/Belgrade -}
europe_sarajevo : () -> TimeZone
europe_sarajevo () = force (link "Europe/Belgrade" europe_belgrade_l)

{-| Europe/Saratov -}
europe_saratov : () -> TimeZone
europe_saratov () = force europe_saratov_l

{-| Europe/Simferopol -}
europe_simferopol : () -> TimeZone
europe_simferopol () = force europe_simferopol_l

{-| Europe/Belgrade -}
europe_skopje : () -> TimeZone
europe_skopje () = force (link "Europe/Belgrade" europe_belgrade_l)

{-| Europe/Sofia -}
europe_sofia : () -> TimeZone
europe_sofia () = force europe_sofia_l

{-| Europe/Stockholm -}
europe_stockholm : () -> TimeZone
europe_stockholm () = force europe_stockholm_l

{-| Europe/Tallinn -}
europe_tallinn : () -> TimeZone
europe_tallinn () = force europe_tallinn_l

{-| Europe/Tirane -}
europe_tirane : () -> TimeZone
europe_tirane () = force europe_tirane_l

{-| Europe/Chisinau -}
europe_tiraspol : () -> TimeZone
europe_tiraspol () = force (link "Europe/Chisinau" europe_chisinau_l)

{-| Europe/Ulyanovsk -}
europe_ulyanovsk : () -> TimeZone
europe_ulyanovsk () = force europe_ulyanovsk_l

{-| Europe/Uzhgorod -}
europe_uzhgorod : () -> TimeZone
europe_uzhgorod () = force europe_uzhgorod_l

{-| Europe/Zurich -}
europe_vaduz : () -> TimeZone
europe_vaduz () = force (link "Europe/Zurich" europe_zurich_l)

{-| Europe/Rome -}
europe_vatican : () -> TimeZone
europe_vatican () = force (link "Europe/Rome" europe_rome_l)

{-| Europe/Vienna -}
europe_vienna : () -> TimeZone
europe_vienna () = force europe_vienna_l

{-| Europe/Vilnius -}
europe_vilnius : () -> TimeZone
europe_vilnius () = force europe_vilnius_l

{-| Europe/Volgograd -}
europe_volgograd : () -> TimeZone
europe_volgograd () = force europe_volgograd_l

{-| Europe/Warsaw -}
europe_warsaw : () -> TimeZone
europe_warsaw () = force europe_warsaw_l

{-| Europe/Belgrade -}
europe_zagreb : () -> TimeZone
europe_zagreb () = force (link "Europe/Belgrade" europe_belgrade_l)

{-| Europe/Zaporozhye -}
europe_zaporozhye : () -> TimeZone
europe_zaporozhye () = force europe_zaporozhye_l

{-| Europe/Zurich -}
europe_zurich : () -> TimeZone
europe_zurich () = force europe_zurich_l

{-| Europe/London -}
gb : () -> TimeZone
gb () = force (link "Europe/London" europe_london_l)

{-| Europe/London -}
gb_eire : () -> TimeZone
gb_eire () = force (link "Europe/London" europe_london_l)

{-| Etc/GMT+0 -}
gmt : () -> TimeZone
gmt () = force (link "Etc/GMT+0" etc_gmt_plus_0_l)

{-| Etc/GMT+0 -}
gmt_0 : () -> TimeZone
gmt_0 () = force (link "Etc/GMT+0" etc_gmt_plus_0_l)

{-| Etc/GMT+0 -}
gmt_minus_0 : () -> TimeZone
gmt_minus_0 () = force (link "Etc/GMT+0" etc_gmt_plus_0_l)

{-| Etc/GMT+0 -}
gmt_plus_0 : () -> TimeZone
gmt_plus_0 () = force (link "Etc/GMT+0" etc_gmt_plus_0_l)

{-| Etc/GMT+0 -}
greenwich : () -> TimeZone
greenwich () = force (link "Etc/GMT+0" etc_gmt_plus_0_l)

{-| Asia/Hong_Kong -}
hongkong : () -> TimeZone
hongkong () = force (link "Asia/Hong_Kong" asia_hong_kong_l)

{-| HST -}
hst : () -> TimeZone
hst () = force hst_l

{-| Atlantic/Reykjavik -}
iceland : () -> TimeZone
iceland () = force (link "Atlantic/Reykjavik" atlantic_reykjavik_l)

{-| Africa/Nairobi -}
indian_antananarivo : () -> TimeZone
indian_antananarivo () = force (link "Africa/Nairobi" africa_nairobi_l)

{-| Indian/Chagos -}
indian_chagos : () -> TimeZone
indian_chagos () = force indian_chagos_l

{-| Indian/Christmas -}
indian_christmas : () -> TimeZone
indian_christmas () = force indian_christmas_l

{-| Indian/Cocos -}
indian_cocos : () -> TimeZone
indian_cocos () = force indian_cocos_l

{-| Africa/Nairobi -}
indian_comoro : () -> TimeZone
indian_comoro () = force (link "Africa/Nairobi" africa_nairobi_l)

{-| Indian/Kerguelen -}
indian_kerguelen : () -> TimeZone
indian_kerguelen () = force indian_kerguelen_l

{-| Indian/Mahe -}
indian_mahe : () -> TimeZone
indian_mahe () = force indian_mahe_l

{-| Indian/Maldives -}
indian_maldives : () -> TimeZone
indian_maldives () = force indian_maldives_l

{-| Indian/Mauritius -}
indian_mauritius : () -> TimeZone
indian_mauritius () = force indian_mauritius_l

{-| Africa/Nairobi -}
indian_mayotte : () -> TimeZone
indian_mayotte () = force (link "Africa/Nairobi" africa_nairobi_l)

{-| Indian/Reunion -}
indian_reunion : () -> TimeZone
indian_reunion () = force indian_reunion_l

{-| Asia/Tehran -}
iran : () -> TimeZone
iran () = force (link "Asia/Tehran" asia_tehran_l)

{-| Asia/Jerusalem -}
israel : () -> TimeZone
israel () = force (link "Asia/Jerusalem" asia_jerusalem_l)

{-| America/Jamaica -}
jamaica : () -> TimeZone
jamaica () = force (link "America/Jamaica" america_jamaica_l)

{-| Asia/Tokyo -}
japan : () -> TimeZone
japan () = force (link "Asia/Tokyo" asia_tokyo_l)

{-| Pacific/Kwajalein -}
kwajalein : () -> TimeZone
kwajalein () = force (link "Pacific/Kwajalein" pacific_kwajalein_l)

{-| Africa/Tripoli -}
libya : () -> TimeZone
libya () = force (link "Africa/Tripoli" africa_tripoli_l)

{-| MET -}
met : () -> TimeZone
met () = force met_l

{-| America/Tijuana -}
mexico_bajanorte : () -> TimeZone
mexico_bajanorte () = force (link "America/Tijuana" america_tijuana_l)

{-| America/Mazatlan -}
mexico_bajasur : () -> TimeZone
mexico_bajasur () = force (link "America/Mazatlan" america_mazatlan_l)

{-| America/Mexico_City -}
mexico_general : () -> TimeZone
mexico_general () = force (link "America/Mexico_City" america_mexico_city_l)

{-| MST -}
mst : () -> TimeZone
mst () = force mst_l

{-| MST7MDT -}
mst7mdt : () -> TimeZone
mst7mdt () = force mst7mdt_l

{-| America/Denver -}
navajo : () -> TimeZone
navajo () = force (link "America/Denver" america_denver_l)

{-| Pacific/Auckland -}
nz : () -> TimeZone
nz () = force (link "Pacific/Auckland" pacific_auckland_l)

{-| Pacific/Chatham -}
nz_chat : () -> TimeZone
nz_chat () = force (link "Pacific/Chatham" pacific_chatham_l)

{-| Pacific/Apia -}
pacific_apia : () -> TimeZone
pacific_apia () = force pacific_apia_l

{-| Pacific/Auckland -}
pacific_auckland : () -> TimeZone
pacific_auckland () = force pacific_auckland_l

{-| Pacific/Bougainville -}
pacific_bougainville : () -> TimeZone
pacific_bougainville () = force pacific_bougainville_l

{-| Pacific/Chatham -}
pacific_chatham : () -> TimeZone
pacific_chatham () = force pacific_chatham_l

{-| Pacific/Chuuk -}
pacific_chuuk : () -> TimeZone
pacific_chuuk () = force pacific_chuuk_l

{-| Pacific/Easter -}
pacific_easter : () -> TimeZone
pacific_easter () = force pacific_easter_l

{-| Pacific/Efate -}
pacific_efate : () -> TimeZone
pacific_efate () = force pacific_efate_l

{-| Pacific/Enderbury -}
pacific_enderbury : () -> TimeZone
pacific_enderbury () = force pacific_enderbury_l

{-| Pacific/Fakaofo -}
pacific_fakaofo : () -> TimeZone
pacific_fakaofo () = force pacific_fakaofo_l

{-| Pacific/Fiji -}
pacific_fiji : () -> TimeZone
pacific_fiji () = force pacific_fiji_l

{-| Pacific/Funafuti -}
pacific_funafuti : () -> TimeZone
pacific_funafuti () = force pacific_funafuti_l

{-| Pacific/Galapagos -}
pacific_galapagos : () -> TimeZone
pacific_galapagos () = force pacific_galapagos_l

{-| Pacific/Gambier -}
pacific_gambier : () -> TimeZone
pacific_gambier () = force pacific_gambier_l

{-| Pacific/Guadalcanal -}
pacific_guadalcanal : () -> TimeZone
pacific_guadalcanal () = force pacific_guadalcanal_l

{-| Pacific/Guam -}
pacific_guam : () -> TimeZone
pacific_guam () = force pacific_guam_l

{-| Pacific/Honolulu -}
pacific_honolulu : () -> TimeZone
pacific_honolulu () = force pacific_honolulu_l

{-| Pacific/Honolulu -}
pacific_johnston : () -> TimeZone
pacific_johnston () = force (link "Pacific/Honolulu" pacific_honolulu_l)

{-| Pacific/Kiritimati -}
pacific_kiritimati : () -> TimeZone
pacific_kiritimati () = force pacific_kiritimati_l

{-| Pacific/Kosrae -}
pacific_kosrae : () -> TimeZone
pacific_kosrae () = force pacific_kosrae_l

{-| Pacific/Kwajalein -}
pacific_kwajalein : () -> TimeZone
pacific_kwajalein () = force pacific_kwajalein_l

{-| Pacific/Majuro -}
pacific_majuro : () -> TimeZone
pacific_majuro () = force pacific_majuro_l

{-| Pacific/Marquesas -}
pacific_marquesas : () -> TimeZone
pacific_marquesas () = force pacific_marquesas_l

{-| Pacific/Pago_Pago -}
pacific_midway : () -> TimeZone
pacific_midway () = force (link "Pacific/Pago_Pago" pacific_pago_pago_l)

{-| Pacific/Nauru -}
pacific_nauru : () -> TimeZone
pacific_nauru () = force pacific_nauru_l

{-| Pacific/Niue -}
pacific_niue : () -> TimeZone
pacific_niue () = force pacific_niue_l

{-| Pacific/Norfolk -}
pacific_norfolk : () -> TimeZone
pacific_norfolk () = force pacific_norfolk_l

{-| Pacific/Noumea -}
pacific_noumea : () -> TimeZone
pacific_noumea () = force pacific_noumea_l

{-| Pacific/Pago_Pago -}
pacific_pago_pago : () -> TimeZone
pacific_pago_pago () = force pacific_pago_pago_l

{-| Pacific/Palau -}
pacific_palau : () -> TimeZone
pacific_palau () = force pacific_palau_l

{-| Pacific/Pitcairn -}
pacific_pitcairn : () -> TimeZone
pacific_pitcairn () = force pacific_pitcairn_l

{-| Pacific/Pohnpei -}
pacific_pohnpei : () -> TimeZone
pacific_pohnpei () = force pacific_pohnpei_l

{-| Pacific/Pohnpei -}
pacific_ponape : () -> TimeZone
pacific_ponape () = force (link "Pacific/Pohnpei" pacific_pohnpei_l)

{-| Pacific/Port_Moresby -}
pacific_port_moresby : () -> TimeZone
pacific_port_moresby () = force pacific_port_moresby_l

{-| Pacific/Rarotonga -}
pacific_rarotonga : () -> TimeZone
pacific_rarotonga () = force pacific_rarotonga_l

{-| Pacific/Guam -}
pacific_saipan : () -> TimeZone
pacific_saipan () = force (link "Pacific/Guam" pacific_guam_l)

{-| Pacific/Pago_Pago -}
pacific_samoa : () -> TimeZone
pacific_samoa () = force (link "Pacific/Pago_Pago" pacific_pago_pago_l)

{-| Pacific/Tahiti -}
pacific_tahiti : () -> TimeZone
pacific_tahiti () = force pacific_tahiti_l

{-| Pacific/Tarawa -}
pacific_tarawa : () -> TimeZone
pacific_tarawa () = force pacific_tarawa_l

{-| Pacific/Tongatapu -}
pacific_tongatapu : () -> TimeZone
pacific_tongatapu () = force pacific_tongatapu_l

{-| Pacific/Chuuk -}
pacific_truk : () -> TimeZone
pacific_truk () = force (link "Pacific/Chuuk" pacific_chuuk_l)

{-| Pacific/Wake -}
pacific_wake : () -> TimeZone
pacific_wake () = force pacific_wake_l

{-| Pacific/Wallis -}
pacific_wallis : () -> TimeZone
pacific_wallis () = force pacific_wallis_l

{-| Pacific/Chuuk -}
pacific_yap : () -> TimeZone
pacific_yap () = force (link "Pacific/Chuuk" pacific_chuuk_l)

{-| Europe/Warsaw -}
poland : () -> TimeZone
poland () = force (link "Europe/Warsaw" europe_warsaw_l)

{-| Europe/Lisbon -}
portugal : () -> TimeZone
portugal () = force (link "Europe/Lisbon" europe_lisbon_l)

{-| Asia/Shanghai -}
prc : () -> TimeZone
prc () = force (link "Asia/Shanghai" asia_shanghai_l)

{-| PST8PDT -}
pst8pdt : () -> TimeZone
pst8pdt () = force pst8pdt_l

{-| Asia/Taipei -}
roc : () -> TimeZone
roc () = force (link "Asia/Taipei" asia_taipei_l)

{-| Asia/Seoul -}
rok : () -> TimeZone
rok () = force (link "Asia/Seoul" asia_seoul_l)

{-| Asia/Singapore -}
singapore : () -> TimeZone
singapore () = force (link "Asia/Singapore" asia_singapore_l)

{-| Europe/Istanbul -}
turkey : () -> TimeZone
turkey () = force (link "Europe/Istanbul" europe_istanbul_l)

{-| Etc/UCT -}
uct : () -> TimeZone
uct () = force (link "Etc/UCT" etc_uct_l)

{-| Etc/UTC -}
universal : () -> TimeZone
universal () = force (link "Etc/UTC" etc_utc_l)

{-| America/Anchorage -}
us_alaska : () -> TimeZone
us_alaska () = force (link "America/Anchorage" america_anchorage_l)

{-| America/Adak -}
us_aleutian : () -> TimeZone
us_aleutian () = force (link "America/Adak" america_adak_l)

{-| America/Phoenix -}
us_arizona : () -> TimeZone
us_arizona () = force (link "America/Phoenix" america_phoenix_l)

{-| America/Chicago -}
us_central : () -> TimeZone
us_central () = force (link "America/Chicago" america_chicago_l)

{-| America/Fort_Wayne -}
us_east_indiana : () -> TimeZone
us_east_indiana () = force (link "America/Fort_Wayne" america_fort_wayne_l)

{-| America/New_York -}
us_eastern : () -> TimeZone
us_eastern () = force (link "America/New_York" america_new_york_l)

{-| Pacific/Honolulu -}
us_hawaii : () -> TimeZone
us_hawaii () = force (link "Pacific/Honolulu" pacific_honolulu_l)

{-| America/Indiana/Knox -}
us_indiana_starke : () -> TimeZone
us_indiana_starke () = force (link "America/Indiana/Knox" america_indiana_knox_l)

{-| America/Detroit -}
us_michigan : () -> TimeZone
us_michigan () = force (link "America/Detroit" america_detroit_l)

{-| America/Denver -}
us_mountain : () -> TimeZone
us_mountain () = force (link "America/Denver" america_denver_l)

{-| America/Los_Angeles -}
us_pacific : () -> TimeZone
us_pacific () = force (link "America/Los_Angeles" america_los_angeles_l)

{-| America/Los_Angeles -}
us_pacific_new : () -> TimeZone
us_pacific_new () = force (link "America/Los_Angeles" america_los_angeles_l)

{-| Pacific/Pago_Pago -}
us_samoa : () -> TimeZone
us_samoa () = force (link "Pacific/Pago_Pago" pacific_pago_pago_l)

{-| Etc/UTC -}
utc : () -> TimeZone
utc () = force (link "Etc/UTC" etc_utc_l)

{-| Europe/Moscow -}
w_su : () -> TimeZone
w_su () = force (link "Europe/Moscow" europe_moscow_l)

{-| WET -}
wet : () -> TimeZone
wet () = force wet_l

{-| Etc/UTC -}
zulu : () -> TimeZone
zulu () = force (link "Etc/UTC" etc_utc_l)


-- Utils
-- -----
{-| A mapping from TimeZone names to their respective functions.  Use
this to look up TimeZones by name. -}
all : Dict String (() -> TimeZone)
all =
    Dict.fromList <| List.concat [ [ ( "Africa/Abidjan", africa_abidjan ), ( "Africa/Accra", africa_accra ), ( "Africa/Nairobi", africa_addis_ababa ), ( "Africa/Algiers", africa_algiers ), ( "Africa/Nairobi", africa_asmara ), ( "Africa/Nairobi", africa_asmera ), ( "Africa/Abidjan", africa_bamako ), ( "Africa/Lagos", africa_bangui ), ( "Africa/Abidjan", africa_banjul ), ( "Africa/Bissau", africa_bissau ), ( "Africa/Maputo", africa_blantyre ), ( "Africa/Lagos", africa_brazzaville ), ( "Africa/Maputo", africa_bujumbura ), ( "Africa/Cairo", africa_cairo ), ( "Africa/Casablanca", africa_casablanca ), ( "Africa/Ceuta", africa_ceuta ), ( "Africa/Abidjan", africa_conakry ), ( "Africa/Abidjan", africa_dakar ), ( "Africa/Nairobi", africa_dar_es_salaam ), ( "Africa/Nairobi", africa_djibouti ), ( "Africa/Lagos", africa_douala ), ( "Africa/El_Aaiun", africa_el_aaiun ), ( "Africa/Abidjan", africa_freetown ), ( "Africa/Maputo", africa_gaborone ), ( "Africa/Maputo", africa_harare ), ( "Africa/Johannesburg", africa_johannesburg ), ( "Africa/Khartoum", africa_juba ), ( "Africa/Nairobi", africa_kampala ), ( "Africa/Khartoum", africa_khartoum ), ( "Africa/Maputo", africa_kigali ) ], [ ( "Africa/Lagos", africa_kinshasa ), ( "Africa/Lagos", africa_lagos ), ( "Africa/Lagos", africa_libreville ), ( "Africa/Abidjan", africa_lome ), ( "Africa/Lagos", africa_luanda ), ( "Africa/Maputo", africa_lubumbashi ), ( "Africa/Maputo", africa_lusaka ), ( "Africa/Lagos", africa_malabo ), ( "Africa/Maputo", africa_maputo ), ( "Africa/Johannesburg", africa_maseru ), ( "Africa/Johannesburg", africa_mbabane ), ( "Africa/Nairobi", africa_mogadishu ), ( "Africa/Monrovia", africa_monrovia ), ( "Africa/Nairobi", africa_nairobi ), ( "Africa/Ndjamena", africa_ndjamena ), ( "Africa/Lagos", africa_niamey ), ( "Africa/Abidjan", africa_nouakchott ), ( "Africa/Abidjan", africa_ouagadougou ), ( "Africa/Lagos", africa_porto_novo ), ( "Africa/Abidjan", africa_sao_tome ), ( "Africa/Abidjan", africa_timbuktu ), ( "Africa/Tripoli", africa_tripoli ), ( "Africa/Tunis", africa_tunis ), ( "Africa/Windhoek", africa_windhoek ), ( "America/Adak", america_adak ), ( "America/Anchorage", america_anchorage ), ( "America/Port_of_Spain", america_anguilla ), ( "America/Port_of_Spain", america_antigua ), ( "America/Araguaina", america_araguaina ), ( "America/Argentina/Buenos_Aires", america_argentina_buenos_aires ) ], [ ( "America/Argentina/Catamarca", america_argentina_catamarca ), ( "America/Argentina/Catamarca", america_argentina_comodrivadavia ), ( "America/Argentina/Cordoba", america_argentina_cordoba ), ( "America/Argentina/Jujuy", america_argentina_jujuy ), ( "America/Argentina/La_Rioja", america_argentina_la_rioja ), ( "America/Argentina/Mendoza", america_argentina_mendoza ), ( "America/Argentina/Rio_Gallegos", america_argentina_rio_gallegos ), ( "America/Argentina/Salta", america_argentina_salta ), ( "America/Argentina/San_Juan", america_argentina_san_juan ), ( "America/Argentina/San_Luis", america_argentina_san_luis ), ( "America/Argentina/Tucuman", america_argentina_tucuman ), ( "America/Argentina/Ushuaia", america_argentina_ushuaia ), ( "America/Curacao", america_aruba ), ( "America/Asuncion", america_asuncion ), ( "America/Atikokan", america_atikokan ), ( "America/Adak", america_atka ), ( "America/Bahia", america_bahia ), ( "America/Bahia_Banderas", america_bahia_banderas ), ( "America/Barbados", america_barbados ), ( "America/Belem", america_belem ), ( "America/Belize", america_belize ), ( "America/Blanc-Sablon", america_blanc_sablon ), ( "America/Boa_Vista", america_boa_vista ), ( "America/Bogota", america_bogota ), ( "America/Boise", america_boise ), ( "America/Argentina/Buenos_Aires", america_buenos_aires ), ( "America/Cambridge_Bay", america_cambridge_bay ), ( "America/Campo_Grande", america_campo_grande ), ( "America/Cancun", america_cancun ), ( "America/Caracas", america_caracas ) ], [ ( "America/Argentina/Catamarca", america_catamarca ), ( "America/Cayenne", america_cayenne ), ( "America/Panama", america_cayman ), ( "America/Chicago", america_chicago ), ( "America/Chihuahua", america_chihuahua ), ( "America/Atikokan", america_coral_harbour ), ( "America/Argentina/Cordoba", america_cordoba ), ( "America/Costa_Rica", america_costa_rica ), ( "America/Creston", america_creston ), ( "America/Cuiaba", america_cuiaba ), ( "America/Curacao", america_curacao ), ( "America/Danmarkshavn", america_danmarkshavn ), ( "America/Dawson", america_dawson ), ( "America/Dawson_Creek", america_dawson_creek ), ( "America/Denver", america_denver ), ( "America/Detroit", america_detroit ), ( "America/Port_of_Spain", america_dominica ), ( "America/Edmonton", america_edmonton ), ( "America/Eirunepe", america_eirunepe ), ( "America/El_Salvador", america_el_salvador ), ( "America/Tijuana", america_ensenada ), ( "America/Fort_Nelson", america_fort_nelson ), ( "America/Fort_Wayne", america_fort_wayne ), ( "America/Fortaleza", america_fortaleza ), ( "America/Glace_Bay", america_glace_bay ), ( "America/Godthab", america_godthab ), ( "America/Goose_Bay", america_goose_bay ), ( "America/Grand_Turk", america_grand_turk ), ( "America/Port_of_Spain", america_grenada ), ( "America/Port_of_Spain", america_guadeloupe ) ], [ ( "America/Guatemala", america_guatemala ), ( "America/Guayaquil", america_guayaquil ), ( "America/Guyana", america_guyana ), ( "America/Halifax", america_halifax ), ( "America/Havana", america_havana ), ( "America/Hermosillo", america_hermosillo ), ( "America/Fort_Wayne", america_indiana_indianapolis ), ( "America/Indiana/Knox", america_indiana_knox ), ( "America/Indiana/Marengo", america_indiana_marengo ), ( "America/Indiana/Petersburg", america_indiana_petersburg ), ( "America/Indiana/Tell_City", america_indiana_tell_city ), ( "America/Indiana/Vevay", america_indiana_vevay ), ( "America/Indiana/Vincennes", america_indiana_vincennes ), ( "America/Indiana/Winamac", america_indiana_winamac ), ( "America/Fort_Wayne", america_indianapolis ), ( "America/Inuvik", america_inuvik ), ( "America/Iqaluit", america_iqaluit ), ( "America/Jamaica", america_jamaica ), ( "America/Argentina/Jujuy", america_jujuy ), ( "America/Juneau", america_juneau ), ( "America/Kentucky/Louisville", america_kentucky_louisville ), ( "America/Kentucky/Monticello", america_kentucky_monticello ), ( "America/Indiana/Knox", america_knox_in ), ( "America/Curacao", america_kralendijk ), ( "America/La_Paz", america_la_paz ), ( "America/Lima", america_lima ), ( "America/Los_Angeles", america_los_angeles ), ( "America/Kentucky/Louisville", america_louisville ), ( "America/Curacao", america_lower_princes ), ( "America/Maceio", america_maceio ) ], [ ( "America/Managua", america_managua ), ( "America/Manaus", america_manaus ), ( "America/Port_of_Spain", america_marigot ), ( "America/Martinique", america_martinique ), ( "America/Matamoros", america_matamoros ), ( "America/Mazatlan", america_mazatlan ), ( "America/Argentina/Mendoza", america_mendoza ), ( "America/Menominee", america_menominee ), ( "America/Merida", america_merida ), ( "America/Metlakatla", america_metlakatla ), ( "America/Mexico_City", america_mexico_city ), ( "America/Miquelon", america_miquelon ), ( "America/Moncton", america_moncton ), ( "America/Monterrey", america_monterrey ), ( "America/Montevideo", america_montevideo ), ( "America/Toronto", america_montreal ), ( "America/Port_of_Spain", america_montserrat ), ( "America/Nassau", america_nassau ), ( "America/New_York", america_new_york ), ( "America/Nipigon", america_nipigon ), ( "America/Nome", america_nome ), ( "America/Noronha", america_noronha ), ( "America/North_Dakota/Beulah", america_north_dakota_beulah ), ( "America/North_Dakota/Center", america_north_dakota_center ), ( "America/North_Dakota/New_Salem", america_north_dakota_new_salem ), ( "America/Ojinaga", america_ojinaga ), ( "America/Panama", america_panama ), ( "America/Pangnirtung", america_pangnirtung ), ( "America/Paramaribo", america_paramaribo ), ( "America/Phoenix", america_phoenix ) ], [ ( "America/Port-au-Prince", america_port_au_prince ), ( "America/Port_of_Spain", america_port_of_spain ), ( "America/Rio_Branco", america_porto_acre ), ( "America/Porto_Velho", america_porto_velho ), ( "America/Puerto_Rico", america_puerto_rico ), ( "America/Rainy_River", america_rainy_river ), ( "America/Rankin_Inlet", america_rankin_inlet ), ( "America/Recife", america_recife ), ( "America/Regina", america_regina ), ( "America/Resolute", america_resolute ), ( "America/Rio_Branco", america_rio_branco ), ( "America/Argentina/Cordoba", america_rosario ), ( "America/Tijuana", america_santa_isabel ), ( "America/Santarem", america_santarem ), ( "America/Santiago", america_santiago ), ( "America/Santo_Domingo", america_santo_domingo ), ( "America/Sao_Paulo", america_sao_paulo ), ( "America/Scoresbysund", america_scoresbysund ), ( "America/Denver", america_shiprock ), ( "America/Sitka", america_sitka ), ( "America/Port_of_Spain", america_st_barthelemy ), ( "America/St_Johns", america_st_johns ), ( "America/Port_of_Spain", america_st_kitts ), ( "America/Port_of_Spain", america_st_lucia ), ( "America/Port_of_Spain", america_st_thomas ), ( "America/Port_of_Spain", america_st_vincent ), ( "America/Swift_Current", america_swift_current ), ( "America/Tegucigalpa", america_tegucigalpa ), ( "America/Thule", america_thule ), ( "America/Thunder_Bay", america_thunder_bay ) ], [ ( "America/Tijuana", america_tijuana ), ( "America/Toronto", america_toronto ), ( "America/Port_of_Spain", america_tortola ), ( "America/Vancouver", america_vancouver ), ( "America/Port_of_Spain", america_virgin ), ( "America/Whitehorse", america_whitehorse ), ( "America/Winnipeg", america_winnipeg ), ( "America/Yakutat", america_yakutat ), ( "America/Yellowknife", america_yellowknife ), ( "Antarctica/Casey", antarctica_casey ), ( "Antarctica/Davis", antarctica_davis ), ( "Antarctica/DumontDUrville", antarctica_dumontdurville ), ( "Antarctica/Macquarie", antarctica_macquarie ), ( "Antarctica/Mawson", antarctica_mawson ), ( "Pacific/Auckland", antarctica_mcmurdo ), ( "Antarctica/Palmer", antarctica_palmer ), ( "Antarctica/Rothera", antarctica_rothera ), ( "Pacific/Auckland", antarctica_south_pole ), ( "Antarctica/Syowa", antarctica_syowa ), ( "Antarctica/Troll", antarctica_troll ), ( "Antarctica/Vostok", antarctica_vostok ), ( "Europe/Oslo", arctic_longyearbyen ), ( "Asia/Riyadh", asia_aden ), ( "Asia/Almaty", asia_almaty ), ( "Asia/Amman", asia_amman ), ( "Asia/Anadyr", asia_anadyr ), ( "Asia/Aqtau", asia_aqtau ), ( "Asia/Aqtobe", asia_aqtobe ), ( "Asia/Ashgabat", asia_ashgabat ), ( "Asia/Ashgabat", asia_ashkhabad ) ], [ ( "Asia/Atyrau", asia_atyrau ), ( "Asia/Baghdad", asia_baghdad ), ( "Asia/Qatar", asia_bahrain ), ( "Asia/Baku", asia_baku ), ( "Asia/Bangkok", asia_bangkok ), ( "Asia/Barnaul", asia_barnaul ), ( "Asia/Beirut", asia_beirut ), ( "Asia/Bishkek", asia_bishkek ), ( "Asia/Brunei", asia_brunei ), ( "Asia/Kolkata", asia_calcutta ), ( "Asia/Chita", asia_chita ), ( "Asia/Choibalsan", asia_choibalsan ), ( "Asia/Shanghai", asia_chongqing ), ( "Asia/Shanghai", asia_chungking ), ( "Asia/Colombo", asia_colombo ), ( "Asia/Dhaka", asia_dacca ), ( "Asia/Damascus", asia_damascus ), ( "Asia/Dhaka", asia_dhaka ), ( "Asia/Dili", asia_dili ), ( "Asia/Dubai", asia_dubai ), ( "Asia/Dushanbe", asia_dushanbe ), ( "Asia/Famagusta", asia_famagusta ), ( "Asia/Gaza", asia_gaza ), ( "Asia/Shanghai", asia_harbin ), ( "Asia/Hebron", asia_hebron ), ( "Asia/Ho_Chi_Minh", asia_ho_chi_minh ), ( "Asia/Hong_Kong", asia_hong_kong ), ( "Asia/Hovd", asia_hovd ), ( "Asia/Irkutsk", asia_irkutsk ), ( "Europe/Istanbul", asia_istanbul ) ], [ ( "Asia/Jakarta", asia_jakarta ), ( "Asia/Jayapura", asia_jayapura ), ( "Asia/Jerusalem", asia_jerusalem ), ( "Asia/Kabul", asia_kabul ), ( "Asia/Kamchatka", asia_kamchatka ), ( "Asia/Karachi", asia_karachi ), ( "Asia/Urumqi", asia_kashgar ), ( "Asia/Kathmandu", asia_kathmandu ), ( "Asia/Kathmandu", asia_katmandu ), ( "Asia/Khandyga", asia_khandyga ), ( "Asia/Kolkata", asia_kolkata ), ( "Asia/Krasnoyarsk", asia_krasnoyarsk ), ( "Asia/Kuala_Lumpur", asia_kuala_lumpur ), ( "Asia/Kuching", asia_kuching ), ( "Asia/Riyadh", asia_kuwait ), ( "Asia/Macau", asia_macao ), ( "Asia/Macau", asia_macau ), ( "Asia/Magadan", asia_magadan ), ( "Asia/Makassar", asia_makassar ), ( "Asia/Manila", asia_manila ), ( "Asia/Dubai", asia_muscat ), ( "Asia/Nicosia", asia_nicosia ), ( "Asia/Novokuznetsk", asia_novokuznetsk ), ( "Asia/Novosibirsk", asia_novosibirsk ), ( "Asia/Omsk", asia_omsk ), ( "Asia/Oral", asia_oral ), ( "Asia/Bangkok", asia_phnom_penh ), ( "Asia/Pontianak", asia_pontianak ), ( "Asia/Pyongyang", asia_pyongyang ), ( "Asia/Qatar", asia_qatar ) ], [ ( "Asia/Qyzylorda", asia_qyzylorda ), ( "Asia/Rangoon", asia_rangoon ), ( "Asia/Riyadh", asia_riyadh ), ( "Asia/Ho_Chi_Minh", asia_saigon ), ( "Asia/Sakhalin", asia_sakhalin ), ( "Asia/Samarkand", asia_samarkand ), ( "Asia/Seoul", asia_seoul ), ( "Asia/Shanghai", asia_shanghai ), ( "Asia/Singapore", asia_singapore ), ( "Asia/Srednekolymsk", asia_srednekolymsk ), ( "Asia/Taipei", asia_taipei ), ( "Asia/Tashkent", asia_tashkent ), ( "Asia/Tbilisi", asia_tbilisi ), ( "Asia/Tehran", asia_tehran ), ( "Asia/Jerusalem", asia_tel_aviv ), ( "Asia/Thimphu", asia_thimbu ), ( "Asia/Thimphu", asia_thimphu ), ( "Asia/Tokyo", asia_tokyo ), ( "Asia/Tomsk", asia_tomsk ), ( "Asia/Makassar", asia_ujung_pandang ), ( "Asia/Ulaanbaatar", asia_ulaanbaatar ), ( "Asia/Ulaanbaatar", asia_ulan_bator ), ( "Asia/Urumqi", asia_urumqi ), ( "Asia/Ust-Nera", asia_ust_nera ), ( "Asia/Bangkok", asia_vientiane ), ( "Asia/Vladivostok", asia_vladivostok ), ( "Asia/Yakutsk", asia_yakutsk ), ( "Asia/Rangoon", asia_yangon ), ( "Asia/Yekaterinburg", asia_yekaterinburg ), ( "Asia/Yerevan", asia_yerevan ) ], [ ( "Atlantic/Azores", atlantic_azores ), ( "Atlantic/Bermuda", atlantic_bermuda ), ( "Atlantic/Canary", atlantic_canary ), ( "Atlantic/Cape_Verde", atlantic_cape_verde ), ( "Atlantic/Faroe", atlantic_faeroe ), ( "Atlantic/Faroe", atlantic_faroe ), ( "Europe/Oslo", atlantic_jan_mayen ), ( "Atlantic/Madeira", atlantic_madeira ), ( "Atlantic/Reykjavik", atlantic_reykjavik ), ( "Atlantic/South_Georgia", atlantic_south_georgia ), ( "Africa/Abidjan", atlantic_st_helena ), ( "Atlantic/Stanley", atlantic_stanley ), ( "Australia/Sydney", australia_act ), ( "Australia/Adelaide", australia_adelaide ), ( "Australia/Brisbane", australia_brisbane ), ( "Australia/Broken_Hill", australia_broken_hill ), ( "Australia/Sydney", australia_canberra ), ( "Australia/Currie", australia_currie ), ( "Australia/Darwin", australia_darwin ), ( "Australia/Eucla", australia_eucla ), ( "Australia/Hobart", australia_hobart ), ( "Australia/Lord_Howe", australia_lhi ), ( "Australia/Lindeman", australia_lindeman ), ( "Australia/Lord_Howe", australia_lord_howe ), ( "Australia/Melbourne", australia_melbourne ), ( "Australia/Darwin", australia_north ), ( "Australia/Sydney", australia_nsw ), ( "Australia/Perth", australia_perth ), ( "Australia/Brisbane", australia_queensland ), ( "Australia/Adelaide", australia_south ) ], [ ( "Australia/Sydney", australia_sydney ), ( "Australia/Hobart", australia_tasmania ), ( "Australia/Melbourne", australia_victoria ), ( "Australia/Perth", australia_west ), ( "Australia/Broken_Hill", australia_yancowinna ), ( "America/Rio_Branco", brazil_acre ), ( "America/Noronha", brazil_denoronha ), ( "America/Sao_Paulo", brazil_east ), ( "America/Manaus", brazil_west ), ( "America/Halifax", canada_atlantic ), ( "America/Winnipeg", canada_central ), ( "America/Regina", canada_east_saskatchewan ), ( "America/Toronto", canada_eastern ), ( "America/Edmonton", canada_mountain ), ( "America/St_Johns", canada_newfoundland ), ( "America/Vancouver", canada_pacific ), ( "America/Regina", canada_saskatchewan ), ( "America/Whitehorse", canada_yukon ), ( "CET", cet ), ( "America/Santiago", chile_continental ), ( "Pacific/Easter", chile_easterisland ), ( "CST6CDT", cst6cdt ), ( "America/Havana", cuba ), ( "EET", eet ), ( "Africa/Cairo", egypt ), ( "Europe/Dublin", eire ), ( "EST", est ), ( "EST5EDT", est5edt ), ( "Etc/GMT+0", etc_gmt ), ( "Etc/GMT+0", etc_gmt_0 ) ], [ ( "Etc/GMT+0", etc_gmt_minus_0 ), ( "Etc/GMT-1", etc_gmt_minus_1 ), ( "Etc/GMT-10", etc_gmt_minus_10 ), ( "Etc/GMT-11", etc_gmt_minus_11 ), ( "Etc/GMT-12", etc_gmt_minus_12 ), ( "Etc/GMT-13", etc_gmt_minus_13 ), ( "Etc/GMT-14", etc_gmt_minus_14 ), ( "Etc/GMT-2", etc_gmt_minus_2 ), ( "Etc/GMT-3", etc_gmt_minus_3 ), ( "Etc/GMT-4", etc_gmt_minus_4 ), ( "Etc/GMT-5", etc_gmt_minus_5 ), ( "Etc/GMT-6", etc_gmt_minus_6 ), ( "Etc/GMT-7", etc_gmt_minus_7 ), ( "Etc/GMT-8", etc_gmt_minus_8 ), ( "Etc/GMT-9", etc_gmt_minus_9 ), ( "Etc/GMT+0", etc_gmt_plus_0 ), ( "Etc/GMT+1", etc_gmt_plus_1 ), ( "Etc/GMT+10", etc_gmt_plus_10 ), ( "Etc/GMT+11", etc_gmt_plus_11 ), ( "Etc/GMT+12", etc_gmt_plus_12 ), ( "Etc/GMT+2", etc_gmt_plus_2 ), ( "Etc/GMT+3", etc_gmt_plus_3 ), ( "Etc/GMT+4", etc_gmt_plus_4 ), ( "Etc/GMT+5", etc_gmt_plus_5 ), ( "Etc/GMT+6", etc_gmt_plus_6 ), ( "Etc/GMT+7", etc_gmt_plus_7 ), ( "Etc/GMT+8", etc_gmt_plus_8 ), ( "Etc/GMT+9", etc_gmt_plus_9 ), ( "Etc/GMT+0", etc_greenwich ), ( "Etc/UCT", etc_uct ) ], [ ( "Etc/UTC", etc_universal ), ( "Etc/UTC", etc_utc ), ( "Etc/UTC", etc_zulu ), ( "Europe/Amsterdam", europe_amsterdam ), ( "Europe/Andorra", europe_andorra ), ( "Europe/Astrakhan", europe_astrakhan ), ( "Europe/Athens", europe_athens ), ( "Europe/London", europe_belfast ), ( "Europe/Belgrade", europe_belgrade ), ( "Europe/Berlin", europe_berlin ), ( "Europe/Prague", europe_bratislava ), ( "Europe/Brussels", europe_brussels ), ( "Europe/Bucharest", europe_bucharest ), ( "Europe/Budapest", europe_budapest ), ( "Europe/Zurich", europe_busingen ), ( "Europe/Chisinau", europe_chisinau ), ( "Europe/Copenhagen", europe_copenhagen ), ( "Europe/Dublin", europe_dublin ), ( "Europe/Gibraltar", europe_gibraltar ), ( "Europe/London", europe_guernsey ), ( "Europe/Helsinki", europe_helsinki ), ( "Europe/London", europe_isle_of_man ), ( "Europe/Istanbul", europe_istanbul ), ( "Europe/London", europe_jersey ), ( "Europe/Kaliningrad", europe_kaliningrad ), ( "Europe/Kiev", europe_kiev ), ( "Europe/Kirov", europe_kirov ), ( "Europe/Lisbon", europe_lisbon ), ( "Europe/Belgrade", europe_ljubljana ), ( "Europe/London", europe_london ) ], [ ( "Europe/Luxembourg", europe_luxembourg ), ( "Europe/Madrid", europe_madrid ), ( "Europe/Malta", europe_malta ), ( "Europe/Helsinki", europe_mariehamn ), ( "Europe/Minsk", europe_minsk ), ( "Europe/Monaco", europe_monaco ), ( "Europe/Moscow", europe_moscow ), ( "Asia/Nicosia", europe_nicosia ), ( "Europe/Oslo", europe_oslo ), ( "Europe/Paris", europe_paris ), ( "Europe/Belgrade", europe_podgorica ), ( "Europe/Prague", europe_prague ), ( "Europe/Riga", europe_riga ), ( "Europe/Rome", europe_rome ), ( "Europe/Samara", europe_samara ), ( "Europe/Rome", europe_san_marino ), ( "Europe/Belgrade", europe_sarajevo ), ( "Europe/Saratov", europe_saratov ), ( "Europe/Simferopol", europe_simferopol ), ( "Europe/Belgrade", europe_skopje ), ( "Europe/Sofia", europe_sofia ), ( "Europe/Stockholm", europe_stockholm ), ( "Europe/Tallinn", europe_tallinn ), ( "Europe/Tirane", europe_tirane ), ( "Europe/Chisinau", europe_tiraspol ), ( "Europe/Ulyanovsk", europe_ulyanovsk ), ( "Europe/Uzhgorod", europe_uzhgorod ), ( "Europe/Zurich", europe_vaduz ), ( "Europe/Rome", europe_vatican ), ( "Europe/Vienna", europe_vienna ) ], [ ( "Europe/Vilnius", europe_vilnius ), ( "Europe/Volgograd", europe_volgograd ), ( "Europe/Warsaw", europe_warsaw ), ( "Europe/Belgrade", europe_zagreb ), ( "Europe/Zaporozhye", europe_zaporozhye ), ( "Europe/Zurich", europe_zurich ), ( "Europe/London", gb ), ( "Europe/London", gb_eire ), ( "Etc/GMT+0", gmt ), ( "Etc/GMT+0", gmt_0 ), ( "Etc/GMT+0", gmt_minus_0 ), ( "Etc/GMT+0", gmt_plus_0 ), ( "Etc/GMT+0", greenwich ), ( "Asia/Hong_Kong", hongkong ), ( "HST", hst ), ( "Atlantic/Reykjavik", iceland ), ( "Africa/Nairobi", indian_antananarivo ), ( "Indian/Chagos", indian_chagos ), ( "Indian/Christmas", indian_christmas ), ( "Indian/Cocos", indian_cocos ), ( "Africa/Nairobi", indian_comoro ), ( "Indian/Kerguelen", indian_kerguelen ), ( "Indian/Mahe", indian_mahe ), ( "Indian/Maldives", indian_maldives ), ( "Indian/Mauritius", indian_mauritius ), ( "Africa/Nairobi", indian_mayotte ), ( "Indian/Reunion", indian_reunion ), ( "Asia/Tehran", iran ), ( "Asia/Jerusalem", israel ), ( "America/Jamaica", jamaica ) ], [ ( "Asia/Tokyo", japan ), ( "Pacific/Kwajalein", kwajalein ), ( "Africa/Tripoli", libya ), ( "MET", met ), ( "America/Tijuana", mexico_bajanorte ), ( "America/Mazatlan", mexico_bajasur ), ( "America/Mexico_City", mexico_general ), ( "MST", mst ), ( "MST7MDT", mst7mdt ), ( "America/Denver", navajo ), ( "Pacific/Auckland", nz ), ( "Pacific/Chatham", nz_chat ), ( "Pacific/Apia", pacific_apia ), ( "Pacific/Auckland", pacific_auckland ), ( "Pacific/Bougainville", pacific_bougainville ), ( "Pacific/Chatham", pacific_chatham ), ( "Pacific/Chuuk", pacific_chuuk ), ( "Pacific/Easter", pacific_easter ), ( "Pacific/Efate", pacific_efate ), ( "Pacific/Enderbury", pacific_enderbury ), ( "Pacific/Fakaofo", pacific_fakaofo ), ( "Pacific/Fiji", pacific_fiji ), ( "Pacific/Funafuti", pacific_funafuti ), ( "Pacific/Galapagos", pacific_galapagos ), ( "Pacific/Gambier", pacific_gambier ), ( "Pacific/Guadalcanal", pacific_guadalcanal ), ( "Pacific/Guam", pacific_guam ), ( "Pacific/Honolulu", pacific_honolulu ), ( "Pacific/Honolulu", pacific_johnston ), ( "Pacific/Kiritimati", pacific_kiritimati ) ], [ ( "Pacific/Kosrae", pacific_kosrae ), ( "Pacific/Kwajalein", pacific_kwajalein ), ( "Pacific/Majuro", pacific_majuro ), ( "Pacific/Marquesas", pacific_marquesas ), ( "Pacific/Pago_Pago", pacific_midway ), ( "Pacific/Nauru", pacific_nauru ), ( "Pacific/Niue", pacific_niue ), ( "Pacific/Norfolk", pacific_norfolk ), ( "Pacific/Noumea", pacific_noumea ), ( "Pacific/Pago_Pago", pacific_pago_pago ), ( "Pacific/Palau", pacific_palau ), ( "Pacific/Pitcairn", pacific_pitcairn ), ( "Pacific/Pohnpei", pacific_pohnpei ), ( "Pacific/Pohnpei", pacific_ponape ), ( "Pacific/Port_Moresby", pacific_port_moresby ), ( "Pacific/Rarotonga", pacific_rarotonga ), ( "Pacific/Guam", pacific_saipan ), ( "Pacific/Pago_Pago", pacific_samoa ), ( "Pacific/Tahiti", pacific_tahiti ), ( "Pacific/Tarawa", pacific_tarawa ), ( "Pacific/Tongatapu", pacific_tongatapu ), ( "Pacific/Chuuk", pacific_truk ), ( "Pacific/Wake", pacific_wake ), ( "Pacific/Wallis", pacific_wallis ), ( "Pacific/Chuuk", pacific_yap ), ( "Europe/Warsaw", poland ), ( "Europe/Lisbon", portugal ), ( "Asia/Shanghai", prc ), ( "PST8PDT", pst8pdt ), ( "Asia/Taipei", roc ) ], [ ( "Asia/Seoul", rok ), ( "Asia/Singapore", singapore ), ( "Europe/Istanbul", turkey ), ( "Etc/UCT", uct ), ( "Etc/UTC", universal ), ( "America/Anchorage", us_alaska ), ( "America/Adak", us_aleutian ), ( "America/Phoenix", us_arizona ), ( "America/Chicago", us_central ), ( "America/Fort_Wayne", us_east_indiana ), ( "America/New_York", us_eastern ), ( "Pacific/Honolulu", us_hawaii ), ( "America/Indiana/Knox", us_indiana_starke ), ( "America/Detroit", us_michigan ), ( "America/Denver", us_mountain ), ( "America/Los_Angeles", us_pacific ), ( "America/Los_Angeles", us_pacific_new ), ( "Pacific/Pago_Pago", us_samoa ), ( "Etc/UTC", utc ), ( "Europe/Moscow", w_su ), ( "WET", wet ), ( "Etc/UTC", zulu ) ] ]


{-| Look up a TimeZone by name. -}
fromName : String -> Maybe TimeZone
fromName name =
  case Dict.get name all of
    Nothing ->
      Nothing

    Just f ->
      Just (f ())