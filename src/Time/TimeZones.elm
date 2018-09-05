module Time.TimeZones exposing (all, fromName, africa_abidjan, africa_accra, africa_addis_ababa, africa_algiers, africa_asmara, africa_asmera, africa_bamako, africa_bangui, africa_banjul, africa_bissau, africa_blantyre, africa_brazzaville, africa_bujumbura, africa_cairo, africa_casablanca, africa_ceuta, africa_conakry, africa_dakar, africa_dar_es_salaam, africa_djibouti, africa_douala, africa_el_aaiun, africa_freetown, africa_gaborone, africa_harare, africa_johannesburg, africa_juba, africa_kampala, africa_khartoum, africa_kigali, africa_kinshasa, africa_lagos, africa_libreville, africa_lome, africa_luanda, africa_lubumbashi, africa_lusaka, africa_malabo, africa_maputo, africa_maseru, africa_mbabane, africa_mogadishu, africa_monrovia, africa_nairobi, africa_ndjamena, africa_niamey, africa_nouakchott, africa_ouagadougou, africa_porto_novo, africa_sao_tome, africa_timbuktu, africa_tripoli, africa_tunis, africa_windhoek, america_adak, america_anchorage, america_anguilla, america_antigua, america_araguaina, america_argentina_buenos_aires, america_argentina_catamarca, america_argentina_comodrivadavia, america_argentina_cordoba, america_argentina_jujuy, america_argentina_la_rioja, america_argentina_mendoza, america_argentina_rio_gallegos, america_argentina_salta, america_argentina_san_juan, america_argentina_san_luis, america_argentina_tucuman, america_argentina_ushuaia, america_aruba, america_asuncion, america_atikokan, america_atka, america_bahia, america_bahia_banderas, america_barbados, america_belem, america_belize, america_blanc_sablon, america_boa_vista, america_bogota, america_boise, america_buenos_aires, america_cambridge_bay, america_campo_grande, america_cancun, america_caracas, america_catamarca, america_cayenne, america_cayman, america_chicago, america_chihuahua, america_coral_harbour, america_cordoba, america_costa_rica, america_creston, america_cuiaba, america_curacao, america_danmarkshavn, america_dawson, america_dawson_creek, america_denver, america_detroit, america_dominica, america_edmonton, america_eirunepe, america_el_salvador, america_ensenada, america_fort_nelson, america_fort_wayne, america_fortaleza, america_glace_bay, america_godthab, america_goose_bay, america_grand_turk, america_grenada, america_guadeloupe, america_guatemala, america_guayaquil, america_guyana, america_halifax, america_havana, america_hermosillo, america_indiana_indianapolis, america_indiana_knox, america_indiana_marengo, america_indiana_petersburg, america_indiana_tell_city, america_indiana_vevay, america_indiana_vincennes, america_indiana_winamac, america_indianapolis, america_inuvik, america_iqaluit, america_jamaica, america_jujuy, america_juneau, america_kentucky_louisville, america_kentucky_monticello, america_knox_in, america_kralendijk, america_la_paz, america_lima, america_los_angeles, america_louisville, america_lower_princes, america_maceio, america_managua, america_manaus, america_marigot, america_martinique, america_matamoros, america_mazatlan, america_mendoza, america_menominee, america_merida, america_metlakatla, america_mexico_city, america_miquelon, america_moncton, america_monterrey, america_montevideo, america_montreal, america_montserrat, america_nassau, america_new_york, america_nipigon, america_nome, america_noronha, america_north_dakota_beulah, america_north_dakota_center, america_north_dakota_new_salem, america_ojinaga, america_panama, america_pangnirtung, america_paramaribo, america_phoenix, america_port_au_prince, america_port_of_spain, america_porto_acre, america_porto_velho, america_puerto_rico, america_rainy_river, america_rankin_inlet, america_recife, america_regina, america_resolute, america_rio_branco, america_rosario, america_santa_isabel, america_santarem, america_santiago, america_santo_domingo, america_sao_paulo, america_scoresbysund, america_shiprock, america_sitka, america_st_barthelemy, america_st_johns, america_st_kitts, america_st_lucia, america_st_thomas, america_st_vincent, america_swift_current, america_tegucigalpa, america_thule, america_thunder_bay, america_tijuana, america_toronto, america_tortola, america_vancouver, america_virgin, america_whitehorse, america_winnipeg, america_yakutat, america_yellowknife, antarctica_casey, antarctica_davis, antarctica_dumontdurville, antarctica_macquarie, antarctica_mawson, antarctica_mcmurdo, antarctica_palmer, antarctica_rothera, antarctica_south_pole, antarctica_syowa, antarctica_troll, antarctica_vostok, arctic_longyearbyen, asia_aden, asia_almaty, asia_amman, asia_anadyr, asia_aqtau, asia_aqtobe, asia_ashgabat, asia_ashkhabad, asia_baghdad, asia_bahrain, asia_baku, asia_bangkok, asia_barnaul, asia_beirut, asia_bishkek, asia_brunei, asia_calcutta, asia_chita, asia_choibalsan, asia_chongqing, asia_chungking, asia_colombo, asia_dacca, asia_damascus, asia_dhaka, asia_dili, asia_dubai, asia_dushanbe, asia_famagusta, asia_gaza, asia_harbin, asia_hebron, asia_ho_chi_minh, asia_hong_kong, asia_hovd, asia_irkutsk, asia_istanbul, asia_jakarta, asia_jayapura, asia_jerusalem, asia_kabul, asia_kamchatka, asia_karachi, asia_kashgar, asia_kathmandu, asia_katmandu, asia_khandyga, asia_kolkata, asia_krasnoyarsk, asia_kuala_lumpur, asia_kuching, asia_kuwait, asia_macao, asia_macau, asia_magadan, asia_makassar, asia_manila, asia_muscat, asia_nicosia, asia_novokuznetsk, asia_novosibirsk, asia_omsk, asia_oral, asia_phnom_penh, asia_pontianak, asia_pyongyang, asia_qatar, asia_qyzylorda, asia_rangoon, asia_riyadh, asia_saigon, asia_sakhalin, asia_samarkand, asia_seoul, asia_shanghai, asia_singapore, asia_srednekolymsk, asia_taipei, asia_tashkent, asia_tbilisi, asia_tehran, asia_tel_aviv, asia_thimbu, asia_thimphu, asia_tokyo, asia_tomsk, asia_ujung_pandang, asia_ulaanbaatar, asia_ulan_bator, asia_urumqi, asia_ust_nera, asia_vientiane, asia_vladivostok, asia_yakutsk, asia_yangon, asia_yekaterinburg, asia_yerevan, atlantic_azores, atlantic_bermuda, atlantic_canary, atlantic_cape_verde, atlantic_faeroe, atlantic_faroe, atlantic_jan_mayen, atlantic_madeira, atlantic_reykjavik, atlantic_south_georgia, atlantic_st_helena, atlantic_stanley, australia_act, australia_adelaide, australia_brisbane, australia_broken_hill, australia_canberra, australia_currie, australia_darwin, australia_eucla, australia_hobart, australia_lhi, australia_lindeman, australia_lord_howe, australia_melbourne, australia_north, australia_nsw, australia_perth, australia_queensland, australia_south, australia_sydney, australia_tasmania, australia_victoria, australia_west, australia_yancowinna, brazil_acre, brazil_denoronha, brazil_east, brazil_west, canada_atlantic, canada_central, canada_east_saskatchewan, canada_eastern, canada_mountain, canada_newfoundland, canada_pacific, canada_saskatchewan, canada_yukon, cet, chile_continental, chile_easterisland, cst6cdt, cuba, eet, egypt, eire, est, est5edt, etc_gmt, etc_gmt_0, etc_gmt_minus_0, etc_gmt_minus_1, etc_gmt_minus_10, etc_gmt_minus_11, etc_gmt_minus_12, etc_gmt_minus_13, etc_gmt_minus_14, etc_gmt_minus_2, etc_gmt_minus_3, etc_gmt_minus_4, etc_gmt_minus_5, etc_gmt_minus_6, etc_gmt_minus_7, etc_gmt_minus_8, etc_gmt_minus_9, etc_gmt_plus_0, etc_gmt_plus_1, etc_gmt_plus_10, etc_gmt_plus_11, etc_gmt_plus_12, etc_gmt_plus_2, etc_gmt_plus_3, etc_gmt_plus_4, etc_gmt_plus_5, etc_gmt_plus_6, etc_gmt_plus_7, etc_gmt_plus_8, etc_gmt_plus_9, etc_greenwich, etc_uct, etc_universal, etc_utc, etc_zulu, europe_amsterdam, europe_andorra, europe_astrakhan, europe_athens, europe_belfast, europe_belgrade, europe_berlin, europe_bratislava, europe_brussels, europe_bucharest, europe_budapest, europe_busingen, europe_chisinau, europe_copenhagen, europe_dublin, europe_gibraltar, europe_guernsey, europe_helsinki, europe_isle_of_man, europe_istanbul, europe_jersey, europe_kaliningrad, europe_kiev, europe_kirov, europe_lisbon, europe_ljubljana, europe_london, europe_luxembourg, europe_madrid, europe_malta, europe_mariehamn, europe_minsk, europe_monaco, europe_moscow, europe_nicosia, europe_oslo, europe_paris, europe_podgorica, europe_prague, europe_riga, europe_rome, europe_samara, europe_san_marino, europe_sarajevo, europe_simferopol, europe_skopje, europe_sofia, europe_stockholm, europe_tallinn, europe_tirane, europe_tiraspol, europe_ulyanovsk, europe_uzhgorod, europe_vaduz, europe_vatican, europe_vienna, europe_vilnius, europe_volgograd, europe_warsaw, europe_zagreb, europe_zaporozhye, europe_zurich, gb, gb_eire, gmt, gmt_0, gmt_minus_0, gmt_plus_0, greenwich, hongkong, hst, iceland, indian_antananarivo, indian_chagos, indian_christmas, indian_cocos, indian_comoro, indian_kerguelen, indian_mahe, indian_maldives, indian_mauritius, indian_mayotte, indian_reunion, iran, israel, jamaica, japan, kwajalein, libya, met, mexico_bajanorte, mexico_bajasur, mexico_general, mst, mst7mdt, navajo, nz, nz_chat, pacific_apia, pacific_auckland, pacific_bougainville, pacific_chatham, pacific_chuuk, pacific_easter, pacific_efate, pacific_enderbury, pacific_fakaofo, pacific_fiji, pacific_funafuti, pacific_galapagos, pacific_gambier, pacific_guadalcanal, pacific_guam, pacific_honolulu, pacific_johnston, pacific_kiritimati, pacific_kosrae, pacific_kwajalein, pacific_majuro, pacific_marquesas, pacific_midway, pacific_nauru, pacific_niue, pacific_norfolk, pacific_noumea, pacific_pago_pago, pacific_palau, pacific_pitcairn, pacific_pohnpei, pacific_ponape, pacific_port_moresby, pacific_rarotonga, pacific_saipan, pacific_samoa, pacific_tahiti, pacific_tarawa, pacific_tongatapu, pacific_truk, pacific_wake, pacific_wallis, pacific_yap, poland, portugal, prc, pst8pdt, roc, rok, singapore, turkey, uct, universal, us_alaska, us_aleutian, us_arizona, us_central, us_east_indiana, us_eastern, us_hawaii, us_indiana_starke, us_michigan, us_mountain, us_pacific, us_pacific_new, us_samoa, utc, w_su, wet, zulu)

{-| This module contains TimeZone definitions for all Timezones as they
are defined in the IANA zoneinfo database.

Make sure to use --optimize for production, to strip out unused zone
data from your production app.

    import Time.DateTime exposing (epoch, toTimestamp)
    import Time.TimeZone exposing (abbreviation)
    import Time.TimeZones exposing (europe_bucharest)

    abbreviation (toTimestamp epoch) europe_bucharest

@docs all, fromName, africa_abidjan, africa_accra, africa_addis_ababa, africa_algiers, africa_asmara, africa_asmera, africa_bamako, africa_bangui, africa_banjul, africa_bissau, africa_blantyre, africa_brazzaville, africa_bujumbura, africa_cairo, africa_casablanca, africa_ceuta, africa_conakry, africa_dakar, africa_dar_es_salaam, africa_djibouti, africa_douala, africa_el_aaiun, africa_freetown, africa_gaborone, africa_harare, africa_johannesburg, africa_juba, africa_kampala, africa_khartoum, africa_kigali, africa_kinshasa, africa_lagos, africa_libreville, africa_lome, africa_luanda, africa_lubumbashi, africa_lusaka, africa_malabo, africa_maputo, africa_maseru, africa_mbabane, africa_mogadishu, africa_monrovia, africa_nairobi, africa_ndjamena, africa_niamey, africa_nouakchott, africa_ouagadougou, africa_porto_novo, africa_sao_tome, africa_timbuktu, africa_tripoli, africa_tunis, africa_windhoek, america_adak, america_anchorage, america_anguilla, america_antigua, america_araguaina, america_argentina_buenos_aires, america_argentina_catamarca, america_argentina_comodrivadavia, america_argentina_cordoba, america_argentina_jujuy, america_argentina_la_rioja, america_argentina_mendoza, america_argentina_rio_gallegos, america_argentina_salta, america_argentina_san_juan, america_argentina_san_luis, america_argentina_tucuman, america_argentina_ushuaia, america_aruba, america_asuncion, america_atikokan, america_atka, america_bahia, america_bahia_banderas, america_barbados, america_belem, america_belize, america_blanc_sablon, america_boa_vista, america_bogota, america_boise, america_buenos_aires, america_cambridge_bay, america_campo_grande, america_cancun, america_caracas, america_catamarca, america_cayenne, america_cayman, america_chicago, america_chihuahua, america_coral_harbour, america_cordoba, america_costa_rica, america_creston, america_cuiaba, america_curacao, america_danmarkshavn, america_dawson, america_dawson_creek, america_denver, america_detroit, america_dominica, america_edmonton, america_eirunepe, america_el_salvador, america_ensenada, america_fort_nelson, america_fort_wayne, america_fortaleza, america_glace_bay, america_godthab, america_goose_bay, america_grand_turk, america_grenada, america_guadeloupe, america_guatemala, america_guayaquil, america_guyana, america_halifax, america_havana, america_hermosillo, america_indiana_indianapolis, america_indiana_knox, america_indiana_marengo, america_indiana_petersburg, america_indiana_tell_city, america_indiana_vevay, america_indiana_vincennes, america_indiana_winamac, america_indianapolis, america_inuvik, america_iqaluit, america_jamaica, america_jujuy, america_juneau, america_kentucky_louisville, america_kentucky_monticello, america_knox_in, america_kralendijk, america_la_paz, america_lima, america_los_angeles, america_louisville, america_lower_princes, america_maceio, america_managua, america_manaus, america_marigot, america_martinique, america_matamoros, america_mazatlan, america_mendoza, america_menominee, america_merida, america_metlakatla, america_mexico_city, america_miquelon, america_moncton, america_monterrey, america_montevideo, america_montreal, america_montserrat, america_nassau, america_new_york, america_nipigon, america_nome, america_noronha, america_north_dakota_beulah, america_north_dakota_center, america_north_dakota_new_salem, america_ojinaga, america_panama, america_pangnirtung, america_paramaribo, america_phoenix, america_port_au_prince, america_port_of_spain, america_porto_acre, america_porto_velho, america_puerto_rico, america_rainy_river, america_rankin_inlet, america_recife, america_regina, america_resolute, america_rio_branco, america_rosario, america_santa_isabel, america_santarem, america_santiago, america_santo_domingo, america_sao_paulo, america_scoresbysund, america_shiprock, america_sitka, america_st_barthelemy, america_st_johns, america_st_kitts, america_st_lucia, america_st_thomas, america_st_vincent, america_swift_current, america_tegucigalpa, america_thule, america_thunder_bay, america_tijuana, america_toronto, america_tortola, america_vancouver, america_virgin, america_whitehorse, america_winnipeg, america_yakutat, america_yellowknife, antarctica_casey, antarctica_davis, antarctica_dumontdurville, antarctica_macquarie, antarctica_mawson, antarctica_mcmurdo, antarctica_palmer, antarctica_rothera, antarctica_south_pole, antarctica_syowa, antarctica_troll, antarctica_vostok, arctic_longyearbyen, asia_aden, asia_almaty, asia_amman, asia_anadyr, asia_aqtau, asia_aqtobe, asia_ashgabat, asia_ashkhabad, asia_baghdad, asia_bahrain, asia_baku, asia_bangkok, asia_barnaul, asia_beirut, asia_bishkek, asia_brunei, asia_calcutta, asia_chita, asia_choibalsan, asia_chongqing, asia_chungking, asia_colombo, asia_dacca, asia_damascus, asia_dhaka, asia_dili, asia_dubai, asia_dushanbe, asia_famagusta, asia_gaza, asia_harbin, asia_hebron, asia_ho_chi_minh, asia_hong_kong, asia_hovd, asia_irkutsk, asia_istanbul, asia_jakarta, asia_jayapura, asia_jerusalem, asia_kabul, asia_kamchatka, asia_karachi, asia_kashgar, asia_kathmandu, asia_katmandu, asia_khandyga, asia_kolkata, asia_krasnoyarsk, asia_kuala_lumpur, asia_kuching, asia_kuwait, asia_macao, asia_macau, asia_magadan, asia_makassar, asia_manila, asia_muscat, asia_nicosia, asia_novokuznetsk, asia_novosibirsk, asia_omsk, asia_oral, asia_phnom_penh, asia_pontianak, asia_pyongyang, asia_qatar, asia_qyzylorda, asia_rangoon, asia_riyadh, asia_saigon, asia_sakhalin, asia_samarkand, asia_seoul, asia_shanghai, asia_singapore, asia_srednekolymsk, asia_taipei, asia_tashkent, asia_tbilisi, asia_tehran, asia_tel_aviv, asia_thimbu, asia_thimphu, asia_tokyo, asia_tomsk, asia_ujung_pandang, asia_ulaanbaatar, asia_ulan_bator, asia_urumqi, asia_ust_nera, asia_vientiane, asia_vladivostok, asia_yakutsk, asia_yangon, asia_yekaterinburg, asia_yerevan, atlantic_azores, atlantic_bermuda, atlantic_canary, atlantic_cape_verde, atlantic_faeroe, atlantic_faroe, atlantic_jan_mayen, atlantic_madeira, atlantic_reykjavik, atlantic_south_georgia, atlantic_st_helena, atlantic_stanley, australia_act, australia_adelaide, australia_brisbane, australia_broken_hill, australia_canberra, australia_currie, australia_darwin, australia_eucla, australia_hobart, australia_lhi, australia_lindeman, australia_lord_howe, australia_melbourne, australia_north, australia_nsw, australia_perth, australia_queensland, australia_south, australia_sydney, australia_tasmania, australia_victoria, australia_west, australia_yancowinna, brazil_acre, brazil_denoronha, brazil_east, brazil_west, canada_atlantic, canada_central, canada_east_saskatchewan, canada_eastern, canada_mountain, canada_newfoundland, canada_pacific, canada_saskatchewan, canada_yukon, cet, chile_continental, chile_easterisland, cst6cdt, cuba, eet, egypt, eire, est, est5edt, etc_gmt, etc_gmt_0, etc_gmt_minus_0, etc_gmt_minus_1, etc_gmt_minus_10, etc_gmt_minus_11, etc_gmt_minus_12, etc_gmt_minus_13, etc_gmt_minus_14, etc_gmt_minus_2, etc_gmt_minus_3, etc_gmt_minus_4, etc_gmt_minus_5, etc_gmt_minus_6, etc_gmt_minus_7, etc_gmt_minus_8, etc_gmt_minus_9, etc_gmt_plus_0, etc_gmt_plus_1, etc_gmt_plus_10, etc_gmt_plus_11, etc_gmt_plus_12, etc_gmt_plus_2, etc_gmt_plus_3, etc_gmt_plus_4, etc_gmt_plus_5, etc_gmt_plus_6, etc_gmt_plus_7, etc_gmt_plus_8, etc_gmt_plus_9, etc_greenwich, etc_uct, etc_universal, etc_utc, etc_zulu, europe_amsterdam, europe_andorra, europe_astrakhan, europe_athens, europe_belfast, europe_belgrade, europe_berlin, europe_bratislava, europe_brussels, europe_bucharest, europe_budapest, europe_busingen, europe_chisinau, europe_copenhagen, europe_dublin, europe_gibraltar, europe_guernsey, europe_helsinki, europe_isle_of_man, europe_istanbul, europe_jersey, europe_kaliningrad, europe_kiev, europe_kirov, europe_lisbon, europe_ljubljana, europe_london, europe_luxembourg, europe_madrid, europe_malta, europe_mariehamn, europe_minsk, europe_monaco, europe_moscow, europe_nicosia, europe_oslo, europe_paris, europe_podgorica, europe_prague, europe_riga, europe_rome, europe_samara, europe_san_marino, europe_sarajevo, europe_simferopol, europe_skopje, europe_sofia, europe_stockholm, europe_tallinn, europe_tirane, europe_tiraspol, europe_ulyanovsk, europe_uzhgorod, europe_vaduz, europe_vatican, europe_vienna, europe_vilnius, europe_volgograd, europe_warsaw, europe_zagreb, europe_zaporozhye, europe_zurich, gb, gb_eire, gmt, gmt_0, gmt_minus_0, gmt_plus_0, greenwich, hongkong, hst, iceland, indian_antananarivo, indian_chagos, indian_christmas, indian_cocos, indian_comoro, indian_kerguelen, indian_mahe, indian_maldives, indian_mauritius, indian_mayotte, indian_reunion, iran, israel, jamaica, japan, kwajalein, libya, met, mexico_bajanorte, mexico_bajasur, mexico_general, mst, mst7mdt, navajo, nz, nz_chat, pacific_apia, pacific_auckland, pacific_bougainville, pacific_chatham, pacific_chuuk, pacific_easter, pacific_efate, pacific_enderbury, pacific_fakaofo, pacific_fiji, pacific_funafuti, pacific_galapagos, pacific_gambier, pacific_guadalcanal, pacific_guam, pacific_honolulu, pacific_johnston, pacific_kiritimati, pacific_kosrae, pacific_kwajalein, pacific_majuro, pacific_marquesas, pacific_midway, pacific_nauru, pacific_niue, pacific_norfolk, pacific_noumea, pacific_pago_pago, pacific_palau, pacific_pitcairn, pacific_pohnpei, pacific_ponape, pacific_port_moresby, pacific_rarotonga, pacific_saipan, pacific_samoa, pacific_tahiti, pacific_tarawa, pacific_tongatapu, pacific_truk, pacific_wake, pacific_wallis, pacific_yap, poland, portugal, prc, pst8pdt, roc, rok, singapore, turkey, uct, universal, us_alaska, us_aleutian, us_arizona, us_central, us_east_indiana, us_eastern, us_hawaii, us_indiana_starke, us_michigan, us_mountain, us_pacific, us_pacific_new, us_samoa, utc, w_su, wet, zulu

-}

import Dict exposing (Dict)
import Time.TimeZone exposing (TimeZone)
import Time.TimeZoneData exposing (..)



-- TimeZones
-- ---------


{-| Africa/Abidjan
-}
africa_abidjan : TimeZone
africa_abidjan =
    africa_abidjan_l


{-| Africa/Accra
-}
africa_accra : TimeZone
africa_accra =
    africa_accra_l


{-| Africa/Addis\_Ababa
-}
africa_addis_ababa : TimeZone
africa_addis_ababa =
    link "Africa/Addis_Ababa" africa_nairobi_l


{-| Africa/Algiers
-}
africa_algiers : TimeZone
africa_algiers =
    africa_algiers_l


{-| Africa/Asmara
-}
africa_asmara : TimeZone
africa_asmara =
    link "Africa/Asmara" africa_nairobi_l


{-| Africa/Asmera
-}
africa_asmera : TimeZone
africa_asmera =
    link "Africa/Asmera" africa_nairobi_l


{-| Africa/Bamako
-}
africa_bamako : TimeZone
africa_bamako =
    link "Africa/Bamako" africa_abidjan_l


{-| Africa/Bangui
-}
africa_bangui : TimeZone
africa_bangui =
    link "Africa/Bangui" africa_lagos_l


{-| Africa/Banjul
-}
africa_banjul : TimeZone
africa_banjul =
    link "Africa/Banjul" africa_abidjan_l


{-| Africa/Bissau
-}
africa_bissau : TimeZone
africa_bissau =
    africa_bissau_l


{-| Africa/Blantyre
-}
africa_blantyre : TimeZone
africa_blantyre =
    link "Africa/Blantyre" africa_maputo_l


{-| Africa/Brazzaville
-}
africa_brazzaville : TimeZone
africa_brazzaville =
    link "Africa/Brazzaville" africa_lagos_l


{-| Africa/Bujumbura
-}
africa_bujumbura : TimeZone
africa_bujumbura =
    link "Africa/Bujumbura" africa_maputo_l


{-| Africa/Cairo
-}
africa_cairo : TimeZone
africa_cairo =
    africa_cairo_l


{-| Africa/Casablanca
-}
africa_casablanca : TimeZone
africa_casablanca =
    africa_casablanca_l


{-| Africa/Ceuta
-}
africa_ceuta : TimeZone
africa_ceuta =
    africa_ceuta_l


{-| Africa/Conakry
-}
africa_conakry : TimeZone
africa_conakry =
    link "Africa/Conakry" africa_abidjan_l


{-| Africa/Dakar
-}
africa_dakar : TimeZone
africa_dakar =
    link "Africa/Dakar" africa_abidjan_l


{-| Africa/Dar\_es\_Salaam
-}
africa_dar_es_salaam : TimeZone
africa_dar_es_salaam =
    link "Africa/Dar_es_Salaam" africa_nairobi_l


{-| Africa/Djibouti
-}
africa_djibouti : TimeZone
africa_djibouti =
    link "Africa/Djibouti" africa_nairobi_l


{-| Africa/Douala
-}
africa_douala : TimeZone
africa_douala =
    link "Africa/Douala" africa_lagos_l


{-| Africa/El\_Aaiun
-}
africa_el_aaiun : TimeZone
africa_el_aaiun =
    africa_el_aaiun_l


{-| Africa/Freetown
-}
africa_freetown : TimeZone
africa_freetown =
    link "Africa/Freetown" africa_abidjan_l


{-| Africa/Gaborone
-}
africa_gaborone : TimeZone
africa_gaborone =
    link "Africa/Gaborone" africa_maputo_l


{-| Africa/Harare
-}
africa_harare : TimeZone
africa_harare =
    link "Africa/Harare" africa_maputo_l


{-| Africa/Johannesburg
-}
africa_johannesburg : TimeZone
africa_johannesburg =
    africa_johannesburg_l


{-| Africa/Juba
-}
africa_juba : TimeZone
africa_juba =
    link "Africa/Juba" africa_khartoum_l


{-| Africa/Kampala
-}
africa_kampala : TimeZone
africa_kampala =
    link "Africa/Kampala" africa_nairobi_l


{-| Africa/Khartoum
-}
africa_khartoum : TimeZone
africa_khartoum =
    africa_khartoum_l


{-| Africa/Kigali
-}
africa_kigali : TimeZone
africa_kigali =
    link "Africa/Kigali" africa_maputo_l


{-| Africa/Kinshasa
-}
africa_kinshasa : TimeZone
africa_kinshasa =
    link "Africa/Kinshasa" africa_lagos_l


{-| Africa/Lagos
-}
africa_lagos : TimeZone
africa_lagos =
    africa_lagos_l


{-| Africa/Libreville
-}
africa_libreville : TimeZone
africa_libreville =
    link "Africa/Libreville" africa_lagos_l


{-| Africa/Lome
-}
africa_lome : TimeZone
africa_lome =
    link "Africa/Lome" africa_abidjan_l


{-| Africa/Luanda
-}
africa_luanda : TimeZone
africa_luanda =
    link "Africa/Luanda" africa_lagos_l


{-| Africa/Lubumbashi
-}
africa_lubumbashi : TimeZone
africa_lubumbashi =
    link "Africa/Lubumbashi" africa_maputo_l


{-| Africa/Lusaka
-}
africa_lusaka : TimeZone
africa_lusaka =
    link "Africa/Lusaka" africa_maputo_l


{-| Africa/Malabo
-}
africa_malabo : TimeZone
africa_malabo =
    link "Africa/Malabo" africa_lagos_l


{-| Africa/Maputo
-}
africa_maputo : TimeZone
africa_maputo =
    africa_maputo_l


{-| Africa/Maseru
-}
africa_maseru : TimeZone
africa_maseru =
    link "Africa/Maseru" africa_johannesburg_l


{-| Africa/Mbabane
-}
africa_mbabane : TimeZone
africa_mbabane =
    link "Africa/Mbabane" africa_johannesburg_l


{-| Africa/Mogadishu
-}
africa_mogadishu : TimeZone
africa_mogadishu =
    link "Africa/Mogadishu" africa_nairobi_l


{-| Africa/Monrovia
-}
africa_monrovia : TimeZone
africa_monrovia =
    africa_monrovia_l


{-| Africa/Nairobi
-}
africa_nairobi : TimeZone
africa_nairobi =
    africa_nairobi_l


{-| Africa/Ndjamena
-}
africa_ndjamena : TimeZone
africa_ndjamena =
    africa_ndjamena_l


{-| Africa/Niamey
-}
africa_niamey : TimeZone
africa_niamey =
    link "Africa/Niamey" africa_lagos_l


{-| Africa/Nouakchott
-}
africa_nouakchott : TimeZone
africa_nouakchott =
    link "Africa/Nouakchott" africa_abidjan_l


{-| Africa/Ouagadougou
-}
africa_ouagadougou : TimeZone
africa_ouagadougou =
    link "Africa/Ouagadougou" africa_abidjan_l


{-| Africa/Porto-Novo
-}
africa_porto_novo : TimeZone
africa_porto_novo =
    link "Africa/Porto-Novo" africa_lagos_l


{-| Africa/Sao\_Tome
-}
africa_sao_tome : TimeZone
africa_sao_tome =
    link "Africa/Sao_Tome" africa_abidjan_l


{-| Africa/Timbuktu
-}
africa_timbuktu : TimeZone
africa_timbuktu =
    link "Africa/Timbuktu" africa_abidjan_l


{-| Africa/Tripoli
-}
africa_tripoli : TimeZone
africa_tripoli =
    africa_tripoli_l


{-| Africa/Tunis
-}
africa_tunis : TimeZone
africa_tunis =
    africa_tunis_l


{-| Africa/Windhoek
-}
africa_windhoek : TimeZone
africa_windhoek =
    africa_windhoek_l


{-| America/Adak
-}
america_adak : TimeZone
america_adak =
    america_adak_l


{-| America/Anchorage
-}
america_anchorage : TimeZone
america_anchorage =
    america_anchorage_l


{-| America/Anguilla
-}
america_anguilla : TimeZone
america_anguilla =
    link "America/Anguilla" america_port_of_spain_l


{-| America/Antigua
-}
america_antigua : TimeZone
america_antigua =
    link "America/Antigua" america_port_of_spain_l


{-| America/Araguaina
-}
america_araguaina : TimeZone
america_araguaina =
    america_araguaina_l


{-| America/Argentina/Buenos\_Aires
-}
america_argentina_buenos_aires : TimeZone
america_argentina_buenos_aires =
    america_argentina_buenos_aires_l


{-| America/Argentina/Catamarca
-}
america_argentina_catamarca : TimeZone
america_argentina_catamarca =
    america_argentina_catamarca_l


{-| America/Argentina/ComodRivadavia
-}
america_argentina_comodrivadavia : TimeZone
america_argentina_comodrivadavia =
    link "America/Argentina/ComodRivadavia" america_argentina_catamarca_l


{-| America/Argentina/Cordoba
-}
america_argentina_cordoba : TimeZone
america_argentina_cordoba =
    america_argentina_cordoba_l


{-| America/Argentina/Jujuy
-}
america_argentina_jujuy : TimeZone
america_argentina_jujuy =
    america_argentina_jujuy_l


{-| America/Argentina/La\_Rioja
-}
america_argentina_la_rioja : TimeZone
america_argentina_la_rioja =
    america_argentina_la_rioja_l


{-| America/Argentina/Mendoza
-}
america_argentina_mendoza : TimeZone
america_argentina_mendoza =
    america_argentina_mendoza_l


{-| America/Argentina/Rio\_Gallegos
-}
america_argentina_rio_gallegos : TimeZone
america_argentina_rio_gallegos =
    america_argentina_rio_gallegos_l


{-| America/Argentina/Salta
-}
america_argentina_salta : TimeZone
america_argentina_salta =
    america_argentina_salta_l


{-| America/Argentina/San\_Juan
-}
america_argentina_san_juan : TimeZone
america_argentina_san_juan =
    america_argentina_san_juan_l


{-| America/Argentina/San\_Luis
-}
america_argentina_san_luis : TimeZone
america_argentina_san_luis =
    america_argentina_san_luis_l


{-| America/Argentina/Tucuman
-}
america_argentina_tucuman : TimeZone
america_argentina_tucuman =
    america_argentina_tucuman_l


{-| America/Argentina/Ushuaia
-}
america_argentina_ushuaia : TimeZone
america_argentina_ushuaia =
    america_argentina_ushuaia_l


{-| America/Aruba
-}
america_aruba : TimeZone
america_aruba =
    link "America/Aruba" america_curacao_l


{-| America/Asuncion
-}
america_asuncion : TimeZone
america_asuncion =
    america_asuncion_l


{-| America/Atikokan
-}
america_atikokan : TimeZone
america_atikokan =
    america_atikokan_l


{-| America/Atka
-}
america_atka : TimeZone
america_atka =
    link "America/Atka" america_adak_l


{-| America/Bahia
-}
america_bahia : TimeZone
america_bahia =
    america_bahia_l


{-| America/Bahia\_Banderas
-}
america_bahia_banderas : TimeZone
america_bahia_banderas =
    america_bahia_banderas_l


{-| America/Barbados
-}
america_barbados : TimeZone
america_barbados =
    america_barbados_l


{-| America/Belem
-}
america_belem : TimeZone
america_belem =
    america_belem_l


{-| America/Belize
-}
america_belize : TimeZone
america_belize =
    america_belize_l


{-| America/Blanc-Sablon
-}
america_blanc_sablon : TimeZone
america_blanc_sablon =
    america_blanc_sablon_l


{-| America/Boa\_Vista
-}
america_boa_vista : TimeZone
america_boa_vista =
    america_boa_vista_l


{-| America/Bogota
-}
america_bogota : TimeZone
america_bogota =
    america_bogota_l


{-| America/Boise
-}
america_boise : TimeZone
america_boise =
    america_boise_l


{-| America/Buenos\_Aires
-}
america_buenos_aires : TimeZone
america_buenos_aires =
    link "America/Buenos_Aires" america_argentina_buenos_aires_l


{-| America/Cambridge\_Bay
-}
america_cambridge_bay : TimeZone
america_cambridge_bay =
    america_cambridge_bay_l


{-| America/Campo\_Grande
-}
america_campo_grande : TimeZone
america_campo_grande =
    america_campo_grande_l


{-| America/Cancun
-}
america_cancun : TimeZone
america_cancun =
    america_cancun_l


{-| America/Caracas
-}
america_caracas : TimeZone
america_caracas =
    america_caracas_l


{-| America/Catamarca
-}
america_catamarca : TimeZone
america_catamarca =
    link "America/Catamarca" america_argentina_catamarca_l


{-| America/Cayenne
-}
america_cayenne : TimeZone
america_cayenne =
    america_cayenne_l


{-| America/Cayman
-}
america_cayman : TimeZone
america_cayman =
    link "America/Cayman" america_panama_l


{-| America/Chicago
-}
america_chicago : TimeZone
america_chicago =
    america_chicago_l


{-| America/Chihuahua
-}
america_chihuahua : TimeZone
america_chihuahua =
    america_chihuahua_l


{-| America/Coral\_Harbour
-}
america_coral_harbour : TimeZone
america_coral_harbour =
    link "America/Coral_Harbour" america_atikokan_l


{-| America/Cordoba
-}
america_cordoba : TimeZone
america_cordoba =
    link "America/Cordoba" america_argentina_cordoba_l


{-| America/Costa\_Rica
-}
america_costa_rica : TimeZone
america_costa_rica =
    america_costa_rica_l


{-| America/Creston
-}
america_creston : TimeZone
america_creston =
    america_creston_l


{-| America/Cuiaba
-}
america_cuiaba : TimeZone
america_cuiaba =
    america_cuiaba_l


{-| America/Curacao
-}
america_curacao : TimeZone
america_curacao =
    america_curacao_l


{-| America/Danmarkshavn
-}
america_danmarkshavn : TimeZone
america_danmarkshavn =
    america_danmarkshavn_l


{-| America/Dawson
-}
america_dawson : TimeZone
america_dawson =
    america_dawson_l


{-| America/Dawson\_Creek
-}
america_dawson_creek : TimeZone
america_dawson_creek =
    america_dawson_creek_l


{-| America/Denver
-}
america_denver : TimeZone
america_denver =
    america_denver_l


{-| America/Detroit
-}
america_detroit : TimeZone
america_detroit =
    america_detroit_l


{-| America/Dominica
-}
america_dominica : TimeZone
america_dominica =
    link "America/Dominica" america_port_of_spain_l


{-| America/Edmonton
-}
america_edmonton : TimeZone
america_edmonton =
    america_edmonton_l


{-| America/Eirunepe
-}
america_eirunepe : TimeZone
america_eirunepe =
    america_eirunepe_l


{-| America/El\_Salvador
-}
america_el_salvador : TimeZone
america_el_salvador =
    america_el_salvador_l


{-| America/Ensenada
-}
america_ensenada : TimeZone
america_ensenada =
    link "America/Ensenada" america_tijuana_l


{-| America/Fort\_Nelson
-}
america_fort_nelson : TimeZone
america_fort_nelson =
    america_fort_nelson_l


{-| America/Fort\_Wayne
-}
america_fort_wayne : TimeZone
america_fort_wayne =
    america_fort_wayne_l


{-| America/Fortaleza
-}
america_fortaleza : TimeZone
america_fortaleza =
    america_fortaleza_l


{-| America/Glace\_Bay
-}
america_glace_bay : TimeZone
america_glace_bay =
    america_glace_bay_l


{-| America/Godthab
-}
america_godthab : TimeZone
america_godthab =
    america_godthab_l


{-| America/Goose\_Bay
-}
america_goose_bay : TimeZone
america_goose_bay =
    america_goose_bay_l


{-| America/Grand\_Turk
-}
america_grand_turk : TimeZone
america_grand_turk =
    america_grand_turk_l


{-| America/Grenada
-}
america_grenada : TimeZone
america_grenada =
    link "America/Grenada" america_port_of_spain_l


{-| America/Guadeloupe
-}
america_guadeloupe : TimeZone
america_guadeloupe =
    link "America/Guadeloupe" america_port_of_spain_l


{-| America/Guatemala
-}
america_guatemala : TimeZone
america_guatemala =
    america_guatemala_l


{-| America/Guayaquil
-}
america_guayaquil : TimeZone
america_guayaquil =
    america_guayaquil_l


{-| America/Guyana
-}
america_guyana : TimeZone
america_guyana =
    america_guyana_l


{-| America/Halifax
-}
america_halifax : TimeZone
america_halifax =
    america_halifax_l


{-| America/Havana
-}
america_havana : TimeZone
america_havana =
    america_havana_l


{-| America/Hermosillo
-}
america_hermosillo : TimeZone
america_hermosillo =
    america_hermosillo_l


{-| America/Indiana/Indianapolis
-}
america_indiana_indianapolis : TimeZone
america_indiana_indianapolis =
    link "America/Indiana/Indianapolis" america_fort_wayne_l


{-| America/Indiana/Knox
-}
america_indiana_knox : TimeZone
america_indiana_knox =
    america_indiana_knox_l


{-| America/Indiana/Marengo
-}
america_indiana_marengo : TimeZone
america_indiana_marengo =
    america_indiana_marengo_l


{-| America/Indiana/Petersburg
-}
america_indiana_petersburg : TimeZone
america_indiana_petersburg =
    america_indiana_petersburg_l


{-| America/Indiana/Tell\_City
-}
america_indiana_tell_city : TimeZone
america_indiana_tell_city =
    america_indiana_tell_city_l


{-| America/Indiana/Vevay
-}
america_indiana_vevay : TimeZone
america_indiana_vevay =
    america_indiana_vevay_l


{-| America/Indiana/Vincennes
-}
america_indiana_vincennes : TimeZone
america_indiana_vincennes =
    america_indiana_vincennes_l


{-| America/Indiana/Winamac
-}
america_indiana_winamac : TimeZone
america_indiana_winamac =
    america_indiana_winamac_l


{-| America/Indianapolis
-}
america_indianapolis : TimeZone
america_indianapolis =
    link "America/Indianapolis" america_fort_wayne_l


{-| America/Inuvik
-}
america_inuvik : TimeZone
america_inuvik =
    america_inuvik_l


{-| America/Iqaluit
-}
america_iqaluit : TimeZone
america_iqaluit =
    america_iqaluit_l


{-| America/Jamaica
-}
america_jamaica : TimeZone
america_jamaica =
    america_jamaica_l


{-| America/Jujuy
-}
america_jujuy : TimeZone
america_jujuy =
    link "America/Jujuy" america_argentina_jujuy_l


{-| America/Juneau
-}
america_juneau : TimeZone
america_juneau =
    america_juneau_l


{-| America/Kentucky/Louisville
-}
america_kentucky_louisville : TimeZone
america_kentucky_louisville =
    america_kentucky_louisville_l


{-| America/Kentucky/Monticello
-}
america_kentucky_monticello : TimeZone
america_kentucky_monticello =
    america_kentucky_monticello_l


{-| America/Knox\_IN
-}
america_knox_in : TimeZone
america_knox_in =
    link "America/Knox_IN" america_indiana_knox_l


{-| America/Kralendijk
-}
america_kralendijk : TimeZone
america_kralendijk =
    link "America/Kralendijk" america_curacao_l


{-| America/La\_Paz
-}
america_la_paz : TimeZone
america_la_paz =
    america_la_paz_l


{-| America/Lima
-}
america_lima : TimeZone
america_lima =
    america_lima_l


{-| America/Los\_Angeles
-}
america_los_angeles : TimeZone
america_los_angeles =
    america_los_angeles_l


{-| America/Louisville
-}
america_louisville : TimeZone
america_louisville =
    link "America/Louisville" america_kentucky_louisville_l


{-| America/Lower\_Princes
-}
america_lower_princes : TimeZone
america_lower_princes =
    link "America/Lower_Princes" america_curacao_l


{-| America/Maceio
-}
america_maceio : TimeZone
america_maceio =
    america_maceio_l


{-| America/Managua
-}
america_managua : TimeZone
america_managua =
    america_managua_l


{-| America/Manaus
-}
america_manaus : TimeZone
america_manaus =
    america_manaus_l


{-| America/Marigot
-}
america_marigot : TimeZone
america_marigot =
    link "America/Marigot" america_port_of_spain_l


{-| America/Martinique
-}
america_martinique : TimeZone
america_martinique =
    america_martinique_l


{-| America/Matamoros
-}
america_matamoros : TimeZone
america_matamoros =
    america_matamoros_l


{-| America/Mazatlan
-}
america_mazatlan : TimeZone
america_mazatlan =
    america_mazatlan_l


{-| America/Mendoza
-}
america_mendoza : TimeZone
america_mendoza =
    link "America/Mendoza" america_argentina_mendoza_l


{-| America/Menominee
-}
america_menominee : TimeZone
america_menominee =
    america_menominee_l


{-| America/Merida
-}
america_merida : TimeZone
america_merida =
    america_merida_l


{-| America/Metlakatla
-}
america_metlakatla : TimeZone
america_metlakatla =
    america_metlakatla_l


{-| America/Mexico\_City
-}
america_mexico_city : TimeZone
america_mexico_city =
    america_mexico_city_l


{-| America/Miquelon
-}
america_miquelon : TimeZone
america_miquelon =
    america_miquelon_l


{-| America/Moncton
-}
america_moncton : TimeZone
america_moncton =
    america_moncton_l


{-| America/Monterrey
-}
america_monterrey : TimeZone
america_monterrey =
    america_monterrey_l


{-| America/Montevideo
-}
america_montevideo : TimeZone
america_montevideo =
    america_montevideo_l


{-| America/Montreal
-}
america_montreal : TimeZone
america_montreal =
    link "America/Montreal" america_toronto_l


{-| America/Montserrat
-}
america_montserrat : TimeZone
america_montserrat =
    link "America/Montserrat" america_port_of_spain_l


{-| America/Nassau
-}
america_nassau : TimeZone
america_nassau =
    america_nassau_l


{-| America/New\_York
-}
america_new_york : TimeZone
america_new_york =
    america_new_york_l


{-| America/Nipigon
-}
america_nipigon : TimeZone
america_nipigon =
    america_nipigon_l


{-| America/Nome
-}
america_nome : TimeZone
america_nome =
    america_nome_l


{-| America/Noronha
-}
america_noronha : TimeZone
america_noronha =
    america_noronha_l


{-| America/North\_Dakota/Beulah
-}
america_north_dakota_beulah : TimeZone
america_north_dakota_beulah =
    america_north_dakota_beulah_l


{-| America/North\_Dakota/Center
-}
america_north_dakota_center : TimeZone
america_north_dakota_center =
    america_north_dakota_center_l


{-| America/North\_Dakota/New\_Salem
-}
america_north_dakota_new_salem : TimeZone
america_north_dakota_new_salem =
    america_north_dakota_new_salem_l


{-| America/Ojinaga
-}
america_ojinaga : TimeZone
america_ojinaga =
    america_ojinaga_l


{-| America/Panama
-}
america_panama : TimeZone
america_panama =
    america_panama_l


{-| America/Pangnirtung
-}
america_pangnirtung : TimeZone
america_pangnirtung =
    america_pangnirtung_l


{-| America/Paramaribo
-}
america_paramaribo : TimeZone
america_paramaribo =
    america_paramaribo_l


{-| America/Phoenix
-}
america_phoenix : TimeZone
america_phoenix =
    america_phoenix_l


{-| America/Port-au-Prince
-}
america_port_au_prince : TimeZone
america_port_au_prince =
    america_port_au_prince_l


{-| America/Port\_of\_Spain
-}
america_port_of_spain : TimeZone
america_port_of_spain =
    america_port_of_spain_l


{-| America/Porto\_Acre
-}
america_porto_acre : TimeZone
america_porto_acre =
    link "America/Porto_Acre" america_rio_branco_l


{-| America/Porto\_Velho
-}
america_porto_velho : TimeZone
america_porto_velho =
    america_porto_velho_l


{-| America/Puerto\_Rico
-}
america_puerto_rico : TimeZone
america_puerto_rico =
    america_puerto_rico_l


{-| America/Rainy\_River
-}
america_rainy_river : TimeZone
america_rainy_river =
    america_rainy_river_l


{-| America/Rankin\_Inlet
-}
america_rankin_inlet : TimeZone
america_rankin_inlet =
    america_rankin_inlet_l


{-| America/Recife
-}
america_recife : TimeZone
america_recife =
    america_recife_l


{-| America/Regina
-}
america_regina : TimeZone
america_regina =
    america_regina_l


{-| America/Resolute
-}
america_resolute : TimeZone
america_resolute =
    america_resolute_l


{-| America/Rio\_Branco
-}
america_rio_branco : TimeZone
america_rio_branco =
    america_rio_branco_l


{-| America/Rosario
-}
america_rosario : TimeZone
america_rosario =
    link "America/Rosario" america_argentina_cordoba_l


{-| America/Santa\_Isabel
-}
america_santa_isabel : TimeZone
america_santa_isabel =
    link "America/Santa_Isabel" america_tijuana_l


{-| America/Santarem
-}
america_santarem : TimeZone
america_santarem =
    america_santarem_l


{-| America/Santiago
-}
america_santiago : TimeZone
america_santiago =
    america_santiago_l


{-| America/Santo\_Domingo
-}
america_santo_domingo : TimeZone
america_santo_domingo =
    america_santo_domingo_l


{-| America/Sao\_Paulo
-}
america_sao_paulo : TimeZone
america_sao_paulo =
    america_sao_paulo_l


{-| America/Scoresbysund
-}
america_scoresbysund : TimeZone
america_scoresbysund =
    america_scoresbysund_l


{-| America/Shiprock
-}
america_shiprock : TimeZone
america_shiprock =
    link "America/Shiprock" america_denver_l


{-| America/Sitka
-}
america_sitka : TimeZone
america_sitka =
    america_sitka_l


{-| America/St\_Barthelemy
-}
america_st_barthelemy : TimeZone
america_st_barthelemy =
    link "America/St_Barthelemy" america_port_of_spain_l


{-| America/St\_Johns
-}
america_st_johns : TimeZone
america_st_johns =
    america_st_johns_l


{-| America/St\_Kitts
-}
america_st_kitts : TimeZone
america_st_kitts =
    link "America/St_Kitts" america_port_of_spain_l


{-| America/St\_Lucia
-}
america_st_lucia : TimeZone
america_st_lucia =
    link "America/St_Lucia" america_port_of_spain_l


{-| America/St\_Thomas
-}
america_st_thomas : TimeZone
america_st_thomas =
    link "America/St_Thomas" america_port_of_spain_l


{-| America/St\_Vincent
-}
america_st_vincent : TimeZone
america_st_vincent =
    link "America/St_Vincent" america_port_of_spain_l


{-| America/Swift\_Current
-}
america_swift_current : TimeZone
america_swift_current =
    america_swift_current_l


{-| America/Tegucigalpa
-}
america_tegucigalpa : TimeZone
america_tegucigalpa =
    america_tegucigalpa_l


{-| America/Thule
-}
america_thule : TimeZone
america_thule =
    america_thule_l


{-| America/Thunder\_Bay
-}
america_thunder_bay : TimeZone
america_thunder_bay =
    america_thunder_bay_l


{-| America/Tijuana
-}
america_tijuana : TimeZone
america_tijuana =
    america_tijuana_l


{-| America/Toronto
-}
america_toronto : TimeZone
america_toronto =
    america_toronto_l


{-| America/Tortola
-}
america_tortola : TimeZone
america_tortola =
    link "America/Tortola" america_port_of_spain_l


{-| America/Vancouver
-}
america_vancouver : TimeZone
america_vancouver =
    america_vancouver_l


{-| America/Virgin
-}
america_virgin : TimeZone
america_virgin =
    link "America/Virgin" america_port_of_spain_l


{-| America/Whitehorse
-}
america_whitehorse : TimeZone
america_whitehorse =
    america_whitehorse_l


{-| America/Winnipeg
-}
america_winnipeg : TimeZone
america_winnipeg =
    america_winnipeg_l


{-| America/Yakutat
-}
america_yakutat : TimeZone
america_yakutat =
    america_yakutat_l


{-| America/Yellowknife
-}
america_yellowknife : TimeZone
america_yellowknife =
    america_yellowknife_l


{-| Antarctica/Casey
-}
antarctica_casey : TimeZone
antarctica_casey =
    antarctica_casey_l


{-| Antarctica/Davis
-}
antarctica_davis : TimeZone
antarctica_davis =
    antarctica_davis_l


{-| Antarctica/DumontDUrville
-}
antarctica_dumontdurville : TimeZone
antarctica_dumontdurville =
    antarctica_dumontdurville_l


{-| Antarctica/Macquarie
-}
antarctica_macquarie : TimeZone
antarctica_macquarie =
    antarctica_macquarie_l


{-| Antarctica/Mawson
-}
antarctica_mawson : TimeZone
antarctica_mawson =
    antarctica_mawson_l


{-| Antarctica/McMurdo
-}
antarctica_mcmurdo : TimeZone
antarctica_mcmurdo =
    link "Antarctica/McMurdo" pacific_auckland_l


{-| Antarctica/Palmer
-}
antarctica_palmer : TimeZone
antarctica_palmer =
    antarctica_palmer_l


{-| Antarctica/Rothera
-}
antarctica_rothera : TimeZone
antarctica_rothera =
    antarctica_rothera_l


{-| Antarctica/South\_Pole
-}
antarctica_south_pole : TimeZone
antarctica_south_pole =
    link "Antarctica/South_Pole" pacific_auckland_l


{-| Antarctica/Syowa
-}
antarctica_syowa : TimeZone
antarctica_syowa =
    antarctica_syowa_l


{-| Antarctica/Troll
-}
antarctica_troll : TimeZone
antarctica_troll =
    antarctica_troll_l


{-| Antarctica/Vostok
-}
antarctica_vostok : TimeZone
antarctica_vostok =
    antarctica_vostok_l


{-| Arctic/Longyearbyen
-}
arctic_longyearbyen : TimeZone
arctic_longyearbyen =
    link "Arctic/Longyearbyen" europe_oslo_l


{-| Asia/Aden
-}
asia_aden : TimeZone
asia_aden =
    link "Asia/Aden" asia_riyadh_l


{-| Asia/Almaty
-}
asia_almaty : TimeZone
asia_almaty =
    asia_almaty_l


{-| Asia/Amman
-}
asia_amman : TimeZone
asia_amman =
    asia_amman_l


{-| Asia/Anadyr
-}
asia_anadyr : TimeZone
asia_anadyr =
    asia_anadyr_l


{-| Asia/Aqtau
-}
asia_aqtau : TimeZone
asia_aqtau =
    asia_aqtau_l


{-| Asia/Aqtobe
-}
asia_aqtobe : TimeZone
asia_aqtobe =
    asia_aqtobe_l


{-| Asia/Ashgabat
-}
asia_ashgabat : TimeZone
asia_ashgabat =
    asia_ashgabat_l


{-| Asia/Ashkhabad
-}
asia_ashkhabad : TimeZone
asia_ashkhabad =
    link "Asia/Ashkhabad" asia_ashgabat_l


{-| Asia/Baghdad
-}
asia_baghdad : TimeZone
asia_baghdad =
    asia_baghdad_l


{-| Asia/Bahrain
-}
asia_bahrain : TimeZone
asia_bahrain =
    link "Asia/Bahrain" asia_qatar_l


{-| Asia/Baku
-}
asia_baku : TimeZone
asia_baku =
    asia_baku_l


{-| Asia/Bangkok
-}
asia_bangkok : TimeZone
asia_bangkok =
    asia_bangkok_l


{-| Asia/Barnaul
-}
asia_barnaul : TimeZone
asia_barnaul =
    asia_barnaul_l


{-| Asia/Beirut
-}
asia_beirut : TimeZone
asia_beirut =
    asia_beirut_l


{-| Asia/Bishkek
-}
asia_bishkek : TimeZone
asia_bishkek =
    asia_bishkek_l


{-| Asia/Brunei
-}
asia_brunei : TimeZone
asia_brunei =
    asia_brunei_l


{-| Asia/Calcutta
-}
asia_calcutta : TimeZone
asia_calcutta =
    link "Asia/Calcutta" asia_kolkata_l


{-| Asia/Chita
-}
asia_chita : TimeZone
asia_chita =
    asia_chita_l


{-| Asia/Choibalsan
-}
asia_choibalsan : TimeZone
asia_choibalsan =
    asia_choibalsan_l


{-| Asia/Chongqing
-}
asia_chongqing : TimeZone
asia_chongqing =
    link "Asia/Chongqing" asia_shanghai_l


{-| Asia/Chungking
-}
asia_chungking : TimeZone
asia_chungking =
    link "Asia/Chungking" asia_shanghai_l


{-| Asia/Colombo
-}
asia_colombo : TimeZone
asia_colombo =
    asia_colombo_l


{-| Asia/Dacca
-}
asia_dacca : TimeZone
asia_dacca =
    link "Asia/Dacca" asia_dhaka_l


{-| Asia/Damascus
-}
asia_damascus : TimeZone
asia_damascus =
    asia_damascus_l


{-| Asia/Dhaka
-}
asia_dhaka : TimeZone
asia_dhaka =
    asia_dhaka_l


{-| Asia/Dili
-}
asia_dili : TimeZone
asia_dili =
    asia_dili_l


{-| Asia/Dubai
-}
asia_dubai : TimeZone
asia_dubai =
    asia_dubai_l


{-| Asia/Dushanbe
-}
asia_dushanbe : TimeZone
asia_dushanbe =
    asia_dushanbe_l


{-| Asia/Famagusta
-}
asia_famagusta : TimeZone
asia_famagusta =
    asia_famagusta_l


{-| Asia/Gaza
-}
asia_gaza : TimeZone
asia_gaza =
    asia_gaza_l


{-| Asia/Harbin
-}
asia_harbin : TimeZone
asia_harbin =
    link "Asia/Harbin" asia_shanghai_l


{-| Asia/Hebron
-}
asia_hebron : TimeZone
asia_hebron =
    asia_hebron_l


{-| Asia/Ho\_Chi\_Minh
-}
asia_ho_chi_minh : TimeZone
asia_ho_chi_minh =
    asia_ho_chi_minh_l


{-| Asia/Hong\_Kong
-}
asia_hong_kong : TimeZone
asia_hong_kong =
    asia_hong_kong_l


{-| Asia/Hovd
-}
asia_hovd : TimeZone
asia_hovd =
    asia_hovd_l


{-| Asia/Irkutsk
-}
asia_irkutsk : TimeZone
asia_irkutsk =
    asia_irkutsk_l


{-| Asia/Istanbul
-}
asia_istanbul : TimeZone
asia_istanbul =
    link "Asia/Istanbul" europe_istanbul_l


{-| Asia/Jakarta
-}
asia_jakarta : TimeZone
asia_jakarta =
    asia_jakarta_l


{-| Asia/Jayapura
-}
asia_jayapura : TimeZone
asia_jayapura =
    asia_jayapura_l


{-| Asia/Jerusalem
-}
asia_jerusalem : TimeZone
asia_jerusalem =
    asia_jerusalem_l


{-| Asia/Kabul
-}
asia_kabul : TimeZone
asia_kabul =
    asia_kabul_l


{-| Asia/Kamchatka
-}
asia_kamchatka : TimeZone
asia_kamchatka =
    asia_kamchatka_l


{-| Asia/Karachi
-}
asia_karachi : TimeZone
asia_karachi =
    asia_karachi_l


{-| Asia/Kashgar
-}
asia_kashgar : TimeZone
asia_kashgar =
    link "Asia/Kashgar" asia_urumqi_l


{-| Asia/Kathmandu
-}
asia_kathmandu : TimeZone
asia_kathmandu =
    asia_kathmandu_l


{-| Asia/Katmandu
-}
asia_katmandu : TimeZone
asia_katmandu =
    link "Asia/Katmandu" asia_kathmandu_l


{-| Asia/Khandyga
-}
asia_khandyga : TimeZone
asia_khandyga =
    asia_khandyga_l


{-| Asia/Kolkata
-}
asia_kolkata : TimeZone
asia_kolkata =
    asia_kolkata_l


{-| Asia/Krasnoyarsk
-}
asia_krasnoyarsk : TimeZone
asia_krasnoyarsk =
    asia_krasnoyarsk_l


{-| Asia/Kuala\_Lumpur
-}
asia_kuala_lumpur : TimeZone
asia_kuala_lumpur =
    asia_kuala_lumpur_l


{-| Asia/Kuching
-}
asia_kuching : TimeZone
asia_kuching =
    asia_kuching_l


{-| Asia/Kuwait
-}
asia_kuwait : TimeZone
asia_kuwait =
    link "Asia/Kuwait" asia_riyadh_l


{-| Asia/Macao
-}
asia_macao : TimeZone
asia_macao =
    link "Asia/Macao" asia_macau_l


{-| Asia/Macau
-}
asia_macau : TimeZone
asia_macau =
    asia_macau_l


{-| Asia/Magadan
-}
asia_magadan : TimeZone
asia_magadan =
    asia_magadan_l


{-| Asia/Makassar
-}
asia_makassar : TimeZone
asia_makassar =
    asia_makassar_l


{-| Asia/Manila
-}
asia_manila : TimeZone
asia_manila =
    asia_manila_l


{-| Asia/Muscat
-}
asia_muscat : TimeZone
asia_muscat =
    link "Asia/Muscat" asia_dubai_l


{-| Asia/Nicosia
-}
asia_nicosia : TimeZone
asia_nicosia =
    asia_nicosia_l


{-| Asia/Novokuznetsk
-}
asia_novokuznetsk : TimeZone
asia_novokuznetsk =
    asia_novokuznetsk_l


{-| Asia/Novosibirsk
-}
asia_novosibirsk : TimeZone
asia_novosibirsk =
    asia_novosibirsk_l


{-| Asia/Omsk
-}
asia_omsk : TimeZone
asia_omsk =
    asia_omsk_l


{-| Asia/Oral
-}
asia_oral : TimeZone
asia_oral =
    asia_oral_l


{-| Asia/Phnom\_Penh
-}
asia_phnom_penh : TimeZone
asia_phnom_penh =
    link "Asia/Phnom_Penh" asia_bangkok_l


{-| Asia/Pontianak
-}
asia_pontianak : TimeZone
asia_pontianak =
    asia_pontianak_l


{-| Asia/Pyongyang
-}
asia_pyongyang : TimeZone
asia_pyongyang =
    asia_pyongyang_l


{-| Asia/Qatar
-}
asia_qatar : TimeZone
asia_qatar =
    asia_qatar_l


{-| Asia/Qyzylorda
-}
asia_qyzylorda : TimeZone
asia_qyzylorda =
    asia_qyzylorda_l


{-| Asia/Rangoon
-}
asia_rangoon : TimeZone
asia_rangoon =
    asia_rangoon_l


{-| Asia/Riyadh
-}
asia_riyadh : TimeZone
asia_riyadh =
    asia_riyadh_l


{-| Asia/Saigon
-}
asia_saigon : TimeZone
asia_saigon =
    link "Asia/Saigon" asia_ho_chi_minh_l


{-| Asia/Sakhalin
-}
asia_sakhalin : TimeZone
asia_sakhalin =
    asia_sakhalin_l


{-| Asia/Samarkand
-}
asia_samarkand : TimeZone
asia_samarkand =
    asia_samarkand_l


{-| Asia/Seoul
-}
asia_seoul : TimeZone
asia_seoul =
    asia_seoul_l


{-| Asia/Shanghai
-}
asia_shanghai : TimeZone
asia_shanghai =
    asia_shanghai_l


{-| Asia/Singapore
-}
asia_singapore : TimeZone
asia_singapore =
    asia_singapore_l


{-| Asia/Srednekolymsk
-}
asia_srednekolymsk : TimeZone
asia_srednekolymsk =
    asia_srednekolymsk_l


{-| Asia/Taipei
-}
asia_taipei : TimeZone
asia_taipei =
    asia_taipei_l


{-| Asia/Tashkent
-}
asia_tashkent : TimeZone
asia_tashkent =
    asia_tashkent_l


{-| Asia/Tbilisi
-}
asia_tbilisi : TimeZone
asia_tbilisi =
    asia_tbilisi_l


{-| Asia/Tehran
-}
asia_tehran : TimeZone
asia_tehran =
    asia_tehran_l


{-| Asia/Tel\_Aviv
-}
asia_tel_aviv : TimeZone
asia_tel_aviv =
    link "Asia/Tel_Aviv" asia_jerusalem_l


{-| Asia/Thimbu
-}
asia_thimbu : TimeZone
asia_thimbu =
    link "Asia/Thimbu" asia_thimphu_l


{-| Asia/Thimphu
-}
asia_thimphu : TimeZone
asia_thimphu =
    asia_thimphu_l


{-| Asia/Tokyo
-}
asia_tokyo : TimeZone
asia_tokyo =
    asia_tokyo_l


{-| Asia/Tomsk
-}
asia_tomsk : TimeZone
asia_tomsk =
    asia_tomsk_l


{-| Asia/Ujung\_Pandang
-}
asia_ujung_pandang : TimeZone
asia_ujung_pandang =
    link "Asia/Ujung_Pandang" asia_makassar_l


{-| Asia/Ulaanbaatar
-}
asia_ulaanbaatar : TimeZone
asia_ulaanbaatar =
    asia_ulaanbaatar_l


{-| Asia/Ulan\_Bator
-}
asia_ulan_bator : TimeZone
asia_ulan_bator =
    link "Asia/Ulan_Bator" asia_ulaanbaatar_l


{-| Asia/Urumqi
-}
asia_urumqi : TimeZone
asia_urumqi =
    asia_urumqi_l


{-| Asia/Ust-Nera
-}
asia_ust_nera : TimeZone
asia_ust_nera =
    asia_ust_nera_l


{-| Asia/Vientiane
-}
asia_vientiane : TimeZone
asia_vientiane =
    link "Asia/Vientiane" asia_bangkok_l


{-| Asia/Vladivostok
-}
asia_vladivostok : TimeZone
asia_vladivostok =
    asia_vladivostok_l


{-| Asia/Yakutsk
-}
asia_yakutsk : TimeZone
asia_yakutsk =
    asia_yakutsk_l


{-| Asia/Yangon
-}
asia_yangon : TimeZone
asia_yangon =
    link "Asia/Yangon" asia_rangoon_l


{-| Asia/Yekaterinburg
-}
asia_yekaterinburg : TimeZone
asia_yekaterinburg =
    asia_yekaterinburg_l


{-| Asia/Yerevan
-}
asia_yerevan : TimeZone
asia_yerevan =
    asia_yerevan_l


{-| Atlantic/Azores
-}
atlantic_azores : TimeZone
atlantic_azores =
    atlantic_azores_l


{-| Atlantic/Bermuda
-}
atlantic_bermuda : TimeZone
atlantic_bermuda =
    atlantic_bermuda_l


{-| Atlantic/Canary
-}
atlantic_canary : TimeZone
atlantic_canary =
    atlantic_canary_l


{-| Atlantic/Cape\_Verde
-}
atlantic_cape_verde : TimeZone
atlantic_cape_verde =
    atlantic_cape_verde_l


{-| Atlantic/Faeroe
-}
atlantic_faeroe : TimeZone
atlantic_faeroe =
    link "Atlantic/Faeroe" atlantic_faroe_l


{-| Atlantic/Faroe
-}
atlantic_faroe : TimeZone
atlantic_faroe =
    atlantic_faroe_l


{-| Atlantic/Jan\_Mayen
-}
atlantic_jan_mayen : TimeZone
atlantic_jan_mayen =
    link "Atlantic/Jan_Mayen" europe_oslo_l


{-| Atlantic/Madeira
-}
atlantic_madeira : TimeZone
atlantic_madeira =
    atlantic_madeira_l


{-| Atlantic/Reykjavik
-}
atlantic_reykjavik : TimeZone
atlantic_reykjavik =
    atlantic_reykjavik_l


{-| Atlantic/South\_Georgia
-}
atlantic_south_georgia : TimeZone
atlantic_south_georgia =
    atlantic_south_georgia_l


{-| Atlantic/St\_Helena
-}
atlantic_st_helena : TimeZone
atlantic_st_helena =
    link "Atlantic/St_Helena" africa_abidjan_l


{-| Atlantic/Stanley
-}
atlantic_stanley : TimeZone
atlantic_stanley =
    atlantic_stanley_l


{-| Australia/ACT
-}
australia_act : TimeZone
australia_act =
    link "Australia/ACT" australia_sydney_l


{-| Australia/Adelaide
-}
australia_adelaide : TimeZone
australia_adelaide =
    australia_adelaide_l


{-| Australia/Brisbane
-}
australia_brisbane : TimeZone
australia_brisbane =
    australia_brisbane_l


{-| Australia/Broken\_Hill
-}
australia_broken_hill : TimeZone
australia_broken_hill =
    australia_broken_hill_l


{-| Australia/Canberra
-}
australia_canberra : TimeZone
australia_canberra =
    link "Australia/Canberra" australia_sydney_l


{-| Australia/Currie
-}
australia_currie : TimeZone
australia_currie =
    australia_currie_l


{-| Australia/Darwin
-}
australia_darwin : TimeZone
australia_darwin =
    australia_darwin_l


{-| Australia/Eucla
-}
australia_eucla : TimeZone
australia_eucla =
    australia_eucla_l


{-| Australia/Hobart
-}
australia_hobart : TimeZone
australia_hobart =
    australia_hobart_l


{-| Australia/LHI
-}
australia_lhi : TimeZone
australia_lhi =
    link "Australia/LHI" australia_lord_howe_l


{-| Australia/Lindeman
-}
australia_lindeman : TimeZone
australia_lindeman =
    australia_lindeman_l


{-| Australia/Lord\_Howe
-}
australia_lord_howe : TimeZone
australia_lord_howe =
    australia_lord_howe_l


{-| Australia/Melbourne
-}
australia_melbourne : TimeZone
australia_melbourne =
    australia_melbourne_l


{-| Australia/North
-}
australia_north : TimeZone
australia_north =
    link "Australia/North" australia_darwin_l


{-| Australia/NSW
-}
australia_nsw : TimeZone
australia_nsw =
    link "Australia/NSW" australia_sydney_l


{-| Australia/Perth
-}
australia_perth : TimeZone
australia_perth =
    australia_perth_l


{-| Australia/Queensland
-}
australia_queensland : TimeZone
australia_queensland =
    link "Australia/Queensland" australia_brisbane_l


{-| Australia/South
-}
australia_south : TimeZone
australia_south =
    link "Australia/South" australia_adelaide_l


{-| Australia/Sydney
-}
australia_sydney : TimeZone
australia_sydney =
    australia_sydney_l


{-| Australia/Tasmania
-}
australia_tasmania : TimeZone
australia_tasmania =
    link "Australia/Tasmania" australia_hobart_l


{-| Australia/Victoria
-}
australia_victoria : TimeZone
australia_victoria =
    link "Australia/Victoria" australia_melbourne_l


{-| Australia/West
-}
australia_west : TimeZone
australia_west =
    link "Australia/West" australia_perth_l


{-| Australia/Yancowinna
-}
australia_yancowinna : TimeZone
australia_yancowinna =
    link "Australia/Yancowinna" australia_broken_hill_l


{-| Brazil/Acre
-}
brazil_acre : TimeZone
brazil_acre =
    link "Brazil/Acre" america_rio_branco_l


{-| Brazil/DeNoronha
-}
brazil_denoronha : TimeZone
brazil_denoronha =
    link "Brazil/DeNoronha" america_noronha_l


{-| Brazil/East
-}
brazil_east : TimeZone
brazil_east =
    link "Brazil/East" america_sao_paulo_l


{-| Brazil/West
-}
brazil_west : TimeZone
brazil_west =
    link "Brazil/West" america_manaus_l


{-| Canada/Atlantic
-}
canada_atlantic : TimeZone
canada_atlantic =
    link "Canada/Atlantic" america_halifax_l


{-| Canada/Central
-}
canada_central : TimeZone
canada_central =
    link "Canada/Central" america_winnipeg_l


{-| Canada/East-Saskatchewan
-}
canada_east_saskatchewan : TimeZone
canada_east_saskatchewan =
    link "Canada/East-Saskatchewan" america_regina_l


{-| Canada/Eastern
-}
canada_eastern : TimeZone
canada_eastern =
    link "Canada/Eastern" america_toronto_l


{-| Canada/Mountain
-}
canada_mountain : TimeZone
canada_mountain =
    link "Canada/Mountain" america_edmonton_l


{-| Canada/Newfoundland
-}
canada_newfoundland : TimeZone
canada_newfoundland =
    link "Canada/Newfoundland" america_st_johns_l


{-| Canada/Pacific
-}
canada_pacific : TimeZone
canada_pacific =
    link "Canada/Pacific" america_vancouver_l


{-| Canada/Saskatchewan
-}
canada_saskatchewan : TimeZone
canada_saskatchewan =
    link "Canada/Saskatchewan" america_regina_l


{-| Canada/Yukon
-}
canada_yukon : TimeZone
canada_yukon =
    link "Canada/Yukon" america_whitehorse_l


{-| CET
-}
cet : TimeZone
cet =
    cet_l


{-| Chile/Continental
-}
chile_continental : TimeZone
chile_continental =
    link "Chile/Continental" america_santiago_l


{-| Chile/EasterIsland
-}
chile_easterisland : TimeZone
chile_easterisland =
    link "Chile/EasterIsland" pacific_easter_l


{-| CST6CDT
-}
cst6cdt : TimeZone
cst6cdt =
    cst6cdt_l


{-| Cuba
-}
cuba : TimeZone
cuba =
    link "Cuba" america_havana_l


{-| EET
-}
eet : TimeZone
eet =
    eet_l


{-| Egypt
-}
egypt : TimeZone
egypt =
    link "Egypt" africa_cairo_l


{-| Eire
-}
eire : TimeZone
eire =
    link "Eire" europe_dublin_l


{-| EST
-}
est : TimeZone
est =
    est_l


{-| EST5EDT
-}
est5edt : TimeZone
est5edt =
    est5edt_l


{-| Etc/GMT
-}
etc_gmt : TimeZone
etc_gmt =
    link "Etc/GMT" etc_gmt_plus_0_l


{-| Etc/GMT0
-}
etc_gmt_0 : TimeZone
etc_gmt_0 =
    link "Etc/GMT0" etc_gmt_plus_0_l


{-| Etc/GMT-0
-}
etc_gmt_minus_0 : TimeZone
etc_gmt_minus_0 =
    link "Etc/GMT-0" etc_gmt_plus_0_l


{-| Etc/GMT-1
-}
etc_gmt_minus_1 : TimeZone
etc_gmt_minus_1 =
    etc_gmt_minus_1_l


{-| Etc/GMT-10
-}
etc_gmt_minus_10 : TimeZone
etc_gmt_minus_10 =
    etc_gmt_minus_10_l


{-| Etc/GMT-11
-}
etc_gmt_minus_11 : TimeZone
etc_gmt_minus_11 =
    etc_gmt_minus_11_l


{-| Etc/GMT-12
-}
etc_gmt_minus_12 : TimeZone
etc_gmt_minus_12 =
    etc_gmt_minus_12_l


{-| Etc/GMT-13
-}
etc_gmt_minus_13 : TimeZone
etc_gmt_minus_13 =
    etc_gmt_minus_13_l


{-| Etc/GMT-14
-}
etc_gmt_minus_14 : TimeZone
etc_gmt_minus_14 =
    etc_gmt_minus_14_l


{-| Etc/GMT-2
-}
etc_gmt_minus_2 : TimeZone
etc_gmt_minus_2 =
    etc_gmt_minus_2_l


{-| Etc/GMT-3
-}
etc_gmt_minus_3 : TimeZone
etc_gmt_minus_3 =
    etc_gmt_minus_3_l


{-| Etc/GMT-4
-}
etc_gmt_minus_4 : TimeZone
etc_gmt_minus_4 =
    etc_gmt_minus_4_l


{-| Etc/GMT-5
-}
etc_gmt_minus_5 : TimeZone
etc_gmt_minus_5 =
    etc_gmt_minus_5_l


{-| Etc/GMT-6
-}
etc_gmt_minus_6 : TimeZone
etc_gmt_minus_6 =
    etc_gmt_minus_6_l


{-| Etc/GMT-7
-}
etc_gmt_minus_7 : TimeZone
etc_gmt_minus_7 =
    etc_gmt_minus_7_l


{-| Etc/GMT-8
-}
etc_gmt_minus_8 : TimeZone
etc_gmt_minus_8 =
    etc_gmt_minus_8_l


{-| Etc/GMT-9
-}
etc_gmt_minus_9 : TimeZone
etc_gmt_minus_9 =
    etc_gmt_minus_9_l


{-| Etc/GMT+0
-}
etc_gmt_plus_0 : TimeZone
etc_gmt_plus_0 =
    etc_gmt_plus_0_l


{-| Etc/GMT+1
-}
etc_gmt_plus_1 : TimeZone
etc_gmt_plus_1 =
    etc_gmt_plus_1_l


{-| Etc/GMT+10
-}
etc_gmt_plus_10 : TimeZone
etc_gmt_plus_10 =
    etc_gmt_plus_10_l


{-| Etc/GMT+11
-}
etc_gmt_plus_11 : TimeZone
etc_gmt_plus_11 =
    etc_gmt_plus_11_l


{-| Etc/GMT+12
-}
etc_gmt_plus_12 : TimeZone
etc_gmt_plus_12 =
    etc_gmt_plus_12_l


{-| Etc/GMT+2
-}
etc_gmt_plus_2 : TimeZone
etc_gmt_plus_2 =
    etc_gmt_plus_2_l


{-| Etc/GMT+3
-}
etc_gmt_plus_3 : TimeZone
etc_gmt_plus_3 =
    etc_gmt_plus_3_l


{-| Etc/GMT+4
-}
etc_gmt_plus_4 : TimeZone
etc_gmt_plus_4 =
    etc_gmt_plus_4_l


{-| Etc/GMT+5
-}
etc_gmt_plus_5 : TimeZone
etc_gmt_plus_5 =
    etc_gmt_plus_5_l


{-| Etc/GMT+6
-}
etc_gmt_plus_6 : TimeZone
etc_gmt_plus_6 =
    etc_gmt_plus_6_l


{-| Etc/GMT+7
-}
etc_gmt_plus_7 : TimeZone
etc_gmt_plus_7 =
    etc_gmt_plus_7_l


{-| Etc/GMT+8
-}
etc_gmt_plus_8 : TimeZone
etc_gmt_plus_8 =
    etc_gmt_plus_8_l


{-| Etc/GMT+9
-}
etc_gmt_plus_9 : TimeZone
etc_gmt_plus_9 =
    etc_gmt_plus_9_l


{-| Etc/Greenwich
-}
etc_greenwich : TimeZone
etc_greenwich =
    link "Etc/Greenwich" etc_gmt_plus_0_l


{-| Etc/UCT
-}
etc_uct : TimeZone
etc_uct =
    etc_uct_l


{-| Etc/Universal
-}
etc_universal : TimeZone
etc_universal =
    link "Etc/Universal" etc_utc_l


{-| Etc/UTC
-}
etc_utc : TimeZone
etc_utc =
    etc_utc_l


{-| Etc/Zulu
-}
etc_zulu : TimeZone
etc_zulu =
    link "Etc/Zulu" etc_utc_l


{-| Europe/Amsterdam
-}
europe_amsterdam : TimeZone
europe_amsterdam =
    europe_amsterdam_l


{-| Europe/Andorra
-}
europe_andorra : TimeZone
europe_andorra =
    europe_andorra_l


{-| Europe/Astrakhan
-}
europe_astrakhan : TimeZone
europe_astrakhan =
    europe_astrakhan_l


{-| Europe/Athens
-}
europe_athens : TimeZone
europe_athens =
    europe_athens_l


{-| Europe/Belfast
-}
europe_belfast : TimeZone
europe_belfast =
    link "Europe/Belfast" europe_london_l


{-| Europe/Belgrade
-}
europe_belgrade : TimeZone
europe_belgrade =
    europe_belgrade_l


{-| Europe/Berlin
-}
europe_berlin : TimeZone
europe_berlin =
    europe_berlin_l


{-| Europe/Bratislava
-}
europe_bratislava : TimeZone
europe_bratislava =
    link "Europe/Bratislava" europe_prague_l


{-| Europe/Brussels
-}
europe_brussels : TimeZone
europe_brussels =
    europe_brussels_l


{-| Europe/Bucharest
-}
europe_bucharest : TimeZone
europe_bucharest =
    europe_bucharest_l


{-| Europe/Budapest
-}
europe_budapest : TimeZone
europe_budapest =
    europe_budapest_l


{-| Europe/Busingen
-}
europe_busingen : TimeZone
europe_busingen =
    link "Europe/Busingen" europe_zurich_l


{-| Europe/Chisinau
-}
europe_chisinau : TimeZone
europe_chisinau =
    europe_chisinau_l


{-| Europe/Copenhagen
-}
europe_copenhagen : TimeZone
europe_copenhagen =
    europe_copenhagen_l


{-| Europe/Dublin
-}
europe_dublin : TimeZone
europe_dublin =
    europe_dublin_l


{-| Europe/Gibraltar
-}
europe_gibraltar : TimeZone
europe_gibraltar =
    europe_gibraltar_l


{-| Europe/Guernsey
-}
europe_guernsey : TimeZone
europe_guernsey =
    link "Europe/Guernsey" europe_london_l


{-| Europe/Helsinki
-}
europe_helsinki : TimeZone
europe_helsinki =
    europe_helsinki_l


{-| Europe/Isle\_of\_Man
-}
europe_isle_of_man : TimeZone
europe_isle_of_man =
    link "Europe/Isle_of_Man" europe_london_l


{-| Europe/Istanbul
-}
europe_istanbul : TimeZone
europe_istanbul =
    europe_istanbul_l


{-| Europe/Jersey
-}
europe_jersey : TimeZone
europe_jersey =
    link "Europe/Jersey" europe_london_l


{-| Europe/Kaliningrad
-}
europe_kaliningrad : TimeZone
europe_kaliningrad =
    europe_kaliningrad_l


{-| Europe/Kiev
-}
europe_kiev : TimeZone
europe_kiev =
    europe_kiev_l


{-| Europe/Kirov
-}
europe_kirov : TimeZone
europe_kirov =
    europe_kirov_l


{-| Europe/Lisbon
-}
europe_lisbon : TimeZone
europe_lisbon =
    europe_lisbon_l


{-| Europe/Ljubljana
-}
europe_ljubljana : TimeZone
europe_ljubljana =
    link "Europe/Ljubljana" europe_belgrade_l


{-| Europe/London
-}
europe_london : TimeZone
europe_london =
    europe_london_l


{-| Europe/Luxembourg
-}
europe_luxembourg : TimeZone
europe_luxembourg =
    europe_luxembourg_l


{-| Europe/Madrid
-}
europe_madrid : TimeZone
europe_madrid =
    europe_madrid_l


{-| Europe/Malta
-}
europe_malta : TimeZone
europe_malta =
    europe_malta_l


{-| Europe/Mariehamn
-}
europe_mariehamn : TimeZone
europe_mariehamn =
    link "Europe/Mariehamn" europe_helsinki_l


{-| Europe/Minsk
-}
europe_minsk : TimeZone
europe_minsk =
    europe_minsk_l


{-| Europe/Monaco
-}
europe_monaco : TimeZone
europe_monaco =
    europe_monaco_l


{-| Europe/Moscow
-}
europe_moscow : TimeZone
europe_moscow =
    europe_moscow_l


{-| Europe/Nicosia
-}
europe_nicosia : TimeZone
europe_nicosia =
    link "Europe/Nicosia" asia_nicosia_l


{-| Europe/Oslo
-}
europe_oslo : TimeZone
europe_oslo =
    europe_oslo_l


{-| Europe/Paris
-}
europe_paris : TimeZone
europe_paris =
    europe_paris_l


{-| Europe/Podgorica
-}
europe_podgorica : TimeZone
europe_podgorica =
    link "Europe/Podgorica" europe_belgrade_l


{-| Europe/Prague
-}
europe_prague : TimeZone
europe_prague =
    europe_prague_l


{-| Europe/Riga
-}
europe_riga : TimeZone
europe_riga =
    europe_riga_l


{-| Europe/Rome
-}
europe_rome : TimeZone
europe_rome =
    europe_rome_l


{-| Europe/Samara
-}
europe_samara : TimeZone
europe_samara =
    europe_samara_l


{-| Europe/San\_Marino
-}
europe_san_marino : TimeZone
europe_san_marino =
    link "Europe/San_Marino" europe_rome_l


{-| Europe/Sarajevo
-}
europe_sarajevo : TimeZone
europe_sarajevo =
    link "Europe/Sarajevo" europe_belgrade_l


{-| Europe/Simferopol
-}
europe_simferopol : TimeZone
europe_simferopol =
    europe_simferopol_l


{-| Europe/Skopje
-}
europe_skopje : TimeZone
europe_skopje =
    link "Europe/Skopje" europe_belgrade_l


{-| Europe/Sofia
-}
europe_sofia : TimeZone
europe_sofia =
    europe_sofia_l


{-| Europe/Stockholm
-}
europe_stockholm : TimeZone
europe_stockholm =
    europe_stockholm_l


{-| Europe/Tallinn
-}
europe_tallinn : TimeZone
europe_tallinn =
    europe_tallinn_l


{-| Europe/Tirane
-}
europe_tirane : TimeZone
europe_tirane =
    europe_tirane_l


{-| Europe/Tiraspol
-}
europe_tiraspol : TimeZone
europe_tiraspol =
    link "Europe/Tiraspol" europe_chisinau_l


{-| Europe/Ulyanovsk
-}
europe_ulyanovsk : TimeZone
europe_ulyanovsk =
    europe_ulyanovsk_l


{-| Europe/Uzhgorod
-}
europe_uzhgorod : TimeZone
europe_uzhgorod =
    europe_uzhgorod_l


{-| Europe/Vaduz
-}
europe_vaduz : TimeZone
europe_vaduz =
    link "Europe/Vaduz" europe_zurich_l


{-| Europe/Vatican
-}
europe_vatican : TimeZone
europe_vatican =
    link "Europe/Vatican" europe_rome_l


{-| Europe/Vienna
-}
europe_vienna : TimeZone
europe_vienna =
    europe_vienna_l


{-| Europe/Vilnius
-}
europe_vilnius : TimeZone
europe_vilnius =
    europe_vilnius_l


{-| Europe/Volgograd
-}
europe_volgograd : TimeZone
europe_volgograd =
    europe_volgograd_l


{-| Europe/Warsaw
-}
europe_warsaw : TimeZone
europe_warsaw =
    europe_warsaw_l


{-| Europe/Zagreb
-}
europe_zagreb : TimeZone
europe_zagreb =
    link "Europe/Zagreb" europe_belgrade_l


{-| Europe/Zaporozhye
-}
europe_zaporozhye : TimeZone
europe_zaporozhye =
    europe_zaporozhye_l


{-| Europe/Zurich
-}
europe_zurich : TimeZone
europe_zurich =
    europe_zurich_l


{-| GB
-}
gb : TimeZone
gb =
    link "GB" europe_london_l


{-| GB-Eire
-}
gb_eire : TimeZone
gb_eire =
    link "GB-Eire" europe_london_l


{-| GMT
-}
gmt : TimeZone
gmt =
    link "GMT" etc_gmt_plus_0_l


{-| GMT0
-}
gmt_0 : TimeZone
gmt_0 =
    link "GMT0" etc_gmt_plus_0_l


{-| GMT-0
-}
gmt_minus_0 : TimeZone
gmt_minus_0 =
    link "GMT-0" etc_gmt_plus_0_l


{-| GMT+0
-}
gmt_plus_0 : TimeZone
gmt_plus_0 =
    link "GMT+0" etc_gmt_plus_0_l


{-| Greenwich
-}
greenwich : TimeZone
greenwich =
    link "Greenwich" etc_gmt_plus_0_l


{-| Hongkong
-}
hongkong : TimeZone
hongkong =
    link "Hongkong" asia_hong_kong_l


{-| HST
-}
hst : TimeZone
hst =
    hst_l


{-| Iceland
-}
iceland : TimeZone
iceland =
    link "Iceland" atlantic_reykjavik_l


{-| Indian/Antananarivo
-}
indian_antananarivo : TimeZone
indian_antananarivo =
    link "Indian/Antananarivo" africa_nairobi_l


{-| Indian/Chagos
-}
indian_chagos : TimeZone
indian_chagos =
    indian_chagos_l


{-| Indian/Christmas
-}
indian_christmas : TimeZone
indian_christmas =
    indian_christmas_l


{-| Indian/Cocos
-}
indian_cocos : TimeZone
indian_cocos =
    indian_cocos_l


{-| Indian/Comoro
-}
indian_comoro : TimeZone
indian_comoro =
    link "Indian/Comoro" africa_nairobi_l


{-| Indian/Kerguelen
-}
indian_kerguelen : TimeZone
indian_kerguelen =
    indian_kerguelen_l


{-| Indian/Mahe
-}
indian_mahe : TimeZone
indian_mahe =
    indian_mahe_l


{-| Indian/Maldives
-}
indian_maldives : TimeZone
indian_maldives =
    indian_maldives_l


{-| Indian/Mauritius
-}
indian_mauritius : TimeZone
indian_mauritius =
    indian_mauritius_l


{-| Indian/Mayotte
-}
indian_mayotte : TimeZone
indian_mayotte =
    link "Indian/Mayotte" africa_nairobi_l


{-| Indian/Reunion
-}
indian_reunion : TimeZone
indian_reunion =
    indian_reunion_l


{-| Iran
-}
iran : TimeZone
iran =
    link "Iran" asia_tehran_l


{-| Israel
-}
israel : TimeZone
israel =
    link "Israel" asia_jerusalem_l


{-| Jamaica
-}
jamaica : TimeZone
jamaica =
    link "Jamaica" america_jamaica_l


{-| Japan
-}
japan : TimeZone
japan =
    link "Japan" asia_tokyo_l


{-| Kwajalein
-}
kwajalein : TimeZone
kwajalein =
    link "Kwajalein" pacific_kwajalein_l


{-| Libya
-}
libya : TimeZone
libya =
    link "Libya" africa_tripoli_l


{-| MET
-}
met : TimeZone
met =
    met_l


{-| Mexico/BajaNorte
-}
mexico_bajanorte : TimeZone
mexico_bajanorte =
    link "Mexico/BajaNorte" america_tijuana_l


{-| Mexico/BajaSur
-}
mexico_bajasur : TimeZone
mexico_bajasur =
    link "Mexico/BajaSur" america_mazatlan_l


{-| Mexico/General
-}
mexico_general : TimeZone
mexico_general =
    link "Mexico/General" america_mexico_city_l


{-| MST
-}
mst : TimeZone
mst =
    mst_l


{-| MST7MDT
-}
mst7mdt : TimeZone
mst7mdt =
    mst7mdt_l


{-| Navajo
-}
navajo : TimeZone
navajo =
    link "Navajo" america_denver_l


{-| NZ
-}
nz : TimeZone
nz =
    link "NZ" pacific_auckland_l


{-| NZ-CHAT
-}
nz_chat : TimeZone
nz_chat =
    link "NZ-CHAT" pacific_chatham_l


{-| Pacific/Apia
-}
pacific_apia : TimeZone
pacific_apia =
    pacific_apia_l


{-| Pacific/Auckland
-}
pacific_auckland : TimeZone
pacific_auckland =
    pacific_auckland_l


{-| Pacific/Bougainville
-}
pacific_bougainville : TimeZone
pacific_bougainville =
    pacific_bougainville_l


{-| Pacific/Chatham
-}
pacific_chatham : TimeZone
pacific_chatham =
    pacific_chatham_l


{-| Pacific/Chuuk
-}
pacific_chuuk : TimeZone
pacific_chuuk =
    pacific_chuuk_l


{-| Pacific/Easter
-}
pacific_easter : TimeZone
pacific_easter =
    pacific_easter_l


{-| Pacific/Efate
-}
pacific_efate : TimeZone
pacific_efate =
    pacific_efate_l


{-| Pacific/Enderbury
-}
pacific_enderbury : TimeZone
pacific_enderbury =
    pacific_enderbury_l


{-| Pacific/Fakaofo
-}
pacific_fakaofo : TimeZone
pacific_fakaofo =
    pacific_fakaofo_l


{-| Pacific/Fiji
-}
pacific_fiji : TimeZone
pacific_fiji =
    pacific_fiji_l


{-| Pacific/Funafuti
-}
pacific_funafuti : TimeZone
pacific_funafuti =
    pacific_funafuti_l


{-| Pacific/Galapagos
-}
pacific_galapagos : TimeZone
pacific_galapagos =
    pacific_galapagos_l


{-| Pacific/Gambier
-}
pacific_gambier : TimeZone
pacific_gambier =
    pacific_gambier_l


{-| Pacific/Guadalcanal
-}
pacific_guadalcanal : TimeZone
pacific_guadalcanal =
    pacific_guadalcanal_l


{-| Pacific/Guam
-}
pacific_guam : TimeZone
pacific_guam =
    pacific_guam_l


{-| Pacific/Honolulu
-}
pacific_honolulu : TimeZone
pacific_honolulu =
    pacific_honolulu_l


{-| Pacific/Johnston
-}
pacific_johnston : TimeZone
pacific_johnston =
    link "Pacific/Johnston" pacific_honolulu_l


{-| Pacific/Kiritimati
-}
pacific_kiritimati : TimeZone
pacific_kiritimati =
    pacific_kiritimati_l


{-| Pacific/Kosrae
-}
pacific_kosrae : TimeZone
pacific_kosrae =
    pacific_kosrae_l


{-| Pacific/Kwajalein
-}
pacific_kwajalein : TimeZone
pacific_kwajalein =
    pacific_kwajalein_l


{-| Pacific/Majuro
-}
pacific_majuro : TimeZone
pacific_majuro =
    pacific_majuro_l


{-| Pacific/Marquesas
-}
pacific_marquesas : TimeZone
pacific_marquesas =
    pacific_marquesas_l


{-| Pacific/Midway
-}
pacific_midway : TimeZone
pacific_midway =
    link "Pacific/Midway" pacific_pago_pago_l


{-| Pacific/Nauru
-}
pacific_nauru : TimeZone
pacific_nauru =
    pacific_nauru_l


{-| Pacific/Niue
-}
pacific_niue : TimeZone
pacific_niue =
    pacific_niue_l


{-| Pacific/Norfolk
-}
pacific_norfolk : TimeZone
pacific_norfolk =
    pacific_norfolk_l


{-| Pacific/Noumea
-}
pacific_noumea : TimeZone
pacific_noumea =
    pacific_noumea_l


{-| Pacific/Pago\_Pago
-}
pacific_pago_pago : TimeZone
pacific_pago_pago =
    pacific_pago_pago_l


{-| Pacific/Palau
-}
pacific_palau : TimeZone
pacific_palau =
    pacific_palau_l


{-| Pacific/Pitcairn
-}
pacific_pitcairn : TimeZone
pacific_pitcairn =
    pacific_pitcairn_l


{-| Pacific/Pohnpei
-}
pacific_pohnpei : TimeZone
pacific_pohnpei =
    pacific_pohnpei_l


{-| Pacific/Ponape
-}
pacific_ponape : TimeZone
pacific_ponape =
    link "Pacific/Ponape" pacific_pohnpei_l


{-| Pacific/Port\_Moresby
-}
pacific_port_moresby : TimeZone
pacific_port_moresby =
    pacific_port_moresby_l


{-| Pacific/Rarotonga
-}
pacific_rarotonga : TimeZone
pacific_rarotonga =
    pacific_rarotonga_l


{-| Pacific/Saipan
-}
pacific_saipan : TimeZone
pacific_saipan =
    link "Pacific/Saipan" pacific_guam_l


{-| Pacific/Samoa
-}
pacific_samoa : TimeZone
pacific_samoa =
    link "Pacific/Samoa" pacific_pago_pago_l


{-| Pacific/Tahiti
-}
pacific_tahiti : TimeZone
pacific_tahiti =
    pacific_tahiti_l


{-| Pacific/Tarawa
-}
pacific_tarawa : TimeZone
pacific_tarawa =
    pacific_tarawa_l


{-| Pacific/Tongatapu
-}
pacific_tongatapu : TimeZone
pacific_tongatapu =
    pacific_tongatapu_l


{-| Pacific/Truk
-}
pacific_truk : TimeZone
pacific_truk =
    link "Pacific/Truk" pacific_chuuk_l


{-| Pacific/Wake
-}
pacific_wake : TimeZone
pacific_wake =
    pacific_wake_l


{-| Pacific/Wallis
-}
pacific_wallis : TimeZone
pacific_wallis =
    pacific_wallis_l


{-| Pacific/Yap
-}
pacific_yap : TimeZone
pacific_yap =
    link "Pacific/Yap" pacific_chuuk_l


{-| Poland
-}
poland : TimeZone
poland =
    link "Poland" europe_warsaw_l


{-| Portugal
-}
portugal : TimeZone
portugal =
    link "Portugal" europe_lisbon_l


{-| PRC
-}
prc : TimeZone
prc =
    link "PRC" asia_shanghai_l


{-| PST8PDT
-}
pst8pdt : TimeZone
pst8pdt =
    pst8pdt_l


{-| ROC
-}
roc : TimeZone
roc =
    link "ROC" asia_taipei_l


{-| ROK
-}
rok : TimeZone
rok =
    link "ROK" asia_seoul_l


{-| Singapore
-}
singapore : TimeZone
singapore =
    link "Singapore" asia_singapore_l


{-| Turkey
-}
turkey : TimeZone
turkey =
    link "Turkey" europe_istanbul_l


{-| UCT
-}
uct : TimeZone
uct =
    link "UCT" etc_uct_l


{-| Universal
-}
universal : TimeZone
universal =
    link "Universal" etc_utc_l


{-| US/Alaska
-}
us_alaska : TimeZone
us_alaska =
    link "US/Alaska" america_anchorage_l


{-| US/Aleutian
-}
us_aleutian : TimeZone
us_aleutian =
    link "US/Aleutian" america_adak_l


{-| US/Arizona
-}
us_arizona : TimeZone
us_arizona =
    link "US/Arizona" america_phoenix_l


{-| US/Central
-}
us_central : TimeZone
us_central =
    link "US/Central" america_chicago_l


{-| US/East-Indiana
-}
us_east_indiana : TimeZone
us_east_indiana =
    link "US/East-Indiana" america_fort_wayne_l


{-| US/Eastern
-}
us_eastern : TimeZone
us_eastern =
    link "US/Eastern" america_new_york_l


{-| US/Hawaii
-}
us_hawaii : TimeZone
us_hawaii =
    link "US/Hawaii" pacific_honolulu_l


{-| US/Indiana-Starke
-}
us_indiana_starke : TimeZone
us_indiana_starke =
    link "US/Indiana-Starke" america_indiana_knox_l


{-| US/Michigan
-}
us_michigan : TimeZone
us_michigan =
    link "US/Michigan" america_detroit_l


{-| US/Mountain
-}
us_mountain : TimeZone
us_mountain =
    link "US/Mountain" america_denver_l


{-| US/Pacific
-}
us_pacific : TimeZone
us_pacific =
    link "US/Pacific" america_los_angeles_l


{-| US/Pacific-New
-}
us_pacific_new : TimeZone
us_pacific_new =
    link "US/Pacific-New" america_los_angeles_l


{-| US/Samoa
-}
us_samoa : TimeZone
us_samoa =
    link "US/Samoa" pacific_pago_pago_l


{-| UTC
-}
utc : TimeZone
utc =
    link "UTC" etc_utc_l


{-| W-SU
-}
w_su : TimeZone
w_su =
    link "W-SU" europe_moscow_l


{-| WET
-}
wet : TimeZone
wet =
    wet_l


{-| Zulu
-}
zulu : TimeZone
zulu =
    link "Zulu" etc_utc_l



-- Utils
-- -----


{-| A mapping from TimeZone names to their respective functions. Use
this to look up TimeZones by name.
-}
all : Dict String TimeZone
all =
    Dict.fromList <|
        List.concat
            [ [ ( "Africa/Abidjan", africa_abidjan )
              , ( "Africa/Accra", africa_accra )
              , ( "Africa/Addis_Ababa", africa_addis_ababa )
              , ( "Africa/Algiers", africa_algiers )
              , ( "Africa/Asmara", africa_asmara )
              , ( "Africa/Asmera", africa_asmera )
              , ( "Africa/Bamako", africa_bamako )
              , ( "Africa/Bangui", africa_bangui )
              , ( "Africa/Banjul", africa_banjul )
              , ( "Africa/Bissau", africa_bissau )
              , ( "Africa/Blantyre", africa_blantyre )
              , ( "Africa/Brazzaville", africa_brazzaville )
              , ( "Africa/Bujumbura", africa_bujumbura )
              , ( "Africa/Cairo", africa_cairo )
              , ( "Africa/Casablanca", africa_casablanca )
              , ( "Africa/Ceuta", africa_ceuta )
              , ( "Africa/Conakry", africa_conakry )
              , ( "Africa/Dakar", africa_dakar )
              , ( "Africa/Dar_es_Salaam", africa_dar_es_salaam )
              , ( "Africa/Djibouti", africa_djibouti )
              , ( "Africa/Douala", africa_douala )
              , ( "Africa/El_Aaiun", africa_el_aaiun )
              , ( "Africa/Freetown", africa_freetown )
              , ( "Africa/Gaborone", africa_gaborone )
              , ( "Africa/Harare", africa_harare )
              , ( "Africa/Johannesburg", africa_johannesburg )
              , ( "Africa/Juba", africa_juba )
              , ( "Africa/Kampala", africa_kampala )
              , ( "Africa/Khartoum", africa_khartoum )
              , ( "Africa/Kigali", africa_kigali )
              , ( "Africa/Kinshasa", africa_kinshasa )
              , ( "Africa/Lagos", africa_lagos )
              , ( "Africa/Libreville", africa_libreville )
              , ( "Africa/Lome", africa_lome )
              , ( "Africa/Luanda", africa_luanda )
              , ( "Africa/Lubumbashi", africa_lubumbashi )
              , ( "Africa/Lusaka", africa_lusaka )
              , ( "Africa/Malabo", africa_malabo )
              , ( "Africa/Maputo", africa_maputo )
              , ( "Africa/Maseru", africa_maseru )
              , ( "Africa/Mbabane", africa_mbabane )
              , ( "Africa/Mogadishu", africa_mogadishu )
              , ( "Africa/Monrovia", africa_monrovia )
              , ( "Africa/Nairobi", africa_nairobi )
              , ( "Africa/Ndjamena", africa_ndjamena )
              , ( "Africa/Niamey", africa_niamey )
              , ( "Africa/Nouakchott", africa_nouakchott )
              , ( "Africa/Ouagadougou", africa_ouagadougou )
              , ( "Africa/Porto-Novo", africa_porto_novo )
              , ( "Africa/Sao_Tome", africa_sao_tome )
              ]
            , [ ( "Africa/Timbuktu", africa_timbuktu )
              , ( "Africa/Tripoli", africa_tripoli )
              , ( "Africa/Tunis", africa_tunis )
              , ( "Africa/Windhoek", africa_windhoek )
              , ( "America/Adak", america_adak )
              , ( "America/Anchorage", america_anchorage )
              , ( "America/Anguilla", america_anguilla )
              , ( "America/Antigua", america_antigua )
              , ( "America/Araguaina", america_araguaina )
              , ( "America/Argentina/Buenos_Aires", america_argentina_buenos_aires )
              , ( "America/Argentina/Catamarca", america_argentina_catamarca )
              , ( "America/Argentina/ComodRivadavia", america_argentina_comodrivadavia )
              , ( "America/Argentina/Cordoba", america_argentina_cordoba )
              , ( "America/Argentina/Jujuy", america_argentina_jujuy )
              , ( "America/Argentina/La_Rioja", america_argentina_la_rioja )
              , ( "America/Argentina/Mendoza", america_argentina_mendoza )
              , ( "America/Argentina/Rio_Gallegos", america_argentina_rio_gallegos )
              , ( "America/Argentina/Salta", america_argentina_salta )
              , ( "America/Argentina/San_Juan", america_argentina_san_juan )
              , ( "America/Argentina/San_Luis", america_argentina_san_luis )
              , ( "America/Argentina/Tucuman", america_argentina_tucuman )
              , ( "America/Argentina/Ushuaia", america_argentina_ushuaia )
              , ( "America/Aruba", america_aruba )
              , ( "America/Asuncion", america_asuncion )
              , ( "America/Atikokan", america_atikokan )
              , ( "America/Atka", america_atka )
              , ( "America/Bahia", america_bahia )
              , ( "America/Bahia_Banderas", america_bahia_banderas )
              , ( "America/Barbados", america_barbados )
              , ( "America/Belem", america_belem )
              , ( "America/Belize", america_belize )
              , ( "America/Blanc-Sablon", america_blanc_sablon )
              , ( "America/Boa_Vista", america_boa_vista )
              , ( "America/Bogota", america_bogota )
              , ( "America/Boise", america_boise )
              , ( "America/Buenos_Aires", america_buenos_aires )
              , ( "America/Cambridge_Bay", america_cambridge_bay )
              , ( "America/Campo_Grande", america_campo_grande )
              , ( "America/Cancun", america_cancun )
              , ( "America/Caracas", america_caracas )
              , ( "America/Catamarca", america_catamarca )
              , ( "America/Cayenne", america_cayenne )
              , ( "America/Cayman", america_cayman )
              , ( "America/Chicago", america_chicago )
              , ( "America/Chihuahua", america_chihuahua )
              , ( "America/Coral_Harbour", america_coral_harbour )
              , ( "America/Cordoba", america_cordoba )
              , ( "America/Costa_Rica", america_costa_rica )
              , ( "America/Creston", america_creston )
              , ( "America/Cuiaba", america_cuiaba )
              ]
            , [ ( "America/Curacao", america_curacao )
              , ( "America/Danmarkshavn", america_danmarkshavn )
              , ( "America/Dawson", america_dawson )
              , ( "America/Dawson_Creek", america_dawson_creek )
              , ( "America/Denver", america_denver )
              , ( "America/Detroit", america_detroit )
              , ( "America/Dominica", america_dominica )
              , ( "America/Edmonton", america_edmonton )
              , ( "America/Eirunepe", america_eirunepe )
              , ( "America/El_Salvador", america_el_salvador )
              , ( "America/Ensenada", america_ensenada )
              , ( "America/Fort_Nelson", america_fort_nelson )
              , ( "America/Fort_Wayne", america_fort_wayne )
              , ( "America/Fortaleza", america_fortaleza )
              , ( "America/Glace_Bay", america_glace_bay )
              , ( "America/Godthab", america_godthab )
              , ( "America/Goose_Bay", america_goose_bay )
              , ( "America/Grand_Turk", america_grand_turk )
              , ( "America/Grenada", america_grenada )
              , ( "America/Guadeloupe", america_guadeloupe )
              , ( "America/Guatemala", america_guatemala )
              , ( "America/Guayaquil", america_guayaquil )
              , ( "America/Guyana", america_guyana )
              , ( "America/Halifax", america_halifax )
              , ( "America/Havana", america_havana )
              , ( "America/Hermosillo", america_hermosillo )
              , ( "America/Indiana/Indianapolis", america_indiana_indianapolis )
              , ( "America/Indiana/Knox", america_indiana_knox )
              , ( "America/Indiana/Marengo", america_indiana_marengo )
              , ( "America/Indiana/Petersburg", america_indiana_petersburg )
              , ( "America/Indiana/Tell_City", america_indiana_tell_city )
              , ( "America/Indiana/Vevay", america_indiana_vevay )
              , ( "America/Indiana/Vincennes", america_indiana_vincennes )
              , ( "America/Indiana/Winamac", america_indiana_winamac )
              , ( "America/Indianapolis", america_indianapolis )
              , ( "America/Inuvik", america_inuvik )
              , ( "America/Iqaluit", america_iqaluit )
              , ( "America/Jamaica", america_jamaica )
              , ( "America/Jujuy", america_jujuy )
              , ( "America/Juneau", america_juneau )
              , ( "America/Kentucky/Louisville", america_kentucky_louisville )
              , ( "America/Kentucky/Monticello", america_kentucky_monticello )
              , ( "America/Knox_IN", america_knox_in )
              , ( "America/Kralendijk", america_kralendijk )
              , ( "America/La_Paz", america_la_paz )
              , ( "America/Lima", america_lima )
              , ( "America/Los_Angeles", america_los_angeles )
              , ( "America/Louisville", america_louisville )
              , ( "America/Lower_Princes", america_lower_princes )
              , ( "America/Maceio", america_maceio )
              ]
            , [ ( "America/Managua", america_managua )
              , ( "America/Manaus", america_manaus )
              , ( "America/Marigot", america_marigot )
              , ( "America/Martinique", america_martinique )
              , ( "America/Matamoros", america_matamoros )
              , ( "America/Mazatlan", america_mazatlan )
              , ( "America/Mendoza", america_mendoza )
              , ( "America/Menominee", america_menominee )
              , ( "America/Merida", america_merida )
              , ( "America/Metlakatla", america_metlakatla )
              , ( "America/Mexico_City", america_mexico_city )
              , ( "America/Miquelon", america_miquelon )
              , ( "America/Moncton", america_moncton )
              , ( "America/Monterrey", america_monterrey )
              , ( "America/Montevideo", america_montevideo )
              , ( "America/Montreal", america_montreal )
              , ( "America/Montserrat", america_montserrat )
              , ( "America/Nassau", america_nassau )
              , ( "America/New_York", america_new_york )
              , ( "America/Nipigon", america_nipigon )
              , ( "America/Nome", america_nome )
              , ( "America/Noronha", america_noronha )
              , ( "America/North_Dakota/Beulah", america_north_dakota_beulah )
              , ( "America/North_Dakota/Center", america_north_dakota_center )
              , ( "America/North_Dakota/New_Salem", america_north_dakota_new_salem )
              , ( "America/Ojinaga", america_ojinaga )
              , ( "America/Panama", america_panama )
              , ( "America/Pangnirtung", america_pangnirtung )
              , ( "America/Paramaribo", america_paramaribo )
              , ( "America/Phoenix", america_phoenix )
              , ( "America/Port-au-Prince", america_port_au_prince )
              , ( "America/Port_of_Spain", america_port_of_spain )
              , ( "America/Porto_Acre", america_porto_acre )
              , ( "America/Porto_Velho", america_porto_velho )
              , ( "America/Puerto_Rico", america_puerto_rico )
              , ( "America/Rainy_River", america_rainy_river )
              , ( "America/Rankin_Inlet", america_rankin_inlet )
              , ( "America/Recife", america_recife )
              , ( "America/Regina", america_regina )
              , ( "America/Resolute", america_resolute )
              , ( "America/Rio_Branco", america_rio_branco )
              , ( "America/Rosario", america_rosario )
              , ( "America/Santa_Isabel", america_santa_isabel )
              , ( "America/Santarem", america_santarem )
              , ( "America/Santiago", america_santiago )
              , ( "America/Santo_Domingo", america_santo_domingo )
              , ( "America/Sao_Paulo", america_sao_paulo )
              , ( "America/Scoresbysund", america_scoresbysund )
              , ( "America/Shiprock", america_shiprock )
              , ( "America/Sitka", america_sitka )
              ]
            , [ ( "America/St_Barthelemy", america_st_barthelemy )
              , ( "America/St_Johns", america_st_johns )
              , ( "America/St_Kitts", america_st_kitts )
              , ( "America/St_Lucia", america_st_lucia )
              , ( "America/St_Thomas", america_st_thomas )
              , ( "America/St_Vincent", america_st_vincent )
              , ( "America/Swift_Current", america_swift_current )
              , ( "America/Tegucigalpa", america_tegucigalpa )
              , ( "America/Thule", america_thule )
              , ( "America/Thunder_Bay", america_thunder_bay )
              , ( "America/Tijuana", america_tijuana )
              , ( "America/Toronto", america_toronto )
              , ( "America/Tortola", america_tortola )
              , ( "America/Vancouver", america_vancouver )
              , ( "America/Virgin", america_virgin )
              , ( "America/Whitehorse", america_whitehorse )
              , ( "America/Winnipeg", america_winnipeg )
              , ( "America/Yakutat", america_yakutat )
              , ( "America/Yellowknife", america_yellowknife )
              , ( "Antarctica/Casey", antarctica_casey )
              , ( "Antarctica/Davis", antarctica_davis )
              , ( "Antarctica/DumontDUrville", antarctica_dumontdurville )
              , ( "Antarctica/Macquarie", antarctica_macquarie )
              , ( "Antarctica/Mawson", antarctica_mawson )
              , ( "Antarctica/McMurdo", antarctica_mcmurdo )
              , ( "Antarctica/Palmer", antarctica_palmer )
              , ( "Antarctica/Rothera", antarctica_rothera )
              , ( "Antarctica/South_Pole", antarctica_south_pole )
              , ( "Antarctica/Syowa", antarctica_syowa )
              , ( "Antarctica/Troll", antarctica_troll )
              , ( "Antarctica/Vostok", antarctica_vostok )
              , ( "Arctic/Longyearbyen", arctic_longyearbyen )
              , ( "Asia/Aden", asia_aden )
              , ( "Asia/Almaty", asia_almaty )
              , ( "Asia/Amman", asia_amman )
              , ( "Asia/Anadyr", asia_anadyr )
              , ( "Asia/Aqtau", asia_aqtau )
              , ( "Asia/Aqtobe", asia_aqtobe )
              , ( "Asia/Ashgabat", asia_ashgabat )
              , ( "Asia/Ashkhabad", asia_ashkhabad )
              , ( "Asia/Baghdad", asia_baghdad )
              , ( "Asia/Bahrain", asia_bahrain )
              , ( "Asia/Baku", asia_baku )
              , ( "Asia/Bangkok", asia_bangkok )
              , ( "Asia/Barnaul", asia_barnaul )
              , ( "Asia/Beirut", asia_beirut )
              , ( "Asia/Bishkek", asia_bishkek )
              , ( "Asia/Brunei", asia_brunei )
              , ( "Asia/Calcutta", asia_calcutta )
              , ( "Asia/Chita", asia_chita )
              ]
            , [ ( "Asia/Choibalsan", asia_choibalsan )
              , ( "Asia/Chongqing", asia_chongqing )
              , ( "Asia/Chungking", asia_chungking )
              , ( "Asia/Colombo", asia_colombo )
              , ( "Asia/Dacca", asia_dacca )
              , ( "Asia/Damascus", asia_damascus )
              , ( "Asia/Dhaka", asia_dhaka )
              , ( "Asia/Dili", asia_dili )
              , ( "Asia/Dubai", asia_dubai )
              , ( "Asia/Dushanbe", asia_dushanbe )
              , ( "Asia/Famagusta", asia_famagusta )
              , ( "Asia/Gaza", asia_gaza )
              , ( "Asia/Harbin", asia_harbin )
              , ( "Asia/Hebron", asia_hebron )
              , ( "Asia/Ho_Chi_Minh", asia_ho_chi_minh )
              , ( "Asia/Hong_Kong", asia_hong_kong )
              , ( "Asia/Hovd", asia_hovd )
              , ( "Asia/Irkutsk", asia_irkutsk )
              , ( "Asia/Istanbul", asia_istanbul )
              , ( "Asia/Jakarta", asia_jakarta )
              , ( "Asia/Jayapura", asia_jayapura )
              , ( "Asia/Jerusalem", asia_jerusalem )
              , ( "Asia/Kabul", asia_kabul )
              , ( "Asia/Kamchatka", asia_kamchatka )
              , ( "Asia/Karachi", asia_karachi )
              , ( "Asia/Kashgar", asia_kashgar )
              , ( "Asia/Kathmandu", asia_kathmandu )
              , ( "Asia/Katmandu", asia_katmandu )
              , ( "Asia/Khandyga", asia_khandyga )
              , ( "Asia/Kolkata", asia_kolkata )
              , ( "Asia/Krasnoyarsk", asia_krasnoyarsk )
              , ( "Asia/Kuala_Lumpur", asia_kuala_lumpur )
              , ( "Asia/Kuching", asia_kuching )
              , ( "Asia/Kuwait", asia_kuwait )
              , ( "Asia/Macao", asia_macao )
              , ( "Asia/Macau", asia_macau )
              , ( "Asia/Magadan", asia_magadan )
              , ( "Asia/Makassar", asia_makassar )
              , ( "Asia/Manila", asia_manila )
              , ( "Asia/Muscat", asia_muscat )
              , ( "Asia/Nicosia", asia_nicosia )
              , ( "Asia/Novokuznetsk", asia_novokuznetsk )
              , ( "Asia/Novosibirsk", asia_novosibirsk )
              , ( "Asia/Omsk", asia_omsk )
              , ( "Asia/Oral", asia_oral )
              , ( "Asia/Phnom_Penh", asia_phnom_penh )
              , ( "Asia/Pontianak", asia_pontianak )
              , ( "Asia/Pyongyang", asia_pyongyang )
              , ( "Asia/Qatar", asia_qatar )
              , ( "Asia/Qyzylorda", asia_qyzylorda )
              ]
            , [ ( "Asia/Rangoon", asia_rangoon )
              , ( "Asia/Riyadh", asia_riyadh )
              , ( "Asia/Saigon", asia_saigon )
              , ( "Asia/Sakhalin", asia_sakhalin )
              , ( "Asia/Samarkand", asia_samarkand )
              , ( "Asia/Seoul", asia_seoul )
              , ( "Asia/Shanghai", asia_shanghai )
              , ( "Asia/Singapore", asia_singapore )
              , ( "Asia/Srednekolymsk", asia_srednekolymsk )
              , ( "Asia/Taipei", asia_taipei )
              , ( "Asia/Tashkent", asia_tashkent )
              , ( "Asia/Tbilisi", asia_tbilisi )
              , ( "Asia/Tehran", asia_tehran )
              , ( "Asia/Tel_Aviv", asia_tel_aviv )
              , ( "Asia/Thimbu", asia_thimbu )
              , ( "Asia/Thimphu", asia_thimphu )
              , ( "Asia/Tokyo", asia_tokyo )
              , ( "Asia/Tomsk", asia_tomsk )
              , ( "Asia/Ujung_Pandang", asia_ujung_pandang )
              , ( "Asia/Ulaanbaatar", asia_ulaanbaatar )
              , ( "Asia/Ulan_Bator", asia_ulan_bator )
              , ( "Asia/Urumqi", asia_urumqi )
              , ( "Asia/Ust-Nera", asia_ust_nera )
              , ( "Asia/Vientiane", asia_vientiane )
              , ( "Asia/Vladivostok", asia_vladivostok )
              , ( "Asia/Yakutsk", asia_yakutsk )
              , ( "Asia/Yangon", asia_yangon )
              , ( "Asia/Yekaterinburg", asia_yekaterinburg )
              , ( "Asia/Yerevan", asia_yerevan )
              , ( "Atlantic/Azores", atlantic_azores )
              , ( "Atlantic/Bermuda", atlantic_bermuda )
              , ( "Atlantic/Canary", atlantic_canary )
              , ( "Atlantic/Cape_Verde", atlantic_cape_verde )
              , ( "Atlantic/Faeroe", atlantic_faeroe )
              , ( "Atlantic/Faroe", atlantic_faroe )
              , ( "Atlantic/Jan_Mayen", atlantic_jan_mayen )
              , ( "Atlantic/Madeira", atlantic_madeira )
              , ( "Atlantic/Reykjavik", atlantic_reykjavik )
              , ( "Atlantic/South_Georgia", atlantic_south_georgia )
              , ( "Atlantic/St_Helena", atlantic_st_helena )
              , ( "Atlantic/Stanley", atlantic_stanley )
              , ( "Australia/ACT", australia_act )
              , ( "Australia/Adelaide", australia_adelaide )
              , ( "Australia/Brisbane", australia_brisbane )
              , ( "Australia/Broken_Hill", australia_broken_hill )
              , ( "Australia/Canberra", australia_canberra )
              , ( "Australia/Currie", australia_currie )
              , ( "Australia/Darwin", australia_darwin )
              , ( "Australia/Eucla", australia_eucla )
              , ( "Australia/Hobart", australia_hobart )
              ]
            , [ ( "Australia/LHI", australia_lhi )
              , ( "Australia/Lindeman", australia_lindeman )
              , ( "Australia/Lord_Howe", australia_lord_howe )
              , ( "Australia/Melbourne", australia_melbourne )
              , ( "Australia/North", australia_north )
              , ( "Australia/NSW", australia_nsw )
              , ( "Australia/Perth", australia_perth )
              , ( "Australia/Queensland", australia_queensland )
              , ( "Australia/South", australia_south )
              , ( "Australia/Sydney", australia_sydney )
              , ( "Australia/Tasmania", australia_tasmania )
              , ( "Australia/Victoria", australia_victoria )
              , ( "Australia/West", australia_west )
              , ( "Australia/Yancowinna", australia_yancowinna )
              , ( "Brazil/Acre", brazil_acre )
              , ( "Brazil/DeNoronha", brazil_denoronha )
              , ( "Brazil/East", brazil_east )
              , ( "Brazil/West", brazil_west )
              , ( "Canada/Atlantic", canada_atlantic )
              , ( "Canada/Central", canada_central )
              , ( "Canada/East-Saskatchewan", canada_east_saskatchewan )
              , ( "Canada/Eastern", canada_eastern )
              , ( "Canada/Mountain", canada_mountain )
              , ( "Canada/Newfoundland", canada_newfoundland )
              , ( "Canada/Pacific", canada_pacific )
              , ( "Canada/Saskatchewan", canada_saskatchewan )
              , ( "Canada/Yukon", canada_yukon )
              , ( "CET", cet )
              , ( "Chile/Continental", chile_continental )
              , ( "Chile/EasterIsland", chile_easterisland )
              , ( "CST6CDT", cst6cdt )
              , ( "Cuba", cuba )
              , ( "EET", eet )
              , ( "Egypt", egypt )
              , ( "Eire", eire )
              , ( "EST", est )
              , ( "EST5EDT", est5edt )
              , ( "Etc/GMT", etc_gmt )
              , ( "Etc/GMT0", etc_gmt_0 )
              , ( "Etc/GMT-0", etc_gmt_minus_0 )
              , ( "Etc/GMT-1", etc_gmt_minus_1 )
              , ( "Etc/GMT-10", etc_gmt_minus_10 )
              , ( "Etc/GMT-11", etc_gmt_minus_11 )
              , ( "Etc/GMT-12", etc_gmt_minus_12 )
              , ( "Etc/GMT-13", etc_gmt_minus_13 )
              , ( "Etc/GMT-14", etc_gmt_minus_14 )
              , ( "Etc/GMT-2", etc_gmt_minus_2 )
              , ( "Etc/GMT-3", etc_gmt_minus_3 )
              , ( "Etc/GMT-4", etc_gmt_minus_4 )
              , ( "Etc/GMT-5", etc_gmt_minus_5 )
              ]
            , [ ( "Etc/GMT-6", etc_gmt_minus_6 )
              , ( "Etc/GMT-7", etc_gmt_minus_7 )
              , ( "Etc/GMT-8", etc_gmt_minus_8 )
              , ( "Etc/GMT-9", etc_gmt_minus_9 )
              , ( "Etc/GMT+0", etc_gmt_plus_0 )
              , ( "Etc/GMT+1", etc_gmt_plus_1 )
              , ( "Etc/GMT+10", etc_gmt_plus_10 )
              , ( "Etc/GMT+11", etc_gmt_plus_11 )
              , ( "Etc/GMT+12", etc_gmt_plus_12 )
              , ( "Etc/GMT+2", etc_gmt_plus_2 )
              , ( "Etc/GMT+3", etc_gmt_plus_3 )
              , ( "Etc/GMT+4", etc_gmt_plus_4 )
              , ( "Etc/GMT+5", etc_gmt_plus_5 )
              , ( "Etc/GMT+6", etc_gmt_plus_6 )
              , ( "Etc/GMT+7", etc_gmt_plus_7 )
              , ( "Etc/GMT+8", etc_gmt_plus_8 )
              , ( "Etc/GMT+9", etc_gmt_plus_9 )
              , ( "Etc/Greenwich", etc_greenwich )
              , ( "Etc/UCT", etc_uct )
              , ( "Etc/Universal", etc_universal )
              , ( "Etc/UTC", etc_utc )
              , ( "Etc/Zulu", etc_zulu )
              , ( "Europe/Amsterdam", europe_amsterdam )
              , ( "Europe/Andorra", europe_andorra )
              , ( "Europe/Astrakhan", europe_astrakhan )
              , ( "Europe/Athens", europe_athens )
              , ( "Europe/Belfast", europe_belfast )
              , ( "Europe/Belgrade", europe_belgrade )
              , ( "Europe/Berlin", europe_berlin )
              , ( "Europe/Bratislava", europe_bratislava )
              , ( "Europe/Brussels", europe_brussels )
              , ( "Europe/Bucharest", europe_bucharest )
              , ( "Europe/Budapest", europe_budapest )
              , ( "Europe/Busingen", europe_busingen )
              , ( "Europe/Chisinau", europe_chisinau )
              , ( "Europe/Copenhagen", europe_copenhagen )
              , ( "Europe/Dublin", europe_dublin )
              , ( "Europe/Gibraltar", europe_gibraltar )
              , ( "Europe/Guernsey", europe_guernsey )
              , ( "Europe/Helsinki", europe_helsinki )
              , ( "Europe/Isle_of_Man", europe_isle_of_man )
              , ( "Europe/Istanbul", europe_istanbul )
              , ( "Europe/Jersey", europe_jersey )
              , ( "Europe/Kaliningrad", europe_kaliningrad )
              , ( "Europe/Kiev", europe_kiev )
              , ( "Europe/Kirov", europe_kirov )
              , ( "Europe/Lisbon", europe_lisbon )
              , ( "Europe/Ljubljana", europe_ljubljana )
              , ( "Europe/London", europe_london )
              , ( "Europe/Luxembourg", europe_luxembourg )
              ]
            , [ ( "Europe/Madrid", europe_madrid )
              , ( "Europe/Malta", europe_malta )
              , ( "Europe/Mariehamn", europe_mariehamn )
              , ( "Europe/Minsk", europe_minsk )
              , ( "Europe/Monaco", europe_monaco )
              , ( "Europe/Moscow", europe_moscow )
              , ( "Europe/Nicosia", europe_nicosia )
              , ( "Europe/Oslo", europe_oslo )
              , ( "Europe/Paris", europe_paris )
              , ( "Europe/Podgorica", europe_podgorica )
              , ( "Europe/Prague", europe_prague )
              , ( "Europe/Riga", europe_riga )
              , ( "Europe/Rome", europe_rome )
              , ( "Europe/Samara", europe_samara )
              , ( "Europe/San_Marino", europe_san_marino )
              , ( "Europe/Sarajevo", europe_sarajevo )
              , ( "Europe/Simferopol", europe_simferopol )
              , ( "Europe/Skopje", europe_skopje )
              , ( "Europe/Sofia", europe_sofia )
              , ( "Europe/Stockholm", europe_stockholm )
              , ( "Europe/Tallinn", europe_tallinn )
              , ( "Europe/Tirane", europe_tirane )
              , ( "Europe/Tiraspol", europe_tiraspol )
              , ( "Europe/Ulyanovsk", europe_ulyanovsk )
              , ( "Europe/Uzhgorod", europe_uzhgorod )
              , ( "Europe/Vaduz", europe_vaduz )
              , ( "Europe/Vatican", europe_vatican )
              , ( "Europe/Vienna", europe_vienna )
              , ( "Europe/Vilnius", europe_vilnius )
              , ( "Europe/Volgograd", europe_volgograd )
              , ( "Europe/Warsaw", europe_warsaw )
              , ( "Europe/Zagreb", europe_zagreb )
              , ( "Europe/Zaporozhye", europe_zaporozhye )
              , ( "Europe/Zurich", europe_zurich )
              , ( "GB", gb )
              , ( "GB-Eire", gb_eire )
              , ( "GMT", gmt )
              , ( "GMT0", gmt_0 )
              , ( "GMT-0", gmt_minus_0 )
              , ( "GMT+0", gmt_plus_0 )
              , ( "Greenwich", greenwich )
              , ( "Hongkong", hongkong )
              , ( "HST", hst )
              , ( "Iceland", iceland )
              , ( "Indian/Antananarivo", indian_antananarivo )
              , ( "Indian/Chagos", indian_chagos )
              , ( "Indian/Christmas", indian_christmas )
              , ( "Indian/Cocos", indian_cocos )
              , ( "Indian/Comoro", indian_comoro )
              , ( "Indian/Kerguelen", indian_kerguelen )
              ]
            , [ ( "Indian/Mahe", indian_mahe )
              , ( "Indian/Maldives", indian_maldives )
              , ( "Indian/Mauritius", indian_mauritius )
              , ( "Indian/Mayotte", indian_mayotte )
              , ( "Indian/Reunion", indian_reunion )
              , ( "Iran", iran )
              , ( "Israel", israel )
              , ( "Jamaica", jamaica )
              , ( "Japan", japan )
              , ( "Kwajalein", kwajalein )
              , ( "Libya", libya )
              , ( "MET", met )
              , ( "Mexico/BajaNorte", mexico_bajanorte )
              , ( "Mexico/BajaSur", mexico_bajasur )
              , ( "Mexico/General", mexico_general )
              , ( "MST", mst )
              , ( "MST7MDT", mst7mdt )
              , ( "Navajo", navajo )
              , ( "NZ", nz )
              , ( "NZ-CHAT", nz_chat )
              , ( "Pacific/Apia", pacific_apia )
              , ( "Pacific/Auckland", pacific_auckland )
              , ( "Pacific/Bougainville", pacific_bougainville )
              , ( "Pacific/Chatham", pacific_chatham )
              , ( "Pacific/Chuuk", pacific_chuuk )
              , ( "Pacific/Easter", pacific_easter )
              , ( "Pacific/Efate", pacific_efate )
              , ( "Pacific/Enderbury", pacific_enderbury )
              , ( "Pacific/Fakaofo", pacific_fakaofo )
              , ( "Pacific/Fiji", pacific_fiji )
              , ( "Pacific/Funafuti", pacific_funafuti )
              , ( "Pacific/Galapagos", pacific_galapagos )
              , ( "Pacific/Gambier", pacific_gambier )
              , ( "Pacific/Guadalcanal", pacific_guadalcanal )
              , ( "Pacific/Guam", pacific_guam )
              , ( "Pacific/Honolulu", pacific_honolulu )
              , ( "Pacific/Johnston", pacific_johnston )
              , ( "Pacific/Kiritimati", pacific_kiritimati )
              , ( "Pacific/Kosrae", pacific_kosrae )
              , ( "Pacific/Kwajalein", pacific_kwajalein )
              , ( "Pacific/Majuro", pacific_majuro )
              , ( "Pacific/Marquesas", pacific_marquesas )
              , ( "Pacific/Midway", pacific_midway )
              , ( "Pacific/Nauru", pacific_nauru )
              , ( "Pacific/Niue", pacific_niue )
              , ( "Pacific/Norfolk", pacific_norfolk )
              , ( "Pacific/Noumea", pacific_noumea )
              , ( "Pacific/Pago_Pago", pacific_pago_pago )
              , ( "Pacific/Palau", pacific_palau )
              , ( "Pacific/Pitcairn", pacific_pitcairn )
              ]
            , [ ( "Pacific/Pohnpei", pacific_pohnpei )
              , ( "Pacific/Ponape", pacific_ponape )
              , ( "Pacific/Port_Moresby", pacific_port_moresby )
              , ( "Pacific/Rarotonga", pacific_rarotonga )
              , ( "Pacific/Saipan", pacific_saipan )
              , ( "Pacific/Samoa", pacific_samoa )
              , ( "Pacific/Tahiti", pacific_tahiti )
              , ( "Pacific/Tarawa", pacific_tarawa )
              , ( "Pacific/Tongatapu", pacific_tongatapu )
              , ( "Pacific/Truk", pacific_truk )
              , ( "Pacific/Wake", pacific_wake )
              , ( "Pacific/Wallis", pacific_wallis )
              , ( "Pacific/Yap", pacific_yap )
              , ( "Poland", poland )
              , ( "Portugal", portugal )
              , ( "PRC", prc )
              , ( "PST8PDT", pst8pdt )
              , ( "ROC", roc )
              , ( "ROK", rok )
              , ( "Singapore", singapore )
              , ( "Turkey", turkey )
              , ( "UCT", uct )
              , ( "Universal", universal )
              , ( "US/Alaska", us_alaska )
              , ( "US/Aleutian", us_aleutian )
              , ( "US/Arizona", us_arizona )
              , ( "US/Central", us_central )
              , ( "US/East-Indiana", us_east_indiana )
              , ( "US/Eastern", us_eastern )
              , ( "US/Hawaii", us_hawaii )
              , ( "US/Indiana-Starke", us_indiana_starke )
              , ( "US/Michigan", us_michigan )
              , ( "US/Mountain", us_mountain )
              , ( "US/Pacific", us_pacific )
              , ( "US/Pacific-New", us_pacific_new )
              , ( "US/Samoa", us_samoa )
              , ( "UTC", utc )
              , ( "W-SU", w_su )
              , ( "WET", wet )
              , ( "Zulu", zulu )
              ]
            ]


{-| Look up a TimeZone by name.
-}
fromName : String -> Maybe TimeZone
fromName name =
    Dict.get name all
