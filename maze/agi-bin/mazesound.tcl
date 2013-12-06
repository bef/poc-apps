array set sounds {}
switch $language {
	en {
		set sounds(intro) {
			{yintro maze/en/init/welcome maze/en/init/begin maze/en/init/skip_help maze/en/init/help maze/en/init/fnord}
			}
		set sounds(help) {
			{maze/en/init/help}
			}
		set sounds(level_intro) {
			{maze/en/init/commence_level}
			}
		set sounds(stair_error_UP) {
			{maze/en/stairs/go_up}
			{maze/en/stairs/try_sth_else}
			}
		set sounds(stair_error_DOWN) {
			{maze/en/stairs/go_down}
			{maze/en/stairs/try_sth_else}
			}
		set sounds(wall_error) {
			{maze/en/wall/ran_into}
			{maze/en/wall/ouch}
			{maze/en/wall/any_further}
			{maze/en/wall/change_dir}
			{maze/en/wall/no_way_ahead}
			{maze/en/wall/solid_matter}
			{maze/en/wall/suicide}
			{maze/en/wall/not_here}
			}
		set sounds(enter_level_nr) {
			{maze/en/level/enter_level}
			}
		set sounds(enter_level_code) {
			{maze/en/level/enter_code}
			}
		set sounds(level_code_incorrect) {
			{maze/en/level/incorrect_code}
			{maze/en/level/guess_incorrect}
			{maze/en/level/not_meant_for_you}
			{maze/en/level/wrong_code}
			}
		set sounds(level_code) {
			{{maze/en/level/level_code} {maze/en/level/is}}
			}
		set sounds(exit_entrance) {
			{maze/en/exit/found_entrance}
			{maze/en/exit/entrance}
			}
		set sounds(compass_NORTH) {
			{maze/en/compass/headed_north}
			{maze/en/compass/compass_north}
			{maze/en/compass/north}
			}
		set sounds(compass_SOUTH) {
			{maze/en/compass/headed_south}
			{maze/en/compass/compass_south}
			{maze/en/compass/south}
			}
		set sounds(compass_EAST) {
			{maze/en/compass/headed_east}
			{maze/en/compass/compass_east}
			{maze/en/compass/east}
			}
		set sounds(compass_WEST) {
			{maze/en/compass/headed_west}
			{maze/en/compass/compass_west}
			{maze/en/compass/west}
			}
		set sounds(level_succeeded) {
			{maze/en/exit/well_done}
			{maze/en/exit/congratulations}
			}
		set sounds(imp_wall) {
			{maze/en/impression/facing_wall}
			}
		set sounds(imp_free) {
			{maze/en/impression/move_on}
			}
		set sounds(imp_intersect) {
			{maze/en/impression/move_or_turn}
			}
		set sounds(game_over) {
			{/home/bef/sounds/blaue_berge}
			{{/home/bef/sounds/Zimmer Frei Theme_telefon}}
			}

	}
	de -
	default {
		set sounds(intro) {
			{beep maze/de/init/willkommen maze/de/init/skip_intro maze/de/init/help}
			{beep maze/de/init/hallo_erstmal maze/de/init/herumlaber maze/de/init/help2 maze/de/init/viel_spass}
			}
		set sounds(help) {
			{maze/de/init/help}
			{maze/de/init/help2}
			}
		set sounds(level_intro) {
			{maze/de/init/level}
			{maze/de/init/beginnt_level}
			}
		set sounds(stair_error_UP) {
			{maze/de/stairs/keine_treppe_nach_oben}
			{maze/de/stairs/nicht_hoch}
			}
		set sounds(stair_error_DOWN) {
			{maze/de/stairs/keine_treppe_nach_unten}
			{maze/de/stairs/nicht_runter}
			}
		set sounds(wall_error) {
			{maze/de/wall/hier_gehts_nicht_weiter}
			{maze/de/wall/au}
			{maze/de/wall/richtungsaenderung}
			{maze/de/wall/andere_tasten}
			{maze/de/wall/umdreh}
			{maze/de/wall/nicht_weiter}
			{maze/de/wall/durch_waende}
			{maze/de/wall/mauer}
			{maze/de/wall/versuch_kein_durchgang}
			{maze/de/wall/kein_durchgang}
			{maze/de/wall/nicht_durch_wand}
			{maze/de/wall/solide_mauer}
			}
		set sounds(enter_level_nr) {
			{maze/de/level/eingabe_level}
			{maze/de/level/eingabe_level2}
			}
		set sounds(enter_level_code) {
			{maze/de/level/eingabe_code}
			{maze/de/level/eingabe_code2}
		}
		set sounds(level_code_incorrect) {
			{maze/de/level/code_falsch}
			{maze/de/level/code_nicht_korrekt}
			{maze/de/level/verwaehlt}
			}
		set sounds(level_code) {
			{{maze/de/level/der_levelcode} {maze/de/level/ist}}
			{{maze/de/level/level} {maze/de/level/hat_den_code}}
			{{maze/de/level/in_level} {maze/de/level/code_lautet}}
			}
		set sounds(exit_entrance) {
			{maze/de/exit/eingang_gefunden}
			{maze/de/exit/eingang_geflohen}
			{maze/de/exit/war_eingang}
			}
		set sounds(compass_NORTH) {
			{maze/de/compass/blick_norden}
			{maze/de/compass/kompass_norden}
			{maze/de/compass/norden}
			{maze/de/compass/richtung_norwegen}
			}
		set sounds(compass_SOUTH) {
			{maze/de/compass/blick_sueden}
			{maze/de/compass/kompass_sueden}
			{maze/de/compass/sueden}
			{maze/de/compass/richtung_italien}
			}
		set sounds(compass_EAST) {
			{maze/de/compass/blick_osten}
			{maze/de/compass/kompass_osten}
			{maze/de/compass/osten}
			{maze/de/compass/richtung_polen}
			}
		set sounds(compass_WEST) {
			{maze/de/compass/blick_westen}
			{maze/de/compass/kompass_westen}
			{maze/de/compass/westen}
			{maze/de/compass/richtung_frankreich}
			}
		set sounds(level_succeeded) {
			{maze/de/exit/geschafft}
			{maze/de/exit/jaa}
			{maze/de/exit/na_endlich}
			{maze/de/exit/kinderspiel}
			{maze/de/exit/kein_aufwand}
			{maze/de/exit/toe_toe}
			}
		set sounds(imp_wall) {
			{maze/de/impression/wand}
			{maze/de/impression/wand2}
			}
		set sounds(imp_free) {
			{maze/de/impression/geradeaus}
			{maze/de/impression/geh_weiter}
			{maze/de/impression/frei}
			}
		set sounds(imp_intersect) {
			{maze/de/impression/abzweigung}
			{maze/de/impression/koenntest_abbiegen}
			}
		set sounds(game_over) {
			{/home/bef/sounds/blaue_berge}
			{{/home/bef/sounds/Zimmer Frei Theme_telefon}}
			}
	}
}
