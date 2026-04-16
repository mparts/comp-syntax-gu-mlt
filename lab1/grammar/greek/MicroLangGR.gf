--# -path=.:../abstract
concrete MicroLangGR of MicroLang = open MicroResGR, Prelude in {

-----------------------------------------------------
---------------- Grammar part -----------------------
-----------------------------------------------------

  lincat
  -- 

  lin
  -- 

-----------------------------------------------------
---------------- Lexicon part -----------------------
-----------------------------------------------------

lin already_Adv = mkAdv "ήδη" ;
lin animal_N = mkN "ζώο" ;
lin apple_N = mkN "μήλο" ;
lin baby_N = mkN "μωρό" ;
lin bad_A = mkA "κακό" ;
lin beer_N = mkN "μπύρα" ;
lin big_A = mkA "μεγάλο" ;
lin bike_N = mkN "ποδηλάτο" ;
lin bird_N = mkN "πουλί" ;
lin black_A = mkA "μαύρο" ;
lin blood_N = mkN "αίμα" ;
lin blue_A = mkA "μπλε" ;
lin boat_N = mkN "πλοίο" ;
lin book_N = mkN "βιβλίο" ;
lin boy_N = mkN "αγόρι" ;
lin bread_N = mkN "ψωμί" ;
-- lin break_V2 = mkV2 (mkV "break" "broke" "broken") ;
-- lin buy_V2 = mkV2 (mkV "buy" "bought" "bought") ;
lin car_N = mkN "αυτοκίνητο" ;
lin cat_N = mkN "γάτα" ;
lin child_N = mkN "παιδί" "παιδιά" ;
lin city_N = mkN "πόλη" ;
lin clean_A = mkA "καθαρό" ;
lin clever_A = mkA "έξυπνο" ;
lin cloud_N = mkN "σύννεφο" ;
lin cold_A = mkA "κρύο" ;
-- lin come_V = mkV "έρχομαι" "ήλθα" "έρχομαι" ;
lin computer_N = mkN "υπολογιστής" ;
lin cow_N = mkN "αγελάδα" ;
lin dirty_A = mkA "βρόμικο" ;
lin dog_N = mkN "σκύλος" ;
-- lin drink_V2 = mkV2 (mkV "πίνω" "έπια" "πιεν" ) ;
-- lin eat_V2 = mkV2 (mkV "τρώω" "έφαγα" "φαγμένο" ) ;
-- lin find_V2 = mkV2 (mkV "βρίσκω" "βρήκα" "βρεθέν" ) ;
lin fire_N = mkN "φωτιά" ;
lin fish_N = mkN "ψάρι" ;
lin flower_N = mkN "λουλούδι" ;
lin friend_N = mkN "φίλος" ;
lin girl_N = mkN "κορίτσι" ;
lin good_A = mkA "καλό" ;
-- lin go_V = mkV "go" "went" "gone" ;
lin grammar_N = mkN "γραμματική" ;
lin green_A = mkA "πράσινο" ;
lin heavy_A = mkA "βαρύ" ;
lin horse_N = mkN "άλογο" ;
lin hot_A = mkA "ζεστό" ;
lin house_N = mkN "σπίτι" ;
-- lin john_PN = mkPN "John" ;
-- lin jump_V = mkV "jump" ;
-- lin kill_V2 = mkV2 "kill" ;
-- lin know_VS = mkVS (mkV "know" "knew" "known") ;
lin language_N = mkN "γλώσσα" ;
-- lin live_V = mkV "ζω" ;
-- lin love_V2 = mkV2 (mkV "αγαπώ") ;
lin man_N = mkN "άνδρας" "άνδρες" ;
lin milk_N = mkN "γάλα" ;
lin music_N = mkN "μουσική" ;
lin new_A = mkA "νέο" ;
lin now_Adv = mkAdv "τώρα" ;
lin old_A = mkA "παλιό" ;
-- lin paris_PN = mkPN "Paris" ;
lin play_V = mkV "παίζω" ;
-- lin read_V2 = mkV2 (mkV "διαβάζω" "διάβασα" "διαβασμένο" ) ;
lin ready_A = mkA "έτοιμο" ;
lin red_A = mkA "κόκκινο" ;
lin river_N = mkN "ποταμός" ;
-- lin run_V = mkV "τρέχω" "τρέξα" "τρεχόν" ;
lin sea_N = mkN "θάλασσα" ;
-- lin see_V2 = mkV2 (mkV "βλέπω" "είδα" "δεδομένο" ) ;
lin ship_N = mkN "πλοίο" ;
-- lin sleep_V = mkV "κοιμάμαι" "κοιμήθηκα" "κοιμημένο" ;
lin small_A = mkA "μικρό" ;
lin star_N = mkN "αστέρι" ;
-- lin swim_V = mkV "δείχνω" "διώξα" "διωγμένο" ;
-- lin teach_V2 = mkV2 (mkV "διδάσκω" "δίδαξα" "διδαχθέν" ) ;
lin train_N = mkN "τρένο" ;
-- lin travel_V = mkV "ταξιδεύω" ;
lin tree_N = mkN "δέντρο" ;
-- lin understand_V2 = mkV2 (mkV "κατανοώ" "κατάνοησα" "κατανοημένο") ;
-- lin wait_V2 = mkV2 "περιμένω" "για" ;
-- lin walk_V = mkV "περπατώ" ;
lin warm_A = mkA "ζεστό" ;
lin water_N = mkN "νερό" ;
lin white_A = mkA "λευκό" ;
lin wine_N = mkN "κρασί" ;
lin woman_N = mkN "γυναίκα" "γυναίκες" ;
lin yellow_A = mkA "κίτρινο" ;
lin young_A = mkA "νέος" ;

---------------------------
-- Paradigms part ---------
---------------------------

oper
  mkN = overload {
    mkN : Str -> Noun   -- predictable noun, e.g. car-cars, boy-boys, fly-flies, bush-bushes
      = \n -> lin N (smartNoun n) ;
    mkN : Str -> Str -> Noun  -- irregular noun, e.g. man-men
      = \sg,pl -> lin N (mkNoun sg pl) ;
    } ;

  mkA : Str -> A
    = \s -> lin A {s = s} ;

  mkV = overload {
    mkV : (inf : Str) -> V  -- predictable verb, e.g. play-plays, cry-cries, wash-washes
      = \s -> lin V (smartVerb s) ;
    mkV : (inf,pres,part : Str) -> V  -- irregular verb, e.g. drink-drank-drunk
      = \inf,pres,part -> lin V (irregVerb inf pres part) ;
    } ;

  mkV2 = overload {
    mkV2 : Str -> V2          -- predictable verb with direct object, e.g. "wash"
      = \s   -> lin V2 (smartVerb s ** {c = []}) ;
    mkV2 : Str  -> Str -> V2  -- predictable verb with preposition, e.g. "wait - for"
      = \s,p -> lin V2 (smartVerb s ** {c = p}) ;
    mkV2 : V -> V2            -- any verb with direct object, e.g. "drink"
      = \v   -> lin V2 (v ** {c = []}) ;
    mkV2 : V -> Str -> V2     -- any verb with preposition
      = \v,p -> lin V2 (v ** {c = p}) ;
    } ;

  mkAdv : Str -> Adv
    = \s -> lin Adv {s = s} ;
  
  mkPrep : Str -> Prep
    = \s -> lin Prep {s = s} ;

}
