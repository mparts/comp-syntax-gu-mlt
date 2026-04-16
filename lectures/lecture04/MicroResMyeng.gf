resource MicroResMyeng = {

------------------------------
-- API: overloaded paradigms

oper
  mkN = overload {
    mkN : (baby : Str) -> Noun
      = \baby -> smartNoun baby ;
    mkN : (man, men : Str) -> Noun
      = \man, men -> mkNoun man men ;
    } ;

  mkA : Str -> Adjective
    = \adj -> {s = adj} ;
    
  mkAdv : Str -> Adverb
    = \adv -> {s = adv} ;

  mkV = overload {
    mkV : (try : Str) -> Verb
      = \try -> smartVerb try ;
    mkV : (go, went, gone : Str) -> Verb
      = \go, went, gone -> irregVerb go went gone ;
    mkV : (sing, sings, sang, sung, singing : Str) -> Verb
     = \sing, sings, sang, sung, singing ->
       mkVerb sing sings sang sung singing ;
    } ;

  mkV2 = overload {
    mkV2 : (kill : Str) -> Verb2
      = \kill -> mkV kill ** {prep = ""} ;
    mkV2 : (wait, for : Str) -> Verb2
      = \wait, for -> mkV wait ** {prep = for} ;
    mkV2 : Verb -> Verb2
      = \verb -> verb ** {prep = ""} ;
    } ;
    
  

------------------------------
param
  Number = Sg | Pl ;

  VerbForm = Inf | Pres3Sg | Past | PastPart | PresPart ;

oper
  Noun : Type = {s : Number => Str} ;

  -- constructor
  mkNoun : (dog, dogs : Str) -> Noun
    = \dog, dogs -> {
      s = table {Sg => dog ; Pl => dogs}
      } ;

  regNoun : (dog : Str) -> Noun
    = \dog -> mkNoun dog (dog + "s") ;

  smartNoun : (noun : Str) -> Noun
    = \noun -> case noun of {
       b + ("a" | "e" | "o" | "u") + "y" => regNoun noun ;
       bab + "y" => mkNoun noun (bab + "ies") ;
       _ => regNoun noun
      } ;

  Adjective : Type = {s : Str} ;

  Adverb : Type = {s : Str} ;

  Verb : Type = {s : VerbForm => Str} ;
  
  -- constructor; worst case paradigm
  mkVerb : (sing, sings, sang, sung, singing : Str) -> Verb
     = \sing, sings, sang, sung, singing -> {
       s = table {
         Inf => sing ;
	 Pres3Sg => sings ;
	 Past => sang ;
	 PastPart => sung ;
	 PresPart => singing
         }
       } ;

   regVerb : (walk : Str) -> Verb
     = \walk ->
       mkVerb walk (walk + "s") (walk + "ed")
                   (walk + "ed")  (walk + "ing") ;

   smartVerb : (verb : Str) -> Verb
     = \verb -> case verb of {
         b + ("a" | "e" | "o" | "u") + "y" => regVerb verb ;
	 cr + "y" => mkVerb verb (cr + "ies")
	               (cr + "ied") (cr + "ied") (cr + "ying") ;
         refer + "ee" => let refereed = refer + "eed" in
	             mkVerb verb (verb + "s") refereed refereed (verb + "ing") ;
         us + "e" => let used = us + "ed" in
	             mkVerb verb (verb + "s") used used (us + "ing") ;
         wa + ("ch" | "sh" | "s" | "z" | "x") =>
	     mkVerb verb (verb + "es") (verb + "ed") (verb + "ed")
	       (verb + "ing") ; 
         _ => regVerb verb
         } ;

    irregVerb : (sing, sang, sung : Str) -> Verb
      = \sing, sang, sung -> {s =
        table {
	  Past => sang ;
	  PastPart => sung ;
	  x => (smartVerb sing).s ! x
	  }
	} ;

  Verb2 : Type = {s : VerbForm => Str ; prep : Str} ;

}