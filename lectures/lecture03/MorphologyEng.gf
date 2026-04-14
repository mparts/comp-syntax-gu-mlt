resource MorphologyEng = {

-- to use:
--  i -retain MorphologyEng.gf
--  cc -table dog_N

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


-- lexicon

  dog_N = mkNoun "dog" "dogs" ;
  girl_N = mkNoun "girl" "girls" ;
  house_N = regNoun "house" ;




}