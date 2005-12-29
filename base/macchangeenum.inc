{ okresl symbole CHANGE_ENUM_TYPE (typ enumerated),
  CHANGE_ENUM_NAME (nazwa zmiennej typu CHANGE_ENUM_TYPE),
  CHANGE_ENUM_CHANGE (integer (nazwa zmiennej lub stala))
  
  potem uzyj makra CHANGE_ENUM aby uzyskac wartosc zmiennej CHANGE_ENUM_NAME
  zwiekszonej o CHANGE_ENUM_CHANGE cyklicznie (tzn. nie zwiekszy samej zmiennej
  tylko zwroci odpowiednia wartosc !)

  (tzn. bedzie sie odpowiednio przewijac, CHANGE_ENUM_CHANGE moze byc dowolnie
  duze i dowolnie male (w szczegolnosci, moze tez byc dowolne ujemne)). }

{$define CHANGE_ENUM:=
  (* dodatkowe nawiasy naokolo dla pewnosci *)
  (
    CHANGE_ENUM_TYPE( ChangeIntCycle(Ord(CHANGE_ENUM_NAME), CHANGE_ENUM_CHANGE,
      Ord(High(CHANGE_ENUM_NAME)) ))
  )
}

