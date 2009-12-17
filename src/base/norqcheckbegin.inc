{ Uzywaj klamer NoRQCheck Begin/End aby zawierac w nich kod ktory musi 
  byc wylaczony z kontroli RangeCheck i OverflowCheck, nawet przy debuggowaniu.
  Innymi slowy, zdarza sie ze trzeba napisac kod o ktorym
  wiadamo ze nie bedzie poprawny przy wlaczonym R+ / Q+
  (czyli gdzies tam beda robione jakies "brudne" variable-casty).

  Wtedy ponizsze includy beda przydatne.
}

{$IFOPT R+} {$DEFINE _WAS_RANGE_CHECK} {$ENDIF} {$R-}
{$IFOPT Q+} {$DEFINE _WAS_OVERFLOW_CHECK} {$ENDIF} {$Q-}
