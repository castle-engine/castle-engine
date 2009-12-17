var SPoz: Integer;
begin
 SPoz := 1;
 result[0] := StrToFloat(NextToken(s, SPoz));
 result[1] := StrToFloat(NextToken(s, SPoz));
 result[2] := StrToFloat(NextToken(s, SPoz));
 Check(NextToken(s, SPoz) = '', 'expected end of string in Vector*FromStr');

 (* ta funkcje moznaby tez zrealizowac jako jedna z trzech wersji
   DeFormat(s, '%.single. %.single. %.single.', [@result[0], @result[1], @result[2]]);
   DeFormat(s, '%.double. %.double. %.double.', [@result[0], @result[1], @result[2]]);
   DeFormat(s, '%.extended. %.extended. %.extended.', [@result[0], @result[1], @result[2]]);
 ale tak jak jest jest szybciej - DeFormat musi wykonac wiecej pracy
 (przeciez parsuje format i data w duzo bardziej ogolny sposob),
 a szybkosc tej procedury jest przydatna dla parsera VRMLa *)
end;
