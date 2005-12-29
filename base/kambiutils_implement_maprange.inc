begin
 { sprowadz zakres sourceBegin .. sourceEnd do zakresu 0..1 (i wyraz w nim sourceVal).
   Zauwazmy ze gdy zakres source jest malejacy to tez robi dobrze. }
 result:=(sourceVal-sourceBegin) / (sourceEnd - sourceBegin);
 { zamien sourceVal z 0..1 do destBegin ..destEnd 
   (dziala takze gdy zakres dest jest malejacy) }
 result:=result*(destEnd-destBegin) + destBegin;
end;
