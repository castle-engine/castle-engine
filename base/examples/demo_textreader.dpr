uses KambiClassUtils;
begin
 while not StdInReader.Eof do
  Writeln('Next input line is "', StdInReader.Readln, '"');
end.