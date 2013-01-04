var
  SPosition: Integer;
begin
  SPosition := 1;
  result[0] := StrToFloat(NextToken(S, SPosition));
  result[1] := StrToFloat(NextToken(S, SPosition));
  result[2] := StrToFloat(NextToken(S, SPosition));
  if NextToken(S, SPosition) <> '' then
    raise EConvertError.Create('Expected end of data when reading vector from string');

  { This could also be implemented by DeFormat, but this is faster
    (which is important for e.g. reading 3D data where this function may be used
    often) }
end;
