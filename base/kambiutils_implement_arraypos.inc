begin
 for result:=0 to High(Arr) do
  if Arr[result]=A then exit;
 result:=-1;
end;