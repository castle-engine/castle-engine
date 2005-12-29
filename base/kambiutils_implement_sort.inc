{ Implementation of Sort and SortByObject }

  procedure SimpleSort(
    L: Pointer; LIndex: Integer;
    R: Pointer; RIndex: Integer);
  { Simple sort by choosing }
  var NowIndex: Integer;
      MinPtr, NowPtr, Temp: Pointer;
  begin
   Temp := GetMem(ArrRecordSize);
   try
    while LIndex < RIndex do
    begin
     MinPtr := L;
     NowPtr := L;
     for NowIndex := LIndex + 1 to RIndex do
     begin
      TPointerSInt(NowPtr) := TPointerSInt(NowPtr) + ArrStride;
      if IsSmallerFunc(NowPtr, MinPtr
        {$ifndef SORT_BY_OBJECT},IsSmallerFuncData{$endif}) then
       MinPtr := NowPtr;
     end;

     { swap L and MinPtr }
     Move(L^, Temp^, ArrRecordSize);
     Move(MinPtr^, L^, ArrRecordSize);
     Move(Temp^, MinPtr^, ArrRecordSize);

     TPointerSInt(L) := TPointerSInt(L) + ArrStride;
     Inc(LIndex);
    end;
   finally FreeMem(Temp) end;
  end;

  procedure InternalSort(
    L: Pointer; LIndex: Integer;
    R: Pointer; RIndex: Integer);
  var
    I, J: Pointer;
    IIndex, JIndex: Integer;
    Temp, Middle: Pointer;
  begin
   if LIndex >= RIndex then Exit;

   if RIndex - LIndex <= CountToUseSimpleSort - 1 then
   begin
    SimpleSort(L, LIndex, R, RIndex);
    Exit;
   end;

   I := L; IIndex := LIndex;
   J := R; JIndex := RIndex;

   Middle := nil;
   Temp := nil;
   try

    { Set Middle pointer.
      Many strategies possible here. }
    Middle := GetMem(ArrRecordSize);
    Move(PointerAdd(Arr, ((LIndex + RIndex) div 2) * ArrStride)^,
      Middle^, ArrRecordSize);

    Temp := GetMem(ArrRecordSize);

    repeat
     while IsSmallerFunc(I, Middle {$ifndef SORT_BY_OBJECT},IsSmallerFuncData{$endif}) do
     begin
      { next i }
      TPointerSInt(I) := TPointerSInt(I) + ArrStride;
      Inc(IIndex);
     end;

     while IsSmallerFunc(Middle, J {$ifndef SORT_BY_OBJECT},IsSmallerFuncData{$endif}) do
     begin
      { previous j }
      TPointerSInt(J) := TPointerSInt(J) - ArrStride;
      Dec(JIndex);
     end;

     if IIndex <= JIndex then
     begin
      { swap ith and jth items }
      Move(I^, Temp^, ArrRecordSize);  { temp:=Arr[i] }
      Move(J^, I^, ArrRecordSize);     { Arr[i]:=Arr[j] }
      Move(Temp^, J^, ArrRecordSize);  { Arr[j]:=temp }

      { next i }
      TPointerSInt(I) := TPointerSInt(I) + ArrStride;
      Inc(IIndex);

      { previous j }
      TPointerSInt(J) := TPointerSInt(J) - ArrStride;
      Dec(JIndex);
     end;
    until IIndex > JIndex;

   finally
    FreeMem(Temp);
    FreeMem(Middle);
   end;

   InternalSort(L, LIndex, J, JIndex);
   InternalSort(I, IIndex, R, RIndex);
  end;

begin
 InternalSort(
   PointerAdd(Arr, FirstIndex * ArrStride), FirstIndex,
   PointerAdd(Arr, LastIndex  * ArrStride), LastIndex);
end;