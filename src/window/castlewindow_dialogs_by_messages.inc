function TCastleWindowBase.FileDialog(const Title: string; var FileName: string;
  OpenDialog: boolean; FileFilters: TFileFilterList): boolean;
begin
  Result := MessageInputQuery(Self, Title, FileName, taLeft);
end;

function TCastleWindowBase.ColorDialog(var Color: TVector3Single): boolean;
var
  NewColor: TVector3Single;
begin
  Result := false;
  if not MessageInputQuery(Self, 'Input Red value:'  , NewColor[0], taLeft) then Exit;
  if not MessageInputQuery(Self, 'Input Green value:', NewColor[1], taLeft) then Exit;
  if not MessageInputQuery(Self, 'Input Blue value:' , NewColor[2], taLeft) then Exit;

  Result := true;
  Color := NewColor;
end;

procedure TCastleWindowBase.MessageOK(const S: string; const MessageType: TWindowMessageType);
begin
  CastleMessages.MessageOK(Self, S);
end;

function TCastleWindowBase.MessageYesNo(const S: string;
  const MessageType: TWindowMessageType): boolean;
begin
  Result := CastleMessages.MessageYesNo(Self, S);
end;
