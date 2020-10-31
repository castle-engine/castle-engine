{$ifdef read_implementation_uses}
CastleMessages,
{$endif}

{$ifdef read_implementation}
function TCastleWindowBase.BackendFileDialog(const Title: string; var FileName: string;
  OpenDialog: Boolean; FileFilters: TFileFilterList): Boolean;
begin
  Result := MessageInputQuery(Self, Title, FileName);
end;

function TCastleWindowBase.ColorDialog(var Color: TVector3): Boolean;
var
  NewColor: TVector3;
begin
  Result := false;
  if not MessageInputQuery(Self, 'Input Red value:'  , NewColor.Data[0]) then Exit;
  if not MessageInputQuery(Self, 'Input Green value:', NewColor.Data[1]) then Exit;
  if not MessageInputQuery(Self, 'Input Blue value:' , NewColor.Data[2]) then Exit;

  Result := true;
  Color := NewColor;
end;

procedure TCastleWindowBase.MessageOK(const S: string; const MessageType: TWindowMessageType);
begin
  CastleMessages.MessageOK(Self, S);
end;

function TCastleWindowBase.MessageYesNo(const S: string;
  const MessageType: TWindowMessageType): Boolean;
begin
  Result := CastleMessages.MessageYesNo(Self, S);
end;
{$endif read_implementation}
