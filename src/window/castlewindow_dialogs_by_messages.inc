{$ifdef read_implementation_uses}
CastleMessages,
{$endif}

{$ifdef read_implementation}
function TCastleWindow.BackendFileDialog(const Title: string; var FileName: string;
  OpenDialog: boolean; FileFilters: TFileFilterList): boolean;
begin
  Result := MessageInputQuery(Self, Title, FileName);
end;

function TCastleWindow.ColorDialog(var Color: TCastleColor): boolean;
var
  NewColor: TCastleColor;
begin
  Result := false;
  if not MessageInputQuery(Self, 'Input Red value:'  , NewColor.X) then Exit;
  if not MessageInputQuery(Self, 'Input Green value:', NewColor.Y) then Exit;
  if not MessageInputQuery(Self, 'Input Blue value:' , NewColor.Z) then Exit;
  if not MessageInputQuery(Self, 'Input Alpha value:', NewColor.W) then Exit;

  Result := true;
  Color := NewColor;
end;

procedure TCastleWindow.MessageOK(const S: string; const MessageType: TWindowMessageType);
begin
  CastleMessages.MessageOK(Self, S);
end;

function TCastleWindow.MessageYesNo(const S: string;
  const MessageType: TWindowMessageType): boolean;
begin
  Result := CastleMessages.MessageYesNo(Self, S);
end;
{$endif read_implementation}
