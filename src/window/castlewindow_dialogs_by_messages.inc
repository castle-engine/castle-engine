{$ifdef read_implementation_uses}
CastleMessages,
{$endif}

{$ifdef read_implementation}
function TCastleWindowBase.BackendFileDialog(const Title: string; var FileName: string;
  OpenDialog: boolean; FileFilters: TFileFilterList): boolean;
begin
  if not (Self is TCastleWindowCustom) then Exit;
  Result := MessageInputQuery(Self as TCastleWindowCustom, Title, FileName);
end;

function TCastleWindowBase.ColorDialog(var Color: TVector3Single): boolean;
var
  NewColor: TVector3Single;
begin
  Result := false;
  if not (Self is TCastleWindowCustom) then Exit;
  if not MessageInputQuery(Self as TCastleWindowCustom, 'Input Red value:'  , NewColor[0]) then Exit;
  if not MessageInputQuery(Self as TCastleWindowCustom, 'Input Green value:', NewColor[1]) then Exit;
  if not MessageInputQuery(Self as TCastleWindowCustom, 'Input Blue value:' , NewColor[2]) then Exit;

  Result := true;
  Color := NewColor;
end;

procedure TCastleWindowBase.MessageOK(const S: string; const MessageType: TWindowMessageType);
begin
  if not (Self is TCastleWindowCustom) then Exit;
  CastleMessages.MessageOK(Self as TCastleWindowCustom, S);
end;

function TCastleWindowBase.MessageYesNo(const S: string;
  const MessageType: TWindowMessageType): boolean;
begin
  if not (Self is TCastleWindowCustom) then Exit;
  Result := CastleMessages.MessageYesNo(Self as TCastleWindowCustom, S);
end;
{$endif read_implementation}
