{
  Copyright 2008-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}

type
  { Parse a sequence of integeres inside a string inside an XML element. }
  TIntegersParser = class
  private
    Content: string;
    FCurrent: Int64;
    SeekPos: Integer;
    Ended: boolean;
  public
    constructor Create(const PElement: TDOMElement);
    function GetNext: boolean;
    property Current: Int64 read FCurrent;
  end;

{$endif read_interface}

{$ifdef read_implementation}

constructor TIntegersParser.Create(const PElement: TDOMElement);
begin
  inherited Create;
  Content := PElement.TextData;
  SeekPos := 1;
end;

function TIntegersParser.GetNext: boolean;
var
  Token: string;
begin
  if Ended then Exit(false);

  Token := NextToken(Content, SeekPos);
  if Token = '' then
  begin
    Ended := true;
    Result := false;
  end else
  try
    FCurrent := StrToInt(Token);
    Result := true;
  except
    on E: EConvertError do
    begin
      WritelnWarning('Collada', 'Invalid integer: ' + E.Message);
      Ended := true; { don't read further }
      Result := false;
    end;
  end;
end;

{$endif read_implementation}
