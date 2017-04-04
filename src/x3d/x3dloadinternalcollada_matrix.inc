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

{$endif read_interface}

{$ifdef read_implementation}

{$endif read_implementation}

{$ifdef read_implementation_nested}

{ Read <matrix> or <bind_shape_matrix> element to given Matrix. }
function ReadMatrix(MatrixElement: TDOMElement): TMatrix4Single;
var
  SeekPos: Integer;
  Row, Col: Integer;
  Token, Content: string;
begin
  Content := MatrixElement.TextData;

  SeekPos := 1;

  for Row := 0 to 3 do
    for Col := 0 to 3 do
    begin
      Token := NextToken(Content, SeekPos);
      if Token = '' then
      begin
        WritelnWarning('Collada', 'Matrix (<matrix> or <bind_shape_matrix> ' +
          'element) has not enough items');
        Break;
      end;
      Result[Col, Row] := StrToFloat(Token);
    end;
end;

{ Read <lookat> element, return appropriate matrix. }
function ReadLookAt(MatrixElement: TDOMElement): TMatrix4Single;
var
  SeekPos: Integer;
  Content: string;

  function ReadVector(out Vector: TVector3Single): boolean;
  var
    Token: string;
    I: Integer;
  begin
    Result := true;

    for I := 0 to 2 do
    begin
      Token := NextToken(Content, SeekPos);
      if Token = '' then
      begin
        WritelnWarning('Collada', 'Unexpected end of data of <lookat>');
        Exit(false);
      end;
      Vector[I] := StrToFloat(Token);
    end;
  end;

var
  Eye, Center, Up: TVector3Single;
begin
  Content := MatrixElement.TextData;

  SeekPos := 1;

  if ReadVector(Eye) and
     ReadVector(Center) and
     ReadVector(Up) then
    Result := LookAtMatrix(Eye, Center, Up);
end;

{$endif read_implementation_nested}
