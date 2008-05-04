{
  Copyright 2004-2007 Michalis Kamburelis.

  This file is part of test_kambi_units.

  test_kambi_units is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  test_kambi_units is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with test_kambi_units; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

unit TestVRMLNodes;

interface

uses
  Classes, SysUtils, fpcunit, testutils, testregistry;

type
  TTestVRMLNodes = class(TTestCase)
    procedure TestNodesManager;

    { This is really large test that reads and writes various VRML files
      and checks whether the generated VRML file is the same.
      It checks "the same" by looking comparing sequence of VRMLLexer
      tokens for them.

      So this checks both reading and writing of VRML files.

      Deactivated... it can't check like this, lexer tokens may be
      different due to the order of fields.
    }
    { procedure TestParseSaveToFile; }
  end;

implementation

uses KambiUtils, VRMLNodes, VRMLLexer, KambiClassUtils, KambiFilesUtils;

{ TNode* ------------------------------------------------------------ }

type
  TNodeSpecial = class(TVRMLNode)
    function NodeTypeName: string; override;
  end;

function TNodeSpecial.NodeTypeName: string;
begin
 result := 'OohImSoSpecial';
end;

type
  TNodeSomething = class(TVRMLNode)
    class function ClassNodeTypeName: string; override;
  end;

class function TNodeSomething.ClassNodeTypeName: string;
begin
 result := 'WellImNothingSpecial';
end;

{ ------------------------------------------------------------ }

procedure TTestVRMLNodes.TestNodesManager;
begin
 try
  { throw exception because TNodeSpecial.ClassNodeTypeName = '' }
  NodesManager.RegisterNodeClass(TNodeSpecial);
  raise Exception.Create('NodesManager.RegisterNodeClass(TNodeSpecial); SHOULD throw exception');
 except on ENodesManagerError do ; end;

 try
  { throw exception because TNodeFog is already registered }
  NodesManager.RegisterNodeClass(TNodeFog);
  raise Exception.Create('NodesManager.RegisterNodeClass(TNodeFog); SHOULD throw exception');
 except on ENodesManagerError do ; end;

 try
  { this should succeed }
  NodesManager.RegisterNodeClass(TNodeSomething);
 finally
  NodesManager.UnRegisterNodeClass(TNodeSomething);
 end;
end;

{ TVRMLTokenInfo and TDynVRMLTokenInfoArray ---------------------------------- }

(*
type
  TVRMLTokenInfo = record
    Token: TVRMLToken;
    Float: Float; // for both vtFloat and vtInteger
    Name: string; // for vtName
    AString: string; // for vtString
    case TVRMLToken of
      vtKeyword: (Keyword: TVRMLKeyword);
      vtInteger: (Integer: Int64);
  end;
  PVRMLTokenInfo = ^TVRMLTokenInfo;

  TDynArrayItem_1 = TVRMLTokenInfo;
  PDynArrayItem_1 = PVRMLTokenInfo;
  {$define DYNARRAY_1_IS_STRUCT}
  {$define DYNARRAY_1_IS_INIT_FINI_TYPE}
  {$define read_interface}
  {$define read_implementation}
  {$I dynarray_1.inc}

type
  TDynVRMLTokenInfoArray = class(TDynArray_1)
    procedure AssertEqual(SecondValue: TDynVRMLTokenInfoArray);
    procedure ReadFromFile(const FileName: string);
  end;

procedure TDynVRMLTokenInfoArray.AssertEqual(
  SecondValue: TDynVRMLTokenInfoArray);

  procedure AssertEqualTokens(const T1, T2: TVRMLTokenInfo);

    function DescribeTokenInfo(const T: TVRMLTokenInfo): string;
    const
      VRMLTokenNames: array[TVRMLToken]of string = (
        'keyword', 'name',
        '"{"', '"}"', '"["', '"]"', '"("', '")"', '"|"', '","', '"."',
        'float', 'integer', 'string', 'end of file');
    begin
      Result := VRMLTokenNames[T.Token];
      case T.Token of
        vtKeyword: result := result +' "' +VRMLKeywords[T.Keyword]+'"';
        vtName: result := '"' +T.Name+'"';
        vtFloat: result := result +' ' +FloatToStr(T.Float);
        vtInteger: result := result +' ' +IntToStr(T.Integer);
        vtString: result := result+' "'+T.AString+'"';
      end;
    end;

  begin
    Assert(
      (T1.Token = T2.Token) and
      ( (T1.Token <> vtKeyword) or (T1.Keyword = T2.Keyword) ) and
      ( (T1.Token <> vtName) or (T1.Name = T2.Name) ) and
      ( (T1.Token <> vtFloat) or (T1.Float = T2.Float) ) and
      ( (T1.Token <> vtInteger) or ( (T1.Float = T2.Float) and
                                        (T1.Integer = T2.Integer) ) ) and
      ( (T1.Token <> vtString) or (T1.AString = T2.AString) ),
      Format('VRML tokens different: %s and %s',
        [DescribeTokenInfo(T1), DescribeTokenInfo(T2)]));
  end;

var
  I: Integer;
begin
  Assert(Count = SecondValue.Count, Format(
    'TDynVRMLTokenInfoArray.Equal: different counts %d and %d',
    [Count, SecondValue.Count]));
  for I := 0 to Count - 1 do
    AssertEqualTokens(Items[I], SecondValue.Items[I]);
end;

{ Note that this can be used to test correctly only files that can
  be correctly parsed by pure Lexer.NextToken calls. All valid VRML >= 2.0
  files are like that, although parser in practice has to use NextTokenForceXxx
  methods because of unfortunately
  1. invalid VRML files (that use some funny node names)
  2. VRML 1.0 ugly feature that string doesn't have to be enclosed in "" }
procedure TDynVRMLTokenInfoArray.ReadFromFile(const FileName: string);
var
  Lexer: TVRMLLexer;

  function CurrentToken: TVRMLTokenInfo;
  begin
    Result.Token := Lexer.Token;
    Result.Keyword := Lexer.TokenKeyword;
    Result.Name := Lexer.TokenName;
    Result.Float := Lexer.TokenFloat;
    Result.Integer := Lexer.TokenInteger;
    Result.AString := Lexer.TokenString;
  end;

var
  I: Integer;
begin
  Count := 0;
  Lexer := TVRMLLexer.CreateFromFile(FileName);
  try
    AppendItem(CurrentToken);
    while Lexer.Token <> vtEnd do
    begin
      Lexer.NextToken;
      AppendItem(CurrentToken);
    end;
  finally FreeAndNil(Lexer); end;
end;

procedure TTestVRMLNodes.TestParseSaveToFile;

  procedure TestReadWrite(const FileName: string);
  var
    First, Second: TDynVRMLTokenInfoArray;
    Node: TVRMLNode;
    S: TMemoryStream;
    SPeek: TPeekCharStream;
    NewFile: string;
  begin
    First := nil;
    Second := nil;
    Node := nil;
    try
      First := TDynVRMLTokenInfoArray.Create;
      First.ReadFromFile(FileName);

      Node := ParseVRMLFile(FileName, false);
      NewFile := GetTempPath + 'test_kambi_vrml_game_engine.wrl';
      SaveToVRMLFile(Node, NewFile, '', false);

      Second := TDynVRMLTokenInfoArray.Create;
      Second.ReadFromFile(NewFile);

      First.AssertEqual(Second);
    finally
      FreeAndNil(First);
      FreeAndNil(Second);
      FreeAndNil(Node);
    end;
  end;

begin
  TestReadWrite('../../kambi_vrml_test_suite/vrml_2/proto_nested.wrl');
end;
*)

initialization
 RegisterTest(TTestVRMLNodes);
end.
