{%MainUnit x3dnodes.pas}
{
  Copyright 2002-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}

  { Not recognized VRML/X3D node type. Used for nodes found when parsing
    VRML/X3D file that are not implemented.

    TX3DUnknownNode is parsed (in classic VRML encoding) in a special way,
    to be able to omit it gracefully.
    While such "unknown" node doesn't really do match in our graph,
    it works correctly with VRML/X3D DEF/USE mechanism.

    Never instantiate this class by a standard constructor.
    Always use CreateUnknown constructor, this way we can safely assume
    that X3DType is always correctly set. }
  TX3DUnknownNode = class(TX3DNode)
  strict private
    fX3DType: string;
  protected
    function DeepCopyCreate(CopyState: TX3DNodeDeepCopyState): TX3DNode; override;
  public
    function X3DType: string; override;
    procedure Parse(Lexer: TX3DLexer; Reader: TX3DReaderNames); override;

    { base Create will throw exception. Always use CreateUnknown* }
    constructor Create(const AName: string; const ABaseUrl: string); override;

    constructor CreateUnknown(const AName, ABaseUrl: string; const AX3DType :string);
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TX3DUnknownNode ---------------------------------------------------------------- }

function TX3DUnknownNode.X3DType: string;
begin
 result := fX3DType;
end;

procedure TX3DUnknownNode.Parse(Lexer: TX3DLexer; Reader: TX3DReaderNames);

(*TODO: use "fields" and "isA" VRML 1.0 extensibility features here.

  For now, we just always parse up to the matching closing "}".
  The VRML1Children*Allowed is set to false.

  We should handle:

  - node that has "fields", but not "isA" field (or "isA" points to
    another unknown node name). In this case we can create Fields
    array and parse fields of this node. We will not know how to handle
    this node anyway, but at least we'll know it's fields names and values,
    and we'll be able to save it back to stream.

    The VRML1Children*Allowed set to true? To allow it to work like
    VRML 1.0 Group?

  - node that has "fields" and valid "isA". In this case we can create Fields
    array, parse fields of this node. Then create a helper node
    of type given by "isA", and replace (or add as a child)
    our TX3DUnknownNode by this helper node.

    The VRML1Children*Allowed should be copied from referred "isA" node.
*)
begin
  { In TX3DUnknownNode case, VRML1Children*Allowed must be initialized during
    parsing. }
  VRML1ChildrenAllowed := false;
  VRML1ChildrenParsingAllowed := false;

  Lexer.CheckTokenIs(vtOpenCurlyBracket);
  Lexer.NextToken;
  ParseIgnoreToMatchingCurlyBracket(Lexer, Reader);

  FBaseUrl := Reader.BaseUrl;

  WritelnWarning('VRML/X3D', 'Unknown node of type "'+ X3DType + '" (named "'+ X3DName +'")');
end;

constructor TX3DUnknownNode.Create(const AName: string; const ABaseUrl: string);
begin
  { Safety check: never create a TX3DUnknownNode instance by this method,
    to not leave FX3DType unset. }
  raise Exception.Create('You cannot create Unknown node using default constructor');
end;

constructor TX3DUnknownNode.CreateUnknown(const AName, ABaseUrl: string; const AX3DType :string);
begin
  inherited Create(AName, ABaseUrl);
  fX3DType := AX3DType;
end;

function TX3DUnknownNode.DeepCopyCreate(
  CopyState: TX3DNodeDeepCopyState): TX3DNode;
begin
  Result := TX3DUnknownNode.CreateUnknown(X3DName, BaseUrl, X3DType);
end;

{$endif read_implementation}
