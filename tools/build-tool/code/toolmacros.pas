{
  Copyright 2014-2019 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Simple macro system, to use in CGE build tool templates. }
unit ToolMacros;

{$I castleconf.inc}

interface

uses CastleStringUtils;

{ Replace macros in given string.
  Provide a Macros as a map, from uppercase keys. }
function ReplaceMacros(const Macros: TStringStringMap; const Source: String): String;

implementation

uses SysUtils, StrUtils,
  CastleLog, CastleScript, CastleScriptParser, CastleUtils;

function ReplaceMacros(const Macros: TStringStringMap; const Source: String): String;

  { Create replacement map to perform the macros. }
  procedure CreateMacros(out Replacements, CastleScriptReplacements: TStringStringMap);

    function QuoteCastleScriptString(const S: String): String;
    begin
      Result := '''' + StringReplace(S, '''', '''''', [rfReplaceAll]) + '''';
    end;

  var
    Macro: TStringStringMap.TDictionaryPair;
  begin
    Replacements := TStringStringMap.Create;
    CastleScriptReplacements := TStringStringMap.Create;

    for Macro in Macros do
    begin
      // add ${} around
      Replacements.Add('${' + Macro.Key + '}', Macro.Value);
      CastleScriptReplacements.Add('${' + Macro.Key + '}', QuoteCastleScriptString(Macro.Value));
    end;
  end;

const
  MacrosIgnoreCase = true;
type
  TMacroExpression = (
    meIf,
    meElse,
    meEndif,
    meCalculate
  );
const
  ExpressionPrefix: array [TMacroExpression] of String = (
    '${if ',
    '${else}',
    '${endif}',
    '${calculate '
  );

  (* Nearest (from StartPositionToSearch) occurrence of expression begin, like '${IF...'.
     Sets also Expression.
     0 if not found (and leaves Expression undefined then). *)
  function NextExpressionPosition(const S, LowercaseS: String;
    const StartPositionToSearch: Integer; out Expression: TMacroExpression): Integer;
  var
    P: Integer;
    E: TMacroExpression;
  begin
    Result := 0;

    for E in TMacroExpression do
    begin
      P := PosEx(ExpressionPrefix[E], LowercaseS, StartPositionToSearch);
      if (P <> 0) and ((Result = 0) or (P < Result)) then
      begin
        Result := P;
        Expression := E;
      end;
    end;
  end;

type
  TIfState = (isNone, isThen, isElse);
var
  IfCondition: Boolean;
  IfState: TIfState;

  (* Process expression that starts as ExpressionBegin in S, ends at ExpressionEnd.
     Sets ExpressionEnd (last character of the expression, always '}')
     and returns the evaluated expression. *)
  function ProcessExpression(const S, LowercaseS: String;
    const Expression: TMacroExpression;
    const ExpressionBegin, ExpressionEnd: Integer;
    const CastleScriptReplacements: TStringStringMap): String;
  var
    ExpStr: String;
    Exp: TCasScriptExpression;
    {$ifdef DEBUG_MACROS}
    Pair: TStringStringMap.TDictionaryPair;
    {$endif}
  begin
    ExpStr := CopyPos(Source, ExpressionBegin + Length(ExpressionPrefix[Expression]), ExpressionEnd - 1);

    {$ifdef DEBUG_MACROS}
    Writeln('Replacements:');
    Writeln(ExpStr);
    Writeln(CastleScriptReplacements.Count);
    for Pair in CastleScriptReplacements do
    begin
      Writeln('  ', Pair.Key, ' -> ', Pair.Value);
    end;
    {$endif}

    ExpStr := SReplacePatterns(ExpStr, CastleScriptReplacements, MacrosIgnoreCase);

    {$ifdef DEBUG_MACROS}
    Writeln('Output:');
    Writeln(ExpStr);
    {$endif}

    case Expression of
      meIf:
        begin
          Result := '';
          if IfState <> isNone then
            raise Exception.Create('Nesting ${IF} within ${IF} is not possible yet');
          IfState := isThen;
          Exp := ParseBoolExpression(ExpStr, []);
          IfCondition := Exp.AsBool;
        end;
      meElse:
        begin
          Result := '';
          if IfState <> isThen then
            raise Exception.Create('Unexpected ${ELSE}, not preceded by ${IF}');
          IfState := isElse;
        end;
      meEndif:
        begin
          Result := '';
          if not (IfState in [isThen, isElse]) then
            raise Exception.Create('Unexpected ${ENDIF}, not preceded by ${IF}');
          IfState := isNone;
        end;
      meCalculate:
        begin
          Exp := ParseStringExpression(ExpStr, []);
          Result := Exp.AsString;
        end;
      {$ifndef COMPILER_CASE_ANALYSIS}
      else raise EInternalError.CreateFmt('Unknown expression %d', [Ord(Expression)]);
      {$endif}
    end;
  end;

  function ProcessText(const S: String; const Replacements: TStringStringMap): String;
  var
    Active: Boolean;
  begin
    case IfState of
      isNone: Active := true;
      isThen: Active := IfCondition;
      isElse: Active := not IfCondition;
      {$ifndef COMPILER_CASE_ANALYSIS}
      else raise EInternalError.Create('Unknown IfState');
      {$endif}
    end;
    if Active then
      Result := SReplacePatterns(S, Replacements, MacrosIgnoreCase)
    else
      Result := '';
  end;

  { Return the content up to (excluding) the first newline
    (but with ellipsis instead of newline). }
  function FirstLine(const Source: String): String;
  var
    P: Integer;
  begin
    P := CharsPos([#13, #10], Source);
    if P <> 0 then
      Result := Copy(Source, 1, P - 1) + '...'
    else
      Result := Source;
  end;

  (* Find matching '}', assuming that at ExpressionBegin we see '{'.
     Raises exception if not found. *)
  function FindMatchingExpressionEnd(const Source: String; const ExpressionBegin: Integer): Integer;
  var
    LevelParen, LevelCurly, I: Integer;
  begin
    LevelCurly := 0;
    LevelParen := 0;
    (* Note that we know that the 1st loop iteration will encounter '{'
       and thus will set LevelCurly = 1. *)
    I := ExpressionBegin;
    repeat
      if I > Length(Source) then
        raise Exception.CreateFmt('Cannot find a matching closing "}" in the template source "%s"',
          [FirstLine(Source)]);
      case Source[I] of
        '{': Inc(LevelCurly);
        '}':
          begin
            Dec(LevelCurly);
            if (LevelCurly = 0) and (LevelParen = 0) then
              Exit(I);
          end;
        '(': Inc(LevelParen);
        ')': Dec(LevelParen);
      end;
      Inc(I);
    until false;
  end;

var
  Replacements, CastleScriptReplacements: TStringStringMap;
  Done, ExpressionBegin: Integer;
  Expression: TMacroExpression;
  LowercaseSource: String;
begin
  CreateMacros(Replacements, CastleScriptReplacements);
  try
    LowercaseSource := AnsiLowerCase(Source);
    Done := 0;
    Result := '';
    IfState := isNone;

    (* Split Source into chunks.
      One "chunk" here is either
      - an expression using macros, like '${CALCULATE xxx}' or '${IF xxx}'
      - not an expression, which is just a normal text a template where only simple
        macro variable replacement is performed.
    *)

    while Done < Length(Source) do
    begin
      ExpressionBegin := NextExpressionPosition(Source, LowercaseSource,
        Done + 1, Expression);
      if ExpressionBegin = 0 then
      begin
        { chunk of normal text, up to the end of the Source }
        Result := Result + ProcessText(SEnding(Source, Done + 1),
          Replacements);
        Done := Length(Source);
      end else
      begin
        { chunk of normal text, up to the beginning of nearest expression }
        Result := Result + ProcessText(CopyPos(Source, Done + 1, ExpressionBegin - 1),
          Replacements);
        { chunk with one expression }
        Done := FindMatchingExpressionEnd(Source, ExpressionBegin);
        Result := Result + ProcessExpression(Source, LowercaseSource, Expression,
          ExpressionBegin, Done, CastleScriptReplacements);
      end;
    end;
  finally
    FreeAndNil(Replacements);
    FreeAndNil(CastleScriptReplacements);
  end;
end;

end.
