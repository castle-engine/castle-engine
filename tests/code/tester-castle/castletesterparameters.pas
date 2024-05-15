{
  Copyright 2023-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Command-line parameters processing of CGE tester. }
unit CastleTesterParameters;

interface

var
  ParamNoWindowCreate: Boolean = false;
  ParamConsole: Boolean = false;
  ParamFilter: String = '';

procedure TesterParseParameters;

implementation

uses CastleUtils, CastleParameters;

const
  Options: array [0..2] of TOption = (
    (Short: 'f'; Long: 'filter'; Argument: oaRequired),
    (Short:  #0; Long: 'console'; Argument: oaNone),
    (Short:  #0; Long: 'no-window-create'; Argument: oaNone)
  );

procedure OptionProc(OptionNum: Integer; HasArgument: boolean;
  const Argument: string; const SeparateArgs: TSeparateArgs; Data: Pointer);
begin
  case OptionNum of
    0: ParamFilter := Argument;
    1: ParamConsole := true;
    2: ParamNoWindowCreate := true;
    else raise EInternalError.CreateFmt('OptionProc: OptionNum = %d', [OptionNum]);
  end;
end;

procedure TesterParseParameters;
begin
  Parameters.Parse(Options, @OptionProc, nil, false);
  Parameters.CheckHigh(0); // check no excessive parameters
end;

end.
