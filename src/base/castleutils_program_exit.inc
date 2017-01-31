{
  Copyright 2002-2017 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Things related to exiting the program. }

{$ifdef read_interface}

type
  { Class of exceptions that will not have ClassName displayed by
    @link(ExceptMessage).
    Sometimes useful, when you know that exception message
    is good enough, and showing exception class is not needed. }
  EWithHiddenClassName = class(Exception);

  { @deprecated Special class to raise an exception that is always catched. }
  TCodeBreaker = class(TObject)
  end deprecated;

{ Nice exception description. Contains exception ClassName
  (if not descends from EWithHiddenClassName), exception Message (if
  descends from Exception), and ExceptAddr (if not @nil, and code is compiled
  with -dDEBUG), and BonusErrorMesssg. }
function ExceptMessage(E: TObject; ExceptAddr: Pointer = nil): string; overload;

procedure ProgramBreak; deprecated 'Use Halt';

{$endif read_interface}

{$ifdef read_implementation}

function ExceptMessage(E: TObject; ExceptAddr: Pointer): string;
var
  Message, NiceMessage: string;
begin
  if E is Exception then
  begin
    Message := Exception(E).Message;

    if E is EInOutError then
    begin
      { EInOutError message text can be improved }
      case EInOutError(E).ErrorCode of
        1..99: NiceMessage := SysErrorMessage(EInOutError(E).ErrorCode);
        100: NiceMessage := 'End of File';
        101: NiceMessage := 'Disk Full';
        102: NiceMessage := 'File variable not assigned';
        103: NiceMessage := 'File not open';
        104: NiceMessage := 'File not open for input';
        105: NiceMessage := 'File not open for output';
        106: NiceMessage := 'Error in formatted input';
        107: NiceMessage := 'File already open';
        else NiceMessage := '';
      end;
      if NiceMessage <> '' then Message := Message +' (' +NiceMessage +')';
    end;

  end else
    Message := '';

  if (E is EWithHiddenClassName) and (Message <> '') then
    result := Message else
  begin
    result := 'Exception "' + E.ClassName + '"';
    // not usually useful
    // {$ifdef DEBUG}
    // if ExceptAddr <> nil then
    //   result := result +' (at address ' + PointerToStr(ExceptAddr) + ')';
    // {$endif}
    if Message <> '' then result := result +' : ' +nl +Message;
  end;
end;

procedure ProgramBreak;
begin
  Halt;
end;

{$endif read_implementation}
