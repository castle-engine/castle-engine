{%MainUnit castleutils.pas}
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

{ Things related to exiting the program. }

{$ifdef read_interface}

type
  { Descend from this to mark exceptions that should be shown
    in more concise way by @link(ExceptMessage) function.
    It avoids showing the ClassName in output (and stack trace, in debug).

    Useful for exceptions that have clear @code(Message),
    and it is normal that they may occur during normal execution of the program
    and be visible to users.
    E.g. EInvalidParams. }
  EShortErrorMessage = class(Exception);
  EWithHiddenClassName = EShortErrorMessage deprecated 'use EShortErrorMessage';

  { @deprecated Special class to raise an exception that is always catched. }
  TCodeBreaker = class(TObject)
  end deprecated;

{ Nice exception description.
  Contains the ClassName (unless it descends from EShortErrorMessage),
  and the exception Message (if it descends from Exception). }
function ExceptMessage(const E: TObject): string;

procedure ProgramBreak; deprecated 'Use Halt';

{$endif read_interface}

{$ifdef read_implementation}

function ExceptMessage(const E: TObject): string;
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
      if NiceMessage <> '' then
        Message := Message + ' (' + NiceMessage + ')';
    end;

  end else
    Message := '';

  if (E is EShortErrorMessage) and (Message <> '') then
  begin
    Result := Message;
  end else
  begin
    Result := 'Exception "' + E.ClassName + '"';
    if Message <> '' then
      Result := Result + ':' + NL + Message;
  end;
end;

procedure ProgramBreak;
begin
  Halt;
end;

{$endif read_implementation}
