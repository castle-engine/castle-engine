unit EditorUtils;

{$mode objfpc}{$H+}

interface

procedure ErrorBox(const Message: String);
procedure WarningBox(const Message: String);

implementation

uses Dialogs;

procedure ErrorBox(const Message: String);
begin
  MessageDlg('Error', Message, mtError, [mbOK], 0);
end;

procedure WarningBox(const Message: String);
begin
  MessageDlg('Warning', Message, mtWarning, [mbOK], 0);
end;

end.

