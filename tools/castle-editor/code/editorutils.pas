unit EditorUtils;

{$mode objfpc}{$H+}

interface

procedure ErrorBox(const Message: String);

implementation

uses Dialogs;

procedure ErrorBox(const Message: String);
begin
  MessageDlg('Error', Message, mtError, [mbOK], 0);
end;

end.

