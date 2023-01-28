unit TestCastleConfig;

interface

uses CastleConfig;

procedure TestCastleConfig1;

implementation

uses CastleLog;

procedure TestCastleConfig1;
begin
  WritelnLog('Load user config');
  UserConfig.Load;

  WritelnLog('XML Dump: ' + UserConfig.Document.InternalDocument.XML.Text);

  WritelnLog('Add int0 with value 123');
  UserConfig.SetInt64('int0', 123);
  WritelnLog('Add path1/int1 with value 123');
  UserConfig.SetInt64('path1/int1', 123);
  WritelnLog('Add text with value text_value');
  UserConfig.SetValue('text', 'text_value');

  WritelnLog('XML Dump: ' + UserConfig.Document.InternalDocument.XML.Text);

  WritelnLog('Delete text');
  UserConfig.DeleteValue('text');

  WritelnLog('Delete path1/int1');
  UserConfig.DeleteValue('path1/int1');

  WritelnLog('XML Dump: ' + UserConfig.Document.InternalDocument.XML.Text);


  UserConfig.Save;

end;

end.

