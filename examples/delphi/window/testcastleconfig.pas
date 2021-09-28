unit TestCastleConfig;

interface

uses CastleConfig;

procedure TestCastleConfig1;

implementation

procedure TestCastleConfig1;
begin
  UserConfig.SetInt64('path1/int1', 123);
  UserConfig.SetValue('text', 'text_value');
end;

end.
