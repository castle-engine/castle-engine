program cge_demortti;

uses Classes, SysUtils, fpjson, fpjsonrtti, Contnrs, TypInfo;

class procedure GetObject(Sender : TOBject; AObject : TObject;
  Info : PPropInfo; AData : TJSONObject; DataName : TJSONStringType; Var AValue : TObject);
begin
  Writeln('TEventHandler.GetObject called');
end;

type
  TUIControl = class(TComponent)
  protected
    // Affects JSON writer with jsoStreamChildren
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;

    // Doesn't affect JSON writer at all?
    // procedure DefineProperties(Filer: TFiler); override;
    // procedure ReadControls(Reader: TReader);
    // procedure WriteControls(Writer: TWriter);
  public
    L: TObjectList;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

constructor TUIControl.Create(AOwner: TComponent);
begin
  inherited;
  L := TObjectList.Create(false);
end;

destructor TUIControl.Destroy;
begin
  FreeAndNil(L);
  inherited;
end;

procedure TUIControl.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  I: Integer;
begin
  for I := 0 to L.Count - 1 do
    Proc(L[I] as TUIControl);
end;

// procedure TUIControl.DefineProperties(Filer: TFiler);
// begin
//   Filer.DefineProperty('Controls',
//     {$ifdef FPC_OBJFPC}@{$endif} ReadControls,
//     {$ifdef FPC_OBJFPC}@{$endif} WriteControls,
//     L.Count <> 0);
// end;

// procedure TUIControl.ReadControls(Reader: TReader);
// var
//   ReadCount, I: Integer;
//   ReadControl: TUIControl;
// begin
//   Reader.ReadListBegin;
//   ReadCount := Reader.ReadInteger;
//   for I := 0 to ReadCount - 1 do
//   begin
//     ReadControl := Reader.ReadComponent(Self) as TUIControl;
//     L.Add(ReadControl);
//   end;
//   Reader.ReadListEnd;
// end;

// procedure TUIControl.WriteControls(Writer: TWriter);
// var
//   I: Integer;
// begin
//   Writer.WriteListBegin;
//   Writer.WriteInteger(L.Count);
//   for I := 0 to L.Count - 1 do
//     Writer.WriteComponent(L[I] as TUIControl);
//   Writer.WriteListEnd;
// end;

procedure CgeDemo;

  Procedure DumpObject(const Header : String; O : TJSONData);
  begin
    Writeln(Header,' : ');
    Writeln(O.FormatJSON());
    writeln();
  end;

var
  Owner: TComponent;
  Root: TUIControl;
  Group: TUIControl;
  Button, Lab: TUIControl;
  Saved: TJSONObject;
  Streamer: TJSONStreamer;
  Loaded: TUIControl;
  DeStreamer: TJSONDeStreamer;
begin
  // TODO: save Root class as 1st line of file?

  Owner := TComponent.Create(nil);

  Root := TUIControl.Create(Owner);
  Root.Name := 'Root';

  Group := TUIControl.Create(Owner);
  // Group.SetSubComponent(true);
  Group.Name := 'Group1';
  Root.L.Add(Group);

  Button := TUIControl.Create(Owner);
  // Button.SetSubComponent(true);
  Button.Name := 'Button';
  Group.L.Add(Button);

  Lab := TUIControl.Create(Owner);
  // Lab.SetSubComponent(true);
  Lab.Name := 'Label1';
  Group.L.Add(Lab);

  Streamer := TJSONStreamer.Create(nil);
  try
    Streamer.Options := [jsoStreamChildren];
    Saved := Streamer.ObjectToJSON(Root);

    FreeAndNil(Owner);

    DumpObject('CGE Options:=[jsoStreamChildren]', Saved);
  finally FreeAndNil(Streamer) end;

  // TODO: use AfterStreamObject to read children?

  DeStreamer := TJSONDeStreamer.Create(nil);
  try
    DeStreamer.OnGetObject := @GetObject;
    Loaded := TUIControl.Create(Owner);
    DeStreamer.JSONToObject(Saved, Loaded);
    FreeAndNil(Saved);

    Writeln('Loaded children: ', Loaded.L.Count);

    Streamer := TJSONStreamer.Create(nil);
    try
      Streamer.Options := [jsoStreamChildren];
      Saved := Streamer.ObjectToJSON(Loaded);
      DumpObject('New', Saved);
    finally FreeAndNil(Streamer) end;
    FreeAndNil(Saved);

  finally FreeAndNil(DeStreamer) end;

end;

begin
  CgeDemo;
end.
