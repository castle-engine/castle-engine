{
  Copyright 2018-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Reading and writing of hierachy of CGE components
  (TUIControl, TCastleTransform) to and from files.
  This is used by CGE editor to read/write components,
  and it can be used at runtime by games to instantiate components designed
  in CGE editor. }
unit CastleComponentSerialize;

{$I castleconf.inc}

interface

uses SysUtils, Classes, FpJson, FpJsonRtti,
  CastleUIControls, CastleTransform;

type
  EInvalidComponentFile = class(Exception);

  { Internal for InternalAddChild methods. @exclude }
  TCastleComponentReader = class
  private
    FJsonReader: TJSONDeStreamer;
    FOwner: TComponent;
    procedure DeStreamerAfterReadObject(Sender: TObject; AObject: TObject; JSON: TJSONObject);
  public
    property JsonReader: TJSONDeStreamer read FJsonReader;
    { Will own all deserialized components. }
    property Owner: TComponent read FOwner;
  end;

procedure TransformSave(const T: TCastleTransform; const Url: String);
function TransformLoad(const Url: String; const Owner: TComponent): TCastleTransform;

procedure UserInterfaceSave(const C: TUIControl; const Url: String);
function UserInterfaceLoad(const Url: String; const Owner: TComponent): TUIControl;

procedure ComponentSave(const C: TComponent; const Url: String);
function ComponentLoad(const Url: String; const Owner: TComponent): TComponent;

implementation

uses JsonParser, TypInfo,
  CastleFilesUtils,
  // TODO: these units are only for temporary FindComponentClass implementation
  CastleControls, CastleScene, Castle2DSceneManager, CastleSceneManager;

function FindComponentClass(const AClassName: string): TComponentClass;
const
  // TODO: should we just register this all, and it should work then automatically?
  Classes: array [0..8] of TComponentClass = (
    TUIControlSizeable,
    TCastleButton,
    TCastleLabel,
    TCastleRectangleControl,
    TCastleSceneManager,
    TCastle2DSceneManager,
    TCastleTransform,
    TCastleScene,
    TCastle2DScene
  );
var
  C: TComponentClass;
begin
  // TODO: browse also registered classes
  for C in Classes do
    if C.ClassName = AClassName then
      Exit(C);
  Result := nil;
end;

{ Read and create suitable component class from JSON. }
function CreateComponentFromJson(const JsonObject: TJSONObject;
  const Owner: TComponent): TComponent;
var
  ResultClassName: String;
  ResultClass: TComponentClass;
begin
  ResultClassName := JsonObject.Strings['_ClassName'];
  // do not confuse TJSONDeStreamer with extra ClassName property
  JsonObject.Delete('_ClassName');

  ResultClass := FindComponentClass(ResultClassName);
  if ResultClass = nil then
    raise EInvalidComponentFile.CreateFmt('Component JSON file contains unrecognized class "%s"',
      [ResultClassName]);
  Result := ResultClass.Create(Owner);
end;

procedure TCastleComponentReader.DeStreamerAfterReadObject(
  Sender: TObject; AObject: TObject; JSON: TJSONObject);
var
  JsonChildren: TJSONArray;
  JsonChild: TJSONObject;
  I: Integer;
  Child: TComponent;
begin
  if AObject is TComponent then
  begin
    JsonChildren := Json.Arrays['_Children'];
    if JsonChildren <> nil then
    begin
      for I := 0 to JsonChildren.Count - 1 do
      begin
        JsonChild := JsonChildren.Objects[I];
        if JsonChild = nil then
          raise EInvalidComponentFile.Create('_Children must be an array of JSON objects');
        Child := CreateComponentFromJson(JsonChild, Owner);
        FJsonReader.JSONToObject(JsonChild, Child);
        if AObject is TUIControl then
          // matches TUIControl.GetChildren implementation
          TUIControl(AObject).InsertBack(Child as TUIControl)
        else
        if AObject is TCastleTransform then
          // matches TCastleTransform.GetChildren implementation
          TCastleTransform(AObject).Add(Child as TCastleTransform)
        else
          raise EInvalidComponentFile.CreateFmt('_Children contains unexpected class, it cannot be added to parent: %s',
            [Child.ClassName]);
      end;
    end;
  end;
end;

{ Load any TComponent.

  It mostly works automatically with any TComponent.
  But it has some special connections to TUIControl and TCastleTransform.
  It expects that they implement InternalAddChild (an analogue to GetChildren
  method), otherwise deserializing custom children (defined by GetChildren)
  is not possible. }
function ComponentLoad(const Url: String; const Owner: TComponent): TComponent;
var
  Reader: TCastleComponentReader;
  JsonData: TJSONData;
  JsonObject: TJSONObject;
begin
  Reader := TCastleComponentReader.Create;
  try
    Reader.FJsonReader := TJSONDeStreamer.Create(nil);
    Reader.FJsonReader.AfterReadObject := @Reader.DeStreamerAfterReadObject;
    Reader.FOwner := Owner;

    JsonData := GetJson(FileToString(Url), true);
    try
      if not (JsonData is TJSONObject) then
        raise EInvalidComponentFile.Create('Component JSON file should contain an object');
      JsonObject := JsonData as TJSONObject;

      { create Result with appropriate class }
      Result := CreateComponentFromJson(JsonObject, Owner);

      { read Result contents from JSON }
      Reader.FJsonReader.JSONToObject(JsonObject, Result);
    finally FreeAndNil(JsonData) end;
  finally
    FreeAndNil(Reader.FJsonReader);
    FreeAndNil(Reader);
  end;
end;

type
  TCastleComponentWriter = class
    class procedure AfterStreamObject(Sender: TObject; AObject: TObject; JSON: TJSONObject);
    class procedure StreamProperty(Sender: TObject; AObject: TObject; Info: PPropInfo; var Res: TJSONData);
  end;

class procedure TCastleComponentWriter.AfterStreamObject(
  Sender: TObject; AObject: TObject; JSON: TJSONObject);
begin
  { set _ClassName string, our reader depends on it }
  Json.Strings['_ClassName'] := AObject.ClassName;
end;

class procedure TCastleComponentWriter.StreamProperty(Sender: TObject;
  AObject: TObject; Info: PPropInfo; var Res: TJSONData);
begin
  // always save it
  if Info^.Name = 'Name' then
    Exit;

  // do not stream null values, as reader makes errors on them
  if Res is TJSONNull then
    FreeAndNil(Res);

  // do not stream properties with stored=false or default values
  if not IsStoredProp(AObject as TPersistent, Info) then
    FreeAndNil(Res);
end;

procedure ComponentSave(const C: TComponent; const Url: String);
var
  JsonWriter: TJSONStreamer;
  Json: TJSONObject;
begin
  JsonWriter := TJSONStreamer.Create(nil);
  try
    JsonWriter.Options := [jsoStreamChildren];
    JsonWriter.AfterStreamObject := @TCastleComponentWriter(nil).AfterStreamObject;
    JsonWriter.OnStreamProperty := @TCastleComponentWriter(nil).StreamProperty;
    JsonWriter.ChildProperty := '_Children';
    Json := JsonWriter.ObjectToJSON(C);
    try
      StringToFile(Url, Json.FormatJSON);
    finally FreeAndNil(Json) end;
  finally FreeAndNil(JsonWriter) end;
end;

procedure TransformSave(const T: TCastleTransform; const Url: String);
begin
  ComponentSave(T, Url);
end;

function TransformLoad(const Url: String; const Owner: TComponent): TCastleTransform;
begin
  Result := ComponentLoad(Url, Owner) as TCastleTransform;
end;

procedure UserInterfaceSave(const C: TUIControl; const Url: String);
begin
  ComponentSave(C, Url);
end;

function UserInterfaceLoad(const Url: String; const Owner: TComponent): TUIControl;
begin
  Result := ComponentLoad(Url, Owner) as TUIControl;
end;

end.
