{
  Copyright 2024 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ MCP server integration with Castle Game Engine editor.
  
  This unit provides real implementations of IMcpProjectProvider and 
  IMcpDesignProvider that integrate with the actual editor components.
}
unit CastleMcpEditorIntegration;

{$I castleconf.inc}

interface

uses
  Classes, SysUtils, fpjson, CastleUtils, CastleStringUtils,
  CastleUriUtils, CastleFilesUtils, CastleFindFiles, CastleLog,
  CastleClassUtils, CastleUIControls, CastleControls,
  ToolManifest, FormProject, FrameDesign, CastleMcpServer;

type
  { Real project provider that integrates with TProjectForm }
  TEditorProjectProvider = class(TInterfacedObject, IMcpProjectProvider)
  private
    FProjectForm: TProjectForm;
  public
    constructor Create(AProjectForm: TProjectForm);
    function GetProjectName: String;
    function GetProjectCaption: String;
    function GetProjectPath: String;
    function GetPascalFiles: TStringList;
    function GetDataFiles: TStringList;
  end;

  { Real design provider that integrates with TDesignFrame }
  TEditorDesignProvider = class(TInterfacedObject, IMcpDesignProvider)
  private
    FProjectForm: TProjectForm;
    
    { Helper methods }
    function GetDesignFrame: TDesignFrame;
    function ComponentToJson(Component: TComponent; const BasePath: String = ''): TJsonObject;
    function FindComponentByPath(const ComponentPath: String): TComponent;
    function GetComponentPropertyValue(Component: TComponent; const PropertyName: String): String;
    function SetComponentPropertyValue(Component: TComponent; const PropertyName, Value: String): Boolean;
    
  public
    constructor Create(AProjectForm: TProjectForm);
    function IsDesignOpen: Boolean;
    function GetComponentHierarchy: TJsonObject;
    function GetComponentProperty(const ComponentPath, PropertyName: String): String;
    function SetComponentProperty(const ComponentPath, PropertyName, Value: String): Boolean;
    function GetDesignScreenshot: String; // Returns base64-encoded PNG data
  end;

implementation

uses
  TypInfo, Variants, CastleControl, CastleRenderContext, CastleImages,
  CastleStreamUtils, base64;

{ TEditorProjectProvider }

constructor TEditorProjectProvider.Create(AProjectForm: TProjectForm);
begin
  inherited Create;
  FProjectForm := AProjectForm;
end;

function TEditorProjectProvider.GetProjectName: String;
begin
  if Assigned(FProjectForm) and Assigned(FProjectForm.Manifest) then
    Result := FProjectForm.Manifest.Name
  else
    Result := 'Unknown Project';
end;

function TEditorProjectProvider.GetProjectCaption: String;
begin
  if Assigned(FProjectForm) and Assigned(FProjectForm.Manifest) then
    Result := FProjectForm.Manifest.Caption
  else
    Result := 'Unknown Project';
end;

function TEditorProjectProvider.GetProjectPath: String;
begin
  if Assigned(FProjectForm) then
    Result := FProjectForm.ProjectPath
  else
    Result := '';
end;

function TEditorProjectProvider.GetPascalFiles: TStringList;
var
  ProjectPath: String;
  
  procedure FindPascalFiles(const Path: String; const Mask: String);
  var
    FileInfo: TFileInfo;
    Files: TFileInfoList;
    I: Integer;
  begin
    Files := TFileInfoList.Create(True);
    try
      FindFiles(Path, Mask, False, @Files.Add, [ffRecursive]);
      for I := 0 to Files.Count - 1 do
      begin
        FileInfo := Files[I];
        Result.Add(PrefixRemove(ProjectPath, FileInfo.AbsoluteName, True));
      end;
    finally
      FreeAndNil(Files);
    end;
  end;

begin
  Result := TStringList.Create;
  ProjectPath := GetProjectPath;
  if ProjectPath <> '' then
  begin
    FindPascalFiles(ProjectPath, '*.pas');
    FindPascalFiles(ProjectPath, '*.dpr');
    FindPascalFiles(ProjectPath, '*.lpr');
    FindPascalFiles(ProjectPath, '*.pp');
  end;
end;

function TEditorProjectProvider.GetDataFiles: TStringList;
var
  ProjectPath, DataPath: String;
  FileInfo: TFileInfo;
  Files: TFileInfoList;
  I: Integer;
begin
  Result := TStringList.Create;
  ProjectPath := GetProjectPath;
  if ProjectPath <> '' then
  begin
    DataPath := CombineURI(ProjectPath, 'data/');
    if DirectoryExists(UriToFilenameSafe(DataPath)) then
    begin
      Files := TFileInfoList.Create(True);
      try
        FindFiles(UriToFilenameSafe(DataPath), '*', False, @Files.Add, [ffRecursive]);
        for I := 0 to Files.Count - 1 do
        begin
          FileInfo := Files[I];
          if not FileInfo.Directory then
            Result.Add('data/' + PrefixRemove(UriToFilenameSafe(DataPath), FileInfo.AbsoluteName, True));
        end;
      finally
        FreeAndNil(Files);
      end;
    end;
  end;
end;

{ TEditorDesignProvider }

constructor TEditorDesignProvider.Create(AProjectForm: TProjectForm);
begin
  inherited Create;
  FProjectForm := AProjectForm;
end;

function TEditorDesignProvider.GetDesignFrame: TDesignFrame;
begin
  if Assigned(FProjectForm) then
    Result := FProjectForm.Design
  else
    Result := nil;
end;

function TEditorDesignProvider.IsDesignOpen: Boolean;
var
  DesignFrame: TDesignFrame;
begin
  DesignFrame := GetDesignFrame;
  Result := Assigned(DesignFrame) and Assigned(DesignFrame.DesignRoot);
end;

function TEditorDesignProvider.ComponentToJson(Component: TComponent; const BasePath: String): TJsonObject;
var
  Children: TJsonArray;
  ChildComponent: TComponent;
  ChildJson: TJsonObject;
  I: Integer;
  ComponentPath: String;
begin
  Result := TJsonObject.Create;
  
  if BasePath = '' then
    ComponentPath := Component.Name
  else
    ComponentPath := BasePath + '.' + Component.Name;
  
  Result.Add('name', Component.Name);
  Result.Add('class', Component.ClassName);
  Result.Add('path', ComponentPath);
  
  // Add children if any
  if Component.ComponentCount > 0 then
  begin
    Children := TJsonArray.Create;
    for I := 0 to Component.ComponentCount - 1 do
    begin
      ChildComponent := Component.Components[I];
      ChildJson := ComponentToJson(ChildComponent, ComponentPath);
      Children.Add(ChildJson);
    end;
    Result.Add('children', Children);
  end;
end;

function TEditorDesignProvider.GetComponentHierarchy: TJsonObject;
var
  DesignFrame: TDesignFrame;
begin
  DesignFrame := GetDesignFrame;
  if not Assigned(DesignFrame) or not Assigned(DesignFrame.DesignRoot) then
    raise Exception.Create('No design is currently open');
  
  Result := ComponentToJson(DesignFrame.DesignRoot);
end;

function TEditorDesignProvider.FindComponentByPath(const ComponentPath: String): TComponent;
var
  DesignFrame: TDesignFrame;
  PathParts: TStringList;
  CurrentComponent: TComponent;
  I: Integer;
  ComponentName: String;
  FoundComponent: TComponent;
  J: Integer;
begin
  Result := nil;
  DesignFrame := GetDesignFrame;
  if not Assigned(DesignFrame) or not Assigned(DesignFrame.DesignRoot) then
    Exit;
  
  PathParts := TStringList.Create;
  try
    SplitString(ComponentPath, '.', PathParts);
    if PathParts.Count = 0 then
      Exit;
    
    // Start with the root component
    CurrentComponent := DesignFrame.DesignRoot;
    if PathParts[0] <> CurrentComponent.Name then
      Exit;
    
    // Navigate through the path
    for I := 1 to PathParts.Count - 1 do
    begin
      ComponentName := PathParts[I];
      FoundComponent := nil;
      
      // Search in children
      for J := 0 to CurrentComponent.ComponentCount - 1 do
      begin
        if CurrentComponent.Components[J].Name = ComponentName then
        begin
          FoundComponent := CurrentComponent.Components[J];
          Break;
        end;
      end;
      
      if not Assigned(FoundComponent) then
        Exit; // Component not found in path
      
      CurrentComponent := FoundComponent;
    end;
    
    Result := CurrentComponent;
  finally
    FreeAndNil(PathParts);
  end;
end;

function TEditorDesignProvider.GetComponentPropertyValue(Component: TComponent; const PropertyName: String): String;
var
  PropInfo: PPropInfo;
  PropValue: Variant;
begin
  Result := '';
  if not Assigned(Component) then
    Exit;
  
  PropInfo := GetPropInfo(Component, PropertyName);
  if not Assigned(PropInfo) then
    raise Exception.CreateFmt('Property "%s" not found in component "%s"', [PropertyName, Component.ClassName]);
  
  case PropInfo^.PropType^.Kind of
    tkInteger, tkInt64:
      Result := IntToStr(GetOrdProp(Component, PropInfo));
    tkFloat:
      Result := FloatToStr(GetFloatProp(Component, PropInfo));
    tkString, tkLString, tkAString, tkUString, tkWString:
      Result := GetStrProp(Component, PropInfo);
    tkBool:
      Result := BoolToStr(GetOrdProp(Component, PropInfo) <> 0, True);
    tkEnumeration:
      Result := GetEnumName(PropInfo^.PropType, GetOrdProp(Component, PropInfo));
    else
      Result := Format('(unsupported property type: %s)', [GetEnumName(TypeInfo(TTypeKind), Ord(PropInfo^.PropType^.Kind))]);
  end;
end;

function TEditorDesignProvider.SetComponentPropertyValue(Component: TComponent; const PropertyName, Value: String): Boolean;
var
  PropInfo: PPropInfo;
  IntValue: Integer;
  FloatValue: Double;
  BoolValue: Boolean;
  EnumValue: Integer;
begin
  Result := False;
  if not Assigned(Component) then
    Exit;
  
  PropInfo := GetPropInfo(Component, PropertyName);
  if not Assigned(PropInfo) then
    Exit;
  
  try
    case PropInfo^.PropType^.Kind of
      tkInteger, tkInt64:
        begin
          IntValue := StrToInt(Value);
          SetOrdProp(Component, PropInfo, IntValue);
          Result := True;
        end;
      tkFloat:
        begin
          FloatValue := StrToFloat(Value);
          SetFloatProp(Component, PropInfo, FloatValue);
          Result := True;
        end;
      tkString, tkLString, tkAString, tkUString, tkWString:
        begin
          SetStrProp(Component, PropInfo, Value);
          Result := True;
        end;
      tkBool:
        begin
          BoolValue := StrToBoolDef(Value, False);
          SetOrdProp(Component, PropInfo, Ord(BoolValue));
          Result := True;
        end;
      tkEnumeration:
        begin
          EnumValue := GetEnumValue(PropInfo^.PropType, Value);
          if EnumValue >= 0 then
          begin
            SetOrdProp(Component, PropInfo, EnumValue);
            Result := True;
          end;
        end;
    end;
  except
    on E: Exception do
    begin
      WritelnLog('Error setting property %s.%s = %s: %s', [Component.Name, PropertyName, Value, E.Message]);
      Result := False;
    end;
  end;
end;

function TEditorDesignProvider.GetComponentProperty(const ComponentPath, PropertyName: String): String;
var
  Component: TComponent;
begin
  Component := FindComponentByPath(ComponentPath);
  if not Assigned(Component) then
    raise Exception.CreateFmt('Component not found: %s', [ComponentPath]);
  
  Result := GetComponentPropertyValue(Component, PropertyName);
end;

function TEditorDesignProvider.SetComponentProperty(const ComponentPath, PropertyName, Value: String): Boolean;
var
  Component: TComponent;
begin
  Component := FindComponentByPath(ComponentPath);
  if not Assigned(Component) then
    raise Exception.CreateFmt('Component not found: %s', [ComponentPath]);
  
  Result := SetComponentPropertyValue(Component, PropertyName, Value);
end;

function TEditorDesignProvider.GetDesignScreenshot: String;
var
  DesignFrame: TDesignFrame;
  CastleControl: TCastleControl;
  Screenshot: TRGBImage;
  Stream: TMemoryStream;
  Base64Data: String;
begin
  DesignFrame := GetDesignFrame;
  if not Assigned(DesignFrame) then
  begin
    WritelnLog('MCP', 'No design frame available for screenshot');
    // Return a minimal 1x1 transparent PNG in base64 as fallback
    Result := 'iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mNkYPhfDwAChAI9jU77zgAAAABJRU5ErkJggg==';
    Exit;
  end;

  CastleControl := DesignFrame.CastleControl;
  if not Assigned(CastleControl) then
  begin
    WritelnLog('MCP', 'No CastleControl available for screenshot');
    // Return fallback image
    Result := 'iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mNkYPhfDwAChAI9jU77zgAAAABJRU5ErkJggg==';
    Exit;
  end;

  try
    // Make sure the control is rendered and up to date
    CastleControl.Invalidate;
    Application.ProcessMessages;

    // Capture the screenshot
    Screenshot := CastleControl.SaveScreen;
    try
      Stream := TMemoryStream.Create;
      try
        // Save as PNG to memory stream
        SaveImage(Screenshot, Stream, 'png');
        Stream.Position := 0;

        // Convert to base64
        Base64Data := EncodeStringBase64(StreamToString(Stream));
        Result := Base64Data;

        WritelnLog('MCP', 'Screenshot captured successfully (%dx%d pixels, %d bytes base64)',
          [Screenshot.Width, Screenshot.Height, Length(Base64Data)]);
      finally
        FreeAndNil(Stream);
      end;
    finally
      FreeAndNil(Screenshot);
    end;
  except
    on E: Exception do
    begin
      WritelnLog('MCP', 'Error capturing screenshot: %s', [E.Message]);
      // Return fallback image on error
      Result := 'iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mNkYPhfDwAChAI9jU77zgAAAABJRU5ErkJggg==';
    end;
  end;
end;

end.
