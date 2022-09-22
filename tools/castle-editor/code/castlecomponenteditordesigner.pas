{
  Copyright 2022-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Define TConcreteEditorDesigner, necessary to use LCL component editors
  from CGE editor. }
unit CastleComponentEditorDesigner;

interface

uses Classes, Controls, ComponentReg, ComponentEditors, PropEdits, Menus,
  LMessages, LCLType,
  // CGE units
  CastleClassUtils, CastleEditorAccess,
  // editor units
  FrameDesign, DesignUndoSystem;

type
  { To get registered component editors (and our CastlePropEdits registers some,
    e.g. to define some verbs, e.g. "Reset Transform" on TCastleTransform)
    we need to define TComponentEditorDesigner descendant that defines
    some functionality using TComponentEditorDesigner API.

    An instance of this corresponds 1-1 to each TDesignFrame,
    and will be passed as parameter of GetComponentEditor. }
  TConcreteEditorDesigner = class(TCastleComponentEditorDesigner)
  strict private
    FDesignFrame: TDesignFrame;
    FPropertyEditorHook: TPropertyEditorHook;
  protected
    function GetPropertyEditorHook: TPropertyEditorHook; override;
    function GetShowNonVisualComponents: boolean; override;
    procedure SetShowNonVisualComponents(AValue: boolean); override;
  public
    constructor Create(const ADesignFrame: TDesignFrame;
      const APropertyEditorHook: TPropertyEditorHook); reintroduce;
    procedure Modified; override;
    function CopySelection: boolean; override;
    function CutSelection: boolean; override;
    function CanCopy: Boolean; override;
    function CanPaste: Boolean; override;
    function PasteSelection(Flags: TComponentPasteSelectionFlags): boolean; override;
    function ClearSelection: boolean; override;
    function DeleteSelection: boolean; override;
    function CopySelectionToStream(s: TStream): boolean; override;
    function InsertFromStream(s: TStream; Parent: TWinControl; Flags: TComponentPasteSelectionFlags): Boolean; override;
    function InvokeComponentEditor(AComponent: TComponent): boolean; override;
    function ChangeClass: boolean; override;
    function CanUndo: Boolean; override;
    function CanRedo: Boolean; override;
    function Undo: Boolean; override;
    function Redo: Boolean; override;
    function AddUndoAction(const aPersistent: TPersistent; aOpType: TUndoOpType;
      IsSetNewId: boolean; aFieldName: string; const aOldVal, aNewVal: variant): boolean; override;
    function IsUndoLocked: boolean; override;
    procedure AddComponent(const NewRegisteredComponent: TRegisteredComponent;
      const NewComponentClass: TComponentClass;
      const NewParent: TComponent;
      const NewLeft,NewTop,NewWidth,NewHeight: Integer); override;
    procedure AddComponentCheckParent(var NewParent: TComponent;
      const OriginComponent: TComponent; const OriginWinControl: TWinControl;
      const NewComponentClass: TComponentClass); override;
    procedure DrawDesignerItems(OnlyIfNeeded: boolean); override;
    function CreateUniqueComponentName(const AClassName: string): string; override;
    function IsDesignMsg(Sender: TControl; var Message: TLMessage): Boolean; override;
    procedure UTF8KeyPress(var UTF8Key: TUTF8Char); override;
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
    procedure PaintGrid; override;
    procedure ValidateRename(AComponent: TComponent;
      const CurName, NewName: string); override;
    function GetShiftState: TShiftState; override;
    procedure SelectOnlyThisComponent(AComponent: TComponent); override;
    function UniqueName(const BaseName: string): string; override;
    procedure PrepareFreeDesigner(AFreeComponent: boolean); override;
    procedure ProposeOpenDesign(const DesignUrl: String); override;
    procedure FreeComponentRecursively(const C: TComponent); override;
  end;

  { Helpful class to put in menu to execute a verb from given component editor. }
  TMenuItemToExecuteVerb = class(TMenuItem)
  strict private
    FComponentToExecute: TComponent;
    FComponentToExecuteObserver: TFreeNotificationObserver;
    procedure SetComponentToExecute(const AValue: TComponent);
    procedure ComponentToExecuteFreeNotification(const Sender: TFreeNotificationObserver);
  public
    VerbIndex: Integer;
    ComponentEditor: TBaseComponentEditor;
    constructor Create(AOwner: TComponent); override;
    property ComponentToExecute: TComponent read FComponentToExecute write SetComponentToExecute;
    procedure Click; override;
  end;

implementation

uses CastleLog;

{ TConcreteEditorDesigner --------------------------------------------- }

constructor TConcreteEditorDesigner.Create(const ADesignFrame: TDesignFrame;
  const APropertyEditorHook: TPropertyEditorHook);
begin
  inherited Create;
  FDesignFrame := ADesignFrame;
  FPropertyEditorHook := APropertyEditorHook;
end;

function TConcreteEditorDesigner.GetPropertyEditorHook: TPropertyEditorHook;
begin
  Result := FPropertyEditorHook;
end;

function TConcreteEditorDesigner.GetShowNonVisualComponents: boolean;
begin
  Result := true; // TODO, unused in practice by CGE components
end;

procedure TConcreteEditorDesigner.SetShowNonVisualComponents(
  AValue: boolean);
begin
  // TODO, unused in practice by CGE components
end;

procedure TConcreteEditorDesigner.Modified;
begin
  inherited;
  FDesignFrame.ModifiedOutsideObjectInspector('', ucLow);
end;

function TConcreteEditorDesigner.CopySelection: boolean;
begin
  Result := false; // TODO, unused in practice by CGE components
end;

function TConcreteEditorDesigner.CutSelection: boolean;
begin
  Result := false; // TODO, unused in practice by CGE components
end;

function TConcreteEditorDesigner.CanCopy: Boolean;
begin
  Result := false; // TODO, unused in practice by CGE components
end;

function TConcreteEditorDesigner.CanPaste: Boolean;
begin
  Result := false; // TODO, unused in practice by CGE components
end;

function TConcreteEditorDesigner.PasteSelection(
  Flags: TComponentPasteSelectionFlags): boolean;
begin
  Result := false; // TODO, unused in practice by CGE components
end;

function TConcreteEditorDesigner.ClearSelection: boolean;
begin
  Result := false; // TODO, unused in practice by CGE components
end;

function TConcreteEditorDesigner.DeleteSelection: boolean;
begin
  Result := false; // TODO, unused in practice by CGE components
end;

function TConcreteEditorDesigner.CopySelectionToStream(s: TStream
  ): boolean;
begin
  Result := false; // TODO, unused in practice by CGE components
end;

function TConcreteEditorDesigner.InsertFromStream(s: TStream;
  Parent: TWinControl; Flags: TComponentPasteSelectionFlags): Boolean;
begin
  Result := false; // TODO, unused in practice by CGE components
end;

function TConcreteEditorDesigner.InvokeComponentEditor(
  AComponent: TComponent): boolean;
begin
  Result := false; // TODO, unused in practice by CGE components
end;

function TConcreteEditorDesigner.ChangeClass: boolean;
begin
  Result := false; // TODO, unused in practice by CGE components
end;

function TConcreteEditorDesigner.CanUndo: Boolean;
begin
  Result := false; // TODO, unused in practice by CGE components
end;

function TConcreteEditorDesigner.CanRedo: Boolean;
begin
  Result := false; // TODO, unused in practice by CGE components
end;

function TConcreteEditorDesigner.Undo: Boolean;
begin
  Result := false; // TODO, unused in practice by CGE components
end;

function TConcreteEditorDesigner.Redo: Boolean;
begin
  Result := false; // TODO, unused in practice by CGE components
end;

function TConcreteEditorDesigner.AddUndoAction(
  const aPersistent: TPersistent; aOpType: TUndoOpType; IsSetNewId: boolean;
  aFieldName: string; const aOldVal, aNewVal: variant): boolean;
begin
  Result := false; // TODO, unused in practice by CGE components
end;

function TConcreteEditorDesigner.IsUndoLocked: boolean;
begin
  Result := false; // TODO, unused in practice by CGE components
end;

procedure TConcreteEditorDesigner.AddComponent(
  const NewRegisteredComponent: TRegisteredComponent;
  const NewComponentClass: TComponentClass; const NewParent: TComponent;
  const NewLeft, NewTop, NewWidth, NewHeight: Integer);
begin
  // TODO, unused in practice by CGE components
end;

procedure TConcreteEditorDesigner.AddComponentCheckParent(
  var NewParent: TComponent; const OriginComponent: TComponent;
  const OriginWinControl: TWinControl; const NewComponentClass: TComponentClass
  );
begin
  // TODO, unused in practice by CGE components
end;

procedure TConcreteEditorDesigner.DrawDesignerItems(OnlyIfNeeded: boolean
  );
begin
  // TODO, unused in practice by CGE components
end;

function TConcreteEditorDesigner.CreateUniqueComponentName(
  const AClassName: string): string;
begin
  // TODO, unused in practice by CGE components
  Result := AClassName;
end;

function TConcreteEditorDesigner.IsDesignMsg(Sender: TControl;
  var Message: TLMessage): Boolean;
begin
  Result := false; // TODO, unused in practice by CGE components
end;

procedure TConcreteEditorDesigner.UTF8KeyPress(var UTF8Key: TUTF8Char);
begin
  // TODO, unused in practice by CGE components
end;

procedure TConcreteEditorDesigner.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  // TODO, unused in practice by CGE components
end;

procedure TConcreteEditorDesigner.PaintGrid;
begin
  // TODO, unused in practice by CGE components
end;

procedure TConcreteEditorDesigner.ValidateRename(AComponent: TComponent;
  const CurName, NewName: string);
begin
  // TODO, unused in practice by CGE components
end;

function TConcreteEditorDesigner.GetShiftState: TShiftState;
begin
  Result := []; // TODO, unused in practice by CGE components
end;

procedure TConcreteEditorDesigner.SelectOnlyThisComponent(
  AComponent: TComponent);
begin
  // TODO, unused in practice by CGE components
end;

function TConcreteEditorDesigner.UniqueName(const BaseName: string
  ): string;
begin
  Result := BaseName; // TODO, unused in practice by CGE components
end;

procedure TConcreteEditorDesigner.PrepareFreeDesigner(
  AFreeComponent: boolean);
begin
  // TODO, unused in practice by CGE components
end;

procedure TConcreteEditorDesigner.ProposeOpenDesign(const DesignUrl: String);
begin
  if Assigned(FDesignFrame.OnProposeOpenDesign) then
    FDesignFrame.OnProposeOpenDesign(DesignUrl);
end;

procedure TConcreteEditorDesigner.FreeComponentRecursively(const C: TComponent);
begin
  FDesignFrame.FreeComponentRecursively(C);
end;

{ TMenuItemToExecuteVerb ----------------------------------------------------- }

constructor TMenuItemToExecuteVerb.Create(AOwner: TComponent);
begin
  inherited;
  { We secure this menu item in case ComponentToExecute will be freed.
    This didn't happen in practice, but it seems possible (if we would
    not update popup menu often enough) so better secure from it. }
  FComponentToExecuteObserver := TFreeNotificationObserver.Create(Self);
  FComponentToExecuteObserver.OnFreeNotification := {$ifdef FPC}@{$endif} ComponentToExecuteFreeNotification;
end;

procedure TMenuItemToExecuteVerb.Click;
begin
  inherited;
  if ComponentToExecute = nil then
  begin
    WritelnWarning('Cannot execute verb of component editor, because the component was freed in the meantime');
    Exit;
  end;
  ComponentEditor.ExecuteVerb(VerbIndex);
end;

procedure TMenuItemToExecuteVerb.SetComponentToExecute(const AValue: TComponent);
begin
  if FComponentToExecute <> AValue then
  begin
    FComponentToExecute := AValue;
    FComponentToExecuteObserver.Observed := AValue;
  end;
end;

procedure TMenuItemToExecuteVerb.ComponentToExecuteFreeNotification(
  const Sender: TFreeNotificationObserver);
begin
  ComponentToExecute := nil;
end;

end.
