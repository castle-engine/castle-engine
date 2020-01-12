{%MainUnit castlepropedits.pas}
{
  Copyright 2010-2019 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ TODO: This is not used anymore.
  Use CGE editor to edit CGE user interface now.
  In the future, we will allow calling "embedded" CGE editor version
  to edit LCL form TCastleControl. }

(*

{ Define this for new Lazarus that has Options (with ocoRenderAtDesignTime)
  (see issue https://bugs.freepascal.org/view.php?id=32026 ). }
{$if LCL_FULLVERSION >= 1090000}
  {$define HAS_RENDER_AT_DESIGN_TIME}
{$endif}

{ TChildrenControlsPropertyEditor -------------------------------------------- }

type
  TChildrenControlsPropertyEditor = class(TListPropertyEditor)
  end;

{ T3DEditorForm -------------------------------------------------------------- }

type
  T3DEditorForm = class(TComponent)
    Control: TCastleControl;
    procedure ShowModal;
  end;

procedure T3DEditorForm.ShowModal;
begin
  if MessageDlg('Edit Castle Game Engine Contents',
    'TODO: Here we will implement a real editor to edit TCastleUserInterface.' + NL +
    'For now: activate CGE rendering?',
    mtConfirmation, mbYesNo, 0) = mrYes then
  begin
    {$ifdef HAS_RENDER_AT_DESIGN_TIME}
    Control.Options := Control.Options + [ocoRenderAtDesignTime];
    {$endif}
  end;
end;

{ TCastleRootTransformPropertyEditor ----------------------------------------------------- }

type
  TCastleRootTransformPropertyEditor = class(TClassPropertyEditor)
  public
    procedure Edit; Override;
    function  GetAttributes: TPropertyAttributes; Override;
  end;

procedure TCastleRootTransformPropertyEditor.Edit;
var
  Dialog: T3DEditorForm;
begin
  Dialog := T3DEditorForm.Create(nil);
  try
    Dialog.Items := TCastleRootTransform(GetObjectValue(TCastleRootTransform));
    Dialog.ShowModal;
  finally FreeAndNil(Dialog) end;
end;

function TCastleRootTransformPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paSubProperties, {paDialog,} paReadOnly];
end;

{ TCastleControlComponentEditor ---------------------------------------------- }

type
  TCastleControlComponentEditor = class(TComponentEditor)
  protected
    procedure DoShowEditor;
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): String; override;
    function GetVerbCount: Integer; override;
  end;

procedure TCastleControlComponentEditor.DoShowEditor;
var
  Dialog: T3DEditorForm;
  Control: TCastleControl;
begin
  Dialog := T3DEditorForm.Create(nil);
  try
    Control := GetComponent as TCastleControl;
    Dialog.Control := Control;
    Dialog.ShowModal;
    Control.Invalidate;
  finally FreeAndNil(Dialog) end;
end;

procedure TCastleControlComponentEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0: DoShowEditor;
  end;
end;

function TCastleControlComponentEditor.GetVerb(Index: Integer): String;
begin
  Result := 'Edit In Castle Game Engine...';
end;

function TCastleControlComponentEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;

procedure RegisterPropEditsControls;
begin
  { TODO: crashes
  RegisterPropertyEditor(TypeInfo(TChildrenControls), TCastleControlBase,
    'Controls', TChildrenControlsPropertyEditor);
  }
  // RegisterComponentEditor(TCastleControl, TCastleControlComponentEditor);
end;
*)
