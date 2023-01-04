{
  Copyright 2022-2022 Michalis Kamburelis, Andrzej Kilijański.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Internal utilities. }
unit CastleInternalClassUtils;

interface

uses SysUtils, Classes, Generics.Collections;

type
  TComponentEvent = procedure(const Component: TComponent) of object;

  TCastleComponentNotification = class(TComponent)
  private
    FEventList: {$ifdef FPC}specialize{$endif} TList<TComponentEvent>;
    FFilterList: {$ifdef FPC}specialize{$endif} TList<TComponentClass>;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

  public
    { Adds callback for without class filter so you will get notifications for
      all classes (the same like AddNotification(AEvent, TComponent)) }
    procedure AddNotification(const AEvent: TComponentEvent); overload;
    { Adds callback and gives you choise about what classes you want be notified
      e.g. AddNotification(AEvent, TCastleBehavior); }
    procedure AddNotification(const AEvent: TComponentEvent;
       const ClassFilter: TComponentClass); overload;
    { Removes all occurrences of the AEvent }
    procedure RemoveNotification(const AEvent: TComponentEvent); overload;
    { Removes only occurrences of the AEvent with specified ClassFilter }
    procedure RemoveNotification(const AEvent: TComponentEvent;
       const ClassFilter: TComponentClass); overload;

    { Procedure to send notification }
    procedure Notify(const AComponent: TComponent);

    destructor Destroy; override;
  end;

  { Raised when trying add event callback to TCastleComponentNotification that
    parent is not TComponent }
  ECastleComponentNotification = class (Exception);

implementation

{ TCastleComponentNotification -------------------------------------------------------- }

procedure TCastleComponentNotification.Notification(AComponent: TComponent;
  Operation: TOperation);
var
  I: Integer;
begin
  inherited Notification(AComponent, Operation);

  if Operation = opRemove then
  begin
    for I := FEventList.Count - 1 downto 0 do
    begin
      if TComponent(TMethod(FEventList[I]).Data) = AComponent then
        FEventList.Delete(I);
    end;
  end;
end;

procedure TCastleComponentNotification.AddNotification(const AEvent: TComponentEvent);
begin
  AddNotification(AEvent, TComponent);
end;

procedure TCastleComponentNotification.AddNotification(
  const AEvent: TComponentEvent; const ClassFilter: TComponentClass);
var
  EventComponent: TComponent;
begin
  EventComponent := TObject(TMethod(AEvent).Data) as TComponent;

  if EventComponent = nil then
    raise ECastleComponentNotification.Create(
      'You can only add callbacks form TComponent instances to TCastleComponentNotification.');

  if FEventList = nil then
  begin
    FEventList := {$ifdef FPC}specialize{$endif} TList<TComponentEvent>.Create;
    FFilterList := {$ifdef FPC}specialize{$endif} TList<TComponentClass>.Create;
  end;

  FEventList.Add(AEvent);
  FFilterList.Add(ClassFilter);
  EventComponent.FreeNotification(Self);
end;

procedure TCastleComponentNotification.RemoveNotification(
  const AEvent: TComponentEvent);
var
  EventComponent: TComponent;
  Index: Integer;
begin
  if FEventList = nil then
    Exit;

  EventComponent := TObject(TMethod(AEvent).Data) as TComponent;
  if EventComponent = nil then
    Exit;

  Index := FEventList.IndexOf(AEvent);
  while Index <> -1 do
  begin
    FEventList.Delete(Index);
    FFilterList.Delete(Index);

    Index := FEventList.IndexOf(AEvent);
  end;
  EventComponent.RemoveFreeNotification(Self);
end;

procedure TCastleComponentNotification.RemoveNotification(
  const AEvent: TComponentEvent; const ClassFilter: TComponentClass);
var
  EventComponent: TComponent;
  I: Integer;
begin
  if FEventList = nil then
    Exit;

  EventComponent := TObject(TMethod(AEvent).Data) as TComponent;
  if EventComponent = nil then
    Exit;

  for I := FEventList.Count - 1 downto 0 do
  begin
    if TComponent(TMethod(FEventList[I]).Data) is ClassFilter then
    begin
      FEventList.Delete(I);
      FFilterList.Delete(I);
    end;
  end;

  if FEventList.IndexOf(AEvent) < 0 then
    EventComponent.RemoveFreeNotification(Self);
end;

procedure TCastleComponentNotification.Notify(const AComponent: TComponent);
var
  I: Integer;
begin
  for I := 0 to FEventList.Count -1 do
  begin
    if AComponent is FFilterList[I] then
       FEventList[I](AComponent);
  end;
end;

destructor TCastleComponentNotification.Destroy;
var
  I: integer;
begin
  for I := 0 to FEventList.Count - 1 do
    TComponent(TMethod(FEventList[i]).Data).RemoveFreeNotification(Self);

  FreeAndNil(FEventList);
  FreeAndNil(FFilterList);
  inherited Destroy;
end;

end.
