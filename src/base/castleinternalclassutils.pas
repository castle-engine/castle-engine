{
  Copyright 2022-2026 Michalis Kamburelis, Andrzej Kilijański.

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

uses SysUtils, Classes, Generics.Collections,
  CastleUtils;

type
  TComponentEvent = procedure (const Component: TComponent) of object;

  { Notifications as a list of TComponentEvent callbacks.

    TODO: Prefer using TNotifyEventList in new code, and remove this class?
    TNotifyEventList is simpler and practically fills the same need. }
  TCastleComponentNotification = class(TComponent)
  private
    FEventList: {$ifdef FPC}specialize{$endif} TMethodList<TComponentEvent>;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    procedure AddNotification(const AEvent: TComponentEvent); overload;
    procedure RemoveNotification(const AEvent: TComponentEvent); overload;

    { Send notification now. }
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

  if (Operation = opRemove) and (FEventList <> nil) then
  begin
    for I := FEventList.Count - 1 downto 0 do
    begin
      if TComponent(TMethod(FEventList[I]).Data) = AComponent then
        FEventList.Delete(I);
    end;
  end;
end;

procedure TCastleComponentNotification.AddNotification(const AEvent: TComponentEvent);
var
  EventComponent: TComponent;
begin
  if not (TObject(TMethod(AEvent).Data) is TComponent) then
    raise ECastleComponentNotification.Create(
      'You can only add callbacks from TComponent instances to TCastleComponentNotification.');
  EventComponent := TObject(TMethod(AEvent).Data) as TComponent;

  if FEventList = nil then
    FEventList := {$ifdef FPC}specialize{$endif} TMethodList<TComponentEvent>.Create;

  FEventList.Add(AEvent);
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

  Index := FEventList.IndexOf(AEvent);
  while Index <> -1 do
  begin
    FEventList.Delete(Index);
    Index := FEventList.IndexOf(AEvent);
  end;
  EventComponent.RemoveFreeNotification(Self);
end;

procedure TCastleComponentNotification.Notify(const AComponent: TComponent);
var
  I: Integer;
begin
  if FEventList = nil then
    Exit;
  for I := 0 to FEventList.Count - 1 do
    FEventList[I](AComponent);
end;

destructor TCastleComponentNotification.Destroy;
var
  I: Integer;
begin
  if FEventList <> nil then
  begin
    for I := 0 to FEventList.Count - 1 do
      TComponent(TMethod(FEventList[I]).Data).RemoveFreeNotification(Self);
    FreeAndNil(FEventList);
  end;
  inherited;
end;

end.
