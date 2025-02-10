{
  Minimized for Castle Game Engine to support TPropInfoList,
  to be able to use it in Delphi.
}
{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2004 by the Free Pascal development team

    Some RTTI utils, based on RX rtti utils.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{ **********************************************************************

  Based on the rttiutils unit that comes with RXLib.
  Adapted to work with FCL, free of VCL dependencies.
  Fixed some errors along the way as well. MVC.

  To make it work across the 'Root Component' (Form/Datamodule etc),
  you MUST set the FindGlobalComponentCallBack event handler.

  Original copyright:
         Delphi VCL Extensions (RX)
         Copyright (c) 1995, 1996 AO ROSNO
         Copyright (c) 1997 Master-Bank
  **********************************************************************}

{$I castleconf.inc}

{$ifdef FPC}
{$mode objfpc}
{$H+}
{$endif}
unit RttiUtils;

interface

uses
  SysUtils, Classes, {Graphics, Controls, Forms,} TypInfo, StrUtils;

type

{ TPropInfoList }

  TPropInfoList = class(TObject)
  private
    FList: PPropList;
    FCount: Integer;
    FSize: Integer;
    function Get(Index: Integer): PPropInfo;
  public
    constructor Create(AObject: TObject; Filter: TTypeKinds);
    destructor Destroy; override;
    property Count: Integer read FCount;
    property Items[Index: Integer]: PPropInfo read Get; default;
  end;

implementation

{ TPropInfoList }

constructor TPropInfoList.Create(AObject: TObject; Filter: TTypeKinds);
begin
  if AObject <> nil then
    begin
    FCount := GetPropList(AObject.ClassInfo, Filter, nil);
    FSize := FCount * SizeOf(Pointer);
    GetMem(FList, FSize);
    GetPropList(AObject.ClassInfo, Filter, FList);
    end
  else
    begin
    FCount := 0;
    FList := nil;
    end;
end;

destructor TPropInfoList.Destroy;
begin
  if FList <> nil then FreeMem(FList, FSize);
end;

function TPropInfoList.Get(Index: Integer): PPropInfo;
begin
  Result := FList^[Index];
end;

end.
