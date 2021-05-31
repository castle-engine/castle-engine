unit DataModuleIcons;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls;

type
  TIcons = class(TDataModule)
    ToolbarStroke2: TImageList;
    ToolbarStroke1_5: TImageList;
    MenuStroke1_5: TImageList;
  private

  public

  end;

var
  Icons: TIcons;

implementation

{$R *.lfm}

end.

