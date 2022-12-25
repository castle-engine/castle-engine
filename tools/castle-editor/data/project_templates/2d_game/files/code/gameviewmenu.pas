{ Simple "menu" user interface, that allows to run the game or quit.

  Feel free to use this code as a starting point for your own projects.
  This template code is in public domain, unlike most other CGE code which
  is covered by BSD or LGPL (see https://castle-engine.io/license). }
unit GameViewMenu;

interface

uses Classes,
  CastleComponentSerialize, CastleUIControls, CastleControls;

type
  { Simple "menu" user interface, that allows to run the game or quit. }
  TViewMenu = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    ButtonPlay, ButtonQuit: TCastleButton;
  private
    procedure ClickPlay(Sender: TObject);
    procedure ClickQuit(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
  end;

var
  ViewMenu: TViewMenu;

implementation

uses CastleApplicationProperties, CastleWindow,
  GameViewPlay;

{ TViewMenu ----------------------------------------------------------------- }

constructor TViewMenu.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewmenu.castle-user-interface';
end;

procedure TViewMenu.Start;
begin
  inherited;
  ButtonPlay.OnClick := {$ifdef FPC}@{$endif} ClickPlay;
  ButtonQuit.OnClick := {$ifdef FPC}@{$endif} ClickQuit;
  // Hide "Quit" button on mobile/console platforms, where users don't expect such button
  ButtonQuit.Exists := ApplicationProperties.ShowUserInterfaceToQuit;
end;

procedure TViewMenu.ClickPlay(Sender: TObject);
begin
  Container.View := ViewPlay;
end;

procedure TViewMenu.ClickQuit(Sender: TObject);
begin
  Application.Terminate;
end;

end.
