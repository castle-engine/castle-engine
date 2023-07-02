{ Main view, where most of the application logic takes place.

  Feel free to use this code as a starting point for your own projects.
  This template code is in public domain, unlike most other CGE code which
  is covered by BSD or LGPL (see https://castle-engine.io/license). }
unit GameViewMain;

interface

uses Classes,
  CastleVectors, CastleComponentSerialize,
  CastleUIControls, CastleControls, CastleKeysMouse, CastleAds;

type
  { Main view, where most of the application logic takes place. }
  TViewMain = class(TCastleView)
  private
    Ads: TAds;
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    LabelFps: TCastleLabel;
    Button1: TCastleButton;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;

    procedure ShowAd(Sender: TObject);
  end;

var
  ViewMain: TViewMain;

implementation

uses SysUtils;

{ TViewMain ----------------------------------------------------------------- }

constructor TViewMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewmain.castle-user-interface';
  Ads := TAds.Create(AOwner);

  { Initialize with test ad ids }
  Ads.InitializeAdMob('ca-app-pub-3940256099942544/6300978111', 'ca-app-pub-3940256099942544/1033173712', 'ca-app-pub-3940256099942544/5224354917',
    [ ]);
end;

procedure TViewMain.Start;
begin
  inherited;
  Button1.OnClick := {$ifdef FPC}@{$endif}ShowAd;
end;

procedure TViewMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  { This virtual method is executed every frame (many times per second). }
  Assert(LabelFps <> nil, 'If you remove LabelFps from the design, remember to remove also the assignment "LabelFps.Caption := ..." from code');
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;
end;

procedure TViewMain.ShowAd(Sender: TObject);
begin
  Ads.ShowFullScreenAd(anAdMob, atInterstitialVideo, true);
end;

end.
