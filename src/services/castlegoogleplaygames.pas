{
  Copyright 2015-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Google Play Game Services integration (TGooglePlayGames). }
unit CastleGooglePlayGames deprecated 'use CastleGameService unit';

{$I castleconf.inc}

interface

uses CastleGameService;

type
  TPlayerBestScoreEvent = CastleGameService.TPlayerBestScoreEvent;
  TSaveGameChoice       = CastleGameService.TSaveGameChoice;
  TSaveGameChosenEvent  = CastleGameService.TSaveGameChosenEvent;
  TSaveGameLoadedEvent  = CastleGameService.TSaveGameLoadedEvent;
  TGooglePlayGames      = CastleGameService.TGameService deprecated 'use TGameService';

const
  sgCancel   = CastleGameService.sgCancel;
  sgNew      = CastleGameService.sgNew;
  sgExisting = CastleGameService.sgExisting;

implementation

end.
