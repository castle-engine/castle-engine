{
  Copyright 2001-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Deprecated CastleDialogViews name. }
unit CastleDialogStates;

interface

uses CastleDialogViews;

type
  TStateDialog = TViewDialog deprecated 'use TViewDialog';
  TStateDialogOK = TViewDialogOK deprecated 'use TViewDialogOK';
  TStateDialogYesNo = TViewDialogYesNo deprecated 'use TViewDialogYesNo';
  TStateDialogChoice = TViewDialogChoice deprecated 'use TViewDialogChoice';
  TStateDialogInput = TViewDialogInput deprecated 'use TViewDialogInput';
  TStateDialogKey = TViewDialogKey deprecated 'use TViewDialogKey';
  TStateDialogPressEvent = TViewDialogPressEvent deprecated 'use TViewDialogPressEvent';

implementation

end.
