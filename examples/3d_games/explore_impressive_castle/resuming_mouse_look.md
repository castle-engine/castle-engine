## Why we need to set `WalkNavigation.MouseLook := true` in `Resume`

The `Resume` method in `code/gameviewplay.pas` does


```delphi
WalkNavigation.MouseLook := true;
```

This is valid (necessary _and_ enough) to make our mouse look work again after resuming the game. Let's analyze why.

First of all, recall that `Resume` is a view method called when user resumed the game. See [views documentation](https://castle-engine.io/views).

Two ways how we can reach code of `Resume`:

## 1. `Resume` called right after `Start`

User starts playing the game, so engine calls `Start`, and then `Resume`.

OK, that's the obvious and trivial situation. We initialize `WalkNavigation.MouseLook` to `true`, we want mouse look to be always active when this view (`TViewPlay`) is the front-most view.

Note that, to achieve this, you could also just set `WalkNavigation.MouseLook` at design-time.

## 2. `Resume` called when we get back from "Options" view

This starts by the user pressing _Escape_ key. Let's analyze the flow on web and non-web platforms:

- Web: Escape is handled by the browser, which cancels pointer lock.

    See https://castle-engine.io/web#pointer_lock .

    On the engine side we capture this "pointer lock cancellation" and:

    - Engine sets `WalkNavigation.MouseLook` and `Container.PointerLock.Active` to `false` (reflecting the de-facto state of the pointer lock). Engine _will never automatically resume mouse look after it was cancelled by the user on web_, as it can be bad UX on web. You must explicitly re-enable mouse look when you want it back.

    - Engine calls `TViewPlay.PointerLockUserCancelled`, implemented in this application to just do `Container.PushView(ViewOptions)`.

- Non-web: Escape key is handled in `TViewPlay.Press`. `Press` calls `Container.PushView(ViewOptions)`.

    Note that `WalkNavigation.MouseLook` remains `true` while user looks at the "Options" screen. Nothing sets it `false`.

    But the mouse look is ineffective because `TViewPlay.Update` does `MainViewport.Items.Paused := not GameActive;` and now (when `ViewPlay` is not the front-most view) this is equivalent to `MainViewport.Items.Paused = true`. When the viewport is paused, mouse look is ineffective, so navigation sets `Container.PointerLock.Active` back to `false`.

- On both web and non-web platforms: User spends some time looking at the "Options" screen and eventually clicks there _"Resume Game"_ button.

- ... which calls `Container.PopView(ViewOptions)`

- Which calls `TViewPlay.Resume`.

So to make the 2nd flow work correctly on the web, we need to re-enable `WalkNavigation.MouseLook` on the web.

Note: On non-web platforms (like desktop), doing this in `Resume` is not necessary (though it doesn't hurt). On desktops, we could just do `WalkNavigation.MouseLook := true;` in `Start`. `WalkNavigation.MouseLook` remains true (and will be effective again when the viewport is unpaused) without the need for handling Resume.

## More information about pointer lock on web

See https://castle-engine.io/web#pointer_lock .
