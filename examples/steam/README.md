# Steam Test

Basic test of Steam integration with [Castle Game Engine](https://castle-engine.io/).

TODO:
- Move docs below to new "Steam integration" manual page, https://castle-engine.io/steam .
- Move there also parts of CastleSteam unitdocs.

TODO: indicate achievement progress fails:
Warning: Failed to SteamAPI_ISteamUserStats_IndicateAchievementProgress

## How to run

- Download Steam dynamic library and copy it into this project, following the instructions in `src/services/steam/castlesteam.pas` (TODO link to API docs).

- Then just compile and run this application as usual.

## Steam application id

By default we are using AppID of SteamWorks game example - SpaceWar (see https://partner.steamgames.com/doc/sdk/api/example ). Note that using this example will add this game to your Steam library.

Once you have created your own application on Steamworks, you can change the AppID to your own.

The application id is specified in 2 places, be sure to change them both:

1. `AppId` constant in `GameInitialize` unit
2. `steam_appid.txt` file in the same directory as the executable

Make sure that you own the app, that you have it is in your Steam library and that it has achievements set up, otherwise no achievements will be displayed here.

## Testing the Steam integration

Depending on how you run the game (clicking "Play" in Steam, or by running the exe) and whether the `steam_appid.txt` file is present, the Steam integration will behave a bit differently.

The important advise is: When you upload the application to Steam, or when you give end-users a ZIP with the game (with Steam integration included), *do not* include the `steam_appid.txt` file. The `steam_appid.txt` file is supposed to be used only during development -- see below, it prevents from ever restarting the process.

TODO: test all below.

Details, how the game behaves:

1. _Ways to run the game for end-users_:

    1. _Running the game by clicking "Play" from Steam_: This is the most common way how end-users run a Steam game. Of course Steam integration "just works" in this case.

    2. _Running the game by running the exe file directly (without the `steam_appid.txt` file present)_: In this case, the exe will immediately close and restart the game through Steam. It will even start Steam (and display Steam login dialog if necessary), if Steam was not running already.

        This even works if Steam is not *installed* (we tested this case on Windows). In this case, the game will *not* restart, and it will continue to work (without Steam integration of course).

        This is a nice way when you want to distribute your game as a ZIP file but you want it to still integrate with Steam (e.g. report achievements), if only user has Steam. TODO: What happens if user doesn't own Steam.

2. _Ways to run the game that should only be used during development_:

    1. _Running the game by running the exe file directly (with the `steam_appid.txt` file present), when Steam runs in the background_: Steam integration still works (and the game doesn't need to restart).

    2. _Running the game by running the exe file directly (with the `steam_appid.txt` file present), when Steam does not run in the background_: Steam integration will not work. But nothing crashes etc. So it's not a problem, just remember during testing that Steam achievements are not collected in this case.

## Building

Compile by:

- [CGE editor](https://castle-engine.io/manual_editor.php). Just use menu item _"Compile"_.

- Or use [CGE command-line build tool](https://castle-engine.io/build_tool). Run `castle-engine compile` in this directory.

- Or use [Lazarus](https://www.lazarus-ide.org/). Open in Lazarus `SteamTest_standalone.lpi` file and compile / run from Lazarus. Make sure to first register [CGE Lazarus packages](https://castle-engine.io/documentation.php).
