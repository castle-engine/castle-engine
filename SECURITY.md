# Security Policy

## What Security Vulnerabilities May Happen in a Game Engine?

_Castle Game Engine_ is, from some point of view, 

- a consumer of many input files (3D models, images, JSON descriptions of UI and more...) 

- and a consumer of user input

- and outputs a simulation (rendering etc.) of the described content.

Security is _usually_ not a big point to worry in a game engine (in contrast to e.g. a web application). The game input is _most of the time, in typical single-user games_ hardcoded (your [game data](https://castle-engine.io/data) is provided with your game) and user input is limited. And the output is just a local simulation, not something that gets send over the Internet or determines how something else works.

But, if you read the above paragraph carefully, you will likely notice that there are cases when security matters, a _lot_, for any software, including a game engine.

Whenever your application allows user-provided data, you need to worry about security. For example 

- [Castle Model Viewer](https://castle-engine.io/castle-model-viewer) allows to open arbitrary 3D / 2D models and we promise it will never crash or do anything surprising on any input (even malformed). You obviously don't want a possibility when opening a random 3D model from the Internet enables attacker a code execution on your computer.

- Similarly, your game may allow users to use MODs, either done by themselves or others. These may come as local files, or be downloaded from the Internet. In neither case do you want to allow the MODs to crash your game or interact (break) unrelated game pieces. 

- This is not only about security, but also about robustness. Even aside from security concerns (like code execution), you don't want to allow a random 3D model to crash your game -- it's much better to have reliable error that shows the problem.

So this is our guarantee:

**Bad input should be handled gracefully. Either by a warning or an error (Pascal exception). It should never result in a crash or undefined behavior. It should never access a memory that it should not, and obviously it should never allow an bad actor (that controls only the input) to execute arbitrary code.**

This also goes for any other input that we process. E.g. a mutli-player game means we consume packets from the Internet -- again, we guarantee we will not crash or do anything unpredictable on any incoming data, even incorrect. This will matter for us more once we [integrate full multi-player solution](https://castle-engine.io/roadmap#_integration_with_nakama_scalable_server_for_social_and_real_time_games_and_apps) in the engine.

## Supported Versions

For security concerns, we support only the latest stable release of _Castle Game Engine_. This implies the latest stable version on https://castle-engine.io/download and https://github.com/castle-engine/castle-engine/releases . 

In case this points to `snapshot` version, then we also support last non-snapshot version, like `7.0-alpha.3`.

This is supported _only on systems that have security support from their vendors_. For example, we do not support _Windows XP_, even if you can get the compiler and our engine to work there. Again: **We only test and support systems that have security support from their vendors.** We strongly believe you should never ever use software that has no security support. No matter how much you like your Windows XP :), it's time to move on, either to a newer Windows or to a different system, like Linux.

## Reporting a Vulnerability

Plese report security vulnerabilities to `michalis@castle-engine.io` . Thank you!
