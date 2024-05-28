# Android project files

This subdirectory contains the

- Android project skeleton files in `integrated`.

- Additional Android services in `integrated-services`.

    See
    - [Android Services](https://castle-engine.io/android_services)
    - [Adding New Android Services](https://castle-engine.io/adding_new_android_services)

## Why `integrated` name?

The name `integrated` indicates that the project can be integrated with services using Java code.

History: we used to have 2 project types: `basic` (no Java at all, just use `NativeActivity`) and `integrated` (own Java code, services possible). With time, `basic` project type became useless (no gain from using it; and it was a trap for users, trying to use it and surprised that a lot of things don't work, e.g. audio; and it was unnecessary burden to maintain). So now we only support `integrated`.

## Upgrading Gradle version

The project contains also Gradle setup to build the project.

Upgrading Gradle:

1. Create any project using Gradle, e.g. build any CGE project for Android:

    ```
    cd ..../castle-openai/
    castle-engine package --target=android --fast --mode=debug
    cd castle-engine-output/android/project # for subsequent commands
    ```

    Note: You cannot directly upgrade Gradle here, in `integrated` project skeleton that doesn't build. You really need some new project, really any project using Gradle (doesn't even have to be CGE project).

2. Upgrade Gradle wrapper:

    - See latest Gradle version on https://docs.gradle.org/current/userguide/gradle_wrapper.html
    - Change `gradle/wrapper/gradle-wrapper.properties` to use new version
    - Run `bash ./gradlew wrapper --gradle-version 8.7` or `./gradlew wrapper --gradle-version latest`
    - Copy `gradle/wrapper/` contents (2 files: `gradle-wrapper.properties`, `gradle-wrapper.jar`) over CGE files in `integrated/gradle/wrapper`.

    See https://docs.gradle.org/current/userguide/gradle_wrapper.html#sec:upgrading_wrapper for explanation.