/* -*- mode: groovy -*-
  Configure how to run our job in Jenkins.
  See https://castle-engine.io/jenkins .
*/

pipeline {
  options {
    /* While concurrent builds of CGE work OK,
       they stuck Jenkins much with too many long-running builds.
       Better to wait for previous build to finish. */
    disableConcurrentBuilds()
    /* Makes failure in any paralel job to stop the build,
       instead of needlessly trying to finish on one node,
       when another node already failed. */
    parallelsAlwaysFailFast()
  }

  /* This job works on a few agents in parallel */
  agent none

  parameters {
    booleanParam(name: 'jenkins_fast', defaultValue: false, description: 'Use at emergencies, to make pipeline build faster')
  }

  stages {
    /* Build for each platform in parallel.
       See https://stackoverflow.com/questions/43913698/jenkinsfile-parallel-directive
       https://www.jenkins.io/blog/2017/09/25/declarative-1/
       for parallel syntax. */
    stage('Run parallel builds') {
      parallel {
        stage('Docker (Linux)') {
          agent {
            docker {
              image 'kambi/castle-engine-cloud-builds-tools:cge-none'
            }
          }
          environment {
            /* Used by CGE build tool ("castle-engine").
               Define env based on another env variable.
               According to https://github.com/jenkinsci/pipeline-model-definition-plugin/pull/110
               this should be supported. */
            CASTLE_ENGINE_PATH = "${WORKSPACE}"
          }
          stages {
            stage('(Docker) Cleanup') {
              steps {
                sh "repository_cleanup . --remove-unversioned"
              }
            }

            stage('(Docker) Shell Tests') {
              steps {
                sh "./tools/internal/cge_shell_tests"
              }
            }

            /* Commands with default FPC version
               (latest stable FPC, most of the time; see https://castle-engine.io/docker ). */
            stage('(Docker) Build Tools (Default FPC)') {
              steps {
                sh 'make clean tools'
              }
            }
            stage('(Docker) Build Examples (Default FPC)') {
              when { not { expression { return params.jenkins_fast } } }
              steps {
                /* clean 1st, to make sure it's OK even when state is "clean" before "make examples" */
                sh 'make clean examples'
              }
            }
            stage('(Docker) Build Examples Using Lazarus (Default FPC/Lazarus)') {
              when { not { expression { return params.jenkins_fast } } }
              steps {
                sh 'make clean examples-laz'
              }
            }
            stage('(Docker) Build And Run Auto-Tests (Default FPC)') {
              steps {
                sh 'make clean tests'
              }
            }
            stage('(Docker) Build Using FpMake (Default FPC)') {
              steps {
                sh 'make clean test-fpmake'
              }
            }

            /* Same with FPC 3.2.0.
               We could use a script to reuse the code,
               but then the detailed time breakdown/statistics would not be available in Jenkins. */

            stage('(Docker) Build Tools (FPC 3.2.0)') {
              steps {
                sh 'source /usr/local/fpclazarus/bin/setup.sh 3.2.0 && make clean tools'
              }
            }
            stage('(Docker) Build Examples (FPC 3.2.0)') {
              when { not { expression { return params.jenkins_fast } } }
              steps {
                /* clean 1st, to make sure it's OK even when state is "clean" before "make examples" */
                sh 'source /usr/local/fpclazarus/bin/setup.sh 3.2.0 && make clean examples'
              }
            }
            stage('(Docker) Build Examples Using Lazarus (FPC 3.2.0/Lazarus)') {
              when { not { expression { return params.jenkins_fast } } }
              steps {
                sh 'source /usr/local/fpclazarus/bin/setup.sh 3.2.0 && make clean examples-laz'
              }
            }
            stage('(Docker) Build And Run Auto-Tests (FPC 3.2.0)') {
              steps {
                sh 'source /usr/local/fpclazarus/bin/setup.sh 3.2.0 && make clean tests'
              }
            }
            stage('(Docker) Build Using FpMake (FPC 3.2.0)') {
              steps {
                sh 'source /usr/local/fpclazarus/bin/setup.sh 3.2.0 && make clean test-fpmake'
              }
            }

            /* Same with FPC trunk.
               We could use a script to reuse the code,
               but then the detailed time breakdown/statistics would not be available in Jenkins. */

            stage('(Docker) Build Tools (FPC trunk)') {
              steps {
                sh 'source /usr/local/fpclazarus/bin/setup.sh trunk && make clean tools'
              }
            }
            stage('(Docker) Build Examples (FPC trunk)') {
              when { not { expression { return params.jenkins_fast } } }
              steps {
                /* clean 1st, to make sure it's OK even when state is "clean" before "make examples" */
                sh 'source /usr/local/fpclazarus/bin/setup.sh trunk && make clean examples'
              }
            }
            stage('(Docker) Build Examples Using Lazarus (FPC trunk/Lazarus)') {
              when { not { expression { return params.jenkins_fast } } }
              steps {
                sh 'source /usr/local/fpclazarus/bin/setup.sh trunk && make clean examples-laz'
              }
            }
            stage('(Docker) Build And Run Auto-Tests (FPC trunk)') {
              steps {
                sh 'source /usr/local/fpclazarus/bin/setup.sh trunk && make clean tests'
              }
            }
            /* fpmake compilation with FPC 3.3.1 from 2022-12-27 is broken,
               TODO investigate and report.

            stage('(Docker) Build Using FpMake (FPC trunk)') {
              steps {
                sh 'source /usr/local/fpclazarus/bin/setup.sh trunk && make clean test-fpmake'
              }
            }
            */

            stage('(Docker) Pack Release (for Windows and Linux)') {
              steps {
                /* remove previous artifacts */
                sh 'rm -f castle-engine*.zip'

                /* build for all targets supported in Docker (Linux and Windows) */
                sh './tools/internal/pack_release/pack_release.sh'

                /* build a "bundle" version (with FPC) */
                copyArtifacts(projectName: 'castle_game_engine_organization/cge-fpc/master', filter: 'fpc-*.zip')
                sh 'CGE_PACK_BUNDLE=yes ./tools/internal/pack_release/pack_release.sh win64 x86_64'
                sh 'CGE_PACK_BUNDLE=yes ./tools/internal/pack_release/pack_release.sh linux x86_64'

                archiveArtifacts artifacts: 'castle-engine*.zip'
              }
            }
          }
        }
        /* Raspberry Pi is very slow and overloaded, rebuild for it only on master */
        stage('Raspberry Pi') {
          when {
            allOf {
              not { expression { return params.jenkins_fast } };
              branch "master"
            }
          }
          agent {
            label 'raspberry-pi-cge-builder'
          }
          environment {
            /* Used by CGE build tool ("castle-engine").
               Define env based on another env variable.
               According to https://github.com/jenkinsci/pipeline-model-definition-plugin/pull/110
               this should be supported. */
            CASTLE_ENGINE_PATH = "${WORKSPACE}"
            PATH = "${PATH}:${CASTLE_ENGINE_PATH}/installed/bin/"
          }
          stages {
            stage('(RPi) Info') {
              steps {
                // check versions (and availability) of our requirements early
                sh 'fpc -iV'
                sh 'lazbuild --version'
                sh 'make --version'
              }
            }
            stage('(RPi) Cleanup') {
              steps {
                sh "repository_cleanup . --remove-unversioned"
              }
            }
            stage('(RPi) Build Tools') {
              steps {
                sh 'rm -Rf installed/'
                sh 'mkdir -p installed/'
                sh 'make clean tools install PREFIX=${CASTLE_ENGINE_PATH}/installed/'
              }
            }
            stage('(RPi) Build Examples') {
              steps {
                sh 'make clean examples CASTLE_CONSERVE_DISK_SPACE=true'
              }
            }
            stage('(RPi) Build And Run Auto-Tests') {
              steps {
                sh 'make tests'
              }
            }
            stage('(RPi) Build Using FpMake') {
              steps {
                sh 'make clean test-fpmake'
              }
            }

            stage('(RPi) Pack Release') {
              steps {
                sh 'rm -f castle-engine*.zip' /* remove previous artifacts */
                sh './tools/internal/pack_release/pack_release.sh linux arm'
                archiveArtifacts artifacts: 'castle-engine*.zip'
              }
            }
          }
        }
        stage('macOS') {
          when { not { expression { return params.jenkins_fast } } }
          agent {
            label 'mac-cge-builder'
          }
          environment {
            /* Used by CGE build tool ("castle-engine").
               Define env based on another env variable.
               According to https://github.com/jenkinsci/pipeline-model-definition-plugin/pull/110
               this should be supported. */
            CASTLE_ENGINE_PATH = "${WORKSPACE}"
            CGE_INSTALL_PREFIX = "${CASTLE_ENGINE_PATH}/jenkins-installed/"
            PATH = "${PATH}:${CGE_INSTALL_PREFIX}/bin/:${WORKSPACE}/pasdoc/bin/"
            /* By default old Lazarus wanted to build with Carbon, which is 32-bit only and deprecated by Apple.
               No longer necessary with Lazarus 2.2.2. */
            // CASTLE_LAZBUILD_OPTIONS = "--widgetset=cocoa"
          }
          stages {
            stage('(macOS) Info') {
              steps {
                // check versions (and availability) of our requirements early
                sh 'fpc -iV'
                sh 'lazbuild --version'
                sh 'make --version'
              }
            }
            stage('(macOS) Cleanup') {
              steps {
                sh "repository_cleanup . --remove-unversioned"
              }
            }
            stage('(macOS) Build Tools') {
              steps {
                sh 'rm -Rf ${CGE_INSTALL_PREFIX}'
                sh 'mkdir -p ${CGE_INSTALL_PREFIX}'
                sh 'make clean tools install PREFIX=${CGE_INSTALL_PREFIX}'
              }
            }
            stage('(macOS) Build Examples') {
              steps {
                sh 'make clean examples'
              }
            }
            stage('(macOS) Build And Run Auto-Tests') {
              steps {
                sh 'make tests'
              }
            }
            /*
            // TODO: ignore fpmake test, we don't have fpmake "opengl" package ok after installing on macOS using fpcupdeluxe?
            stage('Build Using FpMake') {
              steps {
                sh 'make clean test-fpmake'
              }
            }
            */
            stage('(macOS) Build Lazarus Packages') {
              steps {
                sh 'lazbuild $CASTLE_LAZBUILD_OPTIONS packages/castle_base.lpk'
                sh 'lazbuild $CASTLE_LAZBUILD_OPTIONS packages/castle_window.lpk'
                sh 'lazbuild $CASTLE_LAZBUILD_OPTIONS packages/castle_components.lpk'
                sh 'lazbuild $CASTLE_LAZBUILD_OPTIONS packages/castle_editor_components.lpk'
                sh 'lazbuild $CASTLE_LAZBUILD_OPTIONS packages/alternative_castle_window_based_on_lcl.lpk'
              }
            }
            stage('(macOS) Build Editor') {
              steps {
                dir ('tools/castle-editor/') {
                  sh 'castle-engine package'
                }
              }
            }
            stage('(macOS) Get PasDoc') {
              steps {
                /* remove older PasDoc versions, so that later "pasdoc-*-darwin-x86_64.zip"
                   expands "pasdoc-*-darwin-x86_64.zip" only to one file.
                   This matters when PasDoc version change, e.g. from 0.15.0 to 0.16.0. */
                sh 'rm -f pasdoc-*-darwin-x86_64.zip'
                /* Use https://plugins.jenkins.io/copyartifact/ plugin to copy last pasdoc build into this build. */
                copyArtifacts(projectName: 'pasdoc_organization/pasdoc/master', filter: 'pasdoc-*-darwin-x86_64.zip')
                sh 'unzip pasdoc-*-darwin-x86_64.zip'
              }
            }
            stage('(macOS) Pack Release') {
              steps {
                sh 'rm -f castle-engine*.zip' /* remove previous artifacts */
                sh './tools/internal/pack_release/pack_release.sh darwin x86_64'
                archiveArtifacts artifacts: 'castle-engine*.zip'
              }
            }
          }
        }
        stage('Windows (FPC)') {
          agent {
            label 'windows-cge-builder'
          }
          environment {
            /* Used by CGE build tool ("castle-engine").
               Define env based on another env variable.
               According to https://github.com/jenkinsci/pipeline-model-definition-plugin/pull/110
               this should be supported. */
            CASTLE_ENGINE_PATH = "${WORKSPACE}"
            PATH = "${PATH};${CASTLE_ENGINE_PATH}/installed/bin/;${WORKSPACE}/pasdoc/bin/" // Note: on Windows, PATH is separated by ;
          }
          stages {
            stage('(Windows) Info') {
              steps {
                // check versions (and availability) of our requirements early
                sh 'fpc -iV'
                sh 'lazbuild --version'
                sh 'make --version'
              }
            }
            stage('(Windows) Cleanup') {
              steps {
                sh "repository_cleanup . --remove-unversioned"
              }
            }
            stage('(Windows) Build Tools') {
              when { not { expression { return params.jenkins_fast } } }
              steps {
                sh 'rm -Rf installed/'
                sh 'mkdir -p installed/'
                /* TODO: do not use "make install" command, as somewhere the Windows path gets
                   messed up and in the end we have created files like this:

                   "tools/build-tool/data/E\357\200\272jworkspacecastle_game_engine_delphi_master/installed/share/castle-engine/android/integrated-services/google_play_games/app/src/main/java/net/sourceforge/castleengine/ServiceGooglePlayGames.java"
                */
                // sh 'make clean tools install PREFIX=${CASTLE_ENGINE_PATH}/installed/'
                sh 'make clean tools'
                sh 'mkdir -p ${CASTLE_ENGINE_PATH}/installed/bin/'
                sh 'cp tools/build-tool/castle-engine.exe ${CASTLE_ENGINE_PATH}/installed/bin/'
              }
            }
            stage('(Windows) Build Examples') {
              when { not { expression { return params.jenkins_fast } } }
              steps {
                sh 'make clean examples'
              }
            }
            stage('(Windows) Build And Run Auto-Tests') {
              when { not { expression { return params.jenkins_fast } } }
              steps {
                sh 'make tests'
              }
            }
            stage('(Windows) Build Using FpMake') {
              when { not { expression { return params.jenkins_fast } } }
              steps {
                sh 'make clean test-fpmake'
              }
            }
            stage('(Windows) Get PasDoc') {
              steps {
                /* remove older PasDoc versions, so that later "pasdoc-*-win64.zip"
                   expands "pasdoc-*-win64.zip" only to one file.
                   This matters when PasDoc version change, e.g. from 0.15.0 to 0.16.0. */
                sh 'rm -f pasdoc-*-win64.zip'
                /* Use https://plugins.jenkins.io/copyartifact/ plugin to copy last pasdoc build into this build. */
                copyArtifacts(projectName: 'pasdoc_organization/pasdoc/master', filter: 'pasdoc-*-win64.zip')
                sh 'unzip pasdoc-*-win64.zip'
              }
            }
            /* Pack Windows installer on Windows node.
               Note that Windows zip is packed inside Docker (on Linux). */
            stage('(Windows) Pack Windows Installer') {
              steps {
                copyArtifacts(projectName: 'castle_game_engine_organization/cge-fpc/master', filter: 'fpc-*.zip')
                sh 'CGE_PACK_BUNDLE=yes ./tools/internal/pack_release/pack_release.sh windows_installer'
                archiveArtifacts artifacts: 'castle-engine-setup-*.exe'
              }
            }
          }
        }
        stage('Delphi on Windows') {
          agent {
            label 'windows-cge-builder'
          }
          environment {
            /* Used by CGE build tool ("castle-engine").
              Define env based on another env variable.
              According to https://github.com/jenkinsci/pipeline-model-definition-plugin/pull/110
              this should be supported. */
            CASTLE_ENGINE_PATH = "${WORKSPACE}"
            PATH = "${PATH};${CASTLE_ENGINE_PATH}/installed/bin/" // Note: on Windows, PATH is separated by ;
          }
          stages {
            stage('(Delphi) Info') {
              steps {
                /* Check versions (and availability) of our requirements early.
                  Note that we need FPC for Delphi test too, since our internal tools are compiled with FPC. */
                sh 'fpc -iV'
                sh 'lazbuild --version'
                // We want GNU make, not Embarcadero make
                sh 'make --version'
              }
            }
            stage('(Delphi) Cleanup') {
              steps {
                sh "repository_cleanup . --remove-unversioned"
              }
            }
            stage('(Delphi) Build Tools') {
              steps {
                sh 'rm -Rf installed/'
                sh 'mkdir -p installed/'
                /* TODO: do not use "make install" command, as somewhere the Windows path gets
                  messed up and in the end we have created files like this:

                  "tools/build-tool/data/E\357\200\272jworkspacecastle_game_engine_delphi_master/installed/share/castle-engine/android/integrated-services/google_play_games/app/src/main/java/net/sourceforge/castleengine/ServiceGooglePlayGames.java"
                */
                // sh 'make clean tools install PREFIX=${CASTLE_ENGINE_PATH}/installed/'
                sh 'make clean tools'
                sh 'mkdir -p ${CASTLE_ENGINE_PATH}/installed/bin/'
                sh 'cp tools/build-tool/castle-engine.exe ${CASTLE_ENGINE_PATH}/installed/bin/'
              }
            }
            stage('(Delphi) Check AutoTests (Win64)') {
              steps {
                dir ('tests/delphi_tests/') {
                  sh 'castle-engine clean'
                  sh 'castle-engine compile --compiler=delphi --os=win64 --cpu=x86_64'
                  sh 'castle-engine run'
                }
              }
            }
            stage('(Delphi) Check AutoTests (Win32)') {
              steps {
                dir ('tests/delphi_tests/') {
                  sh 'castle-engine clean'
                  sh 'castle-engine compile --compiler=delphi --os=win32 --cpu=i386'
                  sh 'castle-engine run'
                }
              }
            }
            stage('(Delphi) Check AutoTests with NO_WINDOW_SYSTEM (Win64)') {
              steps {
                dir ('tests/') {
                  sh 'castle-engine clean'
                  sh 'castle-engine compile --compiler=delphi --os=win64 --cpu=x86_64 --compiler-option=-dNO_WINDOW_SYSTEM'
                  sh 'castle-engine run -- --console'
                }
              }
            }
            stage('(Delphi) Check AutoTests with NO_WINDOW_SYSTEM (Win32)') {
              steps {
                dir ('tests/') {
                  sh 'castle-engine clean'
                  sh 'castle-engine compile --compiler=delphi --os=win32 --cpu=i386 --compiler-option=-dNO_WINDOW_SYSTEM'
                  sh 'castle-engine run -- --console'
                }
              }
            }
            stage('(Delphi) Build Templates (Win64)') {
              steps {
                sh 'make test-editor-templates CASTLE_ENGINE_TOOL_OPTIONS="--compiler=delphi --os=win64 --cpu=x86_64"'
              }
            }
            stage('(Delphi) Build Templates (Win32)') {
              steps {
                sh 'make test-editor-templates CASTLE_ENGINE_TOOL_OPTIONS="--compiler=delphi --os=win32 --cpu=i386"'
              }
            }
            stage('(Delphi) Build Examples (Win64)') {
              when { not { expression { return params.jenkins_fast } } }
              steps {
                sh 'make examples-delphi CASTLE_ENGINE_TOOL_OPTIONS="--os=win64 --cpu=x86_64"'
              }
            }
            stage('(Delphi) Build Examples (Win32)') {
              when { not { expression { return params.jenkins_fast } } }
              steps {
                sh 'make examples-delphi CASTLE_ENGINE_TOOL_OPTIONS="--os=win32 --cpu=i386"'
              }
            }
            stage('(Delphi) Build Delphi-specific Examples (Win64)') {
              steps {
                dir ('examples/delphi/vcl') {
                  sh 'castle-engine clean'
                  sh 'castle-engine compile --compiler=delphi --os=win64 --cpu=x86_64'
                }
                dir ('examples/delphi/fmx') {
                  sh 'castle-engine clean'
                  sh 'castle-engine compile --compiler=delphi --os=win64 --cpu=x86_64'
                }
              }
            }
            stage('(Delphi) Build Delphi-specific Examples (Win32)') {
              steps {
                dir ('examples/delphi/vcl') {
                  sh 'castle-engine clean'
                  sh 'castle-engine compile --compiler=delphi --os=win32 --cpu=i386'
                }
                dir ('examples/delphi/fmx') {
                  sh 'castle-engine clean'
                  sh 'castle-engine compile --compiler=delphi --os=win32 --cpu=i386'
                }
              }
            }
          }
        }
        stage('Check Dependencies') {
          when { not { expression { return params.jenkins_fast } } }
          agent {
            docker {
              image 'kambi/castle-engine-cloud-builds-tools:cge-none'
            }
          }
          environment {
            /* Used by CGE build tool ("castle-engine").
              Define env based on another env variable.
              According to https://github.com/jenkinsci/pipeline-model-definition-plugin/pull/110
              this should be supported. */
            CASTLE_ENGINE_PATH = "${WORKSPACE}"
          }
          stages {
            stage('(Check Dependencies) Cleanup') {
              steps {
                sh "repository_cleanup . --remove-unversioned"
              }
            }
            stage('(Check Dependencies) Build Tools') {
              steps {
                sh 'rm -Rf installed/'
                sh 'mkdir -p installed/'
                sh 'make clean tools install PREFIX=${CASTLE_ENGINE_PATH}/installed/'
              }
            }
            stage('(Check Dependencies) Check Dependencies') {
              steps {
                dir ('tools/internal/check_units_dependencies/') {
                  sh 'export PATH="${PATH}:${CASTLE_ENGINE_PATH}/installed/bin/" && make'
                }
                archiveArtifacts artifacts: 'test-cge-units-dependencies_all_units.txt,cge_check_units_dependencies.log'
              }
            }
          }
        }
      }
    }
  }
  post {
    regression {
      mail to: 'michalis@castle-engine.io',
        subject: "[jenkins] Build started failing: ${currentBuild.fullDisplayName}",
        body: "See the build details on ${env.BUILD_URL}"
    }
    failure {
      mail to: 'michalis@castle-engine.io',
        subject: "[jenkins] Build failed: ${currentBuild.fullDisplayName}",
        body: "See the build details on ${env.BUILD_URL}"
    }
    fixed {
      mail to: 'michalis@castle-engine.io',
        subject: "[jenkins] Build is again successful: ${currentBuild.fullDisplayName}",
        body: "See the build details on ${env.BUILD_URL}"
    }
  }
}
