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
    /* Trying to resume builds when controller restarts usually results
       in job just being stuck forever. So we disable it. */
    disableResume()
    /* Makes failure in any paralel job to stop the build,
       instead of needlessly trying to finish on one node,
       when another node already failed. */
    parallelsAlwaysFailFast()
  }

  /* This job works on a few agents in parallel */
  agent none

  stages {
    /* Build for each platform in parallel.
       See https://stackoverflow.com/questions/43913698/jenkinsfile-parallel-directive
       https://www.jenkins.io/blog/2017/09/25/declarative-1/
       for parallel syntax. */
    stage('Run parallel builds') {
      parallel {
        stage('Docker (Linux) (Default FPC)') {
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
            LD_LIBRARY_PATH = "${LD_LIBRARY_PATH}:${WORKSPACE}/steamworks_sdk/redistributable_bin/linux64"
          }
          stages {
            stage('(Docker) Cleanup') {
              steps {
                sh "repository_cleanup . --remove-unversioned"
              }
            }

            /* Commands with default FPC version
               (latest stable FPC, most of the time; see https://castle-engine.io/docker ). */
            stage('(Docker) Build Tools (Default FPC)') {
              steps {
                sh 'make clean tools'
              }
            }
            stage('(Docker) Build And Run Auto-Tests (Default FPC)') {
              steps {
                sh 'make clean tests'
              }
            }

            stage('(Docker) Pack Release (for Windows and Linux)') {
              steps {
                /* remove previous artifacts */
                sh 'rm -f castle-engine*.zip'

                /* build for all targets supported in Docker */
                sh './tools/internal/pack_release/pack_release.sh win64 x86_64'
                sh './tools/internal/pack_release/pack_release.sh win32 i386'
                sh './tools/internal/pack_release/pack_release.sh linux x86_64'

                /* build a "bundle" version (with FPC) */
                copyArtifacts(projectName: 'castle_game_engine_organization/cge-fpc/master', filter: 'fpc-*.zip')
                sh 'CGE_PACK_BUNDLE=yes ./tools/internal/pack_release/pack_release.sh win64 x86_64'
                sh 'CGE_PACK_BUNDLE=yes ./tools/internal/pack_release/pack_release.sh linux x86_64'

                archiveArtifacts artifacts: 'castle-engine*.zip'
              }
            }
          }
        }

        stage('Raspberry Pi') {
          /* Raspberry Pi is very slow and overloaded, rebuild for it only on master */
          when { branch "master" }
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
            stage('(RPi) Pack Release') {
              steps {
                sh 'rm -f castle-engine*.zip' /* remove previous artifacts */
                sh './tools/internal/pack_release/pack_release.sh linux arm'
                archiveArtifacts artifacts: 'castle-engine*.zip'
              }
            }
          }
        }
        stage('Raspberry Pi (64-bit)') {
          /* To not overload the slower RPi, use it only with master. */
          when { branch "master" }
          agent {
            label 'raspberry-pi-64-cge-builder'
          }
          environment {
            /* Used by CGE build tool ("castle-engine").
               Define env based on another env variable.
               According to https://github.com/jenkinsci/pipeline-model-definition-plugin/pull/110
               this should be supported. */
            CASTLE_ENGINE_PATH = "${WORKSPACE}"
            PATH = "${PATH}:${CASTLE_ENGINE_PATH}/installed/bin/"
            // We need to use FPC 3.2.3 for packing
            CASTLE_PACK_DISABLE_FPC_VERSION_CHECK = "true"
          }
          stages {
            stage('(RPi64) Info') {
              steps {
                // check versions (and availability) of our requirements early
                sh 'fpc -iV'
                sh 'lazbuild --version'
                sh 'make --version'
              }
            }
            stage('(RPi64) Cleanup') {
              steps {
                sh "repository_cleanup . --remove-unversioned"
              }
            }
            stage('(RPi64) Pack Release') {
              steps {
                sh 'rm -f castle-engine*.zip' /* remove previous artifacts */
                sh './tools/internal/pack_release/pack_release.sh linux aarch64'
                archiveArtifacts artifacts: 'castle-engine*.zip'
              }
            }
          }
        }
        stage('macOS') {
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
                /* Make sure we got GNU Make, not Embarcadero make.
                   This is important, as Delphi installer inserts Embarcadero make into PATH,
                   earlier than Cygwin's. It has to be corrected after installation. */
                sh 'if make -version 2>&1 | grep -i "GNU Make" -; then echo "Make from GNU"; else echo "Make not from GNU!"; exit 1; fi'
              }
            }
            stage('(Windows) Cleanup') {
              steps {
                sh "repository_cleanup . --remove-unversioned"
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
