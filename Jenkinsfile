/* -*- mode: groovy -*-
  Confgure how to run our job in Jenkins.
  See https://github.com/castle-engine/castle-engine/wiki/Cloud-Builds-(Jenkins) .
*/

pipeline {
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
    stage('Build Tools (Default FPC)') {
      steps {
        sh 'make clean tools'
      }
    }

    stage('Pack Release') {
      steps {
        sh 'rm -f castle-engine*.zip' /* remove previous artifacts */
        sh './tools/internal/pack_release/pack_release.sh'
      }
    }
    /* update Docker image only when the "master" branch changes */
    stage('Update Docker Image with CGE') {
      when { branch 'master' }
      steps {
        build job: '../castle_game_engine_update_docker_image'
      }
    }
  }
  post {
    success {
      archiveArtifacts artifacts: 'castle-engine*.zip'
    }
    regression {
      mail to: 'michalis.kambi@gmail.com',
        subject: "[jenkins] Build started failing: ${currentBuild.fullDisplayName}",
        body: "See the build details on ${env.BUILD_URL}"
    }
    failure {
      mail to: 'michalis.kambi@gmail.com',
        subject: "[jenkins] Build failed: ${currentBuild.fullDisplayName}",
        body: "See the build details on ${env.BUILD_URL}"
    }
    fixed {
      mail to: 'michalis.kambi@gmail.com',
        subject: "[jenkins] Build is again successfull: ${currentBuild.fullDisplayName}",
        body: "See the build details on ${env.BUILD_URL}"
    }
  }
}
