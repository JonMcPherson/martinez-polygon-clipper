pipeline {
    agent any
    stages {
        stage('Checkout') {
            steps {
                checkout scm
            }
        }
        stage('Build') {
            steps {
                sh 'sbt -Dsbt.log.noformat=true clean package'
            }
        }
        stage('Test') {
            steps {
                sh 'sbt -Dsbt.log.noformat=true test'
            }
        }
        stage('Publish') {
            steps {
                sh 'sbt -Dsbt.log.noformat=true publish-local'
            }
        }
    }
}