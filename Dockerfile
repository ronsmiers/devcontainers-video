FROM mcr.microsoft.com/devcontainers/universal:2-linux

RUN apt-get update && \
apt-get install ffmpeg && \
npm install -g firebase-tools