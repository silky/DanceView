#!/usr/bin/env sh

set -e

stack build

stack exec -- dance-view   \
    do-image               \
    --video-width 1280     \
    --video-height 720     \
    --source-directory /home/noon/ml-data/pose/gala1 \
    --fps 25               \
    --out-width 3360       \
    --out-height 1920      \
    --rows 11              \
    --columns 12           \
    --style Montage        \
    --out-file a.png

stack exec -- dance-view   \
    do-gif                 \
    --video-width 1280     \
    --video-height 720     \
    --source-directory /home/noon/ml-data/pose/gala1 \
    --fps 15               \
    --out-width 500        \
    --out-file a.gif
