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
    --rows 10              \
    --columns 10           \
    --style Montage        \
    --out-file a.png

composite a.png bgs/a.png out.png
