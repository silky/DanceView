#!/bin/bash

# e.g. /home/noon/ml-data/misc/gala1
data_dir="$1"

# e.g. gala1
out_name="$2"

# # 1. Generate a gif
stack exec -- dance-view do-gif --video-width 1280 --video-height 720      \
    --source-directory $data_dir --out-file output/$out_name.gif           \
    --out-width 640                                                        \
    --fps 15

# 2. Generate a montage
stack exec -- dance-view do-montage --video-width 1280 --video-height 720  \
    --source-directory $data_dir --out-file output/$out_name.png           \
    --rows 8                                                               \
    --columns 10                                                           \
    --out-width 2560

# 3. Generate the json
stack exec -- dance-view json-export --video-width 1280 --video-height 720 \
    --source-directory $data_dir --out-file output/$out_name.json
