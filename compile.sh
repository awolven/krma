#!/bin/bash
#GLSL_COMPILER=/cygdrive/c/VulkanSDK/1.2.189.2/Bin/glslangValidator.exe
#GLSL_COMPILER=~/vulkan/1.3.231.0/x86_64/bin/glslangValidator
GLSL_COMPILER=~/VulkanSDK/1.3.231.1/macOS/bin/glslangValidator
set -x
$GLSL_COMPILER -V standard-2d.vert -o submodules/krma-shader-bin/standard-2d.vert.spv
$GLSL_COMPILER -V standard-3d.vert -o submodules/krma-shader-bin/standard-3d.vert.spv
$GLSL_COMPILER -V 3d-with-normal.vert -o submodules/krma-shader-bin/3d-with-normal.vert.spv
$GLSL_COMPILER -V lighting+texture.frag -o submodules/krma-shader-bin/lighting+texture.frag.spv
$GLSL_COMPILER -V texture.frag -o submodules/krma-shader-bin/texture.frag.spv
$GLSL_COMPILER -V msdf-texture.frag -o submodules/krma-shader-bin/msdf-texture.frag.spv
$GLSL_COMPILER -V notexture.frag -o submodules/krma-shader-bin/notexture.frag.spv

$GLSL_COMPILER -V instanced-line.vert -o submodules/krma-shader-bin/instanced-line.vert.spv
$GLSL_COMPILER -V instanced-tube.vert -o submodules/krma-shader-bin/instanced-tube.vert.spv

$GLSL_COMPILER -V hashset.frag -o submodules/krma-shader-bin/hashset.frag.spv
$GLSL_COMPILER -V hashset2.frag -o submodules/krma-shader-bin/hashset2.frag.spv
$GLSL_COMPILER -V hashset3.frag -o submodules/krma-shader-bin/hashset3.frag.spv

