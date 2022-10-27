#!/bin/bash
#GLSL_COMPILER=/cygdrive/c/VulkanSDK/1.2.189.2/Bin/glslangValidator.exe
GLSL_COMPILER=~/vulkan/1.3.231.0/x86_64/bin/glslangValidator
set -x
$GLSL_COMPILER -V standard-2d.vert -o standard-2d.vert.spv
$GLSL_COMPILER -V standard-3d.vert -o standard-3d.vert.spv
$GLSL_COMPILER -V 3d-with-normal.vert -o 3d-with-normal.vert.spv
$GLSL_COMPILER -V diffuse+texture.frag -o diffuse+texture.frag.spv
$GLSL_COMPILER -V texture.frag -o texture.frag.spv
$GLSL_COMPILER -V msdf-texture.frag -o msdf-texture.frag.spv
$GLSL_COMPILER -V standard-2d-point.vert -o standard-2d-point.vert.spv
$GLSL_COMPILER -V standard-3d-point.vert -o standard-3d-point.vert.spv
$GLSL_COMPILER -V standard-2d-line-list.vert -o standard-2d-line-list.vert.spv
$GLSL_COMPILER -V standard-3d-line-list.vert -o standard-3d-line-list.vert.spv
$GLSL_COMPILER -V standard-2d-line-strip.vert -o standard-2d-line-strip.vert.spv
$GLSL_COMPILER -V standard-3d-line-strip.vert -o standard-3d-line-strip.vert.spv
$GLSL_COMPILER -V notexture.frag -o notexture.frag.spv
$GLSL_COMPILER -V shader.frag -o shader.frag.spv
$GLSL_COMPILER -V standard-2d-test.vert -o standard-2d-test.vert.spv
$GLSL_COMPILER -V standard-test.frag -o standard-test.frag.spv
