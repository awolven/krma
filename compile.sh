#!/bin/bash
set -x
/cygdrive/c/VulkanSDK/1.2.189.2/Bin/glslangValidator.exe -V standard-2d.vert -o standard-2d.vert.spv
/cygdrive/c/VulkanSDK/1.2.189.2/Bin/glslangValidator.exe -V standard-3d.vert -o standard-3d.vert.spv
/cygdrive/c/VulkanSDK/1.2.189.2/Bin/glslangValidator.exe -V 3d-with-normal.vert -o 3d-with-normal.vert.spv
/cygdrive/c/VulkanSDK/1.2.189.2/Bin/glslangValidator.exe -V diffuse+texture.frag -o diffuse+texture.frag.spv
/cygdrive/c/VulkanSDK/1.2.189.2/Bin/glslangValidator.exe -V texture.frag -o texture.frag.spv
/cygdrive/c/VulkanSDK/1.2.189.2/Bin/glslangValidator.exe -V msdf-texture.frag -o msdf-texture.frag.spv
/cygdrive/c/VulkanSDK/1.2.189.2/Bin/glslangValidator.exe -V standard-2d-point.vert -o standard-2d-point.vert.spv
/cygdrive/c/VulkanSDK/1.2.189.2/Bin/glslangValidator.exe -V standard-3d-point.vert -o standard-3d-point.vert.spv
/cygdrive/c/VulkanSDK/1.2.189.2/Bin/glslangValidator.exe -V standard-2d-line-list.vert -o standard-2d-line-list.vert.spv
/cygdrive/c/VulkanSDK/1.2.189.2/Bin/glslangValidator.exe -V standard-3d-line-list.vert -o standard-3d-line-list.vert.spv
/cygdrive/c/VulkanSDK/1.2.189.2/Bin/glslangValidator.exe -V standard-2d-line-strip.vert -o standard-2d-line-strip.vert.spv
/cygdrive/c/VulkanSDK/1.2.189.2/Bin/glslangValidator.exe -V standard-3d-line-strip.vert -o standard-3d-line-strip.vert.spv
/cygdrive/c/VulkanSDK/1.2.189.2/Bin/glslangValidator.exe -V notexture.frag -o notexture.frag.spv
/cygdrive/c/VulkanSDK/1.2.189.2/Bin/glslangValidator.exe -V shader.frag -o shader.frag.spv
/cygdrive/c/VulkanSDK/1.2.189.2/Bin/glslangValidator.exe -V standard-2d-test.vert -o standard-2d-test.vert.spv
/cygdrive/c/VulkanSDK/1.2.189.2/Bin/glslangValidator.exe -V standard-test.frag -o standard-test.frag.spv
