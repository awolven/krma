#version 450
#extension GL_ARB_separate_shader_objects : enable

layout(set = 0, binding = 0) uniform uniformBuffer {
  mat4 mvp;
} ub;

layout(location = 0) in vec3 inPosition;
layout(location = 1) in uint inColor;

layout(location = 0) out vec4 outColor;

void main () {
  gl_Position = ub.mvp * vec4(inPosition.xy, 0.0, 1.0);
  outColor = vec4((0x000000ff & (inColor >> 24))/255.0,
                  (0x000000ff & (inColor >> 16))/255.0,
                  (0x000000ff & (inColor >> 8))/255.0,
                  (0x000000ff & inColor)/255.0);
}
