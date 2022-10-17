#version 450
#extension GL_ARB_separate_shader_objects : enable

layout(set = 0, binding = 0) uniform uniformBuffer {
  mat4 vproj;
} ub;

layout(location = 0) in vec2 inPosition;
layout(location = 1) in vec2 inTexCoord;
layout(location = 2) in uint inColor;

layout(location = 0) out vec4 outColor;

void main () {
  gl_Position = ub.vproj * vec4(inPosition.xy, 0.0, 1.0);
  outColor = vec4(1.0, 0.0, 0.0, 1.0);
}
