#version 450
#extension GL_ARB_separate_shader_objects : enable

layout(set = 0, binding = 0) uniform uniformBuffer {
  mat4 vproj;
} ub;

layout(push_constant) uniform pushConstant {
  mat4 model;
  uint colorOverride;
  bool overrideColor;
} pc;


layout(location = 0) in vec2 inPosition;
layout(location = 1) in uint inColor;

layout(location = 0) out vec4 outColor;

uint color;

void main () {
  gl_Position = ub.vproj * pc.model * vec4(inPosition.xy, 0.0, 1.0);
  if (pc.overrideColor) {
    color = pc.colorOverride;
  } else {
    color = inColor;
  }
  outColor = vec4((0x000000ff & (color >> 24))/255.0,
                  (0x000000ff & (color >> 16))/255.0,
                  (0x000000ff & (color >> 8))/255.0,
                  (0x000000ff & color)/255.0);
}
