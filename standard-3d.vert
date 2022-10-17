#version 450
#extension GL_ARB_separate_shader_objects : enable

layout(set = 0, binding = 0) uniform uniformBuffer {
  mat4 vproj;
} ub;

layout(push_constant) uniform pushConstant {
  mat4 model;
  uint colorOverride;
  float pointSize;
  bool overrideColor;
} pc;


layout(location = 0) in vec3 inPosition;
layout(location = 1) in vec2 inTexCoord;
layout(location = 2) in uint inColor;

layout(location = 0) out vec4 outColor;
layout(location = 1) out vec2 outTexCoord;

uint color;

void main () {
  gl_Position = ub.vproj * pc.model * vec4(inPosition.xyz, 1.0);
  gl_PointSize = pc.pointSize;
  if (pc.overrideColor) {
    color = pc.colorOverride;
  } else {
    color = inColor;
  }
  outColor = vec4((0x000000ff & (color >> 24))/255.0,
                  (0x000000ff & (color >> 16))/255.0,
                  (0x000000ff & (color >> 8))/255.0,
                  (0x000000ff & color)/255.0);
  outTexCoord = inTexCoord;
}
