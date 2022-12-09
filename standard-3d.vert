#version 450
#extension GL_ARB_separate_shader_objects : enable

layout(set = 0, binding = 0) uniform uniformBuffer {
  mat4 vproj;
} ub;

layout(push_constant) uniform pushConstant {
  layout(offset = 0) mat4 model;
  layout(offset = 64) vec3 lightPosition; // 16
  layout(offset = 80) float pointSize; // 20
  layout(offset = 84) uint type; // 21
  layout(offset = 88) uint colorOverride; // 22
  layout(offset = 92) bool overrideColor; // 23
} pc;

layout(location = 0) in uint inObjectId;
layout(location = 1) in vec3 inPosition;
layout(location = 2) in vec2 inTexCoord;
layout(location = 3) in uint inColor;

layout(location = 0) flat out uint outObjectId;
layout(location = 1) out vec4 outColor;
layout(location = 2) out vec2 outTexCoord;
layout(location = 3) flat out uint outPrimType;

uint color;

void main () {
  outObjectId = inObjectId;
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
  outPrimType = pc.type;
}
