#version 450
#extension GL_ARB_separate_shader_objects : enable

layout(set = 0, binding = 0) uniform uniformBuffer {
  mat4 view;
  mat4 proj;
  mat4 vproj;
} ub;

layout(push_constant) uniform pushConstant {
  layout(offset = 0) mat4 model;
  layout(offset = 64) float pointSize;
  layout(offset = 68) uint type;
  layout(offset = 72) uint override_color;
  layout(offset = 76) bool override_color_p;
} pc;

layout(location = 0) in uint inObjectId;
layout(location = 1) in vec3 inPosition;
layout(location = 2) in vec2 inTexCoord;
layout(location = 3) in uint inColor;

layout(location = 0) flat out uint outObjectId;
layout(location = 1) out vec4 outColor;
layout(location = 2) out vec2 outTexCoord;
layout(location = 3) flat out uint outPrimType;

layout(location = 6) flat out uint outIs2d;

uint color;

void main () {
  outObjectId = inObjectId;
  gl_Position = ub.vproj * pc.model * vec4(inPosition.xyz, 1.0);
  gl_PointSize = pc.pointSize;
  if (bool(pc.override_color_p)) {
    color = pc.override_color;
  } else {
    color = inColor;
  }
  outColor = vec4((0x000000ff & (color >> 24))/255.0,
                  (0x000000ff & (color >> 16))/255.0,
                  (0x000000ff & (color >> 8))/255.0,
                  (0x000000ff & color)/255.0);
  outTexCoord = inTexCoord;
  outPrimType = pc.type;
  outObjectId = inObjectId;
  outIs2d = 0;
}
