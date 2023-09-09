#version 450
#extension GL_ARB_separate_shader_objects : enable
#extension GL_EXT_buffer_reference2 : require
#extension GL_EXT_shader_explicit_arithmetic_types_int64 : require

layout(buffer_reference, std430, buffer_reference_align = 8) buffer _pointRef {
  vec2 val;
};

layout(set = 0, binding = 0) uniform uniformBuffer {
  mat4 view;
  mat4 proj;
  mat4 vproj;
} ub;

layout(location = 0) in uint inObjectId;
layout(location = 1) in vec3 inPosition;
layout(location = 2) in vec2 inTexCoord;
layout(location = 3) in uint inColor;

layout(push_constant) uniform Registers {
  layout(offset = 0) mat4 model;
  layout(offset = 64) float width;
  layout(offset = 68) uint type;
  layout(offset = 72) uint override_color;
  layout(offset = 76) uint override_color_p;
  layout(offset = 80) uint64_t ref0;
} pc ;

layout(location = 0) flat out uint outObjectId;
layout(location = 1) out vec4 outColor;
layout(location = 2) out vec2 outTexCoord;
layout(location = 3) flat out uint outPrimType;

layout(location = 6) flat out uint outIs2d;

uint color;

void main () {
  vec2 pointA = (_pointRef(pc.ref0) + uint(gl_InstanceIndex)).val;
  vec2 pointB = (_pointRef(pc.ref0) + uint(gl_InstanceIndex) + 1).val;
  vec2 xBasis = pointB - pointA;
  vec2 yBasis = normalize(vec2(-xBasis.y, xBasis.x));
  vec2 point = pointA + xBasis * inPosition.x + yBasis * pc.width * inPosition.y;
  gl_Position = ub.vproj * pc.model * vec4(point, 0.0, 1.0);

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
  outPrimType  = pc.type;
  outObjectId = inObjectId;
  outIs2d = 1;
}
