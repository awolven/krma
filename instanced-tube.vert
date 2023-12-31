#version 450
#extension GL_ARB_separate_shader_objects : enable
#extension GL_EXT_buffer_reference2 : require
#extension GL_EXT_shader_explicit_arithmetic_types_int64 : require
#extension GL_EXT_scalar_block_layout : require

layout(buffer_reference, std430, buffer_reference_align = 16) readonly buffer _pointRef {
  // spirv to metal translator has bug where it won't accept a struct here:
  vec4 val;
};

layout(set = 0, binding = 0) uniform uniformBuffer {
  mat4 view;
  mat4 proj;
  mat4 vproj;
  vec4 extents;
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
layout(location = 9) out vec4 outExtents;

uint color;

void main () {
  // https://wwwtyro.net/2019/11/18/instanced-lines.html
  vec2 resolution = ub.extents.xy;
  vec3 pointA = (_pointRef(pc.ref0) + gl_InstanceIndex).val.yzw;
  vec3 pointB = (_pointRef(pc.ref0) + gl_InstanceIndex + 1).val.yzw;
  vec4 clip0 = ub.vproj * pc.model * vec4(pointA, 1.0);
  vec4 clip1 = ub.vproj * pc.model * vec4(pointB, 1.0);
  vec2 screen0 = resolution * (0.5 * clip0.xy/clip0.w + 0.5);
  vec2 screen1 = resolution * (0.5 * clip1.xy/clip1.w + 0.5);
  vec2 xBasis = normalize(screen1 - screen0);
  vec2 yBasis = vec2(-xBasis.y, xBasis.x);
  vec2 pt0 = screen0 + pc.width * (inPosition.x * xBasis + inPosition.y * yBasis);
  vec2 pt1 = screen1 + pc.width * (inPosition.x * xBasis + inPosition.y * yBasis);
  vec2 pt = mix(pt0, pt1, inPosition.z);
  vec4 clip = mix(clip0, clip1, inPosition.z);
  gl_Position = vec4(clip.w * ((2.0 * pt) / resolution - 1.0), clip.z, clip.w);

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
  outIs2d = 0;
  outExtents = ub.extents;
}
