#version 450
#extension GL_ARB_separate_shader_objects : enable

#define SELECT_BOX_DEPTH 128
#define MAX_LIGHTS 10

struct light { // not used
  vec4 position;
  vec4 spotDirection;
  uint diffuse;
  uint specular;
  float constantAttenuation, linearAttenuation, quadraticAttenuation;
  float spotCutoff, spotExponent, padding;
};

layout(set = 0, binding = 1) uniform uniformBuffer { // not used
  light lights[MAX_LIGHTS];
  uint num_lights;
  uint scene_ambient;
  uint padding1;
  uint padding2;
} ub;

layout(set = 1, binding = 0) buffer writeonly select_buffer {
  uint data[][SELECT_BOX_DEPTH];
} selected;

layout(push_constant) uniform pushConstant {
  layout(offset = 80) vec4 selectBox;
  layout(offset = 96) float pxRange; // not used
  layout(offset = 100) uint uint_ambient; // not used
  layout(offset = 104) uint uint_diffuse; // not used
  layout(offset = 108) uint uint_specular; // not used
  layout(offset = 112) float shininess; // not used
} pc;

layout(location = 0) flat in uint inObjectId;
layout(location = 1) in vec4 fragColor;

layout(location = 0) out vec4 outColor;

void main () {
  outColor = fragColor;

  if (pc.selectBox.x <= gl_FragCoord.x &&
      pc.selectBox.y <= gl_FragCoord.y &&
      gl_FragCoord.x <= pc.selectBox.z &&
      gl_FragCoord.y <= pc.selectBox.w) {
    
    uint zIndex = uint(gl_FragCoord.z * SELECT_BOX_DEPTH);
    uint row_size = uint(pc.selectBox.z) - uint(pc.selectBox.x);
    uint offset = uint(gl_FragCoord.y - pc.selectBox.y) * row_size
      + uint(gl_FragCoord.x - pc.selectBox.x);
    selected.data[offset][zIndex] = inObjectId;
  } 
}
