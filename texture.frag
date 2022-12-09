#version 450
#extension GL_ARB_separate_shader_objects : enable

#define SELECT_BOX_DEPTH 128

layout(set = 1, binding = 0) buffer writeonly selected_buffer {
  uint data[][SELECT_BOX_DEPTH];
} selected;

layout(set = 2, binding = 0) uniform sampler2D texSampler;

layout(push_constant) uniform pushConstant {
  layout(offset = 80) vec2 selectBoxMin;
  layout(offset = 88) vec2 selectBoxMax;  
  layout(offset = 96) vec4 pxRange;
} pc;

layout(location = 0) flat in uint inObjectId;
layout(location = 1) in vec4 fragColor;
layout(location = 2) in vec2 fragTexCoord;
layout(location = 3) flat in uint primType;

layout(location = 0) out vec4 outColor;

void main () {
  if (primType == 2) {
    outColor = vec4(fragColor * texture(texSampler, fragTexCoord));
  } else {
    outColor = fragColor;
  }
  if (pc.selectBoxMin.x <= gl_FragCoord.x &&
      pc.selectBoxMin.y <= gl_FragCoord.y &&
      gl_FragCoord.x <= pc.selectBoxMax.x &&
      gl_FragCoord.y <= pc.selectBoxMax.y) {

    uint zIndex = uint(gl_FragCoord.z * SELECT_BOX_DEPTH);
    uint row_size = uint(pc.selectBoxMax.x) - uint(pc.selectBoxMin.x);
    uint offset = uint(gl_FragCoord.y - pc.selectBoxMin.y) * row_size
      + uint(gl_FragCoord.x - pc.selectBoxMin.x);
    selected.data[offset][zIndex] = inObjectId;
  }    
}
