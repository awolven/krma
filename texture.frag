#version 450
#extension GL_ARB_separate_shader_objects : enable

layout(set = 2 , binding = 0) uniform sampler2D texSampler;

layout(push_constant) uniform pushConstant {
  layout(offset = 96) vec4 color;
  layout(offset = 112) float pxRange;
} pc;

layout(location = 0) in vec4 fragColor;
layout(location = 1) in vec2 fragTexCoord;
layout(location = 2) flat in uint primType;

layout(location = 0) out vec4 outColor;

void main () {
  if (primType == 2) {
    outColor = vec4(fragColor * texture(texSampler, fragTexCoord));
  } else {
    outColor = fragColor;
  }
}
