#version 450
#extension GL_ARB_separate_shader_objects : enable

layout(set = 2 , binding = 0) uniform sampler2D texSampler;

layout(push_constant) uniform pushConstant {
  layout(offset = 96) vec4 color;
  layout(offset = 112) float pxRange;
} pc;

layout(location = 0) in vec4 fragColor;
layout(location = 1) in vec2 fragTexCoord;
layout(location = 2) in float brightness;

layout(location = 0) out vec4 outColor;

void main () {
  vec4 color = vec4(fragColor * texture(texSampler, fragTexCoord));
  outColor = vec4((0.5 * brightness + 0.5) * color.rgb, color.a);
}
