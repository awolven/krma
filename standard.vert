layout (location = 0) in vec2 Position;
layout (location = 1) in vec2 UV;
layout (location = 2) in vec4 Color;
uniform mat4 ProjMtx;
out vec2 Frag_UV;
out vec4 Frag_Color;
void main()
{
    Frag_UV = UV;
    Frag_Color = Color;
    gl_Position = ProjMtx * vec4(Position.xy,0,1);

#version 450
#extension GL_ARB_separate_shader_objects : enable

layout(binding = 0) uniform UniformBuffer {
  mat4 model;
  mat4 view;
  mat4 proj;
  mat4 clip;
} ubo;

layout(location = 0) in vec3 inPosition;
layout(location = 1) in vec2 inTexCoord;

layout(location = 0) out vec3 fragColor;
layout(location = 1) out vec2 fragTexCoord;

void main () {
  gl_Position = ubo.clip * ubo.proj * ubo.view * ubo.model * vec4(inPosition, 1.0);
  fragColor = vec3(1.0, 1.0, 1.0);
  fragTexCoord = inTexCoord;
}
