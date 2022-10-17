#version 450
#extension GL_ARB_separate_shader_objects : enable

layout(set = 0, binding = 0) uniform uniformBuffer {
  mat4 mvp;
} ub;

layout(push_constant) uniform pushConstant {
  float pointSize;
} pc;


layout(location = 0) in vec3 inPosition;
layout(location = 1) in uint inColor;

out gl_PerVertex {
  vec4 gl_Position;
  float gl_PointSize;
};

layout(location = 0) out vec4 outColor;

uint color;

void main () {
  gl_Position = ub.mvp * vec4(inPosition.xyz, 1.0);
  gl_PointSize = pc.pointSize;
  color = inColor;
  outColor = vec4((0x000000ff & (color >> 24))/255.0,
                  (0x000000ff & (color >> 16))/255.0,
                  (0x000000ff & (color >> 8))/255.0,
                  (0x000000ff & color)/255.0);
}
