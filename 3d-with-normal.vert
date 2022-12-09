#version 450
#extension GL_ARB_separate_shader_objects : enable

#define MAX_LIGHTS 10

struct point_light {
  vec4 position;
  vec4 color;
  float radius;
};

layout(set = 0, binding = 0) uniform uniformBuffer {
  mat4 vproj;
  point_light lights[MAX_LIGHTS];
  uint num_lights;
} ub;

layout(push_constant) uniform pushConstant {
  layout(offset = 0) mat4 model;
  layout(offset = 80) float pointSize;
  layout(offset = 84) uint type;
  layout(offset = 88) uint override_color;
  layout(offset = 92) bool override_color_p;
} pc;

layout(location = 0) in uint inObjectId;
layout(location = 1) in vec3 inPosition;
layout(location = 2) in vec2 inTexCoord;
layout(location = 3) in uint inColor;
layout(location = 4) in vec3 inNormal;

layout(location = 0) flat out uint outObjectId;
layout(location = 1) out vec4 outColor;
layout(location = 2) out vec2 outTexCoord;
layout(location = 3) out float brightness;

uint color;

float epsilon = 0.0001;

vec3 euclid (vec4 v) {
  if (abs(v.w) < epsilon) {
    return vec3(0, 0, 0);
  }{
    return v.xyz/v.w;
  }
}  

void main () {
  vec4 pos = pc.model * vec4(inPosition.xyz, 1.0);
  vec3 norm = (pc.model * vec4(inNormal, 0.0)).xyz;
  gl_Position = ub.vproj * pos;
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
  vec3 lightDir = normalize(pc.lightPosition - euclid(pos));
  brightness = max(min(dot(normalize(norm), lightDir), 1.0), 0.0);
}
