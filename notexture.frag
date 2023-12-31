#version 450
#extension GL_ARB_separate_shader_objects : enable

#define SELECT_BOX_DEPTH_2D 1024
#define SELECT_BOX_DEPTH_3D 1024
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

layout(set = 1, binding = 0) buffer select_buffers {
  uint selected_objects_2d[][SELECT_BOX_DEPTH_2D];
} ;

layout(set = 1, binding = 1) buffer select_buffer_3d {
  uint selected_objects_3d[][SELECT_BOX_DEPTH_3D];
} ;

layout(push_constant) uniform pushConstant {
  layout(offset = 88) float pxRange; // not used
  layout(offset = 92) uint uint_ambient; // not used
  layout(offset = 96) uint uint_diffuse; // not used
  layout(offset = 100) uint uint_specular; // not used
  layout(offset = 104) float shininess; // not used
  layout(offset = 108) uint unused;
  layout(offset = 112) vec4 selectBox;
} pc;

layout(location = 0) flat in uint inObjectId;
layout(location = 1) in vec4 fragColor;

layout(location = 6) flat in uint is2d;
layout(location = 9) in vec4 extents;

layout(location = 0) out vec4 outColor;

void main () {
  outColor = fragColor;

  if (pc.selectBox.x <= gl_FragCoord.x &&
      pc.selectBox.y <= gl_FragCoord.y &&
      gl_FragCoord.x <= pc.selectBox.z &&
      gl_FragCoord.y <= pc.selectBox.w) {

    if ( is2d == 1) {
      uint zIndex = uint(round((1.0 - gl_FragCoord.z) * SELECT_BOX_DEPTH_2D) + 0.5);
      uint row_size = uint(pc.selectBox.z) - uint(pc.selectBox.x);
      uint offset = uint(gl_FragCoord.y - pc.selectBox.y) * row_size
	+ uint(gl_FragCoord.x - pc.selectBox.x);
      if (selected_objects_2d[offset][zIndex] == 0) {
	selected_objects_2d[offset][zIndex] = inObjectId;
      }
    } else {
      float near = extents.z;
      float far = extents.w;
      float z = (2.0 * near) / (far + near - gl_FragCoord.z * (far - near));
      
      uint zIndex = uint(z * SELECT_BOX_DEPTH_3D);
      uint row_size = uint(pc.selectBox.z) - uint(pc.selectBox.x);
      uint offset = uint(gl_FragCoord.y - pc.selectBox.y) * row_size
	+ uint(gl_FragCoord.x - pc.selectBox.x);
      if (selected_objects_3d[offset][zIndex] == 0) {
	selected_objects_3d[offset][zIndex] = inObjectId;
      }
    }
  } 
}
