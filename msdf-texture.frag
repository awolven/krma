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

layout(set = 1, binding = 0) buffer select_buffer_2d {
  uint selected_objects_2d[][SELECT_BOX_DEPTH_2D];
} ;

layout(set = 1, binding = 1) buffer select_buffer_3d {
  uint selected_objects_3d[][SELECT_BOX_DEPTH_3D];
} ;

layout(set = 2 , binding = 0) uniform sampler2D image;

layout(push_constant) uniform pushConstant {
  layout(offset = 80) vec4 selectBox;
  layout(offset = 96) float pxRange;
  layout(offset = 100) uint uint_ambient; // not used
  layout(offset = 104) uint uint_diffuse; // not used
  layout(offset = 108) uint uint_specular; // not used
  layout(offset = 112) float shininess; // not used
} pc;

layout(location = 0) flat in uint inObjectId;
layout(location = 1) in vec4 vert_color;
layout(location = 2) in vec2 uv;
layout(location = 3) flat in uint primType;

layout(location = 6) flat in uint is2d;

layout(location = 0) out vec4 out_color;

float median(float r, float g, float b){
  return max(min(r, g), min(max(r, g), b));
}

vec2 safeNormalize(in vec2 v){
  float len = length(v);
  len = (len > 0.0)? 1.0 / len : 0.0;
  return v * len;
}

void main(){
  vec4 thesample = texture(image, uv);
  float sigDist = median( thesample.r, thesample.g, thesample.b ) - 0.5;

  ivec2 sz = textureSize( image, 0 );
  float dx = dFdx( uv.x ) * sz.x;
  float dy = dFdy( uv.y ) * sz.y;
  float toPixels = pc.pxRange * inversesqrt( dx * dx + dy * dy );
  float opacity = clamp( sigDist * toPixels + 0.5, 0.0, 1.0 );

  vec4 frag_col = vert_color;//mix(pc.color, vert_color, vert_color.a);
  out_color = vec4(frag_col.rgb, frag_col.a*opacity);

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
	selected_objects_2d[offset][0] = zIndex;
      }
    } else {
      float near = 0.1;
      float far = 3000.0;
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
