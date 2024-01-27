#version 450
#extension GL_ARB_separate_shader_objects : enable
#extension GL_EXT_buffer_reference2 : require
#extension GL_EXT_shader_explicit_arithmetic_types_int64 : require

layout(buffer_reference, std430, buffer_reference_align = 32) buffer _bucket {
  uint val[8]; // gpu minimum cache line size, slots 0 through 6 (if non-zero) are items in hash set, slot 7 (if non-zero) is link to next bucket
}; // for hashset logic

layout(buffer_reference, std430, buffer_reference_align = 4) buffer _table {
  uint val;
}; // for hashset logic

layout(buffer_reference, std430, buffer_reference_align = 4) buffer _counter {
  uint val;
}; // for hashset logic

#define max_depth 3 // for hashset logic

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

layout(set = 0, binding = 1) uniform uniformBuffer { // partially used
  light lights[MAX_LIGHTS];
  uint num_lights;
  uint scene_ambient;
  vec2 pointer_pos;
  uint64_t table;  
  uint table_capacity;
} ub;

layout(set = 1, binding = 0) buffer select_buffer_2d {
  uint selected_objects_2d[][SELECT_BOX_DEPTH_2D];
} ;

layout(set = 1, binding = 1) buffer select_buffer_3d {
  uint selected_objects_3d[][SELECT_BOX_DEPTH_3D];
} ;

layout(set = 2 , binding = 0) uniform sampler2D image;

layout(push_constant) uniform pushConstant {
  layout(offset = 88) float pxRange;
  layout(offset = 92) uint uint_ambient; // not used
  layout(offset = 96) uint uint_diffuse; // not used
  layout(offset = 100) uint uint_specular; // not used
  layout(offset = 104) float shininess; // not used
  layout(offset = 108) uint unused;
  layout(offset = 112) vec4 selectBox;
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

uint hash(uint x) {
  x ^= x >> 16;
  x *= 0x7feb352dU;
  x ^= x >> 15;
  x *= 0x846ca68bU;
  x ^= x >> 16;
  return x;
} // for hashset logic

bool sethash (uint value) {
  if ( value == 0 ) return true;
  
  uint slot = hash(value) % (ub.table_capacity - 1);

  uint count = 0;
  
  while ( count < 50 )
    {
      uint prev = atomicCompSwap(_table(ub.table + uint64_t(slot)).val, 0, value);
      if ( prev == 0 || prev == value ) {
	return true;
      }
      count += 1;

      slot = (slot + 1) % (ub.table_capacity - 1);
    }
  return false;
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

  if (min(pc.selectBox.x, pc.selectBox.z) <= gl_FragCoord.x &&
      min(pc.selectBox.y, pc.selectBox.w) <= gl_FragCoord.y &&
      gl_FragCoord.x <= max(pc.selectBox.x, pc.selectBox.z) &&
      gl_FragCoord.y <= max(pc.selectBox.y, pc.selectBox.w)) {
    
    sethash(inObjectId);
  }

  if ((ub.pointer_pos.x - 0.5) <= gl_FragCoord.x &&
      (ub.pointer_pos.y - 0.5) <= gl_FragCoord.y &&
      gl_FragCoord.x <= (ub.pointer_pos.x + 0.5) &&
      gl_FragCoord.y <= (ub.pointer_pos.y + 0.5)) {
    
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

