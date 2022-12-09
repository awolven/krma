#version 450
#extension GL_ARB_separate_shader_objects : enable

#define SELECT_BOX_DEPTH 128

layout(set = 1, binding = 0) buffer writeonly selected_buffer {
  uint data[][SELECT_BOX_DEPTH];
} selected;

layout(set = 2 , binding = 0) uniform sampler2D image;

layout(push_constant) uniform pushConstant {
  layout(offset = 80) vec2 selectBoxMin;
  layout(offset = 88) vec2 selectBoxMax;  
  layout(offset = 96) float pxRange;
} pc;

layout(location = 0) flat in uint inObjectId;
layout(location = 1) in vec4 vert_color;
layout(location = 2) in vec2 uv;
layout(location = 3) flat in uint primType;

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
