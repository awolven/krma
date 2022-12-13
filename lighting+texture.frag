#version 450
#extension GL_ARB_separate_shader_objects : enable

#define MAX_LIGHTS 10
#define SELECT_BOX_DEPTH 128

struct light {
  vec4 position;
  uint diffuse;
  uint specular;
  float constantAttenuation, linearAttenuation, quadraticAttenuation;
  float spotCutoff, spotExponent;
  vec3 spotDirection;
};

layout(set = 0, binding = 1) uniform uniformBuffer {
  light lights[MAX_LIGHTS];
  uint num_lights;
  uint scene_ambient;
} ub;

layout(set = 1, binding = 0) buffer writeonly select_buffer {
  uint selected_objects[][SELECT_BOX_DEPTH];
};

layout(set = 2 , binding = 0) uniform sampler2D texSampler;

layout(push_constant) uniform pushConstant {
  layout(offset = 80) vec4 selectBox;
  layout(offset = 96) float pxRange;
  layout(offset = 100) uint uint_ambient;
  layout(offset = 104) uint uint_diffuse;
  layout(offset = 108) uint uint_specular;
  layout(offset = 112) float shininess;
} pc;

layout(location = 0) flat in uint inObjectId;
layout(location = 1) in vec4 fragColor;
layout(location = 2) in vec2 fragTexCoord;
layout(location = 3) in vec4 world_position;
layout(location = 4) in vec3 normal;
layout(location = 5) in mat4 view_inv;

layout(location = 0) out vec4 outColor;

vec4 float_color(uint uint_color) {
  return vec4((0x000000ff & (uint_color >> 24))/255.0,
	      (0x000000ff & (uint_color >> 16))/255.0,
	      (0x000000ff & (uint_color >> 8))/255.0,
	      (0x000000ff & uint_color)/255.0);
}

void main () {
  vec4 color = vec4(fragColor * texture(texSampler, fragTexCoord));
  vec4 ambient = float_color(pc.uint_ambient);
  vec4 diffuse = float_color(pc.uint_diffuse);
  vec4 specular = float_color(pc.uint_specular);
  float shininess = pc.shininess;
  
  vec3 normalDirection = normalize(normal);
  vec3 viewDirection = normalize(vec3(view_inv * vec4(0.0, 0.0, 0.0, 1.0) - world_position));
  vec3 lightDirection;
  float attenuation;
  vec3 totalLighting = vec3(ub.scene_ambient) * vec3(ambient);

  

  for (int index = 0; index < ub.num_lights; index++) // for all light sources
    {
      if (0.0 == ub.lights[index].position.w) // directional light?
	{
	  attenuation = 1.0; // no attenuation
	  lightDirection = normalize(vec3(ub.lights[index].position));
	} 
      else // point light or spotlight (or other kind of light) 
	{
	  vec3 positionToLightSource = vec3(ub.lights[index].position - world_position);
	  float distance = length(positionToLightSource);
	  lightDirection = normalize(positionToLightSource);
	  attenuation = 1.0 / (ub.lights[index].constantAttenuation
			       + ub.lights[index].linearAttenuation * distance
			       + ub.lights[index].quadraticAttenuation * distance * distance);
	  
	  if (ub.lights[index].spotCutoff <= 90.0) // spotlight?
	    {
	      float clampedCosine = max(0.0, dot(-lightDirection, normalize(ub.lights[index].spotDirection)));
	      if (clampedCosine < cos(radians(ub.lights[index].spotCutoff))) // outside of spotlight cone?
		{
		  attenuation = 0.0;
		}
	      else
		{
		  attenuation = attenuation * pow(clampedCosine, ub.lights[index].spotExponent);   
		}
	    }
	}

      vec3 diffuseReflection = attenuation 
	* vec3(ub.lights[index].diffuse) * vec3(diffuse)
	* max(0.0, dot(normalDirection, lightDirection));
      
      vec3 specularReflection;
      if (dot(normalDirection, lightDirection) < 0.0) // light source on the wrong side?
	{
	  specularReflection = vec3(0.0, 0.0, 0.0); // no specular reflection
	}
      else // light source on the right side
	{
	  specularReflection = attenuation * vec3(ub.lights[index].specular) * vec3(specular) 
	    * pow(max(0.0, dot(reflect(-lightDirection, normalDirection), viewDirection)), shininess);
	}

      totalLighting = totalLighting + diffuseReflection + specularReflection;
    }
  
  outColor = color * vec4(totalLighting, 1.0);

  if (pc.selectBox.x <= gl_FragCoord.x &&
      pc.selectBox.y <= gl_FragCoord.y &&
      gl_FragCoord.x <= pc.selectBox.z &&
      gl_FragCoord.y <= pc.selectBox.w) {

    // does fragcoord.z go from [0, 1) ? if so, zIndex is correct
    uint zIndex = uint(gl_FragCoord.z * SELECT_BOX_DEPTH);
    uint row_size = uint(pc.selectBox.z) - uint(pc.selectBox.x);
    uint offset = uint(gl_FragCoord.y - pc.selectBox.y) * row_size
      + uint(gl_FragCoord.x - pc.selectBox.x); 
    selected_objects[offset][zIndex] = inObjectId;
  } 
}
