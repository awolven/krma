#version 450
#extension GL_ARB_separate_shader_objects : enable

#define SELECT_BOX_DEPTH_2D 1024
#define SELECT_BOX_DEPTH_3D 1024
#define MAX_LIGHTS 10

struct light {
  vec4 position;
  vec4 spotDirection;
  uint diffuse;
  uint specular;
  float constantAttenuation, linearAttenuation, quadraticAttenuation;
  float spotCutoff, spotExponent, padding;
};

layout(set = 0, binding = 1) uniform uniformBuffer {
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

layout(set = 2 , binding = 0) uniform sampler2D texSampler;

layout(push_constant) uniform pushConstant {
  layout(offset = 80) vec4 selectBox;
  layout(offset = 96) float pxRange;
  layout(offset = 100) uint uint_material_ambient;
  layout(offset = 104) uint uint_material_diffuse;
  layout(offset = 108) uint uint_material_specular;
  layout(offset = 112) float material_shininess;
} pc;

layout(location = 0) flat in uint inObjectId;
layout(location = 1) in vec4 fragColor;
layout(location = 2) in vec2 fragTexCoord;
layout(location = 3) in vec4 world_position;
layout(location = 4) in vec3 normal;
layout(location = 5) in mat4 view_inv;
//layout(location = 6) flat in uint is2d;

layout(location = 0) out vec4 outColor;

vec4 float_color(uint uint_color) {
  return vec4((0x000000ff & (uint_color >> 24))/255.0,
	      (0x000000ff & (uint_color >> 16))/255.0,
	      (0x000000ff & (uint_color >> 8))/255.0,
	      (0x000000ff & uint_color)/255.0);
}

uint uint_color(vec4 float_color) {
  return ((uint(float_color.x * 255) << 24) +
	  (uint(float_color.y * 255) << 16) +
	  (uint(float_color.z * 255) << 8) +
	  (uint(float_color.w * 255)));
}  

void main () {
  vec4 color = vec4(fragColor * texture(texSampler, fragTexCoord));
  vec4 material_ambient = float_color(pc.uint_material_ambient);
  vec4 material_diffuse = float_color(pc.uint_material_diffuse);
  vec4 material_specular = float_color(pc.uint_material_specular);
  float material_shininess = pc.material_shininess;
  
  vec3 normalDirection = normalize(normal);
  vec3 viewDirection = normalize((view_inv * vec4(0.0, 0.0, 0.0, 1.0) - world_position).xyz);
  vec3 lightDirection;
  float attenuation;
  vec3 totalLighting = float_color(ub.scene_ambient).xyz * material_ambient.xyz;

  for (int index = 0; index < ub.num_lights; index++) // for all light sources
    {
      vec3 light_diffuse = float_color(ub.lights[index].diffuse).xyz;
      vec3 light_specular = float_color(ub.lights[index].specular).xyz;

      if (0.0 == ub.lights[index].position.w) // directional light?
	{
	  attenuation = 1.0; // no attenuation
	  lightDirection = normalize((ub.lights[index].position).xyz);
	} 
      else // point light or spotlight (or other kind of light) 
	{
	  vec3 positionToLightSource = (ub.lights[index].position - world_position).xyz;
	  float distance = length(positionToLightSource);
	  lightDirection = normalize(positionToLightSource);
	  attenuation = 1.0 / (ub.lights[index].constantAttenuation
			       + ub.lights[index].linearAttenuation * distance
			       + ub.lights[index].quadraticAttenuation * distance * distance);
	  
	  if (ub.lights[index].spotCutoff <= 90.0) // spotlight?
	    {
	      float clampedCosine = max(0.0, dot(-lightDirection, normalize(ub.lights[index].spotDirection.xyz)));
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
	* light_diffuse * (material_diffuse).xyz
	* max(0.0, dot(normalDirection, lightDirection));
      
      vec3 specularReflection;
      if (dot(normalDirection, lightDirection) < 0.0) // light source on the wrong side?
	{
	  specularReflection = vec3(0.0, 0.0, 0.0); // no specular reflection
	}
      else // light source on the right side
	{
	  specularReflection = attenuation * light_specular * (material_specular).xyz
	    * pow(max(0.0, dot(reflect(-lightDirection, normalDirection), viewDirection)), material_shininess);
	}

      totalLighting = totalLighting + diffuseReflection + specularReflection;

      /* // for lighting debug:
      selected_objects_2d[0][0] = pc.uint_material_ambient;
      selected_objects_2d[0][1] = pc.uint_material_diffuse;
      selected_objects_2d[0][2] = pc.uint_material_specular;
      selected_objects_2d[0][3] = ub.lights[0].diffuse;
      selected_objects_2d[0][4] = ub.lights[0].specular;
      selected_objects_2d[0][5] = uint_color(color);
      selected_objects_2d[0][6] = uint_color(vec4(totalLighting, 0.0));
      selected_objects_2d[0][7] = uint_color(vec4(light_diffuse, 0.0));
      selected_objects_2d[0][8] = uint_color(vec4(light_specular, 0.0));
      selected_objects_2d[0][9] = uint_color(vec4(diffuseReflection, 0.0));
      selected_objects_2d[0][10] = uint_color(vec4(specularReflection, 0.0));
      selected_objects_2d[0][11] = uint_color(color * vec4(totalLighting, 1.0));
      */
    }

  outColor = color * vec4(totalLighting, 1.0);
  

  if (pc.selectBox.x <= gl_FragCoord.x &&
      pc.selectBox.y <= gl_FragCoord.y &&
      gl_FragCoord.x <= pc.selectBox.z &&
      gl_FragCoord.y <= pc.selectBox.w) {

    // this shader should never be assigned in a 2d pipeline
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

