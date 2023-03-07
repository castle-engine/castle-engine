/* Generate normal vector based on the vertex position, and time,
   using smooth noise implemented in noise3Dgrad.glsl .
*/

varying vec4 water_vertex_object;
uniform float time;

// OpenGLES requires this, otherwise error: precision mismatch between shaders for uniform (named castle_NormalMatrix)
uniform highp mat3 castle_NormalMatrix;

// Declare snoise from noise3Dgrad.glsl .
float snoise(vec3 v, out vec3 gradient);

// Adjust these values freely
#define wave_speed 1.0
#define wave_size 20.0
#define wave_calmness 10.0
#define wave_perturb_reflection 0.01
//#define wave_faster // uncomment this if you want

vec3 simple_3d_noise(vec3 noise_input)
{
  vec3 result;

  vec3 output1;
  snoise(noise_input, output1);
  result += output1;

#ifndef wave_faster
  /* Add more noise layers, to make it interesting.
     The values that we use to multiply below are just increasing powers
     of two, but disturbed a little, so that their loops are not multiplies
     of each other.
  */

  vec3 output2;
  snoise(noise_input * 2.1, output2);
  result += output2 / 2.5;

  vec3 output3;
  snoise(noise_input * 3.8, output3);
  result += output3 / 4.6;
#endif

  return result;
}

vec3 normal_in_object_space;

void PLUG_fragment_eye_space(const vec4 vertex_eye, inout vec3 normal_eye)
{
  vec3 noise_input = vec3(
    water_vertex_object.x * wave_size,
    time * wave_speed,
    water_vertex_object.z * wave_size);
  normal_in_object_space = simple_3d_noise(noise_input);
  normal_in_object_space = normalize(normal_in_object_space);

  // only positive Y makes sense for us, bring Y in [0..1] range
  normal_in_object_space.y = (normal_in_object_space.y + 1.0) * 0.5;

  // to force calmer waves, just enlarge Y and normalize again
  //normal_in_object_space.y = normal_in_object_space.y * wave_calmness;
  normal_in_object_space.y = mix(wave_calmness * 0.5, wave_calmness, normal_in_object_space.y);

  normal_in_object_space = normalize(normal_in_object_space);

  // just to test, dummy generation instead of using simple_3d_noise
  /*
  normal_in_object_space = vec3(
    sin(water_vertex_object.x * 100.0),
    frac(time),
    sin(water_vertex_object.z * 100.0));
  normal_in_object_space = normalize(normal_in_object_space);
  */

  normal_eye = normalize(castle_NormalMatrix * normal_in_object_space);
}

void PLUG_texture_coord_shift(inout vec2 tex_coord)
{
  /* We perturb reflections simply by shifting a tiny bit tex_coord.xy,
     which is used for sampling the RenderedTexture that contains the mirror.

     This trick is *not* based in reality.
     We should instead use normal_in_object_space to calculate
     the correctly reflected color.
     However, this is much easier, and looks convincing :)
  */
  tex_coord.xy += normal_in_object_space.xz * wave_perturb_reflection;
}
