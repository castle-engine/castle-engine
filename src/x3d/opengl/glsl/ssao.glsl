ivec2 screen_position();
float screen_get_depth_fast(ivec2 position);
vec4 screen_get_color(ivec2 position);

// shader from http://www.pasteall.org/12282, http://www.youtube.com/watch?v=R_L-_oGTbqw

uniform float near; //Z-near
uniform float far; //Z-far

#define PI 3.14159265

//samples on the first ring (was 8)
#define SSAO_SAMPLES 6

//ring count (was 6)
#define SSAO_RINGS 3

vec2 rand(in vec2 coord) //generating random noise
{
  float noiseX = (fract(sin(dot(coord ,vec2(12.9898,78.233))) * 43758.5453));
  float noiseY = (fract(sin(dot(coord ,vec2(12.9898,78.233)*2.0)) * 43758.5453));
  return vec2(noiseX,noiseY)*0.004;
}

float readDepth(const in ivec2 coord)
{
  return (2.0 * near) / (far + near - screen_get_depth_fast(coord) * (far-near));
}

float compareDepths( in float depth1, in float depth2 )
{
  const float aoCap = 0.8;//1.0;
  const float aoMultiplier = 100.0;
  const float depthTolerance = 0.0001;
  const float aorange = 60.0;// units in space the AO effect extends to (this gets divided by the camera far range
  float diff = sqrt(clamp(1.0-(depth1-depth2) / (aorange/(far-near)),0.0,1.0));
  float ao = min(aoCap,max(0.0,depth1-depth2-depthTolerance) * aoMultiplier) * diff;
  return ao;
}

void main(void)
{
  ivec2 current_pos = screen_position();
  float depth = readDepth(current_pos);
  float d;

  float width = float(screen_width);
  float height = float(screen_height);

  float aspect = width/height;
  vec2 noise = rand(vec2(current_pos));

  float w = (1.0 / width)/clamp(depth,0.05,1.0)+(noise.x*(1.0-noise.x));
  float h = (1.0 / height)/clamp(depth,0.05,1.0)+(noise.y*(1.0-noise.y));

  w *= width/2.0; h *= height/2.0;  // JA added this line !!

  float pw;
  float ph;

  float ao = 0.0;
  float s = 0.0;
  float fade = 1.0;

  for (int i = 0 ; i < SSAO_RINGS; i += 1)
  {
    fade *= 0.5;
    for (int j = 0 ; j < SSAO_SAMPLES*i; j += 1)
    {
      float step = PI*2.0 / float(SSAO_SAMPLES*i);
      pw = (cos(float(j)*step)*float(i));
      ph = (sin(float(j)*step)*float(i));
      d = readDepth(current_pos + ivec2(int(pw * w), int(ph * h)));
      ao += compareDepths(depth,d)*fade;
      s += 1.0*fade;
    }
  }

  ao /= s;
  ao = 1.0-ao;

  vec3 color = screen_get_color(current_pos).rgb;
  //vec3 luminance = texture2D(bgl_LuminanceTexture,vec2(current_pos)).rgb;
  //luminance = clamp(max(0.0,luminance-0.2)+max(0.0,luminance-0.2)+max(0.0,luminance-0.2),0.0,1.0);
  //gl_FragColor = vec4(color*mix(vec3(ao),vec3(1.0),luminance),1.0);

  ao = ao * 0.8 + 0.2;
  gl_FragColor = vec4(color*vec3(ao),1.0);
  //gl_FragColor = vec4(vec3(ao), 1.0);  // ssao only
}
