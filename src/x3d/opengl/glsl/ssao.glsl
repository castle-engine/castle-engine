#extension GL_ARB_texture_rectangle : enable
uniform sampler2DRect screen;
uniform sampler2DRect screen_depth;
uniform int screen_width;
uniform int screen_height;

// shader from http://www.pasteall.org/12282, http://www.youtube.com/watch?v=R_L-_oGTbqw

#define PI    3.14159265

float width = float(screen_width);
float height = float(screen_height);

uniform float near; //Z-near
uniform float far; //Z-far

int samples = 6;//8; //samples on the first ring
int rings = 3;//6; //ring count

vec2 texCoord = gl_TexCoord[0].st;

vec2 rand(in vec2 coord) //generating random noise
{
  float noiseX = (fract(sin(dot(coord ,vec2(12.9898,78.233))) * 43758.5453));
  float noiseY = (fract(sin(dot(coord ,vec2(12.9898,78.233)*2.0)) * 43758.5453));
  return vec2(noiseX,noiseY)*0.004;
}

float readDepth(in vec2 coord)
{
  return (2.0 * near) / (far + near - texture2DRect(screen_depth, coord).r * (far-near));
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
  float depth = readDepth(texCoord);
  float d;

  float aspect = width/height;
  vec2 noise = rand(texCoord);

  float w = (1.0 / width)/clamp(depth,0.05,1.0)+(noise.x*(1.0-noise.x));
  float h = (1.0 / height)/clamp(depth,0.05,1.0)+(noise.y*(1.0-noise.y));

  w *= width/2.0; h *= height/2.0;  // JA added this line !!

  float pw;
  float ph;

  float ao = 0.0;
  float s = 0.0;
  float fade = 1.0;

  for (int i = 0 ; i < rings; i += 1)
  {
    fade *= 0.5;
    for (int j = 0 ; j < samples*i; j += 1)
    {
      float step = PI*2.0 / (samples*i);
      pw = (cos(j*step)*i);
      ph = (sin(j*step)*i);
      d = readDepth(vec2(texCoord.s+pw*w,texCoord.t+ph*h));
      ao += compareDepths(depth,d)*fade;
      s += 1.0*fade;
    }
  }

  ao /= s;
  ao = 1.0-ao;

  vec3 color = texture2DRect(screen,texCoord).rgb;
  //vec3 luminance = texture2D(bgl_LuminanceTexture,texCoord).rgb;
  //luminance = clamp(max(0.0,luminance-0.2)+max(0.0,luminance-0.2)+max(0.0,luminance-0.2),0.0,1.0);
  //gl_FragColor = vec4(color*mix(vec3(ao),vec3(1.0),luminance),1.0);
  
  ao = ao * 0.8 + 0.2;
  gl_FragColor = vec4(color*vec3(ao),1.0);
  //gl_FragColor = vec4(vec3(ao), 1.0);  // ssao only
}
