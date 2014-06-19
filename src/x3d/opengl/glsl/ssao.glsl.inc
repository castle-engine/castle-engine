{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "ssao.glsl" }
'ivec2 screen_position();' + LineEnding +
'float screen_get_depth_fast(ivec2 position);' + LineEnding +
'vec4 screen_get_color(ivec2 position);' + LineEnding +
'' + LineEnding +
'// shader from http://www.pasteall.org/12282, http://www.youtube.com/watch?v=R_L-_oGTbqw' + LineEnding +
'' + LineEnding +
'uniform float near; //Z-near' + LineEnding +
'uniform float far; //Z-far' + LineEnding +
'' + LineEnding +
'#define PI 3.14159265' + LineEnding +
'' + LineEnding +
'//samples on the first ring (was 8)' + LineEnding +
'#define SSAO_SAMPLES 6' + LineEnding +
'' + LineEnding +
'//ring count (was 6)' + LineEnding +
'#define SSAO_RINGS 3' + LineEnding +
'' + LineEnding +
'vec2 rand(in vec2 coord) //generating random noise' + LineEnding +
'{' + LineEnding +
'  float noiseX = (fract(sin(dot(coord ,vec2(12.9898,78.233))) * 43758.5453));' + LineEnding +
'  float noiseY = (fract(sin(dot(coord ,vec2(12.9898,78.233)*2.0)) * 43758.5453));' + LineEnding +
'  return vec2(noiseX,noiseY)*0.004;' + LineEnding +
'}' + LineEnding +
'' + LineEnding +
'float readDepth(const in ivec2 coord)' + LineEnding +
'{' + LineEnding +
'  return (2.0 * near) / (far + near - screen_get_depth_fast(coord) * (far-near));' + LineEnding +
'}' + LineEnding +
'' + LineEnding +
'float compareDepths( in float depth1, in float depth2 )' + LineEnding +
'{' + LineEnding +
'  const float aoCap = 0.8;//1.0;' + LineEnding +
'  const float aoMultiplier = 100.0;' + LineEnding +
'  const float depthTolerance = 0.0001;' + LineEnding +
'  const float aorange = 60.0;// units in space the AO effect extends to (this gets divided by the camera far range' + LineEnding +
'  float diff = sqrt(clamp(1.0-(depth1-depth2) / (aorange/(far-near)),0.0,1.0));' + LineEnding +
'  float ao = min(aoCap,max(0.0,depth1-depth2-depthTolerance) * aoMultiplier) * diff;' + LineEnding +
'  return ao;' + LineEnding +
'}' + LineEnding +
'' + LineEnding +
'void main(void)' + LineEnding +
'{' + LineEnding +
'  ivec2 current_pos = screen_position();' + LineEnding +
'  float depth = readDepth(current_pos);' + LineEnding +
'  float d;' + LineEnding +
'' + LineEnding +
'  float width = float(screen_width);' + LineEnding +
'  float height = float(screen_height);' + LineEnding +
'' + LineEnding +
'  float aspect = width/height;' + LineEnding +
'  vec2 noise = rand(vec2(current_pos));' + LineEnding +
'' + LineEnding +
'  float w = (1.0 / width)/clamp(depth,0.05,1.0)+(noise.x*(1.0-noise.x));' + LineEnding +
'  float h = (1.0 / height)/clamp(depth,0.05,1.0)+(noise.y*(1.0-noise.y));' + LineEnding +
'' + LineEnding +
'  w *= width/2.0; h *= height/2.0;  // JA added this line !!' + LineEnding +
'' + LineEnding +
'  float pw;' + LineEnding +
'  float ph;' + LineEnding +
'' + LineEnding +
'  float ao = 0.0;' + LineEnding +
'  float s = 0.0;' + LineEnding +
'  float fade = 1.0;' + LineEnding +
'' + LineEnding +
'  for (int i = 0 ; i < SSAO_RINGS; i += 1)' + LineEnding +
'  {' + LineEnding +
'    fade *= 0.5;' + LineEnding +
'    for (int j = 0 ; j < SSAO_SAMPLES*i; j += 1)' + LineEnding +
'    {' + LineEnding +
'      float step = PI*2.0 / float(SSAO_SAMPLES*i);' + LineEnding +
'      pw = (cos(float(j)*step)*float(i));' + LineEnding +
'      ph = (sin(float(j)*step)*float(i));' + LineEnding +
'      d = readDepth(current_pos + ivec2(int(pw * w), int(ph * h)));' + LineEnding +
'      ao += compareDepths(depth,d)*fade;' + LineEnding +
'      s += 1.0*fade;' + LineEnding +
'    }' + LineEnding +
'  }' + LineEnding +
'' + LineEnding +
'  ao /= s;' + LineEnding +
'  ao = 1.0-ao;' + LineEnding +
'' + LineEnding +
'  vec3 color = screen_get_color(current_pos).rgb;' + LineEnding +
'  //vec3 luminance = texture2D(bgl_LuminanceTexture,vec2(current_pos)).rgb;' + LineEnding +
'  //luminance = clamp(max(0.0,luminance-0.2)+max(0.0,luminance-0.2)+max(0.0,luminance-0.2),0.0,1.0);' + LineEnding +
'  //gl_FragColor = vec4(color*mix(vec3(ao),vec3(1.0),luminance),1.0);' + LineEnding +
'' + LineEnding +
'  ao = ao * 0.8 + 0.2;' + LineEnding +
'  gl_FragColor = vec4(color*vec3(ao),1.0);' + LineEnding +
'  //gl_FragColor = vec4(vec3(ao), 1.0);  // ssao only' + LineEnding +
'}' + LineEnding +
''
