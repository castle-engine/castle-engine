/**
* Source code is based on Riccardo Balbo's SSR shader: https://github.com/riccardobl/SimpleSSRShader/
*/

uniform float near;
uniform float far;
uniform float defaultSurfaceGlossiness;
uniform mat4 castle_ViewProjectionMatrix;
uniform mat4 castle_ViewProjectionMatrixInverse;
uniform vec3 castle_CameraPosition;

#define INITIAL_STEP_LENGTH 1.0
#define RAY_SAMPLES 16
#define NEARBY_SAMPLES 4
#define DEPTH_TEST_BIAS 0.0001

/**
* Ray structure used for ray marching
*/
struct Ray {
  // World position of the surface from where the ray is originated
  vec3 worldFrom;
  // Same as before but in screenspace
  vec3 screenFrom;
  // Glossiness of the surface from where the ray is originated. Default is 1
  float surfaceGlossiness;
  // Its direction
  vec3 worldDir;
  // The size of one pixel
  vec2 pixelSize;
};

/**
* Returned when the ray hit or miss the scene
*/
struct HitResult {
  // Last tested screen position (-1,-1 if missed)
  vec3 screenPos;
  // How strong the reflection is
  float reflStrength;
};

const vec2 nearFade = vec2(0.01, 1.0);
const vec2 farFade = vec2(20, 300);
vec2 _SAMPLES[4];

float getLinearDepth(float depth) {
  return (2.0 * near) / (far + near - depth * (far - near));
}

vec2 getPixelSize() {
  return vec2(1.0 / float(screen_width), 1.0 / float(screen_height));
}

vec3 worldToScreen(in vec3 worldPos) {
  vec4 pos = castle_ViewProjectionMatrix * vec4(worldPos, 1.0);
  pos.xyz /= pos.w;
  pos.xyz = pos.xyz * 0.5 + 0.5;
  return pos.xyz;
}

vec3 screenToWorld(in vec3 screenPos){
  vec4 pos = vec4(screenPos, 1.0) * 2.0 - 1.0;
  pos = castle_ViewProjectionMatrixInverse * pos;
  return pos.xyz /= pos.w;
}

/**
* Calculate normal from depth buffer
*/
vec3 getNormal(in vec3 worldPos, in vec2 pos) {
  vec2 pixelSize = getPixelSize();
  float depth2 = screenf_01_get_depth(pos + vec2(pixelSize.x, 0.0));
  float depth3 = screenf_01_get_depth(pos + vec2(0.0, pixelSize.y));
  vec3 worldPos2 = screenToWorld(vec3(pos + vec2(pixelSize.x, 0.0), depth2));
  vec3 worldPos3 = screenToWorld(vec3(pos + vec2(0.0, pixelSize.y), depth3));
  vec3 v1 = worldPos3 - worldPos;
  vec3 v2 = worldPos2 - worldPos;
  return normalize(cross(v1, v2));
}

Ray createRay(in vec2 pos, in float depth) {
  Ray ray;
  ray.screenFrom = vec3(pos, depth);
  ray.worldFrom = screenToWorld(ray.screenFrom);
  ray.surfaceGlossiness = defaultSurfaceGlossiness;
  vec3 worldNormal = getNormal(ray.worldFrom, pos);
  // direction from camera to fragment (in world space)
  vec3 worldDir = normalize(ray.worldFrom - castle_CameraPosition);
  // reflection vector
  ray.worldDir = normalize(reflect(worldDir, normalize(worldNormal)));
  ray.pixelSize = getPixelSize();
  return ray;
}

/**
* Actual ray marching happens here
*/
HitResult performRayMarching(in Ray ray){
  HitResult result;
  result.screenPos = vec3(-1.0);

  // Current position of the sample along the ray
  vec3 sampleWorldPos;
  // Same of before, but in screen space
  vec3 sampleScreenPos;
  // Position of the nearest surface at the sample position (in screen space)
  vec3 hitSurfaceScreenPos;
  // Length of the next step
  float stepLength = INITIAL_STEP_LENGTH;

  float linearSourceDepth = getLinearDepth(ray.screenFrom.z);

  for (int i = 0; i < RAY_SAMPLES; i++) {
    sampleWorldPos = ray.worldFrom + ray.worldDir * stepLength;
    sampleScreenPos = worldToScreen(sampleWorldPos);
    float depth = screenf_01_get_depth(sampleScreenPos.xy);

    hitSurfaceScreenPos = vec3(sampleScreenPos.xy, depth);
    vec3 hitSurfaceWorldPos = screenToWorld(hitSurfaceScreenPos);

    int j = 0;
    do {
      // We need to linearize the depth to have consistent tests for distant samples
      float linearHitSurfaceDepth = getLinearDepth(hitSurfaceScreenPos.z);
      float linearSampleDepth = getLinearDepth(sampleScreenPos.z);
      bool hit =
          linearHitSurfaceDepth > linearSourceDepth // check if the thing we want to reflect is behind the source of the ray
          && abs(linearSampleDepth - linearHitSurfaceDepth) < DEPTH_TEST_BIAS; // check if the ray is (~almost) hitting the surface
      // if first hit (letting the cycle running helds to better performances than breaking it)
      if (hit && result.screenPos.x == -1.0) {
        result.screenPos = sampleScreenPos;
        // Fade distant reflections
        result.reflStrength = distance(hitSurfaceWorldPos, ray.worldFrom);
        result.reflStrength = smoothstep(nearFade.x, nearFade.y, result.reflStrength)
            * (1.0 - smoothstep(farFade.x, farFade.y, result.reflStrength));
      }
      hitSurfaceScreenPos = vec3(sampleScreenPos.xy, screenf_01_get_depth(sampleScreenPos.xy + _SAMPLES[j].xy * ray.pixelSize));
      j++;
    } while (j <= NEARBY_SAMPLES);
    // Compute next step length
    stepLength = length(ray.worldFrom - hitSurfaceWorldPos);
  }
  return result;
}

void main(void)
{
  vec2 pos = screenf_01_position;
  vec4 baseColor = screenf_01_get_color(pos);
  float depth = screenf_01_get_depth(pos);
  _SAMPLES[0] = vec2(1.0, 0.0);
  _SAMPLES[1] = vec2(-1.0, 0.0);
  _SAMPLES[2] = vec2(0.0, 1.0);
  _SAMPLES[3] = vec2(0.0, -1.0);

  if (depth != 1.0) { // ignore the sky
    // Build the ray
    Ray ray = createRay(pos, depth);

    // Perform ray marching
    HitResult result = performRayMarching(ray);

    // Used to fade reflections near screen edges to remove artifacts
    float d = distance(result.screenPos.xy, vec2(0.5));
    d = pow(1.0 - clamp(d, 0.0, 0.5) * 2.0, 2.0);

    // Render reflections
    if (result.screenPos.x > -1.0) {
      vec3 color = screenf_01_get_color(result.screenPos.xy).rgb;
      float a = d * ray.surfaceGlossiness * result.reflStrength;
      color = ((1.0 - a) * baseColor.rgb) + (a * color);
      baseColor.rgb = color.rgb;
    }
  }
  gl_FragColor = baseColor;
}
