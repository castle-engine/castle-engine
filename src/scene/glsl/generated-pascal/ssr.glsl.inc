{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "source/ssr.glsl" }
'/**' + LineEnding +
'* Source code is based on Riccardo Balbo''s SSR shader: https://github.com/riccardobl/SimpleSSRShader/' + LineEnding +
'*/' + LineEnding +
'' + LineEnding +
'uniform float near;' + LineEnding +
'uniform float far;' + LineEnding +
'uniform float defaultSurfaceGlossiness;' + LineEnding +
'uniform mat4 castle_ViewProjectionMatrix;' + LineEnding +
'uniform mat4 castle_ViewProjectionMatrixInverse;' + LineEnding +
'uniform vec3 castle_CameraPosition;' + LineEnding +
'' + LineEnding +
'#define INITIAL_STEP_LENGTH 1.0' + LineEnding +
'#define RAY_SAMPLES 16' + LineEnding +
'#define NEARBY_SAMPLES 4' + LineEnding +
'#define DEPTH_TEST_BIAS 0.0001' + LineEnding +
'' + LineEnding +
'/**' + LineEnding +
'* Ray structure used for ray marching' + LineEnding +
'*/' + LineEnding +
'struct Ray {' + LineEnding +
'  // World position of the surface from where the ray is originated' + LineEnding +
'  vec3 worldFrom;' + LineEnding +
'  // Same as before but in screenspace' + LineEnding +
'  vec3 screenFrom;' + LineEnding +
'  // Glossiness of the surface from where the ray is originated. Default is 1' + LineEnding +
'  float surfaceGlossiness;' + LineEnding +
'  // Its direction' + LineEnding +
'  vec3 worldDir;' + LineEnding +
'  // The size of one pixel' + LineEnding +
'  vec2 pixelSize;' + LineEnding +
'};' + LineEnding +
'' + LineEnding +
'/**' + LineEnding +
'* Returned when the ray hit or miss the scene' + LineEnding +
'*/' + LineEnding +
'struct HitResult {' + LineEnding +
'  // Last tested screen position (-1,-1 if missed)' + LineEnding +
'  vec3 screenPos;' + LineEnding +
'  // How strong the reflection is' + LineEnding +
'  float reflStrength;' + LineEnding +
'};' + LineEnding +
'' + LineEnding +
'const vec2 nearFade = vec2(0.01, 1.0);' + LineEnding +
'const vec2 farFade = vec2(20, 300);' + LineEnding +
'vec2 _SAMPLES[4];' + LineEnding +
'' + LineEnding +
'float getLinearDepth(float depth) {' + LineEnding +
'  return (2.0 * near) / (far + near - depth * (far - near));' + LineEnding +
'}' + LineEnding +
'' + LineEnding +
'vec2 getPixelSize() {' + LineEnding +
'  return vec2(1.0 / float(screen_width), 1.0 / float(screen_height));' + LineEnding +
'}' + LineEnding +
'' + LineEnding +
'vec3 worldToScreen(in vec3 worldPos) {' + LineEnding +
'  vec4 pos = castle_ViewProjectionMatrix * vec4(worldPos, 1.0);' + LineEnding +
'  pos.xyz /= pos.w;' + LineEnding +
'  pos.xyz = pos.xyz * 0.5 + 0.5;' + LineEnding +
'  return pos.xyz;' + LineEnding +
'}' + LineEnding +
'' + LineEnding +
'vec3 screenToWorld(in vec3 screenPos){' + LineEnding +
'  vec4 pos = vec4(screenPos, 1.0) * 2.0 - 1.0;' + LineEnding +
'  pos = castle_ViewProjectionMatrixInverse * pos;' + LineEnding +
'  return pos.xyz /= pos.w;' + LineEnding +
'}' + LineEnding +
'' + LineEnding +
'/**' + LineEnding +
'* Calculate normal from depth buffer' + LineEnding +
'*/' + LineEnding +
'vec3 getNormal(in vec3 worldPos, in vec2 pos) {' + LineEnding +
'  vec2 pixelSize = getPixelSize();' + LineEnding +
'  float depth2 = screenf_01_get_depth(pos + vec2(pixelSize.x, 0.0));' + LineEnding +
'  float depth3 = screenf_01_get_depth(pos + vec2(0.0, pixelSize.y));' + LineEnding +
'  vec3 worldPos2 = screenToWorld(vec3(pos + vec2(pixelSize.x, 0.0), depth2));' + LineEnding +
'  vec3 worldPos3 = screenToWorld(vec3(pos + vec2(0.0, pixelSize.y), depth3));' + LineEnding +
'  vec3 v1 = worldPos3 - worldPos;' + LineEnding +
'  vec3 v2 = worldPos2 - worldPos;' + LineEnding +
'  return normalize(cross(v1, v2));' + LineEnding +
'}' + LineEnding +
'' + LineEnding +
'Ray createRay(in vec2 pos, in float depth) {' + LineEnding +
'  Ray ray;' + LineEnding +
'  ray.screenFrom = vec3(pos, depth);' + LineEnding +
'  ray.worldFrom = screenToWorld(ray.screenFrom);' + LineEnding +
'  ray.surfaceGlossiness = defaultSurfaceGlossiness;' + LineEnding +
'  vec3 worldNormal = getNormal(ray.worldFrom, pos);' + LineEnding +
'  // direction from camera to fragment (in world space)' + LineEnding +
'  vec3 worldDir = normalize(ray.worldFrom - castle_CameraPosition);' + LineEnding +
'  // reflection vector' + LineEnding +
'  ray.worldDir = normalize(reflect(worldDir, normalize(worldNormal)));' + LineEnding +
'  ray.pixelSize = getPixelSize();' + LineEnding +
'  return ray;' + LineEnding +
'}' + LineEnding +
'' + LineEnding +
'/**' + LineEnding +
'* Actual ray marching happens here' + LineEnding +
'*/' + LineEnding +
'HitResult performRayMarching(in Ray ray){' + LineEnding +
'  HitResult result;' + LineEnding +
'  result.screenPos = vec3(-1.0);' + LineEnding +
'' + LineEnding +
'  // Current position of the sample along the ray' + LineEnding +
'  vec3 sampleWorldPos;' + LineEnding +
'  // Same of before, but in screen space' + LineEnding +
'  vec3 sampleScreenPos;' + LineEnding +
'  // Position of the nearest surface at the sample position (in screen space)' + LineEnding +
'  vec3 hitSurfaceScreenPos;' + LineEnding +
'  // Length of the next step' + LineEnding +
'  float stepLength = INITIAL_STEP_LENGTH;' + LineEnding +
'' + LineEnding +
'  float linearSourceDepth = getLinearDepth(ray.screenFrom.z);' + LineEnding +
'' + LineEnding +
'  for (int i = 0; i < RAY_SAMPLES; i++) {' + LineEnding +
'    sampleWorldPos = ray.worldFrom + ray.worldDir * stepLength;' + LineEnding +
'    sampleScreenPos = worldToScreen(sampleWorldPos);' + LineEnding +
'    float depth = screenf_01_get_depth(sampleScreenPos.xy);' + LineEnding +
'' + LineEnding +
'    hitSurfaceScreenPos = vec3(sampleScreenPos.xy, depth);' + LineEnding +
'    vec3 hitSurfaceWorldPos = screenToWorld(hitSurfaceScreenPos);' + LineEnding +
'' + LineEnding +
'    int j = 0;' + LineEnding +
'    do {' + LineEnding +
'      // We need to linearize the depth to have consistent tests for distant samples' + LineEnding +
'      float linearHitSurfaceDepth = getLinearDepth(hitSurfaceScreenPos.z);' + LineEnding +
'      float linearSampleDepth = getLinearDepth(sampleScreenPos.z);' + LineEnding +
'      bool hit =' + LineEnding +
'          linearHitSurfaceDepth > linearSourceDepth // check if the thing we want to reflect is behind the source of the ray' + LineEnding +
'          && abs(linearSampleDepth - linearHitSurfaceDepth) < DEPTH_TEST_BIAS; // check if the ray is (~almost) hitting the surface' + LineEnding +
'      // if first hit (letting the cycle running helds to better performances than breaking it)' + LineEnding +
'      if (hit && result.screenPos.x == -1.0) {' + LineEnding +
'        result.screenPos = sampleScreenPos;' + LineEnding +
'        // Fade distant reflections' + LineEnding +
'        result.reflStrength = distance(hitSurfaceWorldPos, ray.worldFrom);' + LineEnding +
'        result.reflStrength = smoothstep(nearFade.x, nearFade.y, result.reflStrength)' + LineEnding +
'            * (1.0 - smoothstep(farFade.x, farFade.y, result.reflStrength));' + LineEnding +
'      }' + LineEnding +
'      hitSurfaceScreenPos = vec3(sampleScreenPos.xy, screenf_01_get_depth(sampleScreenPos.xy + _SAMPLES[j].xy * ray.pixelSize));' + LineEnding +
'      j++;' + LineEnding +
'    } while (j <= NEARBY_SAMPLES);' + LineEnding +
'    // Compute next step length' + LineEnding +
'    stepLength = length(ray.worldFrom - hitSurfaceWorldPos);' + LineEnding +
'  }' + LineEnding +
'  return result;' + LineEnding +
'}' + LineEnding +
'' + LineEnding +
'void main(void)' + LineEnding +
'{' + LineEnding +
'  vec2 pos = screenf_01_position;' + LineEnding +
'  vec4 baseColor = screenf_01_get_color(pos);' + LineEnding +
'  float depth = screenf_01_get_depth(pos);' + LineEnding +
'  _SAMPLES[0] = vec2(1.0, 0.0);' + LineEnding +
'  _SAMPLES[1] = vec2(-1.0, 0.0);' + LineEnding +
'  _SAMPLES[2] = vec2(0.0, 1.0);' + LineEnding +
'  _SAMPLES[3] = vec2(0.0, -1.0);' + LineEnding +
'' + LineEnding +
'  if (depth != 1.0) { // ignore the sky' + LineEnding +
'    // Build the ray' + LineEnding +
'    Ray ray = createRay(pos, depth);' + LineEnding +
'' + LineEnding +
'    // Perform ray marching' + LineEnding +
'    HitResult result = performRayMarching(ray);' + LineEnding +
'' + LineEnding +
'    // Used to fade reflections near screen edges to remove artifacts' + LineEnding +
'    float d = distance(result.screenPos.xy, vec2(0.5));' + LineEnding +
'    d = pow(1.0 - clamp(d, 0.0, 0.5) * 2.0, 2.0);' + LineEnding +
'' + LineEnding +
'    // Render reflections' + LineEnding +
'    if (result.screenPos.x > -1.0) {' + LineEnding +
'      vec3 color = screenf_01_get_color(result.screenPos.xy).rgb;' + LineEnding +
'      float a = d * ray.surfaceGlossiness * result.reflStrength;' + LineEnding +
'      color = ((1.0 - a) * baseColor.rgb) + (a * color);' + LineEnding +
'      baseColor.rgb = color.rgb;' + LineEnding +
'    }' + LineEnding +
'  }' + LineEnding +
'  gl_FragColor = baseColor;' + LineEnding +
'}' + LineEnding +
''
