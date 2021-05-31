{ -*- buffer-read-only: t -*- }
{ DON'T EDIT -- this file was automatically generated from "source/lighting_model_physical/shading_phong.fs" }
'/*' + LineEnding +
'  Copyright 2020-2020 Michalis Kamburelis and glTF-Sample-Viewer authors.' + LineEnding +
'' + LineEnding +
'  This file is part of "Castle Game Engine".' + LineEnding +
'' + LineEnding +
'  "Castle Game Engine" is free software; see the file COPYING.txt,' + LineEnding +
'  included in this distribution, for details about the copyright.' + LineEnding +
'' + LineEnding +
'  "Castle Game Engine" is distributed in the hope that it will be useful,' + LineEnding +
'  but WITHOUT ANY WARRANTY; without even the implied warranty of' + LineEnding +
'  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.' + LineEnding +
'' + LineEnding +
'  ----------------------------------------------------------------------------' + LineEnding +
'' + LineEnding +
'  Calculate Physical (PBR) lighting model, in Phong shading. */' + LineEnding +
'' + LineEnding +
'uniform vec4 castle_MaterialBaseAlpha;' + LineEnding +
'uniform vec3 castle_MaterialEmissive;' + LineEnding +
'uniform float castle_MaterialMetallic;' + LineEnding +
'uniform float castle_MaterialRoughness;' + LineEnding +
'' + LineEnding +
'/* ---------------------------------------------------------------------------' + LineEnding +
'   Code below is adapted from glTF-Sample-Viewer ( Apache License 2.0 )' + LineEnding +
'   https://github.com/KhronosGroup/glTF-Sample-Viewer/' + LineEnding +
'   In particular src/shaders/metallic-roughness.frag .' + LineEnding +
'   References there:' + LineEnding +
'' + LineEnding +
'   // This fragment shader defines a reference implementation for Physically Based Shading of' + LineEnding +
'   // a microfacet surface material defined by a glTF model.' + LineEnding +
'   //' + LineEnding +
'   // References:' + LineEnding +
'   // [1] Real Shading in Unreal Engine 4' + LineEnding +
'   //     http://blog.selfshadow.com/publications/s2013-shading-course/karis/s2013_pbs_epic_notes_v2.pdf' + LineEnding +
'   // [2] Physically Based Shading at Disney' + LineEnding +
'   //     http://blog.selfshadow.com/publications/s2012-shading-course/burley/s2012_pbs_disney_brdf_notes_v3.pdf' + LineEnding +
'   // [3] README.md - Environment Maps' + LineEnding +
'   //     https://github.com/KhronosGroup/glTF-WebGL-PBR/#environment-maps' + LineEnding +
'   // [4] "An Inexpensive BRDF Model for Physically based Rendering" by Christophe Schlick' + LineEnding +
'   //     https://www.cs.virginia.edu/~jdl/bib/appearance/analytic%20models/schlick94b.pdf' + LineEnding +
'' + LineEnding +
'*/' + LineEnding +
'' + LineEnding +
'const float M_PI = 3.141592653589793;' + LineEnding +
'' + LineEnding +
'// Calculate AngularInfo structure' + LineEnding +
'// pointToLight is assumed to be already normalized.' + LineEnding +
'AngularInfo getAngularInfo(const in vec3 pointToLight, const in vec3 normal, const in vec3 view)' + LineEnding +
'{' + LineEnding +
'  // Standard one-letter names' + LineEnding +
'  vec3 n = normalize(normal);           // Outward direction of surface point' + LineEnding +
'  vec3 v = normalize(view);             // Direction from surface point to view' + LineEnding +
'  vec3 l = /*normalize*/ (pointToLight);     // Direction from surface point to light' + LineEnding +
'  vec3 h = normalize(l + v);            // Direction of the vector between l and v' + LineEnding +
'' + LineEnding +
'  float NdotL = clamp(dot(n, l), 0.0, 1.0);' + LineEnding +
'  float NdotV = clamp(dot(n, v), 0.0, 1.0);' + LineEnding +
'  float NdotH = clamp(dot(n, h), 0.0, 1.0);' + LineEnding +
'  float LdotH = clamp(dot(l, h), 0.0, 1.0);' + LineEnding +
'  float VdotH = clamp(dot(v, h), 0.0, 1.0);' + LineEnding +
'' + LineEnding +
'  return AngularInfo(' + LineEnding +
'    NdotL,' + LineEnding +
'    NdotV,' + LineEnding +
'    NdotH,' + LineEnding +
'    LdotH,' + LineEnding +
'    VdotH,' + LineEnding +
'    vec3(0, 0, 0)' + LineEnding +
'  );' + LineEnding +
'}' + LineEnding +
'' + LineEnding +
'// Lambert lighting' + LineEnding +
'// see https://seblagarde.wordpress.com/2012/01/08/pi-or-not-to-pi-in-game-lighting-equation/' + LineEnding +
'vec3 diffuse(const in MaterialInfo materialInfo)' + LineEnding +
'{' + LineEnding +
'  return materialInfo.diffuseColor / M_PI;' + LineEnding +
'}' + LineEnding +
'' + LineEnding +
'// The following equation models the Fresnel reflectance term of the spec equation (aka F())' + LineEnding +
'// Implementation of fresnel from [4], Equation 15' + LineEnding +
'vec3 specularReflection(const in MaterialInfo materialInfo, const in AngularInfo angularInfo)' + LineEnding +
'{' + LineEnding +
'  return materialInfo.reflectance0 + (materialInfo.reflectance90 - materialInfo.reflectance0) * pow(clamp(1.0 - angularInfo.VdotH, 0.0, 1.0), 5.0);' + LineEnding +
'}' + LineEnding +
'' + LineEnding +
'// Smith Joint GGX' + LineEnding +
'// Note: Vis = G / (4 * NdotL * NdotV)' + LineEnding +
'// see Eric Heitz. 2014. Understanding the Masking-Shadowing Function in Microfacet-Based BRDFs. Journal of Computer Graphics Techniques, 3' + LineEnding +
'// see Real-Time Rendering. Page 331 to 336.' + LineEnding +
'// see https://google.github.io/filament/Filament.md.html#materialsystem/specularbrdf/geometricshadowing(specularg)' + LineEnding +
'float visibilityOcclusion(const in MaterialInfo materialInfo, const in AngularInfo angularInfo)' + LineEnding +
'{' + LineEnding +
'  float NdotL = angularInfo.NdotL;' + LineEnding +
'  float NdotV = angularInfo.NdotV;' + LineEnding +
'  float alphaRoughnessSq = materialInfo.alphaRoughness * materialInfo.alphaRoughness;' + LineEnding +
'' + LineEnding +
'  float GGXV = NdotL * sqrt(NdotV * NdotV * (1.0 - alphaRoughnessSq) + alphaRoughnessSq);' + LineEnding +
'  float GGXL = NdotV * sqrt(NdotL * NdotL * (1.0 - alphaRoughnessSq) + alphaRoughnessSq);' + LineEnding +
'' + LineEnding +
'  float GGX = GGXV + GGXL;' + LineEnding +
'  if (GGX > 0.0)' + LineEnding +
'  {' + LineEnding +
'    return 0.5 / GGX;' + LineEnding +
'  }' + LineEnding +
'  return 0.0;' + LineEnding +
'}' + LineEnding +
'' + LineEnding +
'// The following equation(s) model the distribution of microfacet normals across the area being drawn (aka D())' + LineEnding +
'// Implementation from "Average Irregularity Representation of a Roughened Surface for Ray Reflection" by T. S. Trowbridge, and K. P. Reitz' + LineEnding +
'// Follows the distribution function recommended in the SIGGRAPH 2013 course notes from EPIC Games [1], Equation 3.' + LineEnding +
'float microfacetDistribution(const in MaterialInfo materialInfo, const in AngularInfo angularInfo)' + LineEnding +
'{' + LineEnding +
'  float alphaRoughnessSq = materialInfo.alphaRoughness * materialInfo.alphaRoughness;' + LineEnding +
'  float f = (angularInfo.NdotH * alphaRoughnessSq - angularInfo.NdotH) * angularInfo.NdotH + 1.0;' + LineEnding +
'  return alphaRoughnessSq / (M_PI * f * f);' + LineEnding +
'}' + LineEnding +
'' + LineEnding +
'/* Main function that calculates per-light-source contribution to the final color.' + LineEnding +
'' + LineEnding +
'   pointToLight is the direction from current vertex to light source.' + LineEnding +
'   It is assumed to be already normalized.' + LineEnding +
'' + LineEnding +
'   normal is normal in eye-space.' + LineEnding +
'' + LineEnding +
'   view is direction from current vertex to camera, in eye-space,' + LineEnding +
'   already normalized.' + LineEnding +
'   IOW, "normalize(- vertex_eye)" because camera position is zero in eye-space.' + LineEnding +
'*/' + LineEnding +
'vec3 getPointShade(const in vec3 pointToLight,' + LineEnding +
'  const in MaterialInfo materialInfo,' + LineEnding +
'  const in vec3 normal,' + LineEnding +
'  const in vec3 view)' + LineEnding +
'{' + LineEnding +
'  AngularInfo angularInfo = getAngularInfo(pointToLight, normal, view);' + LineEnding +
'' + LineEnding +
'  if (angularInfo.NdotL > 0.0 || angularInfo.NdotV > 0.0)' + LineEnding +
'  {' + LineEnding +
'    // Calculate the shading terms for the microfacet specular shading model' + LineEnding +
'    vec3 F = specularReflection(materialInfo, angularInfo);' + LineEnding +
'    float Vis = visibilityOcclusion(materialInfo, angularInfo);' + LineEnding +
'    float D = microfacetDistribution(materialInfo, angularInfo);' + LineEnding +
'' + LineEnding +
'    // Calculation of analytical lighting contribution' + LineEnding +
'    vec3 diffuseContrib = (1.0 - F) * diffuse(materialInfo);' + LineEnding +
'    vec3 specContrib = F * Vis * D;' + LineEnding +
'' + LineEnding +
'    // Obtain final intensity as reflectance (BRDF) scaled by the energy of the light (cosine law)' + LineEnding +
'    return angularInfo.NdotL * (diffuseContrib + specContrib);' + LineEnding +
'  }' + LineEnding +
'' + LineEnding +
'  return vec3(0.0, 0.0, 0.0);' + LineEnding +
'}' + LineEnding +
'' + LineEnding +
'/* Get PhysicalMaterial properies.' + LineEnding +
'   Matches glTF logic for metallic-roughness model.' + LineEnding +
'*/' + LineEnding +
'MaterialInfo getPhysicalMaterialInfo(const in vec4 material_base_alpha)' + LineEnding +
'{' + LineEnding +
'  // Metallic and Roughness material properties are packed together' + LineEnding +
'  // In glTF, these factors can be specified by fixed scalar values' + LineEnding +
'  // or from a metallic-roughness map' + LineEnding +
'  float perceptualRoughness = castle_MaterialRoughness;' + LineEnding +
'  float metallic = castle_MaterialMetallic;' + LineEnding +
'  vec3 diffuseColor = vec3(0.0);' + LineEnding +
'  vec3 specularColor = vec3(0.0);' + LineEnding +
'  vec3 f0 = vec3(0.04);' + LineEnding +
'' + LineEnding +
'  /* PLUG: material_metallic_roughness (metallic, perceptualRoughness) */' + LineEnding +
'' + LineEnding +
'  diffuseColor = material_base_alpha.rgb * (vec3(1.0) - f0) * (1.0 - metallic);' + LineEnding +
'' + LineEnding +
'  specularColor = mix(f0, material_base_alpha.rgb, metallic);' + LineEnding +
'' + LineEnding +
'  perceptualRoughness = clamp(perceptualRoughness, 0.0, 1.0);' + LineEnding +
'  metallic = clamp(metallic, 0.0, 1.0);' + LineEnding +
'' + LineEnding +
'  // Roughness is authored as perceptual roughness; as is convention,' + LineEnding +
'  // convert to material roughness by squaring the perceptual roughness [2].' + LineEnding +
'  float alphaRoughness = perceptualRoughness * perceptualRoughness;' + LineEnding +
'' + LineEnding +
'  // Compute reflectance.' + LineEnding +
'  float reflectance = max(max(specularColor.r, specularColor.g), specularColor.b);' + LineEnding +
'' + LineEnding +
'  vec3 specularEnvironmentR0 = specularColor.rgb;' + LineEnding +
'  // Anything less than 2% is physically impossible and is instead considered to be shadowing. Compare to "Real-Time-Rendering" 4th editon on page 325.' + LineEnding +
'  vec3 specularEnvironmentR90 = vec3(clamp(reflectance * 50.0, 0.0, 1.0));' + LineEnding +
'' + LineEnding +
'  return MaterialInfo(' + LineEnding +
'    perceptualRoughness,' + LineEnding +
'    specularEnvironmentR0,' + LineEnding +
'    alphaRoughness,' + LineEnding +
'    diffuseColor,' + LineEnding +
'    specularEnvironmentR90,' + LineEnding +
'    specularColor' + LineEnding +
'  );' + LineEnding +
'}' + LineEnding +
'' + LineEnding +
'/* ---------------------------------------------------------------------------' + LineEnding +
'   End of code adapted from glTF-Sample-Viewer ( Apache License 2.0 )' + LineEnding +
'*/' + LineEnding +
'' + LineEnding +
'void calculate_lighting(out vec4 result, const in vec4 vertex_eye, const in vec3 normal_eye)' + LineEnding +
'{' + LineEnding +
'  /* Calculated color from' + LineEnding +
'     Material.baseColor/transparency (or ColorRGBA node) * base texture.' + LineEnding +
'     Contains complete "base/transparency" information that is independent of light source.' + LineEnding +
'     In case of Gouraud shading it is not multiplied by the base texture' + LineEnding +
'     (because it cannot be, as we''re on vertex shader). */' + LineEnding +
'  vec4 material_base_alpha;' + LineEnding +
'' + LineEnding +
'  material_base_alpha = castle_apply_color_per_vertex(castle_MaterialBaseAlpha);' + LineEnding +
'' + LineEnding +
'  main_texture_apply(material_base_alpha, normal_eye);' + LineEnding +
'' + LineEnding +
'  result = vec4(0.0, 0.0, 0.0, material_base_alpha.a);' + LineEnding +
'' + LineEnding +
'  vec3 view = normalize(-vec3(vertex_eye));' + LineEnding +
'  MaterialInfo material_info = getPhysicalMaterialInfo(material_base_alpha);' + LineEnding +
'  /* PLUG: add_light (result, vertex_eye, normal_eye, material_info, view) */' + LineEnding +
'' + LineEnding +
'  /* PLUG: material_occlusion (result) */' + LineEnding +
'' + LineEnding +
'  vec3 emissive = castle_MaterialEmissive;' + LineEnding +
'  /* PLUG: material_emissive (emissive) */' + LineEnding +
'  result.rgb += emissive;' + LineEnding +
'' + LineEnding +
'  // TODO: No need for this in PBR?' + LineEnding +
'  // TODO: No need for this in Phong lighting with Phong shading, too?' + LineEnding +
'  // result.rgb = min(result.rgb, 1.0);' + LineEnding +
'}' + LineEnding +
''
