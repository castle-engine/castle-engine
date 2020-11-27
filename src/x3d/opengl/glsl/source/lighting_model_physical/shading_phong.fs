/*
  Copyright 2020-2020 Michalis Kamburelis and glTF-Sample-Viewer authors.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------

  Calculate Physical (PBR) lighting model, in Phong shading. */

uniform vec4 castle_MaterialBaseAlpha;
uniform vec3 castle_MaterialEmissive;
uniform float castle_MaterialMetallic;
uniform float castle_MaterialRoughness;

/* ---------------------------------------------------------------------------
   Code below is adapted from glTF-Sample-Viewer ( Apache License 2.0 )
   https://github.com/KhronosGroup/glTF-Sample-Viewer/
   In particular src/shaders/metallic-roughness.frag .
   References there:

   // This fragment shader defines a reference implementation for Physically Based Shading of
   // a microfacet surface material defined by a glTF model.
   //
   // References:
   // [1] Real Shading in Unreal Engine 4
   //     http://blog.selfshadow.com/publications/s2013-shading-course/karis/s2013_pbs_epic_notes_v2.pdf
   // [2] Physically Based Shading at Disney
   //     http://blog.selfshadow.com/publications/s2012-shading-course/burley/s2012_pbs_disney_brdf_notes_v3.pdf
   // [3] README.md - Environment Maps
   //     https://github.com/KhronosGroup/glTF-WebGL-PBR/#environment-maps
   // [4] "An Inexpensive BRDF Model for Physically based Rendering" by Christophe Schlick
   //     https://www.cs.virginia.edu/~jdl/bib/appearance/analytic%20models/schlick94b.pdf

*/

const float M_PI = 3.141592653589793;

// Calculate AngularInfo structure
// pointToLight is assumed to be already normalized.
AngularInfo getAngularInfo(const in vec3 pointToLight, const in vec3 normal, const in vec3 view)
{
  // Standard one-letter names
  vec3 n = normalize(normal);           // Outward direction of surface point
  vec3 v = normalize(view);             // Direction from surface point to view
  vec3 l = /*normalize*/ (pointToLight);     // Direction from surface point to light
  vec3 h = normalize(l + v);            // Direction of the vector between l and v

  float NdotL = clamp(dot(n, l), 0.0, 1.0);
  float NdotV = clamp(dot(n, v), 0.0, 1.0);
  float NdotH = clamp(dot(n, h), 0.0, 1.0);
  float LdotH = clamp(dot(l, h), 0.0, 1.0);
  float VdotH = clamp(dot(v, h), 0.0, 1.0);

  return AngularInfo(
    NdotL,
    NdotV,
    NdotH,
    LdotH,
    VdotH,
    vec3(0, 0, 0)
  );
}

// Lambert lighting
// see https://seblagarde.wordpress.com/2012/01/08/pi-or-not-to-pi-in-game-lighting-equation/
vec3 diffuse(const in MaterialInfo materialInfo)
{
  return materialInfo.diffuseColor / M_PI;
}

// The following equation models the Fresnel reflectance term of the spec equation (aka F())
// Implementation of fresnel from [4], Equation 15
vec3 specularReflection(const in MaterialInfo materialInfo, const in AngularInfo angularInfo)
{
  return materialInfo.reflectance0 + (materialInfo.reflectance90 - materialInfo.reflectance0) * pow(clamp(1.0 - angularInfo.VdotH, 0.0, 1.0), 5.0);
}

// Smith Joint GGX
// Note: Vis = G / (4 * NdotL * NdotV)
// see Eric Heitz. 2014. Understanding the Masking-Shadowing Function in Microfacet-Based BRDFs. Journal of Computer Graphics Techniques, 3
// see Real-Time Rendering. Page 331 to 336.
// see https://google.github.io/filament/Filament.md.html#materialsystem/specularbrdf/geometricshadowing(specularg)
float visibilityOcclusion(const in MaterialInfo materialInfo, const in AngularInfo angularInfo)
{
  float NdotL = angularInfo.NdotL;
  float NdotV = angularInfo.NdotV;
  float alphaRoughnessSq = materialInfo.alphaRoughness * materialInfo.alphaRoughness;

  float GGXV = NdotL * sqrt(NdotV * NdotV * (1.0 - alphaRoughnessSq) + alphaRoughnessSq);
  float GGXL = NdotV * sqrt(NdotL * NdotL * (1.0 - alphaRoughnessSq) + alphaRoughnessSq);

  float GGX = GGXV + GGXL;
  if (GGX > 0.0)
  {
    return 0.5 / GGX;
  }
  return 0.0;
}

// The following equation(s) model the distribution of microfacet normals across the area being drawn (aka D())
// Implementation from "Average Irregularity Representation of a Roughened Surface for Ray Reflection" by T. S. Trowbridge, and K. P. Reitz
// Follows the distribution function recommended in the SIGGRAPH 2013 course notes from EPIC Games [1], Equation 3.
float microfacetDistribution(const in MaterialInfo materialInfo, const in AngularInfo angularInfo)
{
  float alphaRoughnessSq = materialInfo.alphaRoughness * materialInfo.alphaRoughness;
  float f = (angularInfo.NdotH * alphaRoughnessSq - angularInfo.NdotH) * angularInfo.NdotH + 1.0;
  return alphaRoughnessSq / (M_PI * f * f);
}

/* Main function that calculates per-light-source contribution to the final color.

   pointToLight is the direction from current vertex to light source.
   It is assumed to be already normalized.

   normal is normal in eye-space.

   view is direction from current vertex to camera, in eye-space,
   already normalized.
   IOW, "normalize(- vertex_eye)" because camera position is zero in eye-space.
*/
vec3 getPointShade(const in vec3 pointToLight,
  const in MaterialInfo materialInfo,
  const in vec3 normal,
  const in vec3 view)
{
  AngularInfo angularInfo = getAngularInfo(pointToLight, normal, view);

  if (angularInfo.NdotL > 0.0 || angularInfo.NdotV > 0.0)
  {
    // Calculate the shading terms for the microfacet specular shading model
    vec3 F = specularReflection(materialInfo, angularInfo);
    float Vis = visibilityOcclusion(materialInfo, angularInfo);
    float D = microfacetDistribution(materialInfo, angularInfo);

    // Calculation of analytical lighting contribution
    vec3 diffuseContrib = (1.0 - F) * diffuse(materialInfo);
    vec3 specContrib = F * Vis * D;

    // Obtain final intensity as reflectance (BRDF) scaled by the energy of the light (cosine law)
    return angularInfo.NdotL * (diffuseContrib + specContrib);
  }

  return vec3(0.0, 0.0, 0.0);
}

/* Get PhysicalMaterial properies.
   Matches glTF logic for metallic-roughness model.
*/
MaterialInfo getPhysicalMaterialInfo(const in vec4 material_base_alpha)
{
  // Metallic and Roughness material properties are packed together
  // In glTF, these factors can be specified by fixed scalar values
  // or from a metallic-roughness map
  float perceptualRoughness = castle_MaterialRoughness;
  float metallic = castle_MaterialMetallic;
  vec3 diffuseColor = vec3(0.0);
  vec3 specularColor = vec3(0.0);
  vec3 f0 = vec3(0.04);

  /* PLUG: material_metallic_roughness (metallic, perceptualRoughness) */

  diffuseColor = material_base_alpha.rgb * (vec3(1.0) - f0) * (1.0 - metallic);

  specularColor = mix(f0, material_base_alpha.rgb, metallic);

  perceptualRoughness = clamp(perceptualRoughness, 0.0, 1.0);
  metallic = clamp(metallic, 0.0, 1.0);

  // Roughness is authored as perceptual roughness; as is convention,
  // convert to material roughness by squaring the perceptual roughness [2].
  float alphaRoughness = perceptualRoughness * perceptualRoughness;

  // Compute reflectance.
  float reflectance = max(max(specularColor.r, specularColor.g), specularColor.b);

  vec3 specularEnvironmentR0 = specularColor.rgb;
  // Anything less than 2% is physically impossible and is instead considered to be shadowing. Compare to "Real-Time-Rendering" 4th editon on page 325.
  vec3 specularEnvironmentR90 = vec3(clamp(reflectance * 50.0, 0.0, 1.0));

  return MaterialInfo(
    perceptualRoughness,
    specularEnvironmentR0,
    alphaRoughness,
    diffuseColor,
    specularEnvironmentR90,
    specularColor
  );
}

/* ---------------------------------------------------------------------------
   End of code adapted from glTF-Sample-Viewer ( Apache License 2.0 )
*/

void calculate_lighting(out vec4 result, const in vec4 vertex_eye, const in vec3 normal_eye)
{
  /* Calculated color from
     Material.baseColor/transparency (or ColorRGBA node) * base texture.
     Contains complete "base/transparency" information that is independent of light source.
     In case of Gouraud shading it is not multiplied by the base texture
     (because it cannot be, as we're on vertex shader). */
  vec4 material_base_alpha;

  material_base_alpha = castle_apply_color_per_vertex(castle_MaterialBaseAlpha);

  main_texture_apply(material_base_alpha, normal_eye);

  result = vec4(0.0, 0.0, 0.0, material_base_alpha.a);

  vec3 view = normalize(-vec3(vertex_eye));
  MaterialInfo material_info = getPhysicalMaterialInfo(material_base_alpha);
  /* PLUG: add_light (result, vertex_eye, normal_eye, material_info, view) */

  /* PLUG: material_occlusion (result) */

  vec3 emissive = castle_MaterialEmissive;
  /* PLUG: material_emissive (emissive) */
  result.rgb += emissive;

  // TODO: No need for this in PBR?
  // TODO: No need for this in Phong lighting with Phong shading, too?
  // result.rgb = min(result.rgb, 1.0);
}
