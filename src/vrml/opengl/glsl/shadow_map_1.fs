#define PCF16

uniform sampler2D texture0;
uniform sampler2DShadow shadowMap;

void main(void)
{
  float shadow;

#ifdef PCF16
  float offset = gl_TexCoord[1].w / 512.0;

  shadow =
    (
      shadow2DProj(shadowMap, gl_TexCoord[1] + vec4(-offset * 1.5, -offset * 1.5, 0.0, 0.0)).r +
      shadow2DProj(shadowMap, gl_TexCoord[1] + vec4(-offset * 1.5, -offset * 0.5, 0.0, 0.0)).r +
      shadow2DProj(shadowMap, gl_TexCoord[1] + vec4(-offset * 0.5, -offset * 0.5, 0.0, 0.0)).r +
      shadow2DProj(shadowMap, gl_TexCoord[1] + vec4(-offset * 0.5, -offset * 1.5, 0.0, 0.0)).r +

      shadow2DProj(shadowMap, gl_TexCoord[1] + vec4(-offset * 1.5,  offset * 1.5, 0.0, 0.0)).r +
      shadow2DProj(shadowMap, gl_TexCoord[1] + vec4(-offset * 1.5,  offset * 0.5, 0.0, 0.0)).r +
      shadow2DProj(shadowMap, gl_TexCoord[1] + vec4(-offset * 0.5,  offset * 0.5, 0.0, 0.0)).r +
      shadow2DProj(shadowMap, gl_TexCoord[1] + vec4(-offset * 0.5,  offset * 1.5, 0.0, 0.0)).r +

      shadow2DProj(shadowMap, gl_TexCoord[1] + vec4( offset * 1.5,  offset * 1.5, 0.0, 0.0)).r +
      shadow2DProj(shadowMap, gl_TexCoord[1] + vec4( offset * 1.5,  offset * 0.5, 0.0, 0.0)).r +
      shadow2DProj(shadowMap, gl_TexCoord[1] + vec4( offset * 0.5,  offset * 0.5, 0.0, 0.0)).r +
      shadow2DProj(shadowMap, gl_TexCoord[1] + vec4( offset * 0.5,  offset * 1.5, 0.0, 0.0)).r +

      shadow2DProj(shadowMap, gl_TexCoord[1] + vec4( offset * 1.5, -offset * 1.5, 0.0, 0.0)).r +
      shadow2DProj(shadowMap, gl_TexCoord[1] + vec4( offset * 1.5, -offset * 0.5, 0.0, 0.0)).r +
      shadow2DProj(shadowMap, gl_TexCoord[1] + vec4( offset * 0.5, -offset * 0.5, 0.0, 0.0)).r +
      shadow2DProj(shadowMap, gl_TexCoord[1] + vec4( offset * 0.5, -offset * 1.5, 0.0, 0.0)).r
    )
    / 16.0;
#else
  shadow = shadow2DProj(shadowMap, gl_TexCoord[1]).r;
#endif

  /* When shadowMapCoord is outside (0, 0) - (1, 1) square,
     it's always in the shadow. Otherwise shadows would be stretched
     over whole scene, due to clamping. */
  vec2 shadowMapCoord = gl_TexCoord[1].st / gl_TexCoord[1].q;
  if (shadowMapCoord.s < 0.0 || shadowMapCoord.s > 1.0 ||
      shadowMapCoord.t < 0.0 || shadowMapCoord.t > 1.0)
    shadow = 0.0;

  gl_FragColor = texture2D(texture0, gl_TexCoord[0].st) * gl_Color;
  gl_FragColor.rgb = mix(gl_FragColor.rgb / 2.0, gl_FragColor.rgb, shadow);
}
