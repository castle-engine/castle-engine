// Taken from
// http://www.clockworkcoders.com/oglsl/tutorial5.htm

varying vec3 N;
varying vec3 v;

void main (void)
{
vec3 L = normalize(gl_LightSource[0].position.xyz - v);
vec3 E = normalize(-v); // we are in Eye Coordinates, so EyePos is (0,0,0)
vec3 R = normalize(-reflect(L,N));

//calculate Ambient Term:
vec4 Iamb = gl_FrontLightProduct[0].ambient;

//calculate Diffuse Term:
vec4 Idiff = gl_FrontLightProduct[0].diffuse * max(dot(N,L), 0.0);

// calculate Specular Term:
vec4 Ispec = gl_FrontLightProduct[0].specular
  * pow(max(dot(R,E),0.0),
    /* 0.3 * gl_FrontMaterial.shininess was here, but on Radeon with closed
       ATI drivers on Linux MacBookPro this produced always white
       result... Somehow, "gl_FrontMaterial.shininess / 3.0" which
       calculates almost the same in almost the same way...) works OK. */
       gl_FrontMaterial.shininess / 3.0);

// write Total Color:
gl_FragColor = /* gl_FrontLightModelProduct.SceneColor + */Iamb + Idiff + Ispec;

}
