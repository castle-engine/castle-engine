/* GLSL fragment shader to do bump mapping.

   This is converted to glsl_bump_mapping.fs.inc, and is them compiled
   in program's binary.
   When you change this file, rerun `make' and then recompile Pascal sources.
*/

uniform sampler2D tex_normal_map;
uniform sampler2D tex_original;
uniform vec4 light_ambient_color;
uniform vec4 light_diffuse_color;

varying vec3 light_dir_tangent;

void main(void)
{
  /* gl_FragColor = all ambient lighting. */
  gl_FragColor =
    gl_FrontLightModelProduct.sceneColor +
    light_ambient_color * gl_FrontMaterial.ambient;

  /* Both light_dir and normal are in tangent space. */
  vec3 light_dir = normalize(light_dir_tangent);

  /* I read normal from texture, this is the very idea of bump mapping.
     Unpack normals, they are in texture in [0..1] range and I want in [-1..1]. */
  vec3 normal = vec3(
    texture2D(tex_normal_map, gl_TexCoord[0].st)) * 2.0 - vec3(1, 1, 1);

  /* I want to do two-sided lighting, so I want to have normal
     pointing from this side of the face that is currently displayed.
     Current normal is for front face, so negate it if backfacing.
     Since this is in tangent space, "negate" means only negate it's z
     component.

     On fglrx (in ii 324 crypto, "Radeon X300/X550/X1050 Series")
     using "gl_FrontFacing" (with and without if) causes
     (in ProgramLogInfo)

        Link successful.
	The GLSL vertex shader will run in software due to the
	GLSL fragment shader running in software.
	The GLSL fragment shader will run in software -
	unsupported language element used.

     On NVidia on kocury (home) this also fails.
     From "Release Notes for NVIDIA OpenGL Shading Language Support":
     "gl_FrontFacing Is Not Available to Fragment Shaders".
     http://http.download.nvidia.com/developer/GLSL/GLSL%20Release%20Notes%20for%20Release%2060.pdf
     (there's a workaround, TODO: use this in the future ?)

     On Mac Book Pro (chantal) this works Ok.

     I guess that I can't use this, have to live with 1-sided lighting
     for bump mapping now.

     Version with "if" :
       if (!gl_FrontFacing)
	 normal.z = -normal.z;

     Alt version of this, not using "if" just in case for future:
       normal.z -= normal.z * 2.0 * (1.0 - float(gl_FrontFacing)); */

  /* gl_FragColor += diffuse lighting */
  gl_FragColor += light_diffuse_color * gl_FrontMaterial.diffuse *
      max(dot(normal, light_dir), 0.0);

  gl_FragColor *= texture2D(tex_original, gl_TexCoord[0].st);
}
