uniform sampler2D grainTexture;
uniform float time;

/* Larger values -> grain is more noticeable, 0 = no grain is visible */
const float strength = 0.10;
/* Larger values -> grain is "smaller dots" */
const float density = 5.0;

void main (void)
{
  gl_FragColor = screenf_get_color(screenf_position());

  /* Squeeze grainCoord into [0..1,0..1]. */
  float screen_size = float(max(screen_width, screen_height));
  vec2 grainCoord = density *
    vec2(screen_position()) /
    vec2(screen_size, screen_size);

  /* Grain changes constanly, to make the noisy effect.
     We could use a couple of textures (MovieTexture with noises),
     but we can also instead add semi-random shift to grainCoord
     to effectively get a different noise.

     I experimented with changing noise smoothly,
     e.g. in 0.25 of a second one noise changes into other.
     But it looks bad. Simply using random independent noise slice
     at each frame looks best.  */
  grainCoord += vec2(time * 789.0, time * 456.0);

  float grain = strength * (
    /* Take a couple of noise layers.
       It's best to use somewhat distorted weights (not just 2, 4...)
       for nice irregular but smooth effect. */
    (texture2D(grainTexture, grainCoord / 1.1).r +
     texture2D(grainTexture, grainCoord / 3.3).r)
     - 1.0 /* -0.5 for each noise layer */
  )
  // on average, to not make the screen brighter
  - strength / 2.0;

  gl_FragColor.rgb += vec3(grain);
}
