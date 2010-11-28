{ A couple of GLX unit extensions.

  TODO: Will be submitted to be incorporated in FPC GLX's unit.

  @exclude }
unit KambiGLX;

interface

const
  { GLX 1.3 and later: }
  GLX_CONFIG_CAVEAT               = $20;
  GLX_DONT_CARE                   = $FFFFFFFF;
  GLX_X_VISUAL_TYPE               = $22;
  GLX_TRANSPARENT_TYPE            = $23;
  GLX_TRANSPARENT_INDEX_VALUE     = $24;
  GLX_TRANSPARENT_RED_VALUE       = $25;
  GLX_TRANSPARENT_GREEN_VALUE     = $26;
  GLX_TRANSPARENT_BLUE_VALUE      = $27;
  GLX_TRANSPARENT_ALPHA_VALUE     = $28;
  GLX_WINDOW_BIT                  = $00000001;
  GLX_PIXMAP_BIT                  = $00000002;
  GLX_PBUFFER_BIT                 = $00000004;
  GLX_AUX_BUFFERS_BIT             = $00000010;
  GLX_FRONT_LEFT_BUFFER_BIT       = $00000001;
  GLX_FRONT_RIGHT_BUFFER_BIT      = $00000002;
  GLX_BACK_LEFT_BUFFER_BIT        = $00000004;
  GLX_BACK_RIGHT_BUFFER_BIT       = $00000008;
  GLX_DEPTH_BUFFER_BIT            = $00000020;
  GLX_STENCIL_BUFFER_BIT          = $00000040;
  GLX_ACCUM_BUFFER_BIT            = $00000080;
  GLX_NONE                        = $8000;
  GLX_SLOW_CONFIG                 = $8001;
  GLX_TRUE_COLOR                  = $8002;
  GLX_DIRECT_COLOR                = $8003;
  GLX_PSEUDO_COLOR                = $8004;
  GLX_STATIC_COLOR                = $8005;
  GLX_GRAY_SCALE                  = $8006;
  GLX_STATIC_GRAY                 = $8007;
  GLX_TRANSPARENT_RGB             = $8008;
  GLX_TRANSPARENT_INDEX           = $8009;
  GLX_VISUAL_ID                   = $800B;
  GLX_SCREEN                      = $800C;
  GLX_NON_CONFORMANT_CONFIG       = $800D;
  GLX_DRAWABLE_TYPE               = $8010;
  GLX_RENDER_TYPE                 = $8011;
  GLX_X_RENDERABLE                = $8012;
  GLX_FBCONFIG_ID                 = $8013;
  GLX_RGBA_TYPE                   = $8014;
  GLX_COLOR_INDEX_TYPE            = $8015;
  GLX_MAX_PBUFFER_WIDTH           = $8016;
  GLX_MAX_PBUFFER_HEIGHT          = $8017;
  GLX_MAX_PBUFFER_PIXELS          = $8018;
  GLX_PRESERVED_CONTENTS          = $801B;
  GLX_LARGEST_PBUFFER             = $801C;
  GLX_WIDTH                       = $801D;
  GLX_HEIGHT                      = $801E;
  GLX_EVENT_MASK                  = $801F;
  GLX_DAMAGED                     = $8020;
  GLX_SAVED                       = $8021;
  GLX_WINDOW                      = $8022;
  GLX_PBUFFER                     = $8023;
  GLX_PBUFFER_HEIGHT              = $8040;
  GLX_PBUFFER_WIDTH               = $8041;
  GLX_RGBA_BIT                    = $00000001;
  GLX_COLOR_INDEX_BIT             = $00000002;
  GLX_PBUFFER_CLOBBER_MASK        = $08000000;

  { GLX 1.4 and later: }
  GLX_SAMPLE_BUFFERS              = $186a0 { 100000 in decimal };
  GLX_SAMPLES                     = $186a1 { 100001 in decimal };

  { GLX_ARB_multisample extension: }
  GLX_SAMPLE_BUFFERS_ARB = GLX_SAMPLE_BUFFERS;
  GLX_SAMPLES_ARB        = GLX_SAMPLES;

implementation

end.