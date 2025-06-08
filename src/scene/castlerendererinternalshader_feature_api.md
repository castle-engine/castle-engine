# CastleRendererInternalShader "features"

Include files `castlerendererinternalshader_xxx.inc` define shader code generation "features".

They are either records or class instances (sometimes lists of records or instances).

They follow similar naming conventions, and define routines like below:

NOTE: This is just an example, not all features must have all these methods. We deliberately let some details differ for every feature, and some have special needs.

```delphi
TMyShaderFeature = record / class
  // Define ome state, e.g. Enabled: Boolean;

  { Clear the state, making the given feature not activated.
    This is called from TShader.Clear, and should be super-fast,
    as it is called for every shader every frame. }
  procedure Clear;

  { Update Hash, if/how given feature is enabled.
    Call things like Hash.AddInteger, use unique prime numbers
    ( https://en.wikipedia.org/wiki/List_of_prime_numbers ) to generate
    a hash.

    How do we know if a feature is enabled?
    This depends on the feature.
    Some have just "Enabled: Boolean" that is set by other things.
    Some are lists, and "empty list" just means it is unused. }
  procedure PrepareHash(var Hash: TShaderCodeHash);

  { Called only when we actually link a program.
    If given feature is enabled, it should enhance shader code
    e.g. by calling "Shader.Plug(...)".

    Note: It is too late now to change Hash.
    We already made a decision to link this shader. }
  procedure GenerateCode(const Shader: TShader);

  { Setup uniforms once, after linking the shader. }
  procedure SetUniformsOnce(const AProgram: TX3DShaderProgram);

  { Setup "dynamic" uniforms, that have to be updated in every frame
    when we use the shader.

    Note: This cannot depend that GenerateCode has been performed.
    We may reuse a shader created already in the previous frame,
    in which case TShader.LinkProgram and GenerateCode were not called
    on this TShader instance (since we reuse TShader instances for shadow
    generation). }
  procedure SetDynamicUniforms(const AProgram: TX3DShaderProgram;
    const RenderingCamera: TRenderingCamera);
end;
```
