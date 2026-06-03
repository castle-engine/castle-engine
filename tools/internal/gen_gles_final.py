#!/usr/bin/env python3
"""Generate wrapper procedures for castlegles.pas - final version with all fixes."""
import re, sys

with open('castlegles.pas', 'r') as f:
    original = f.read()

content = original

var_name_re = re.compile(r'^\s{2}(gl\w+)\s*:', re.MULTILINE)
all_gl_names = []
for m in var_name_re.finditer(content):
    name = m.group(1)
    if name not in all_gl_names:
        all_gl_names.append(name)

print(f"Found {len(all_gl_names)} unique GL var names", file=sys.stderr)

def rename_var_decl(text, name):
    text = re.sub(r'(\s{2})(' + re.escape(name) + r')(\s*:(?!=))', r'\1' + name + r'_Proc\3', text)
    text = re.sub(r'(\s{2})(' + re.escape(name) + r')(\s*:=)', r'\1' + name + r'_Proc\3', text)
    text = re.sub(r'(\(\s*(?:\{\$ifndef FPC\}@\{\$endif\}\s*)?)(' + re.escape(name) + r')(\s*\))',
                  r'\1' + name + r'_Proc\3', text)
    return text

for name in all_gl_names:
    content = rename_var_decl(content, name)

def parse_var_decl(text, name):
    pattern = re.compile(r'\b' + re.escape(name) + r'_Proc\s*:\s*(procedure|function)(.*?);\{', re.DOTALL)
    m = pattern.search(text)
    if not m:
        return None, None, None
    kind = m.group(1)
    rest = re.sub(r'\s+', ' ', m.group(2).strip())
    ret_type = None
    if kind == 'function':
        if '(' in rest:
            paren_depth = 0
            paren_end = -1
            for i, c in enumerate(rest):
                if c == '(':
                    paren_depth += 1
                elif c == ')':
                    paren_depth -= 1
                    if paren_depth == 0:
                        paren_end = i
                        break
            if paren_end >= 0:
                after = rest[paren_end+1:].strip()
                if after.startswith(':'):
                    ret_type = re.split(r'[;\{ ]', after[1:].strip())[0].strip()
                params_str = rest[1:paren_end] if paren_end > 0 else ''
            else:
                params_str = rest
        else:
            if rest.startswith(':'):
                ret_type = re.split(r'[;\{ ]', rest[1:].strip())[0].strip()
            params_str = ''
    else:
        if rest.startswith('('):
            paren_depth = 0
            paren_end = -1
            for i, c in enumerate(rest):
                if c == '(':
                    paren_depth += 1
                elif c == ')':
                    paren_depth -= 1
                    if paren_depth == 0:
                        paren_end = i
                        break
            params_str = rest[1:paren_end] if paren_end > 0 else ''
        else:
            params_str = ''
    return kind, params_str.strip(), ret_type

sig_map = {}
for name in all_gl_names:
    kind, params_str, ret_type = parse_var_decl(content, name)
    if kind:
        sig_map[name] = (kind, params_str, ret_type)

print(f"Parsed {len(sig_map)} signatures", file=sys.stderr)
for name in ['glCreateProgram', 'glCompressedTexImage2D', 'glGetError', 'glTransformFeedbackVaryings']:
    k, ps, rt = sig_map.get(name, ('?','?','?'))
    print(f"  {name}: {k}({ps[:70]}) -> {rt}", file=sys.stderr)

def extract_param_names(params_str):
    if not params_str.strip():
        return []
    result = []
    for part in params_str.split(';'):
        part = part.strip()
        if not part:
            continue
        if ':' in part:
            names_part, type_part = part.split(':', 1)
            type_str = type_part.strip()
            for n in names_part.split(','):
                words = n.strip().split()
                n = words[-1] if words else ''
                if n:
                    result.append((n, type_str))
        else:
            result.append((part.strip(), ''))
    return result

def build_call_args(params_str):
    return ', '.join(p for p, _ in extract_param_names(params_str))

def is_pointer_type(ptype_lower):
    if ptype_lower in ('pointer', 'ppointer', 'pansichar', 'ppansichar'):
        return True
    if ptype_lower.startswith('pp') and len(ptype_lower) > 2:
        return True
    if ptype_lower.startswith('p') and len(ptype_lower) > 1 and ptype_lower[1] not in ('r', 't'):
        return True
    return False

ENUM_ENTRIES = [
    ('GL_TEXTURE_2D','$0DE1'),('GL_TEXTURE_3D','$806F'),('GL_TEXTURE_2D_ARRAY','$8C1A'),
    ('GL_TEXTURE_CUBE_MAP','$8513'),('GL_ARRAY_BUFFER','$8892'),('GL_ELEMENT_ARRAY_BUFFER','$8893'),
    ('GL_FRAMEBUFFER','$8D40'),('GL_RENDERBUFFER','$8D41'),('GL_BLEND','$0BE2'),
    ('GL_DEPTH_TEST','$0B71'),('GL_CULL_FACE','$0B44'),('GL_STENCIL_TEST','$0B90'),
    ('GL_SCISSOR_TEST','$0C11'),('GL_VERTEX_SHADER','$8B31'),('GL_FRAGMENT_SHADER','$8B30'),
    ('GL_LINE_STRIP','$0003'),('GL_LINE_LOOP','$0002'),('GL_TRIANGLES','$0004'),
    ('GL_TRIANGLE_STRIP','$0005'),('GL_TRIANGLE_FAN','$0006'),
    ('GL_UNSIGNED_BYTE','$1401'),('GL_UNSIGNED_SHORT','$1403'),('GL_UNSIGNED_INT','$1405'),
    ('GL_FLOAT','$1406'),('GL_INT','$1404'),('GL_HALF_FLOAT_OES','$8D61'),
    ('GL_NEAREST','$2600'),('GL_LINEAR','$2601'),
    ('GL_NEAREST_MIPMAP_LINEAR','$2702'),('GL_LINEAR_MIPMAP_LINEAR','$2703'),
    ('GL_TEXTURE_MIN_FILTER','$2801'),('GL_TEXTURE_MAG_FILTER','$2800'),
    ('GL_TEXTURE_WRAP_S','$2802'),('GL_TEXTURE_WRAP_T','$2803'),('GL_TEXTURE_WRAP_R_OES','$8072'),
    ('GL_REPEAT','$2901'),('GL_CLAMP_TO_EDGE','$812F'),('GL_MIRRORED_REPEAT','$8370'),
    ('GL_RGBA','$1908'),('GL_RGB','$1907'),('GL_DEPTH_COMPONENT','$1902'),
    ('GL_DEPTH_STENCIL','$84F9'),('GL_RED','$1903'),
    ('GL_FUNC_ADD','$8006'),('GL_FUNC_SUBTRACT','$800A'),('GL_FUNC_REVERSE_SUBTRACT','$800B'),
    # GL_ZERO=0 and GL_ONE=1 share values with GL_POINTS=0 and GL_LINES=1 - but we list these names
    ('GL_ZERO','$0000'),('GL_ONE','$0001'),
    ('GL_SRC_ALPHA','$0302'),('GL_ONE_MINUS_SRC_ALPHA','$0303'),
    ('GL_SRC_COLOR','$0300'),('GL_ONE_MINUS_SRC_COLOR','$0301'),
    ('GL_DST_ALPHA','$0304'),('GL_ONE_MINUS_DST_ALPHA','$0305'),
    ('GL_DST_COLOR','$0306'),('GL_ONE_MINUS_DST_COLOR','$0307'),
    ('GL_INVALID_ENUM','$0500'),('GL_INVALID_VALUE','$0501'),
    ('GL_INVALID_OPERATION','$0502'),('GL_OUT_OF_MEMORY','$0505'),
    ('GL_INVALID_FRAMEBUFFER_OPERATION','$0506'),
    ('GL_DEPTH_BUFFER_BIT','$00000100'),('GL_STENCIL_BUFFER_BIT','$00000400'),
    ('GL_COLOR_BUFFER_BIT','$00004000'),
    ('GL_FRONT','$0404'),('GL_BACK','$0405'),('GL_FRONT_AND_BACK','$0408'),
    ('GL_STATIC_DRAW','$88E4'),('GL_DYNAMIC_DRAW','$88E8'),('GL_STREAM_DRAW','$88E0'),
    ('GL_RGBA8','$8058'),('GL_RGB8','$8051'),('GL_SRGB8_ALPHA8','$8C43'),
    ('GL_RGBA16F','$881A'),('GL_RGB16F','$881B'),
    ('GL_DEPTH_COMPONENT24','$81A6'),('GL_DEPTH24_STENCIL8','$88F0'),
]

if_lines = []
for i, (ne, vh) in enumerate(ENUM_ENTRIES):
    prefix = '  if' if i == 0 else '  else if'
    if_lines.append(f"{prefix} E = {ne} then Result := '{ne}'")
if_lines.append('  else Result := IntToStr(Integer(E));')
if_else_body = '\n'.join(if_lines)

ENUM_HELPER_IFACE = 'function GLEnumName(const E: GLenum): String;'
ENUM_HELPER_IMPL = (
    '{ Helper to convert common OpenGL ES enum values to strings for logging. }\n'
    'function GLEnumName(const E: GLenum): String;\n'
    'begin\n' + if_else_body + '\nend;\n\n'
    '{ Check GL errors after a call and log any error. }\n'
    'procedure GLESCheckError(const ProcName: String);\n'
    'var\n'
    '  Err: GLenum;\n'
    'begin\n'
    '  if not Assigned(glGetError_Proc) then\n'
    '    Exit;\n'
    '  Err := glGetError_Proc();\n'
    '  if Err <> GL_NO_ERROR then\n'
    "    WritelnWarning('OpenGLES', ProcName + ' error: ' + GLEnumName(Err));\n"
    'end;\n'
)

iface_decls = [ENUM_HELPER_IFACE]
for name in all_gl_names:
    if name not in sig_map:
        continue
    kind, params_str, ret_type = sig_map[name]
    if kind == 'procedure':
        if params_str:
            iface_decls.append(f'procedure {name}({params_str});')
        else:
            iface_decls.append(f'procedure {name};')
    else:
        if params_str:
            iface_decls.append(f'function {name}({params_str}): {ret_type};')
        else:
            iface_decls.append(f'function {name}: {ret_type};')

impl_procs = [ENUM_HELPER_IMPL]

for name in all_gl_names:
    if name not in sig_map:
        continue
    kind, params_str, ret_type = sig_map[name]
    call_args = build_call_args(params_str)
    params_list = extract_param_names(params_str)

    log_parts = []
    for pname, ptype in params_list[:4]:
        pt = ptype.lower().strip()
        if is_pointer_type(pt):
            continue
        if 'glenum' in pt or 'tglenum' in pt:
            log_parts.append(f"' {pname}=' + GLEnumName({pname})")
        elif pt in ('gluint', 'tgluint', 'glbitfield', 'tglbitfield'):
            log_parts.append(f"' {pname}=' + IntToStr(Integer({pname}))")
        elif pt in ('gluint64', 'tgluint64'):
            log_parts.append(f"' {pname}=' + IntToStr(Int64({pname}))")
        elif pt in ('glint64', 'tglint64'):
            log_parts.append(f"' {pname}=' + IntToStr({pname})")
        elif pt in ('glint','tglint','glsizei','tglsizei','glsizeiptr','tglsizeiptr',
                    'glintptr','tglintptr','cint32','cint'):
            log_parts.append(f"' {pname}=' + IntToStr({pname})")

    log_expr = ("'" + name + "'" + ' + ' + ' + '.join(log_parts)) if log_parts else "'" + name + "'"

    lines_impl = []
    if kind == 'procedure':
        lines_impl.append(f'procedure {name}({params_str});' if params_str else f'procedure {name};')
        lines_impl.append('begin')
        lines_impl.append(f"  WritelnLog('OpenGLES', {log_expr});")
        lines_impl.append(f'  {name}_Proc({call_args});' if call_args else f'  {name}_Proc;')
        if name != 'glGetError':
            lines_impl.append(f"  GLESCheckError('{name}');")
        lines_impl.append('end;')
    else:
        lines_impl.append(f'function {name}({params_str}): {ret_type};' if params_str else f'function {name}: {ret_type};')
        lines_impl.append('begin')
        lines_impl.append(f"  WritelnLog('OpenGLES', {log_expr});")
        lines_impl.append(f'  Result := {name}_Proc({call_args});' if call_args else f'  Result := {name}_Proc();')
        if name != 'glGetError':
            lines_impl.append(f"  GLESCheckError('{name}');")
        lines_impl.append('end;')

    impl_procs.append('\n'.join(lines_impl))

print(f"Generated {len(impl_procs)-1} wrapper implementations", file=sys.stderr)

iface_marker = 'procedure GLESInitialization;'
iface_pos = content.find(iface_marker)
iface_block = '\n{ Wrapper procedures that log calls and check errors. }\n' + '\n'.join(iface_decls) + '\n\n'
content = content[:iface_pos] + iface_block + content[iface_pos:]

uses_marker = 'uses CastleInternalEgl;'
uses_pos = content.find(uses_marker)
content = content[:uses_pos] + 'uses CastleInternalEgl, CastleLog;' + content[uses_pos + len(uses_marker):]

impl_keyword = '\nimplementation\n'
impl_pos = content.find(impl_keyword)
init_marker = '\ninitialization\n'
init_pos = content.find(init_marker, impl_pos)
if init_pos < 0:
    init_pos = content.rfind('\nend.')

impl_block = '\n\n{ Wrapper implementations }\n\n' + '\n\n'.join(impl_procs) + '\n'
content = content[:init_pos] + impl_block + content[init_pos:]

with open('castlegles.pas', 'w') as f:
    f.write(content)

print("Done!", file=sys.stderr)
