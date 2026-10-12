#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""Generate castle_test_cff.otf: a small OpenType font with CFF outlines,
to test Castle Game Engine CFF (Type 2 charstrings) reading
(CastleInternalOpenTypeFont unit, TTestCastleInternalOpenTypeFont).

Glyphs are hand-written Type 2 charstrings, each exercising some group
of charstring operators and number encodings,
more than a typical real font would use in a few glyphs:

- A: rmoveto (with width), hlineto, vlineto (both parities), contour with a hole.
- B: rrcurveto (multiple sets), hhcurveto, vvcurveto (both parities).
- C: hvcurveto, vhcurveto (with and without the final "df" argument).
- D: rlineto (multiple pairs), rcurveline, rlinecurve.
- E: flex, hflex, hflex1, flex1 (both flex1 variants).
- F: callsubr, callgsubr (nested), return, endchar inside a subroutine.
- G: hstemhm, vstemhm, hintmask (with implicit vstem), cntrmask,
     hmoveto and vmoveto (with width).
- H: number encodings: 1-byte, 2-byte (247..254), shortint (28), fixed (255).
- I: arithmetic operators: add, sub, mul, div, neg, abs, dup, exch, index,
     roll, drop, put, get, ifelse, eq, and, or, not, sqrt.
- space: empty glyph (only endchar with width).

The font data is public domain (CC0), like this script.

Usage: python3 castle_test_cff_generate.py (writes castle_test_cff.otf
next to this script). Requires only the Python standard library.

-----------------------------------------------------------------------------

Disclosure: This file is largely Claude-generated.
It was reviewed by Michalis, but this is still a
(unusually, for Castle Game Engine) large automatically generated file.
"""

import os
import struct

UNITS_PER_EM = 1000
NOMINAL_WIDTH = 600
DEFAULT_WIDTH = 500


# ---------------------------------------------------------------------------
# Type 2 charstring encoding

def num(v):
    """Encode a number operand."""
    if isinstance(v, float) and not v.is_integer():
        return bytes([255]) + struct.pack('>i', int(round(v * 65536)))
    v = int(v)
    if -107 <= v <= 107:
        return bytes([v + 139])
    if 108 <= v <= 1131:
        v -= 108
        return bytes([(v >> 8) + 247, v & 0xFF])
    if -1131 <= v <= -108:
        v = -v - 108
        return bytes([(v >> 8) + 251, v & 0xFF])
    return bytes([28]) + struct.pack('>h', v)


def shortint(v):
    """Encode integer as shortint (operator 28), even if it fits smaller encoding."""
    return bytes([28]) + struct.pack('>h', v)


def fixed(v):
    """Encode number as 16.16 fixed (operator 255)."""
    return bytes([255]) + struct.pack('>i', int(round(v * 65536)))


OPS = {
    'hstem': [1], 'vstem': [3], 'vmoveto': [4], 'rlineto': [5],
    'hlineto': [6], 'vlineto': [7], 'rrcurveto': [8], 'callsubr': [10],
    'return': [11], 'endchar': [14], 'hstemhm': [18], 'hintmask': [19],
    'cntrmask': [20], 'rmoveto': [21], 'hmoveto': [22], 'vstemhm': [23],
    'rcurveline': [24], 'rlinecurve': [25], 'vvcurveto': [26],
    'hhcurveto': [27], 'callgsubr': [29], 'vhcurveto': [30], 'hvcurveto': [31],
    'and': [12, 3], 'or': [12, 4], 'not': [12, 5], 'abs': [12, 9],
    'add': [12, 10], 'sub': [12, 11], 'div': [12, 12], 'neg': [12, 14],
    'eq': [12, 15], 'drop': [12, 18], 'put': [12, 20], 'get': [12, 21],
    'ifelse': [12, 22], 'mul': [12, 24], 'sqrt': [12, 26], 'dup': [12, 27],
    'exch': [12, 28], 'index': [12, 29], 'roll': [12, 30],
    'hflex': [12, 34], 'flex': [12, 35], 'hflex1': [12, 36], 'flex1': [12, 37],
}


def cs(*items):
    """Build charstring from items: numbers, operator names, or raw bytes."""
    out = b''
    for it in items:
        if isinstance(it, bytes):
            out += it
        elif isinstance(it, str):
            out += bytes(OPS[it])
        else:
            out += num(it)
    return out


SUBR_BIAS = 107  # for less than 1240 subroutines


# Local subroutines
LOCAL_SUBRS = [
    # 0: line to the right, then calls global subroutine 1 (nested call)
    cs(200, 0, 'rlineto', 1 - SUBR_BIAS, 'callgsubr', 'return'),
    # 1: finishes a glyph (endchar inside a subroutine)
    cs(-200, 'hlineto', 'endchar'),
]

# Global subroutines
GLOBAL_SUBRS = [
    # 0: moveto used by glyph F
    cs(100, 100, 'rmoveto', 'return'),
    # 1: line up
    cs(0, 200, 'rlineto', 'return'),
]


GLYPHS = [
    # (name, unicode, advance width, charstring)
    ('.notdef', None, DEFAULT_WIDTH, cs('endchar')),

    ('space', 0x20, 250, cs(250 - NOMINAL_WIDTH, 'endchar')),

    # A: lines, with a hole
    ('A', 0x41, 650, cs(
        650 - NOMINAL_WIDTH, 50, 50, 'rmoveto',     # width + rmoveto
        500, 600, -500, 'hlineto',                  # odd count: h v h
        100, -500, 'rmoveto',
        400, 300, -400, 'vlineto',                  # odd count: v h v (hole)
        300, 50, 'rmoveto',
        100, 50, 'hlineto',                         # even count: h v
        -50, -100, 'vlineto',                       # even count: v h
        'endchar')),

    # B: curves
    ('B', 0x42, DEFAULT_WIDTH, cs(
        100, 0, 'rmoveto',
        150, 0, 100, 50, 0, 150,
        0, 150, -100, 100, -150, 0, 'rrcurveto',    # two sets
        -20, -40, -10, -30, 0, 'hhcurveto',         # odd: dy1 first
        -30, -10, 10, -20, 'hhcurveto',             # even
        10, -100, 20, -50, -100, 'vvcurveto',       # odd: dx1 first
        -50, 5, 20, -50, -40, 10, -20, -20, 'vvcurveto',  # even, two sets
        'endchar')),

    # C: alternating curves
    ('C', 0x43, 700, cs(
        700 - NOMINAL_WIDTH, 350, 0, 'rmoveto',
        150, 100, 150, 200, 'hvcurveto',            # h then v, 4 args
        150, -50, 150, -250, 30, 'vhcurveto',       # 5 args, final df
        -50, 0, 'rmoveto',
        -100, -50, -50, -100, -100, -50, -50, -100, 'hvcurveto',  # 8 args
        -100, 50, 50, 100, 100, 50, 50, 100, -20, 'vhcurveto',  # 9 args, final df
        'endchar')),

    # D: lines and curves mixed
    ('D', 0x44, DEFAULT_WIDTH, cs(
        50, 0, 'rmoveto',
        300, 0, 100, 100, 0, 200, 'rlineto',        # three pairs
        0, 100, -100, 100, -150, 50, -50, -50, 'rcurveline',  # curve + line
        -50, -100, 0, -100, 20, -80, 30, -70, 'rlinecurve',   # line + curve
        'endchar')),

    # E: flex variants
    ('E', 0x45, 800, cs(
        800 - NOMINAL_WIDTH, 50, 50, 'rmoveto',
        100, 20, 100, 30, 100, 0, 100, -30, 100, -20, 100, 0, 50, 'flex',
        20, 30, 'rlineto',
        -100, -50, 50, -100, -100, -50, -100, 'hflex',
        0, 300, 'rlineto',
        100, 10, 100, 20, 100, 100, -20, 100, 100, 'hflex1',
        50, 50, 'rlineto',
        -100, 5, -100, 10, -100, 0, -100, -10, -100, -5, 20, 'flex1',  # |dx| > |dy|
        -30, 0, 'rlineto',
        -10, -50, -20, -50, 0, -50, 10, -50, 20, -50, -40, 'flex1',    # |dy| > |dx|
        'endchar')),

    # F: subroutines, drawing a rectangle
    ('F', 0x46, DEFAULT_WIDTH, cs(
        0 - SUBR_BIAS, 'callgsubr',                 # global 0: rmoveto
        0 - SUBR_BIAS, 'callsubr',                  # local 0: line right, calls global 1 (line up)
        1 - SUBR_BIAS, 'callsubr')),                # local 1: line left + endchar

    # G: hints (ignored when rendering, but must be parsed correctly)
    ('G', 0x47, 550, cs(
        550 - NOMINAL_WIDTH, 0, 50, 600, 50, 'hstemhm',  # odd count: width first
        50, 50, 350, 50, 'vstemhm',
        100, 50, 'hintmask', bytes([0xF0]),          # implicit vstem (1 stem), 5 stems -> 1 byte mask
        'cntrmask', bytes([0xA0]),
        100, 'hmoveto',
        300, 'hlineto',
        600, 'vlineto',
        -300, 'hlineto',
        200, 'hmoveto',                             # hmoveto after the first contour
        -100, 'vmoveto',
        100, 100, -100, 'hlineto',
        'endchar')),

    # H: number encodings
    ('H', 0x48, DEFAULT_WIDTH, cs(
        shortint(50), shortint(20), 'rmoveto',
        fixed(300.5), 0, 'rlineto',
        0, 1000 - 400, 'rlineto',                   # 2-byte encoding (247..250)
        -300, fixed(-0.25), 'rlineto',              # 2-byte negative (251..254)
        shortint(-2), -100, 'rlineto',
        'endchar')),

    # I: arithmetic. Draws (100, 50) -> (400, 50) -> (400, 450) -> (100, 450)
    #    -> (100, 250) -> (120, 260) -> (100, 250).
    ('I', 0x49, DEFAULT_WIDTH, cs(
        50, 2, 'mul', 150, 100, 'sub', 'rmoveto',     # rmoveto 100 50
        600, 2, 'div', 5, 7, 'eq', 'rlineto',         # rlineto 300 0
        0, 400, 'neg', 'abs', 'rlineto',              # rlineto 0 400
        -300, 'dup', 'drop', 0, 'rlineto',            # rlineto -300 0
        200, 3, 'put',                                # transient[3] := 200
        3, 'get', 'neg', 0, 'exch', 'rlineto',        # rlineto 0 -200
        10, 20, 0, 'index', 'drop', 'exch', 'rlineto',  # rlineto 20 10
        -20, 30, 1, 2, 'ifelse', -10, 'rlineto',      # rlineto -20 -10
        # sqrt, and, or, not, add: the result is dropped
        16, 'sqrt', 1, 0, 'and', 'or', 1, 0, 'not', 'and', 'add', 'drop',
        # roll: 1 2 3 -> 3 1 2, then dropped
        1, 2, 3, 3, 1, 'roll', 'drop', 'drop', 'drop',
        'endchar')),
]


# ---------------------------------------------------------------------------
# CFF

def cff_index(items):
    if not items:
        return struct.pack('>H', 0)
    offsets = [1]
    for it in items:
        offsets.append(offsets[-1] + len(it))
    off_size = 1 if offsets[-1] < 0x100 else 2 if offsets[-1] < 0x10000 else 3 if offsets[-1] < 0x1000000 else 4
    out = struct.pack('>HB', len(items), off_size)
    for o in offsets:
        out += o.to_bytes(off_size, 'big')
    for it in items:
        out += it
    return out


def dict_int(v):
    """DICT integer, always 5 bytes (operator 29), so the size doesn't depend on value."""
    return bytes([29]) + struct.pack('>i', v)


def build_cff():
    charstrings = [g[3] for g in GLYPHS]
    name_index = cff_index([b'CastleTestCff'])
    string_index = cff_index([])
    gsubr_index = cff_index(GLOBAL_SUBRS)
    charstrings_index = cff_index(charstrings)

    local_subrs_index = cff_index(LOCAL_SUBRS)

    def private_dict(subrs_offset):
        return (dict_int(DEFAULT_WIDTH) + bytes([20]) +
                dict_int(NOMINAL_WIDTH) + bytes([21]) +
                dict_int(subrs_offset) + bytes([19]))

    private_size = len(private_dict(0))
    private = private_dict(private_size)  # local subrs right after private dict

    def top_dict(charstrings_offset, private_offset):
        return (dict_int(charstrings_offset) + bytes([17]) +
                dict_int(private_size) + dict_int(private_offset) + bytes([18]))

    header = bytes([1, 0, 4, 4])
    top_index_size = len(cff_index([top_dict(0, 0)]))
    charstrings_offset = len(header) + len(name_index) + top_index_size + len(string_index) + len(gsubr_index)
    private_offset = charstrings_offset + len(charstrings_index)
    top_index = cff_index([top_dict(charstrings_offset, private_offset)])
    assert len(top_index) == top_index_size

    return (header + name_index + top_index + string_index + gsubr_index +
            charstrings_index + private + local_subrs_index)


# ---------------------------------------------------------------------------
# Other tables

def build_head():
    return struct.pack('>HHiIIHHqqhhhhHHhhh',
        1, 0, 0x00010000, 0, 0x5F0F3CF5, 0x000B, UNITS_PER_EM,
        0, 0,
        -100, -200, 900, 800,   # bbox (approximate, big enough)
        0, 8, 2, 0, 0)


def build_hhea():
    return struct.pack('>HHhhhHhhhhhhhhhhhH',
        1, 0, 800, -200, 0, max(g[2] for g in GLYPHS),
        0, 0, 800, 1, 0, 0, 0, 0, 0, 0, 0, len(GLYPHS))


def build_maxp():
    return struct.pack('>IH', 0x00005000, len(GLYPHS))


def build_hmtx():
    out = b''
    for g in GLYPHS:
        out += struct.pack('>Hh', g[2], 0)
    return out


def build_os2():
    return struct.pack('>HhHHHhhhhhhhhhhh',
        4, 550, 400, 5, 0, 500, 500, 0, 0, 500, 500, 0, 300, 50, 250, 0) + \
        bytes(10) + bytes(16) + b'CGE ' + struct.pack('>HHHhhhHHIIhhHHH',
        0x0040, 0x20, 0x49, 800, -200, 0, 800, 200, 1, 0, 500, 700, 0, 0x20, 0)


def build_name():
    names = [
        (0, 'Public domain (CC0), generated by castle_test_cff_generate.py'),
        (1, 'Castle Test CFF'),
        (2, 'Regular'),
        (4, 'Castle Test CFF Regular'),
        (6, 'CastleTestCff'),
    ]
    records = b''
    storage = b''
    for name_id, text in names:
        data = text.encode('utf-16-be')
        records += struct.pack('>HHHHHH', 3, 1, 0x409, name_id, len(data), len(storage))
        storage += data
    header = struct.pack('>HHH', 0, len(names), 6 + len(records))
    return header + records + storage


def build_cmap():
    # format 4 subtable: one segment per mapped character, and the final 0xFFFF
    mapping = sorted((g[1], i) for i, g in enumerate(GLYPHS) if g[1] is not None)
    segs = [(c, c, (gid - c) & 0xFFFF) for c, gid in mapping] + [(0xFFFF, 0xFFFF, 1)]
    seg_count = len(segs)
    ends = b''.join(struct.pack('>H', s[1]) for s in segs)
    starts = b''.join(struct.pack('>H', s[0]) for s in segs)
    deltas = b''.join(struct.pack('>H', s[2]) for s in segs)
    range_offsets = b''.join(struct.pack('>H', 0) for s in segs)
    search_range = 2 * (2 ** (seg_count.bit_length() - 1))
    entry_selector = seg_count.bit_length() - 1
    range_shift = 2 * seg_count - search_range
    body = (struct.pack('>HHHH', seg_count * 2, search_range, entry_selector, range_shift) +
            ends + struct.pack('>H', 0) + starts + deltas + range_offsets)
    sub = struct.pack('>HHH', 4, 6 + len(body), 0) + body
    header = struct.pack('>HH', 0, 2)
    records = struct.pack('>HHI', 0, 3, 4 + 8 * 2) + struct.pack('>HHI', 3, 1, 4 + 8 * 2)
    return header + records + sub


def build_post():
    return struct.pack('>IihhIIIII', 0x00030000, 0, -100, 50, 0, 0, 0, 0, 0)


def checksum(data):
    data += bytes((4 - len(data) % 4) % 4)
    return sum(struct.unpack('>%dI' % (len(data) // 4), data)) & 0xFFFFFFFF


def build_font():
    tables = {
        b'CFF ': build_cff(),
        b'OS/2': build_os2(),
        b'cmap': build_cmap(),
        b'head': build_head(),
        b'hhea': build_hhea(),
        b'hmtx': build_hmtx(),
        b'maxp': build_maxp(),
        b'name': build_name(),
        b'post': build_post(),
    }
    tags = sorted(tables)
    num_tables = len(tags)
    entry_selector = num_tables.bit_length() - 1
    search_range = 16 * (2 ** entry_selector)
    range_shift = 16 * num_tables - search_range
    header = b'OTTO' + struct.pack('>HHHH', num_tables, search_range, entry_selector, range_shift)
    offset = 12 + 16 * num_tables
    directory = b''
    data = b''
    for tag in tags:
        t = tables[tag]
        directory += tag + struct.pack('>III', checksum(t), offset + len(data), len(t))
        data += t + bytes((4 - len(t) % 4) % 4)
    font = header + directory + data
    # head.checkSumAdjustment
    head_offset = 12 + 16 * num_tables + sum(
        len(tables[t]) + (4 - len(tables[t]) % 4) % 4 for t in tags[:tags.index(b'head')])
    adjustment = (0xB1B0AFBA - checksum(font)) & 0xFFFFFFFF
    font = font[:head_offset + 8] + struct.pack('>I', adjustment) + font[head_offset + 12:]
    return font


if __name__ == '__main__':
    out = os.path.join(os.path.dirname(os.path.abspath(__file__)), 'castle_test_cff.otf')
    with open(out, 'wb') as f:
        f.write(build_font())
    print('Written', out)
