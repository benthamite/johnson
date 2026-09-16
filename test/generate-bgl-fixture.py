#!/usr/bin/env python3
"""Generate BGL (Babylon) test fixture for johnson-bgl tests.

Creates:
  - test-bgl.bgl: 3 entries (apple, cat, hello), standard format
  - test-bgl-cp1251.bgl: 2 Russian entries encoded in cp1251 (charset 0x44)

Uses only Python stdlib.
"""

import gzip
import os
import struct


# ---------------------------------------------------------------------------
# BGL language codes (subset)
# ---------------------------------------------------------------------------

LANG_ENGLISH = 0x00
LANG_RUSSIAN = 0x07

# BGL charset codes (subset) and their Python codec names.
CHARSET_DEFAULT = 0x41   # cp1252
CHARSET_CYRILLIC = 0x44  # cp1251
CHARSET_CODECS = {CHARSET_DEFAULT: "cp1252", CHARSET_CYRILLIC: "cp1251"}


# ---------------------------------------------------------------------------
# Block builders
# ---------------------------------------------------------------------------

def encode_block(block_type, payload):
    """Encode a BGL block with type and length prefix.

    Block header encoding:
      First byte: low 4 bits = type, high 4 bits = length info.
      If payload length + 4 fits in high nibble (i.e. length <= 11),
      the high nibble = length + 4 and no extra length bytes.
      Otherwise, high nibble encodes how many extra bytes hold the length:
        0 => 1 extra byte (length 0-255)
        1 => 2 extra bytes (length 0-65535)
        2 => 3 extra bytes (length 0-16777215)
    """
    length = len(payload)
    if length <= 11:
        # Length fits in high nibble: high nibble = length + 4
        header = bytes([(block_type & 0x0F) | ((length + 4) << 4)])
        return header + payload
    elif length <= 0xFF:
        # High nibble = 0 (meaning 1 extra byte for length)
        header = bytes([block_type & 0x0F])
        return header + bytes([length]) + payload
    elif length <= 0xFFFF:
        # High nibble = 1 (meaning 2 extra bytes)
        header = bytes([(block_type & 0x0F) | (1 << 4)])
        return header + struct.pack(">H", length) + payload
    else:
        # High nibble = 2 (meaning 3 extra bytes)
        header = bytes([(block_type & 0x0F) | (2 << 4)])
        return header + struct.pack(">I", length)[1:] + payload


def make_metadata_block(subtype, value_bytes):
    """Build a type 3 (metadata) block with the given subtype and value.

    Real Babylon files store the property ID as a 2-byte big-endian
    integer followed by the value (see stardict-tools babylon.cpp and
    pyglossary, which read block.data[1] as the ID and block.data[2:] as
    the value).
    """
    payload = struct.pack(">H", subtype) + value_bytes
    return encode_block(3, payload)


def make_entry_block(headword, definition, alternates=None,
                     source_codec="cp1252", target_codec="cp1252"):
    """Build a type 1 (entry) block.

    Format:
      1-byte headword length + headword bytes
      2-byte BE definition length + definition bytes
      Then for each alternate: 1-byte length + alternate bytes

    Headwords and alternates are encoded with SOURCE_CODEC, definitions
    with TARGET_CODEC, mirroring the source/target charset properties.
    """
    hw_bytes = headword.encode(source_codec)
    def_bytes = definition.encode(target_codec)

    payload = bytes([len(hw_bytes)]) + hw_bytes
    payload += struct.pack(">H", len(def_bytes)) + def_bytes

    if alternates:
        for alt in alternates:
            alt_bytes = alt.encode(source_codec)
            payload += bytes([len(alt_bytes)]) + alt_bytes

    return encode_block(1, payload)


# ---------------------------------------------------------------------------
# BGL file builder
# ---------------------------------------------------------------------------

def build_bgl(entries, title="Test BGL Dictionary",
              source_lang=LANG_ENGLISH, target_lang=LANG_ENGLISH,
              source_charset=CHARSET_DEFAULT, target_charset=CHARSET_DEFAULT):
    """Build a complete BGL file.

    entries: list of (headword, definition_html) tuples.
            Each definition may contain HTML tags.
    """
    source_codec = CHARSET_CODECS[source_charset]
    target_codec = CHARSET_CODECS[target_charset]

    # Build the decompressed stream: metadata blocks + entry blocks.
    stream = b""

    # Metadata: title (property 0x01), encoded in the source charset.
    stream += make_metadata_block(0x01, title.encode(source_codec))

    # Metadata: source language (property 0x07), 4-byte BE value.
    stream += make_metadata_block(0x07, struct.pack(">I", source_lang))

    # Metadata: target language (property 0x08), 4-byte BE value.
    stream += make_metadata_block(0x08, struct.pack(">I", target_lang))

    # Metadata: source charset (property 0x1A), 1-byte value.
    stream += make_metadata_block(0x1A, bytes([source_charset]))

    # Metadata: target charset (property 0x1B), 1-byte value.
    stream += make_metadata_block(0x1B, bytes([target_charset]))

    # Entry blocks (type 1)
    for hw, defn in entries:
        stream += make_entry_block(hw, defn, source_codec=source_codec,
                                   target_codec=target_codec)

    # Gzip compress the stream.
    compressed = gzip.compress(stream)

    # BGL header:
    #   Bytes 0-3: magic \x12\x34\x00\x01
    #   Byte 4: gzip offset (from file start)
    #   Bytes 5+: padding/reserved until gzip offset
    # We use a simple header: magic(4) + gzip_offset_u8(1) + padding.
    # The gzip data starts immediately after a minimal header.
    # Use offset 6 (right after the 6-byte header).
    gzip_offset = 6
    header = b"\x12\x34\x00\x01"
    header += struct.pack(">H", gzip_offset)

    return header + compressed


def main():
    fixtures_dir = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                                "fixtures")
    os.makedirs(fixtures_dir, exist_ok=True)

    entries = [
        ("apple", "<b>apple</b> - A round fruit that grows on trees."),
        ("cat", "<b>cat</b> - A small feline domesticated pet."),
        ("hello", "<b>hello</b> - A greeting used to say hi."),
    ]

    bgl = build_bgl(entries, title="Test BGL Dictionary")
    path = os.path.join(fixtures_dir, "test-bgl.bgl")
    with open(path, "wb") as f:
        f.write(bgl)
    print(f"Created {path} ({len(bgl)} bytes)")

    # Russian-English dictionary in cp1251: non-Latin headwords and a
    # Latin-1 definition prove that headwords use the source charset and
    # definitions the target charset.
    cyrillic_entries = [
        ("\u044f\u0431\u043b\u043e\u043a\u043e",
         "<b>apple</b> - a fruit; caf\u00e9 word for testing"),
        ("\u043a\u043e\u0448\u043a\u0430", "<b>cat</b> - a feline"),
    ]
    cp1251 = build_bgl(cyrillic_entries,
                       title="\u0422\u0435\u0441\u0442\u043e\u0432\u044b\u0439",
                       source_lang=LANG_RUSSIAN, target_lang=LANG_ENGLISH,
                       source_charset=CHARSET_CYRILLIC,
                       target_charset=CHARSET_DEFAULT)
    cp1251_path = os.path.join(fixtures_dir, "test-bgl-cp1251.bgl")
    with open(cp1251_path, "wb") as f:
        f.write(cp1251)
    print(f"Created {cp1251_path} ({len(cp1251)} bytes)")

    # Verify the file can be read back.
    with open(path, "rb") as f:
        data = f.read()
    magic = data[:4]
    assert magic == b"\x12\x34\x00\x01", f"Bad magic: {magic!r}"
    gz_off = struct.unpack(">H", data[4:6])[0]
    decompressed = gzip.decompress(data[gz_off:])
    print(f"  Gzip offset: {gz_off}")
    print(f"  Decompressed stream: {len(decompressed)} bytes")

    # Parse blocks to verify.
    pos = 0
    block_count = 0
    entry_count = 0
    while pos < len(decompressed):
        byte0 = decompressed[pos]
        pos += 1
        btype = byte0 & 0x0F
        length_nibble = byte0 >> 4
        if length_nibble < 4:
            extra_bytes = length_nibble + 1
            length = int.from_bytes(decompressed[pos:pos + extra_bytes], "big")
            pos += extra_bytes
        else:
            length = length_nibble - 4
        payload = decompressed[pos:pos + length]
        pos += length
        block_count += 1

        if btype == 3:
            subtype = struct.unpack(">H", payload[:2])[0]
            value = payload[2:]
            if subtype == 0x01:
                print(f"  Metadata: title = {value.decode('cp1252')}")
            elif subtype == 0x07:
                print(f"  Metadata: source_lang = {value[3]}")
            elif subtype == 0x08:
                print(f"  Metadata: target_lang = {value[3]}")
        elif btype in (1, 7, 10, 11, 13):
            hw_len = payload[0]
            hw = payload[1:1 + hw_len].decode("cp1252")
            def_len = struct.unpack(">H", payload[1 + hw_len:3 + hw_len])[0]
            defn = payload[3 + hw_len:3 + hw_len + def_len].decode("cp1252")
            entry_count += 1
            print(f"  Entry: {hw!r} => {defn[:50]!r}...")

    print(f"  Total blocks: {block_count}, entries: {entry_count}")
    print("Verification passed!")


if __name__ == "__main__":
    main()
