#!/usr/bin/env python3
"""Generate MDict v2.0 test fixtures for johnson-mdict tests.

Creates:
  - test-mdict.mdx: 3 entries, zlib compressed, no encryption
  - test-mdict-encrypted.mdx: 3 entries, zlib compressed, Encrypted="2"

Uses only Python stdlib.
"""

import struct
import zlib
import os


# ---------------------------------------------------------------------------
# Minimal pure-Python RIPEMD-128
# ---------------------------------------------------------------------------

_LEFT_K = [0x00000000, 0x5A827999, 0x6ED9EBA1, 0x8F1BBCDC]
_RIGHT_K = [0x50A28BE6, 0x5C4DD124, 0x6D703EF3, 0x00000000]

_LEFT_R = [
    0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15,
    7, 4, 13, 1, 10, 6, 15, 3, 12, 0, 9, 5, 2, 14, 11, 8,
    3, 10, 14, 4, 9, 15, 8, 1, 2, 7, 0, 6, 13, 11, 5, 12,
    1, 9, 11, 10, 0, 8, 12, 4, 13, 3, 7, 15, 14, 5, 6, 2,
]
_RIGHT_R = [
    5, 14, 7, 0, 9, 2, 11, 4, 13, 6, 15, 8, 1, 10, 3, 12,
    6, 11, 3, 7, 0, 13, 5, 10, 14, 15, 8, 12, 4, 9, 1, 2,
    15, 5, 1, 3, 7, 14, 6, 9, 11, 8, 12, 2, 10, 0, 4, 13,
    8, 6, 4, 1, 3, 11, 15, 0, 5, 12, 2, 13, 9, 7, 10, 14,
]
_LEFT_S = [
    11, 14, 15, 12, 5, 8, 7, 9, 11, 13, 14, 15, 6, 7, 9, 8,
    7, 6, 8, 13, 11, 9, 7, 15, 7, 12, 15, 9, 11, 7, 13, 12,
    11, 13, 6, 7, 14, 9, 13, 15, 14, 8, 13, 6, 5, 12, 7, 5,
    11, 12, 14, 15, 14, 15, 9, 8, 9, 14, 5, 6, 8, 6, 5, 12,
]
_RIGHT_S = [
    8, 9, 9, 11, 13, 15, 15, 5, 7, 7, 8, 11, 14, 14, 12, 6,
    9, 13, 15, 7, 12, 8, 9, 11, 7, 7, 12, 7, 6, 15, 13, 11,
    9, 7, 15, 11, 8, 6, 6, 14, 12, 13, 5, 14, 13, 13, 7, 5,
    15, 5, 8, 11, 14, 14, 6, 14, 6, 9, 12, 9, 12, 5, 15, 8,
]

M32 = 0xFFFFFFFF


def _rol(x, n):
    return ((x << n) | (x >> (32 - n))) & M32


def _f(x, y, z): return x ^ y ^ z
def _g(x, y, z): return (x & y) | ((~x & M32) & z)
def _h(x, y, z): return (x | (~y & M32)) ^ z
def _i(x, y, z): return (x & z) | (y & (~z & M32))


_LEFT_FN = [_f, _g, _h, _i]
_RIGHT_FN = [_i, _h, _g, _f]


def ripemd128(data):
    """Pure Python RIPEMD-128 hash, returns 16 bytes."""
    msg = bytearray(data)
    bit_len = len(msg) * 8
    msg.append(0x80)
    while len(msg) % 64 != 56:
        msg.append(0)
    msg += struct.pack("<Q", bit_len)

    h0, h1, h2, h3 = 0x67452301, 0xEFCDAB89, 0x98BADCFE, 0x10325476

    for offset in range(0, len(msg), 64):
        x = struct.unpack_from("<16I", msg, offset)

        al, bl, cl, dl = h0, h1, h2, h3
        ar, br, cr, dr = h0, h1, h2, h3

        for j in range(64):
            rnd = j // 16
            t = (al + _LEFT_FN[rnd](bl, cl, dl) + x[_LEFT_R[j]] + _LEFT_K[rnd]) & M32
            t = _rol(t, _LEFT_S[j])
            al, bl, cl, dl = dl, t, bl, cl

        for j in range(64):
            rnd = j // 16
            t = (ar + _RIGHT_FN[rnd](br, cr, dr) + x[_RIGHT_R[j]] + _RIGHT_K[rnd]) & M32
            t = _rol(t, _RIGHT_S[j])
            ar, br, cr, dr = dr, t, br, cr

        tt = (h1 + cl + dr) & M32
        h1 = (h2 + dl + ar) & M32
        h2 = (h3 + al + br) & M32
        h3 = (h0 + bl + cr) & M32
        h0 = tt

    return struct.pack("<4I", h0, h1, h2, h3)


# ---------------------------------------------------------------------------
# MDict encryption
# ---------------------------------------------------------------------------

def mdict_encrypt(data, key):
    """Encrypt data using MDict XOR/nibble-swap scheme.

    The decryption algorithm is:
        previous = 0x36
        for i in range(len(data)):
            t = (data[i] >> 4 | data[i] << 4) & 0xFF   # nibble swap
            t = t ^ previous ^ (i & 0xFF) ^ key[i % len(key)]
            previous = data[i]
            data[i] = t

    To encrypt, we reverse this:
        Given plaintext p[i], we want ciphertext c[i] such that
        decrypting c[i] yields p[i].

        p[i] = (c[i] >> 4 | c[i] << 4) ^ prev ^ (i & 0xFF) ^ key[i % klen]
        where prev = c[i-1]  (prev_0 = 0x36)

        So: (c[i] >> 4 | c[i] << 4) = p[i] ^ prev ^ (i & 0xFF) ^ key[i % klen]
        Let v = p[i] ^ prev ^ (i & 0xFF) ^ key[i % klen]
        Then c[i] = swap_nibbles(v)  = (v >> 4 | v << 4) & 0xFF
    """
    b = bytearray(len(data))
    previous = 0x36
    key_len = len(key)
    for idx in range(len(data)):
        v = data[idx] ^ previous ^ (idx & 0xFF) ^ key[idx % key_len]
        c = ((v >> 4) | (v << 4)) & 0xFF
        b[idx] = c
        previous = c  # prev for next iteration is the ciphertext byte
    return bytes(b)


# ---------------------------------------------------------------------------
# Binary helpers
# ---------------------------------------------------------------------------

def u32be(n):
    return struct.pack(">I", n)


def u64be(n):
    return struct.pack(">Q", n)


def adler32_le(data):
    """Adler-32 checksum as 4 little-endian bytes."""
    return struct.pack("<I", zlib.adler32(data) & 0xFFFFFFFF)


def make_zlib_block(data):
    """Wrap data in an MDict compressed block (type=zlib)."""
    compressed = zlib.compress(data)
    return u32be(0x02000000) + adler32_le(data) + compressed


# ---------------------------------------------------------------------------
# MDX builder
# ---------------------------------------------------------------------------

def build_mdx(entries, encrypt=False, title="Test Dict"):
    """Build a complete v2.0 MDX file.

    entries: list of (headword, definition_html) tuples.
    """
    encoding = "utf-8"

    # -- Header section --
    attrs = {
        "GeneratedByEngineVersion": "2.0",
        "RequiredEngineVersion": "2.0",
        "Format": "Html",
        "Encoding": "utf-8",
        "Title": title,
        "Description": "Test dictionary for johnson-mdict",
        "Encrypted": "2" if encrypt else "0",
    }
    xml = "<Dictionary"
    for k, v in attrs.items():
        xml += f' {k}="{v}"'
    xml += "/>\r\n\x00"
    header_bytes = xml.encode("utf-16-le")
    header_section = u32be(len(header_bytes)) + header_bytes + adler32_le(header_bytes)

    # -- Compute record data and offsets --
    record_data_parts = []
    kw_entries = []  # (headword, cumulative_offset)
    cum_offset = 0
    for hw, defn in entries:
        encoded = defn.encode(encoding)
        kw_entries.append((hw, cum_offset))
        record_data_parts.append(encoded)
        cum_offset += len(encoded)
    record_data = b"".join(record_data_parts)

    # -- Keyword block --
    # v2.0: each entry = u64be(record_offset) + headword_bytes + null_terminator
    kw_block_raw = b""
    for hw, offset in kw_entries:
        kw_block_raw += u64be(offset)
        kw_block_raw += hw.encode(encoding) + b"\x00"
    kw_block_compressed = make_zlib_block(kw_block_raw)

    # -- Keyword block info --
    # v2.0: u64 num_entries_in_block
    #        + u16be first_hw_size (in bytes for utf-8)
    #        + first_hw_encoded + null
    #        + u16be last_hw_size
    #        + last_hw_encoded + null
    #        + u64 compressed_block_size
    #        + u64 decompressed_block_size
    first_hw_enc = kw_entries[0][0].encode(encoding)
    last_hw_enc = kw_entries[-1][0].encode(encoding)
    block_info_raw = b""
    block_info_raw += u64be(len(kw_entries))
    block_info_raw += struct.pack(">H", len(first_hw_enc))
    block_info_raw += first_hw_enc + b"\x00"
    block_info_raw += struct.pack(">H", len(last_hw_enc))
    block_info_raw += last_hw_enc + b"\x00"
    block_info_raw += u64be(len(kw_block_compressed))
    block_info_raw += u64be(len(kw_block_raw))
    block_info_compressed = make_zlib_block(block_info_raw)

    # -- Keyword section header --
    # u64 num_blocks + u64 num_entries + u64 decomp_info_size
    # + u64 comp_info_size + u64 total_kw_block_size
    kw_header = b""
    kw_header += u64be(1)                              # num keyword blocks
    kw_header += u64be(len(kw_entries))                # total entries
    kw_header += u64be(len(block_info_raw))            # decompressed info size
    kw_header += u64be(len(block_info_compressed))     # compressed info size
    kw_header += u64be(len(kw_block_compressed))       # total keyword block size
    kw_checksum = adler32_le(kw_header)

    # Apply encryption if needed.
    # Encrypted="2" uses bytes 4-8 (the Adler-32 checksum in the
    # compression header) of each block as the key source, and only
    # encrypts the payload after the 8-byte compression header.
    if encrypt:
        # Encrypted="2" only encrypts the keyword block info (index),
        # NOT the keyword blocks themselves.
        bi_key = ripemd128(block_info_compressed[4:8] + b"\x95\x36\x00\x00")
        block_info_encrypted = (block_info_compressed[:8]
                                + mdict_encrypt(block_info_compressed[8:], bi_key))
    else:
        block_info_encrypted = block_info_compressed

    keyword_section = kw_header + kw_checksum + block_info_encrypted + kw_block_compressed

    # -- Record section --
    rec_block_compressed = make_zlib_block(record_data)

    # Record section header:
    # u64 num_record_blocks + u64 num_entries + u64 rec_block_info_size + u64 rec_blocks_total_size
    rec_block_info = u64be(len(rec_block_compressed)) + u64be(len(record_data))
    rec_header = b""
    rec_header += u64be(1)                              # num record blocks
    rec_header += u64be(len(kw_entries))                # num entries
    rec_header += u64be(len(rec_block_info))            # record block info size
    rec_header += u64be(len(rec_block_compressed))      # record blocks total size

    record_section = rec_header + rec_block_info + rec_block_compressed

    return header_section + keyword_section + record_section


def main():
    fixtures_dir = os.path.join(os.path.dirname(os.path.abspath(__file__)), "fixtures")
    os.makedirs(fixtures_dir, exist_ok=True)

    entries = [
        ("apple", "<b>apple</b> A round fruit."),
        ("cat", "<b>cat</b> A small feline pet."),
        ("hello", "<b>hello</b> A greeting."),
    ]

    # 1. Unencrypted MDX
    mdx = build_mdx(entries, encrypt=False, title="Test MDict Dictionary")
    path = os.path.join(fixtures_dir, "test-mdict.mdx")
    with open(path, "wb") as f:
        f.write(mdx)
    print(f"Created {path} ({len(mdx)} bytes)")

    # 2. Encrypted MDX (Encrypted="2")
    mdx_enc = build_mdx(entries, encrypt=True, title="Test MDict Encrypted")
    path = os.path.join(fixtures_dir, "test-mdict-encrypted.mdx")
    with open(path, "wb") as f:
        f.write(mdx_enc)
    print(f"Created {path} ({len(mdx_enc)} bytes)")


if __name__ == "__main__":
    main()
