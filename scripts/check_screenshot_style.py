#!/usr/bin/env python3
import struct
import sys
import zlib


def read_png(path):
    with open(path, "rb") as png:
        data = png.read()
    if not data.startswith(b"\x89PNG\r\n\x1a\n"):
        raise SystemExit(f"{path}: not a PNG file")

    pos = 8
    width = height = color_type = bit_depth = None
    raw = bytearray()
    while pos < len(data):
        length = struct.unpack(">I", data[pos : pos + 4])[0]
        kind = data[pos + 4 : pos + 8]
        chunk = data[pos + 8 : pos + 8 + length]
        pos += 12 + length
        if kind == b"IHDR":
            width, height, bit_depth, color_type, compression, filter_method, interlace = struct.unpack(
                ">IIBBBBB", chunk
            )
            if bit_depth != 8 or color_type not in (2, 6) or compression != 0 or filter_method != 0 or interlace != 0:
                raise SystemExit("unsupported PNG format")
        elif kind == b"IDAT":
            raw.extend(chunk)
        elif kind == b"IEND":
            break

    channels = 4 if color_type == 6 else 3
    stride = width * channels
    inflated = zlib.decompress(bytes(raw))
    rows = []
    prev = [0] * stride
    i = 0
    for _ in range(height):
        filter_type = inflated[i]
        i += 1
        row = list(inflated[i : i + stride])
        i += stride
        for x in range(stride):
            left = row[x - channels] if x >= channels else 0
            up = prev[x]
            upper_left = prev[x - channels] if x >= channels else 0
            if filter_type == 1:
                row[x] = (row[x] + left) & 0xFF
            elif filter_type == 2:
                row[x] = (row[x] + up) & 0xFF
            elif filter_type == 3:
                row[x] = (row[x] + ((left + up) >> 1)) & 0xFF
            elif filter_type == 4:
                p = left + up - upper_left
                pa = abs(p - left)
                pb = abs(p - up)
                pc = abs(p - upper_left)
                predictor = left if pa <= pb and pa <= pc else up if pb <= pc else upper_left
                row[x] = (row[x] + predictor) & 0xFF
            elif filter_type != 0:
                raise SystemExit(f"unsupported PNG filter: {filter_type}")
        rows.append(row)
        prev = row
    return width, height, channels, rows


def main():
    if len(sys.argv) != 2:
        raise SystemExit("usage: check_screenshot_style.py screenshot.png")

    width, height, channels, rows = read_png(sys.argv[1])
    y0 = int(height * 0.22)
    y1 = int(height * 0.92)
    x0 = int(width * 0.03)
    x1 = int(width * 0.97)
    sampled = 0
    material_pixels = 0

    for y in range(y0, y1, 4):
        row = rows[y]
        for x in range(x0, x1, 4):
            offset = x * channels
            r, g, b = row[offset], row[offset + 1], row[offset + 2]
            sampled += 1
            # Apple-style material should not leave the app body as a plain white canvas.
            colorful = max(r, g, b) - min(r, g, b) >= 14
            soft_gray = 220 <= r <= 248 and 220 <= g <= 248 and 220 <= b <= 248
            not_plain_white = not (r >= 248 and g >= 248 and b >= 248)
            if (colorful and not_plain_white) or soft_gray:
                material_pixels += 1

    ratio = material_pixels / sampled
    if ratio < 0.18:
        raise SystemExit(f"plain canvas detected: material pixel ratio {ratio:.3f} < 0.180")
    print(f"material pixel ratio {ratio:.3f}")


if __name__ == "__main__":
    main()
