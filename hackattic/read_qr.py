import json
import numpy as np
from PIL import Image, ImageChops, ImageFilter
import os
import urllib.request

ACCESS_TOKEN = os.getenv("HACKATTIC_ACCESS_TOKEN")

BASE_URL = "https://hackattic.com/challenges/reading_qr/problem"


def get_image():
    problem = urllib.request.urlopen(f"{BASE_URL}?access_token={ACCESS_TOKEN}")
    image_url = json.load(problem)["image_url"]
    return urllib.request.urlopen(image_url)


def autocrop(img):
    diff = Image.new(img.mode, img.size, 255)
    diff = ImageChops.difference(img, diff)
    return img.crop(diff.getbbox())


def normalize_image(img_io):
    """Convert to B/W, crop and rotate the image"""

    qrcode = Image.open(img_io)
    qrcode = qrcode.convert(mode="1", dither=Image.Dither.NONE)

    qrcode = autocrop(qrcode)

    for _ in range(3):
        # continue to rotate until the qrcode is in the right orientation
        qrcode = qrcode.rotate(
            -3, expand=False, fillcolor=255, resample=Image.Resampling.BICUBIC
        )
        qrcode = autocrop(qrcode)
        a = np.asarray(qrcode)

        w, h = qrcode.size
        w -= 1
        h -= 1
        if (
            a[0, 0] == False
            and a[w, 0] == False
            and a[0, h] == False
            and a[w, h] == True
        ):
            break
    return qrcode


# def post_solution():
