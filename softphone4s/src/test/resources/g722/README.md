Test vectors for `G722Codec`.

- `input.pcm`: 3 200 samples of 16-bit little-endian PCM at 16 kHz, made for the test: a chirp, a
  tone in the upper band, a tone in the lower, noise, a stretch of silence and a burst at full scale.
- `reference.g722`: what the public-domain G.722 encoder of Steve Underwood (based on the Carnegie
  Mellon ADPCM program) makes of it at 64 kbit/s.
- `reference-decoded.pcm`: what his decoder makes of `reference.g722`.

They were made once with that reference code, which is not part of this repository. The test
requires our encoder and decoder to give the same bytes, not merely something close.
