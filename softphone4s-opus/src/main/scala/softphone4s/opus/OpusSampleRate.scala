package softphone4s.opus

/** The sample rates Opus takes PCM at. The audio device has to run at the one
  * chosen, and it is also the rate that decides which codecs this can share a
  * softphone with: 8 kHz can be listed beside G.711, 16 kHz beside G.722.
  */
enum OpusSampleRate(val hz: Int) {
  case Hz8000  extends OpusSampleRate(8000)
  case Hz12000 extends OpusSampleRate(12000)
  case Hz16000 extends OpusSampleRate(16000)
  case Hz24000 extends OpusSampleRate(24000)
  case Hz48000 extends OpusSampleRate(48000)
}
