#include <stdio.h>
#include <math.h>
#include <inttypes.h>
#include <portaudio.h>

#define NUM_SECONDS   (5)
#define CHANNELS (2)
#define SAMPLE_RATE   (44100)
#define FRAMES_PER_BUFFER  (SAMPLE_RATE/60)

uint8_t AUDIO_FRAMES[CHANNELS*FRAMES_PER_BUFFER] = {0};
uint32_t AUDIO_PHASE = 0;

/* This routine will be called by the PortAudio engine when audio is
 * needed.  It may called at interrupt level on some machines so don't
 * do anything that could mess up the system like calling malloc() or
 * free(). */
static int patestCallback( const void *inputBuffer, void *outputBuffer,
                           unsigned long framesPerBuffer,
                           const PaStreamCallbackTimeInfo* timeInfo,
                           PaStreamCallbackFlags statusFlags,
                           void *userData ) {
  uint8_t *out = (uint8_t*)outputBuffer;

  /* Prevent unused variable warnings. */
  (void) userData;
  (void) timeInfo;
  (void) statusFlags;
  (void) inputBuffer;
  
  for( unsigned long i = 0; i < framesPerBuffer; i++ ) {
    *out++ = AUDIO_FRAMES[CHANNELS*AUDIO_PHASE + 0];
    *out++ = AUDIO_FRAMES[CHANNELS*AUDIO_PHASE + 1];
    AUDIO_PHASE = (AUDIO_PHASE + 1) % FRAMES_PER_BUFFER;
  }

  return paContinue;
}

/*******************************************************************/
int audio_main(void) {
  PaStreamParameters outputParameters;
  PaStream *stream;
  PaError err;

  printf("PortAudio Test: output sine wave. SR = %d, BufSize = %d\n",
         SAMPLE_RATE, FRAMES_PER_BUFFER);

  for( uint32_t i = 0; i < FRAMES_PER_BUFFER; i++ ) {
    uint8_t val =
      (uint8_t) ((128.0 * sin( 4400.0 * ((double)i/(double)FRAMES_PER_BUFFER) * M_PI * 2. ))
                 + 128.0);
    AUDIO_FRAMES[CHANNELS*i + 0] = val;
    AUDIO_FRAMES[CHANNELS*i + 1] = val;
  }

  err = Pa_Initialize();
  if( err != paNoError ) goto error;

  outputParameters.device = Pa_GetDefaultOutputDevice();
  if (outputParameters.device == paNoDevice) {
    fprintf(stderr, "Error: No default output device.\n");
    goto error;
  }

  outputParameters.channelCount = CHANNELS;
  outputParameters.sampleFormat = paUInt8;
  outputParameters.suggestedLatency =
    Pa_GetDeviceInfo( outputParameters.device )->defaultLowOutputLatency;
  outputParameters.hostApiSpecificStreamInfo = NULL;

  err =
    Pa_OpenStream(&stream,
                  NULL, /* no input */
                  &outputParameters,
                  SAMPLE_RATE,
                  FRAMES_PER_BUFFER,
                  /* we won't output out of range samples so don't
                     bother clipping them */
                  paClipOff,
                  patestCallback,
                  NULL );
  if( err != paNoError ) goto error;

  err = Pa_StartStream( stream );
  if( err != paNoError ) goto error;

  printf("Play for %d seconds.\n", NUM_SECONDS );
  Pa_Sleep( NUM_SECONDS * 1000 );

  err = Pa_StopStream( stream );
  if( err != paNoError ) goto error;

  err = Pa_CloseStream( stream );
  if( err != paNoError ) goto error;

  Pa_Terminate();
  printf("Test finished.\n");

  return err;
 error:
  Pa_Terminate();
  fprintf( stderr, "An error occured while using the portaudio stream\n" );
  fprintf( stderr, "Error number: %d\n", err );
  fprintf( stderr, "Error message: %s\n", Pa_GetErrorText( err ) );
  return err;
}
