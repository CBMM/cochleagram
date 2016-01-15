Cochleagram
-----------

Tools for psychoacoustics

## Installation 

 1. `git clone git@github.com:CBMM/cochleagram && cd cochleagram`
 1. Install [Haskell](https://www.haskell.org/downloads#minimal)
 1. Install [ghcjs](https://github.com/ghcjs/ghcjs#quick-start)
 1. `git submodule update --init --recursive`
 1. `./init-sandbox.sh`
 1. `cabal configure --ghcjs`
 1. `cabal install --only-dep`
 1. `cabal install`

## Running

`cochleagram` uses your computer's microphone, so the browser protects you by making sure `cochleagram` only runs over a secured connection. This makes it a little bit tough to test your changes `cochleagram` on your local computer - you need an SSL-enabled server.

[http-server](https://www.npmjs.com/package/http-server) is a quick way to do this.

You'll need to [make a self-signed certificate](http://stackoverflow.com/questions/10175812/how-to-create-a-self-signed-certificate-with-openssl).

Then point your server to the cert and key files, from the `cochleagram` compiler output directory:

```
cd dist/build/cochleagram/cochleagram.jsexe/
http-server -S -K ~/key.pem -C ~/cert.pem
```

At this point you can reach the files via the browser at `https://localhost:8080`

If this gives you trouble please feel free to get in touch.

## Brainstorming - Hack with us!

### Teaching & demos

- Show basic principles
  (filters, power spectrum, spectrograms, cochleagrams)

- Use data from computer mic (real time visualization)

- Use data from file on the web (zoomable visualization)

- Tune parameters 

- Demos usable after the talk is over

### Researchy demos

- Template matching in spectrogram

- Auditory scene / texture / cepstra analysis

- Sound generation & playback from scene statistics

- Stereo source localization demos (too ambitious)?


### Goals, Themes

- Separate the sound exploration from grungy coding details

- Abstract the most common tasks into nice libraries

- Push science findings toward demo-level teaching tools

### Related work & Inspiration

- [Web Audio Spectrogram](https://webaudiodemos.appspot.com/input/index.html) RT spectrogram & distortion demo

- [Web Audio Demos](http://webaudiodemos.appspot.com/) Many demos of the web audio api

- [Web Audio API](https://developer.mozilla.org/en-US/docs/Web/API/Web_Audio_API) Mozilla Dev Network. Browser-based audio analysis & production.

- [VisPy](https://www.youtube.com/watch?v=_3YoaeoiIFI) Luke Campagnola (Paul Manis Lab, Allen Inst.) RT spectrograms & general data vis

- [MathBox](https://acko.net/blog/mathbox2/) Web math visualization

- [Pandoc](http://pandoc.org) Generate [slides](http://web.mit.edu/greghale/Public/thesis/build/index.html) from [markdown](https://github.com/imalsogreg/RetroProject/blob/master/thesis/thesis.org)

- [Explorable Explanations](http://explorableexplanations.com/)

- [Neurons](http://ncase.me/neurons/) Nicky Case

- [Parable of the Polygons](http://ncase.me/polygons/) Nicky Case

### Brainstorming session with McDermott lab

- Demo walking through mechanism of generating cochleagram and connection to auditory system. Show traveling wave in basalar membrane. (Sam)

- How hard to specify (how many dimensions) (how many sliders) for parametizing cochleagram.

- Export figures

- Javascript NN?

- Other ways of visualizing sound than spectrograms?

- Code box to change the signal?

- Online training in some way or another (reverse correlation?)

- Provide filters to the thing.

- Sliding correlation matrix.


