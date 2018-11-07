{ mkDerivation, apecs, base, grid, lens, sdl2, sdl2-image
, sdl2-mixer, sdl2-ttf, stdenv, text, vector
}:
mkDerivation {
  pname = "learn-sdl2";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    apecs base grid lens sdl2 sdl2-image sdl2-mixer sdl2-ttf text
    vector
  ];
  license = stdenv.lib.licenses.bsd3;
}
