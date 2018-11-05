{ mkDerivation, base, sdl2, sdl2-image, sdl2-mixer, sdl2-ttf
, stdenv
}:
mkDerivation {
  pname = "learn-sdl2";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [
    base sdl2 sdl2-image sdl2-mixer sdl2-ttf
  ];
  license = stdenv.lib.licenses.bsd3;
}
