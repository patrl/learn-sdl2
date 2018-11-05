{ mkDerivation, base, sdl2, stdenv }:
mkDerivation {
  pname = "learn-sdl2";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base sdl2 ];
  license = stdenv.lib.licenses.bsd3;
}
