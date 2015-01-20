{ mkDerivation, base, stdenv }:
mkDerivation {
  pname = "global-lock";
  version = "0.1";
  sha256 = "0b2sz9ag6wcr0amgrx08l7924brfansnh9rv64wg9s3nk4ni2sxp";
  buildDepends = [ base ];
  description = "A global lock implemented without unsafePerformIO";
  license = stdenv.lib.licenses.bsd3;
}
