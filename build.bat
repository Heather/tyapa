@echo off

echo ---------------------------
echo   Configure
echo ---------------------------

cabal configure

echo ---------------------------
echo   Build
echo ---------------------------

cabal build

pause