@echo off
if [%1]==[] goto :usage

set PUNTVIS=bin\puntvis.exe
set IMAGES_DIR=%1-images
set GRAPHVIZ_DIR=%1-graphviz
set DOT="dot.exe"
set FMT=png
set DPI=250
@echo using DPI %DPI%. if it's too large -- configure your own

@echo reading log...
"%PUNTVIS%" %1
rmdir %IMAGES_DIR% /q /s
mkdir %IMAGES_DIR%

@echo generating images...
for %%i in (%GRAPHVIZ_DIR%/*) do %DOT% -Gdpi="%DPI%" -T%FMT% %GRAPHVIZ_DIR%/%%i -o %IMAGES_DIR%/%%i.%FMT%

goto :eof
:usage
@echo Usage: %0 ^<logFile^>
exit /B 1
