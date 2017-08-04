@echo off
if [%1]==[] goto :usage


@echo reading log...
bin\puntvis.exe %1
rmdir %1-images /q /s
mkdir %1-images

@echo generating images...
for %%i in (%1-graphviz/*) do dot.exe -Tpng %1-graphviz/%%i -o %1-images/%%i.png


goto :eof
:usage
@echo Usage: %0 ^<logFile^>
exit /B 1
