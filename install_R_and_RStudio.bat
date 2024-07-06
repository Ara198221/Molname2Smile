@echo off
setlocal

set "RSTUDIO_VERSION=RStudio-2024.04.1-748.exe"
set "R_VERSION=R-4.4.0-win.exe"
set "INSTALL_DIR=%~dp0"

echo Checking for RStudio installer...
if exist "%INSTALL_DIR%%RSTUDIO_VERSION%" (
    echo RStudio installer found.
) else (
    echo RStudio installer not found. Please download %RSTUDIO_VERSION% and place it in the same directory as this script.
    pause
    exit /b
)

echo Checking for R installer...
if exist "%INSTALL_DIR%%R_VERSION%" (
    echo R installer found.
) else (
    echo R installer not found. Please download %R_VERSION% and place it in the same directory as this script.
    pause
    exit /b
)

echo Installing R...
start /wait "" "%INSTALL_DIR%%R_VERSION%" /SILENT

echo Installing RStudio...
start /wait "" "%INSTALL_DIR%%RSTUDIO_VERSION%" /SILENT

echo Installation complete.
pause
