@echo off
REM Get the current directory
for /f "tokens=* usebackq" %%i in (`Rscript -e "cat(getwd())"`) do set CUR_DIR=%%i

REM Run the Shiny app from the current directory
Rscript -e "shiny::runApp('%CUR_DIR%/app.R', launch.browser=TRUE)"
pause
