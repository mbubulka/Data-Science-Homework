@echo off
echo Creating PowerPoint presentation from R Markdown...
echo.

REM Try to find R installation
set R_PATH=""
if exist "C:\Program Files\R\R-*\bin\Rscript.exe" (
    for /f "delims=" %%i in ('dir "C:\Program Files\R\R-*\bin\Rscript.exe" /b /o-n 2^>nul ^| findstr /r ".*"') do (
        set R_PATH=C:\Program Files\R\%%i
        goto :found
    )
)

if exist "C:\Program Files\R\R-*\bin\x64\Rscript.exe" (
    for /f "delims=" %%i in ('dir "C:\Program Files\R\R-*\bin\x64\Rscript.exe" /b /o-n 2^>nul ^| findstr /r ".*"') do (
        set R_PATH=C:\Program Files\R\%%i\x64
        goto :found
    )
)

:found
if %R_PATH%=="" (
    echo R not found in standard locations. Please run the following command manually:
    echo.
    echo In R Studio or R Console:
    echo rmarkdown::render("potomac_presentation.Rmd")
    echo.
    pause
    exit /b
)

echo Found R at: %R_PATH%
echo.

REM Run the R command to create PowerPoint
"%R_PATH%\Rscript.exe" -e "if (!require(rmarkdown)) install.packages('rmarkdown'); rmarkdown::render('potomac_presentation.Rmd')"

if exist "potomac_presentation.pptx" (
    echo.
    echo SUCCESS! PowerPoint presentation created: potomac_presentation.pptx
    echo.
    echo To complete your presentation:
    echo 1. Open potomac_presentation.pptx
    echo 2. Replace image placeholders with your dashboard screenshots
    echo 3. Customize formatting as needed
    echo 4. Practice timing for 4-8 minute requirement
    echo.
) else (
    echo.
    echo If the automatic method didn't work, try this in R Studio:
    echo rmarkdown::render("potomac_presentation.Rmd")
    echo.
)

pause