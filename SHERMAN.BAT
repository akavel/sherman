@echo off
if "%1" == "run" goto run_script
if "%1" == "compile" goto compile_and_exit

mzscheme
goto end

:compile_and_exit
mzscheme -q -e "(require \"collects/sherman/sherman.ss\")" -e "(sherman \"%2\")" -e "(exit)"
goto end

:run_script
mzscheme -m -q -e "(require \"collects/sherman/runtime.ss\")" -e "(parameterize ((current-namespace sherman-namespace)) (load \"%2\"))" -e "(current-namespace sherman-namespace)" -- %3 %4 %5
goto end

:end
