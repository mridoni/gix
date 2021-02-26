@echo off
REM *** usage: cb2xml <copybookFileName> [debug]
REM *** java.exe should be in your PATH
REM *** to run demo copybook after extracting CB2XML distribution:
REM *** change directory to this "bin" directory and type "cb2xml ../etc/sample.txt"

java -jar cb2xml.jar %1 %2
